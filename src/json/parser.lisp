(in-package #:tokyo.tojo.json/json)

(named-readtables:in-readtable coalton:coalton)

(cl:defmacro alt* (cl:&rest xs)
  (cl:check-type xs cl:list)
  (cl:if (cl:null xs)
         'empty
         `(alt ,(cl:first xs)
               (alt* ,@(cl:rest xs)))))

(coalton-toplevel
  (declare digit? (Char -> Boolean))
  (define (digit? c)
    (char:ascii-digit? c))

  (declare digit1-9? (Char -> Boolean))
  (define (digit1-9? c)
    (and (char:ascii-digit? c)
         (not (== c #\0))))

  (define (message-with msg c)
    (<> msg (<> ": " (into c))))

  (define (whitespace? c)
    (or (== c #\return)
        (== c #\tab)
        (== c #\newline)
        (== c #\space)))

  (declare whitespace-parser (parser:Parser :s Unit))
  (define whitespace-parser
    (>> (parser:collect-while
         (fn (c)
           (if (whitespace? c)
               (Some parser:read-char )
               None)))
        (pure Unit)))

  (declare parse-hex (coalton:String -> (Optional UFix)))
  (define (parse-hex str)
    (>>= (lisp (Optional Integer) (str)
           (cl:handler-case
               (cl:let ((value (cl:parse-integer str :radix 16)))
                 (cl:check-type value cl:integer)
                 (coalton (Some (lisp Integer () value))))
             (cl:error (e)
               (cl:declare (cl:ignore e))
               (coalton None))))
         (.> tryInto as-optional)))

  (define (non-empty-string parser)
    (>>= parser
         (fn (str)
           (if (< 0 (str:length str))
               (pure str)
               (fail "Unexpected empty string")))))

  (declare take-until-parser ((Char -> Boolean) -> (parser:Parser :s coalton:String)))
  (define (take-until-parser end?)
    (parser:take-until-string end?))

  (define (sep? c)
    (or (whitespace? c)
        (== c #\{)
        (== c #\})
        (== c #\[)
        (== c #\])
        (== c #\,)
        (== c #\")))

  (declare word-parser (parser:Parser :s coalton:String))
  (define word-parser (parser:delay (take-until-parser sep?)))

  (declare null-parser (parser:Parser :s Unit))
  (define null-parser
    (do (word <- (non-empty-string word-parser))
        (if (== word "null")
            (pure Unit)
            (fail (message-with "Unexpected string" word)))))

  (declare true-parser (parser:Parser :s Boolean))
  (define true-parser
    (do (word <- (non-empty-string word-parser))
        (if (== word "true")
            (pure coalton:True)
            (fail (message-with "Unexpected empty string" word)))))

  (declare false-parser (parser:Parser :s Boolean))
  (define false-parser
    (do (word <- (non-empty-string word-parser))
        (if (== word "false")
            (pure coalton:False)
            (fail (message-with "Unexpected string" word)))))

  (declare escape-char-map (map:Map Char Char))
  (define escape-char-map
    (foldr (fn ((Tuple k v) acc)
             (map:insert-or-replace acc k v))
           map:empty
           (make-list
            (Tuple #\" #\")
            (Tuple #\\ #\\)
            (Tuple #\/ #\/)
            (Tuple #\b #\backspace)
            (Tuple #\f #\page)
            (Tuple #\n #\newline)
            (Tuple #\r #\return)
            (Tuple #\t #\tab))))

  (define (escape-char? c)
    (optional:some? (map:lookup escape-char-map c)))

  (declare take-parser (UFix -> (parser:Parser :s coalton:String)))
  (define (take-parser n)
    (map into (sequence (list:repeat n parser:peek-char))))

  (declare string-parser (parser:Parser :s coalton:String))
  (define string-parser
    (let ((declare escaped-char-parser (parser:Parser :s coalton:String))
          (escaped-char-parser
            (parser:from-guard
             (alt* (parser:guard-lookup escape-char-map
                                        (fn (c)
                                          (>> parser:read-char
                                              (pure (into (make-list c))))))
                   (parser:guard-char (== #\u)
                                      (do parser:read-char
                                          (str <- (take-parser 4))
                                        (match (>>= (parse-hex str) char:code-char)
                                          ((None)
                                           (fail (message-with "Unexpected string" str)))
                                          ((Some c)
                                           (pure (into (make-list c))))))))))
          (declare substring-parser (parser:Parser :s coalton:String))
          (substring-parser
            (parser:from-guard
             (alt* (parser:guard-char (== #\\)
                                      (>> parser:read-char
                                          escaped-char-parser))
                   (parser:guard-char (const coalton:True)
                                      (take-until-parser (disjoin (== #\\) (== #\"))))))))
      (>> parser:read-char
          (>>= (parser:collect-while
                (fn (c)
                  (if (== c #\")
                      None
                      (Some substring-parser))))
               (fn (lst)
                 (>> parser:read-char
                     (pure (mconcat lst))))))))

  (declare array-parser (UFix -> parser:Parser :s (List JSON)))
  (define (array-parser n)
    (let ((element-list-parser
            (parser:delay
             (do (h <- (json-parser (1- n)))
                 (t <- (parser:collect-while
                        (fn (c)
                          (if (== c #\,)
                              (Some
                               (>> parser:read-char
                                   (>>= (json-parser (1- n))
                                        (fn (v)
                                          (>> whitespace-parser (pure v))))))
                              None))))
               (parser:from-guard
                (parser:guard-char (== #\])
                                   (>> parser:read-char
                                       (pure (Cons h t)))))))))
      (>> (>> parser:read-char
              whitespace-parser)
          (parser:from-guard
           (alt* (parser:guard-char (== #\]) (pure Nil))
                 (parser:guard-char (const coalton:True)
                                    element-list-parser))))))

  (declare object-parser (UFix -> parser:Parser :s (map:Map coalton:String JSON)))
  (define (object-parser n)
    (let ((declare key-value-parser (parser:Parser :s (Tuple coalton:String JSON)))
          (key-value-parser
            (parser:delay
             (do (key <- string-parser)
                 whitespace-parser
               (parser:from-guard
                (parser:guard-char (== #\:)
                                   (do parser:read-char
                                       (value <- (json-parser (1- n)))
                                    whitespace-parser
                                     (pure (Tuple key value))))))))
          (declare key-value-list-parser (parser:Parser :s (List (Tuple coalton:String JSON))))
          (key-value-list-parser
            (parser:delay
             (do (h <- key-value-parser)
                 (t <- (parser:collect-while
                        (fn (c)
                          (if (== c #\,)
                              (Some (do parser:read-char
                                        whitespace-parser
                                     key-value-parser))
                              None))))
               (parser:from-guard
                (parser:guard-char (== #\})
                                   (>> parser:read-char
                                       (pure (Cons h t)))))))))
      (do parser:read-char
          whitespace-parser
        (parser:from-guard
         (alt* (parser:guard-char (== #\}) (pure map:empty))
               (parser:guard-char (const coalton:True)
                                  (map (.< map:collect! iter:into-iter)
                                       key-value-list-parser)))))))

  (declare digits-parser (parser:Parser :s coalton:String))
  (define digits-parser
    (let length>0-check =
      (fn (str)
        (if (< 0 (str:length str))
            (pure str)
            (fail "Unexpected empty string"))))
    (>>= (take-until-parser (complement digit?))
         (fn (str)
           (parser:from-guard
            (alt*
             (parser:guard-eof (parser:delay (length>0-check str)))
             (parser:guard-char (fn (c)
                                  (or (sep? c)
                                      (== c #\.)
                                      (== c #\e)
                                      (== c #\E)))
                                (length>0-check str)))))))

  (declare parse-float (coalton:String -> coalton:String -> coalton:String -> (Optional Double-Float)))
  (define (parse-float head fraction exponent)
    (let str = (mconcat (make-list head "." fraction "d" exponent)))
    (lisp (Optional Double-Float) (str)
      (cl:handler-case
          (cl:let ((cl:*read-eval* cl:nil))
            (cl:let ((value (cl:read-from-string str)))
              (cl:check-type value cl:double-float)
              (coalton (Some (lisp Double-Float () value)))))
        (cl:error (e)
          (cl:declare (cl:ignore e))
          (coalton None)))))

  (declare number-parser (parser:Parser :s Double-Float))
  (define number-parser
    (let ((declare integer-parser (coalton:String -> parser:Parser :s Double-Float))
          (integer-parser
            (fn (head)
              (match (str:parse-int head)
                ((None) (fail (message-with "Unexpected string" head)))
                ((Some int)
                 (match (tryInto int)
                   ((Ok d) (pure d))
                   ((Err _) (fail (message-with "Unexpected string" head))))))))

          (declare float-parser (coalton:String -> coalton:String -> coalton:String -> parser:Parser :s Double-Float))
          (float-parser
            (fn (head fraction exponent)
              (match (parse-float head fraction exponent)
                ((None)
                 (fail (message-with "Unexpected string" (mconcat
                                                          (make-list head "." fraction "e" exponent)))))
                ((Some float)
                 (pure float)))))

          (declare head-parser (parser:Parser :s coalton:String))
          (head-parser
            (let ((sign-parser
                    (fn (continue-parser)
                      (parser:from-guard
                       (alt* (parser:guard-char (== #\-)
                                                (>> parser:read-char
                                                    (liftA2 <> (pure "-") continue-parser)))
                             (parser:guard-else continue-parser)))))
                  (main-parser
                    (parser:from-guard
                     (alt* (parser:guard-char (== #\0)
                                              (do (digits <- digits-parser)
                                                  (if (== digits "0")
                                                      (pure digits)
                                                      (fail (message-with "Unexpected string" digits)))))
                           (parser:guard-char digit1-9? digits-parser)))))
              (sign-parser main-parser)))

          (declare fraction-parser (coalton:String -> (parser:Parser :s Double-Float)))
          (fraction-parser
            (fn (head)
              (>> parser:read-char
                  (>>= digits-parser
                       (fn (fraction)
                         (parser:from-guard
                          (alt*
                           (parser:guard-eof (parser:delay (float-parser head fraction "0")))
                           (parser:guard-char (disjoin (== #\e) (== #\E))
                                              (parser:delay (exponent-parser head fraction)))
                           (parser:guard-char sep?
                                              (parser:delay (float-parser head fraction "0"))))))))))

          (declare exponent-parser (coalton:String -> coalton:String -> (parser:Parser :s Double-Float)))
          (exponent-parser
            (fn (head fraction)
              (>> parser:read-char
                  (parser:from-guard
                   (alt*
                    (parser:guard-char (== #\+)
                                       (>> parser:read-char
                                           (>>= digits-parser (float-parser head fraction))))
                    (parser:guard-char (== #\-)
                                       (>> parser:read-char
                                           (>>= (map (<> "-") digits-parser)
                                                (float-parser head fraction))))
                    (parser:guard-char (const coalton:True)
                                       (>>= digits-parser
                                            (float-parser head fraction)))))))))

      (>>= head-parser
           (fn (head)
             (parser:from-guard
              (alt*
               (parser:guard-eof (parser:delay (integer-parser head)))
               (parser:guard-char (== #\.)
                                  (parser:delay (fraction-parser head)))
               (parser:guard-char (disjoin (== #\e) (== #\E))
                                  (parser:delay (exponent-parser head "0")))
               (parser:guard-char sep?
                                  (parser:delay (integer-parser head)))))))))

  (declare json-parser (Ufix -> parser:Parser :s JSON))
  (define (json-parser n)
    (if (== n 0)
        (fail "Nesting depth exceeded")
        (>> whitespace-parser
            (parser:from-guard
             (alt*
              (parser:guard-char (== #\n)
                                 (>> null-parser (pure Null)))
              (parser:guard-char (== #\t)
                                 (map into true-parser))
              (parser:guard-char (== #\f)
                                 (map into false-parser))
              (parser:guard-char (disjoin digit? (== #\-))
                                 (map into number-parser))
              (parser:guard-char (== #\")
                                 (map into string-parser))
              (parser:guard-char (== #\[)
                                 (map into (parser:delay (array-parser n))))
              (parser:guard-char (== #\{)
                                 (map into (parser:delay (object-parser n)))))))))

  (declare parse! (iter:Iterator Char -> (Result coalton:String JSON)))
  (define (parse! iter)
    (parser:run! (json-parser 1024)
                 (parser:make-port! iter)))

  (declare parse (coalton:String -> (Result coalton:String JSON)))
  (define (parse str)
    (parse! (iter:into-iter str)))

  (define-instance (TryInto coalton:String JSON)
    (define tryInto parse)))
