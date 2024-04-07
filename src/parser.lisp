(defpackage #:tokyo.tojo.json/parser
  (:use #:coalton
        #:coalton-prelude)
  (:local-nicknames
   (#:map #:coalton-library/ord-map)
   (#:iter #:coalton-library/iterator)
   (#:str #:coalton-library/string)
   (#:list #:coalton-library/list)
   (#:fn #:coalton-library/functions)
   (#:char #:coalton-library/char)
   (#:optional #:coalton-library/optional)
   (#:json #:tokyo.tojo.json/json)
   (#:parser #:tokyo.tojo.json/private/parser))
  (:export #:parse
           #:parse!))

(in-package #:tokyo.tojo.json/parser)

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

  (define whitespace-parser (parser:delay (whitespace-parser_)))

  (define (whitespace-parser_)
    (parser:from-guard
     (alt*
      (parser:guard-char whitespace?
                         (>> parser:read-char
                             (parser:delay (whitespace-parser_))))
      (parser:guard-else (pure Unit)))))

  (declare parse-hex (String -> (Optional UFix)))
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

  (declare take-until-parser ((Char -> Boolean) -> (parser:Parser String)))
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

  (declare word-parser (parser:Parser String))
  (define word-parser (parser:delay (take-until-parser sep?)))

  (declare null-parser (parser:Parser Unit))
  (define null-parser
    (do (word <- (non-empty-string word-parser))
        (if (== word "null")
            (pure Unit)
            (fail (message-with "Unexpected string" word)))))

  (declare true-parser (parser:Parser Boolean))
  (define true-parser
    (do (word <- (non-empty-string word-parser))
         (if (== word "true")
             (pure True)
             (fail (message-with "Unexpected empty string" word)))))

  (declare false-parser (parser:Parser Boolean))
  (define false-parser
    (do (word <- (non-empty-string word-parser))
        (if (== word "false")
            (pure False)
            (fail (message-with "Unexpected string" word)))))

  (declare escape-char-map (map:Map Char String))
  (define escape-char-map
    (foldr (fn ((Tuple k v) acc)
             (map:insert-or-replace acc k v))
           map:empty
           (make-list
            (Tuple #\" "\"")
            (Tuple #\\ "\\")
            (Tuple #\/ "/")
            (Tuple #\b (into (make-list #\backspace)))
            (Tuple #\f (into (make-list #\page)))
            (Tuple #\n (into (make-list #\newline)))
            (Tuple #\r (into (make-list #\return)))
            (Tuple #\t (into (make-list #\tab))))))

  (define (escape-char? c)
    (optional:some? (map:lookup escape-char-map c)))

  (declare take-parser (UFix -> (parser:Parser string)))
  (define (take-parser n)
    (map into
         (foldr (liftA2 Cons)
                (pure nil)
                (list:repeat n parser:peek-char))))

  (declare string-parser (parser:Parser String))
  (define string-parser
    (let ((declare string-list-parser (Unit -> (parser:Parser (List String))))
          (string-list-parser
            (fn ()
              (parser:from-guard
               (alt* (parser:guard-char (== #\")
                                        (>> parser:read-char
                                            (pure (make-list))))
                     (parser:guard-char (== #\\)
                                        (>> parser:read-char
                                            (string-list/escape-parser)))
                     (parser:guard-char (const True)
                                        (do (str <- (take-until-parser (disjoin (== #\\) (== #\"))))
                                            (liftA2 Cons (pure str) (string-list-parser))))))))

          (declare string-list/escape-parser (Unit -> (parser:Parser (List String))))
          (string-list/escape-parser
            (fn ()
              (parser:from-guard
               (alt*
                (parser:guard-lookup escape-char-map
                                     (fn (str)
                                       (>> parser:read-char
                                           (liftA2 cons (pure str) (string-list-parser)))))
                (parser:guard-char (== #\u)
                                   (do parser:read-char
                                       (str <- (take-parser 4))
                                     (match (>>= (parse-hex str) char:code-char)
                                       ((None)
                                        (fail (message-with "Unexpected string" str)))
                                       ((Some c)
                                        (liftA2 Cons
                                                (pure (into (make-list c)))
                                                (string-list-parser)))))))))))
      (do parser:read-char
          (lst <- (string-list-parser))
        (pure (mconcat lst)))))

  (declare array-parser (parser:Parser (List json:JSON)))
  (define array-parser (parser:delay (array-parser_)))

  (declare array-parser_ (Unit -> (parser:Parser (List json:JSON))))
  (define (array-parser_)
    (let ((declare elements-parser (Unit -> (parser:Parser (List json:JSON))))
          (elements-parser
            (fn ()
              (do (value <- json-parser)
                  whitespace-parser
                (parser:from-guard
                 (alt* (parser:guard-char (== #\])
                                          (>> parser:read-char
                                              (pure (make-list value))))
                       (parser:guard-char (== #\,)
                                          (>> parser:read-char
                                              (liftA2 Cons (pure value) (elements-parser))))))))))
      (do parser:read-char
          whitespace-parser
        (parser:from-guard
         (alt* (parser:guard-char (== #\])
                                  (>> parser:read-char
                                      (pure Nil)))
               (parser:guard-char (const True)
                                  (parser:delay (elements-parser))))))))

  (define object-parser (parser:delay (object-parser_)))

  (define (object-parser_)
    (let ((declare key-parser (parser:Parser String))
          (key-parser string-parser)

          (declare value-parser (parser:Parser json:JSON))
          (value-parser (>> whitespace-parser json-parser))

          (declare key-value-parser (parser:Parser (Tuple String json:JSON)))
          (key-value-parser
            (do (key <- key-parser)
                whitespace-parser
              (parser:from-guard
               (parser:guard-char (== #\:)
                                  (do parser:read-char
                                      (value <- value-parser)
                                    (pure (Tuple key value)))))))

          (declare map-parser (Unit -> parser:Parser (map:Map String json:JSON)))
          (map-parser
            (fn ()
              (do whitespace-parser
                  (parser:from-guard
                   ((parser:guard-char (== #\"))
                    (do ((Tuple key value) <- key-value-parser)
                        whitespace-parser
                      (parser:from-guard
                       (alt* (parser:guard-char (== #\,)
                                                (do parser:read-char
                                                    (map <- (map-parser))
                                                  (pure (map:insert-or-replace map key value))))
                             (parser:guard-char (== #\})
                                                (do parser:read-char
                                                    (pure (map:insert-or-replace map:empty key value)))))))))))))
      (do parser:read-char
          whitespace-parser
        (parser:from-guard
         (alt* (parser:guard-char (== #\})
                                  (>> parser:read-char (pure map:empty)))
               (parser:guard-char (const True)
                                  (parser:delay (map-parser))))))))

  (declare digits-parser (parser:Parser String))
  (define digits-parser
    (let length>0-check =
      (fn (str)
        (if (< 0 (str:length str))
            (pure str)
            (fail "Unexpected empty string"))))
    (>>= (take-until-parser (fn:complement digit?))
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

  (declare parse-float (String -> String -> String -> (Optional Double-Float)))
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

  (declare number-parser (parser:Parser Double-Float))
  (define number-parser
    (let ((declare integer-parser (String -> parser:Parser Double-Float))
          (integer-parser
            (fn (head)
              (match (str:parse-int head)
                ((None) (fail (message-with "Unexpected string" head)))
                ((Some int)
                 (match (tryInto int)
                   ((Ok d) (pure d))
                   ((Err _) (fail (message-with "Unexpected string" head))))))))

          (declare float-parser (String -> String -> String -> parser:Parser Double-Float))
          (float-parser
            (fn (head fraction exponent)
              (match (parse-float head fraction exponent)
                ((None)
                 (fail (message-with "Unexpected string" (mconcat
                                                          (make-list head "." fraction "e" exponent)))))
                ((Some float)
                 (pure float)))))

          (declare head-parser (parser:Parser String))
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

          (declare fraction-parser (String -> (parser:Parser Double-Float)))
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

          (declare exponent-parser (String -> String -> (parser:Parser Double-Float)))
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
                    (parser:guard-char (const True)
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

  (declare json-parser (parser:Parser json:JSON))
  (define json-parser
    (>> whitespace-parser
        (parser:from-guard
         (alt*
          (parser:guard-char (== #\n)
                             (>> null-parser (pure json:Null)))
          (parser:guard-char (== #\t)
                             (map into true-parser))
          (parser:guard-char (== #\f)
                             (map into false-parser))
          (parser:guard-char (disjoin digit? (== #\-))
                             (map into number-parser))
          (parser:guard-char (== #\")
                             (map into string-parser))
          (parser:guard-char (== #\[)
                             (map into array-parser))
          (parser:guard-char (== #\{)
                             (map into object-parser))))))

  (declare parse! (iter:Iterator Char -> (Result parser:Error json:JSON)))
  (define (parse! iter)
    (parser:run! json-parser
                 (parser:make-stream! iter)))

  (declare parse (String -> (Result parser:Error json:JSON)))
  (define (parse str)
    (parse! (iter:into-iter str))))
