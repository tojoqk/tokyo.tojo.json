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

  (declare whitespace-parser (parser:Parser Unit))
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

  (declare take-until-parser ((Char -> Boolean) -> (parser:Parser coalton:String)))
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

  (declare word-parser (parser:Parser coalton:String))
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
            (pure coalton:True)
            (fail (message-with "Unexpected empty string" word)))))

  (declare false-parser (parser:Parser Boolean))
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

  (declare take-parser (UFix -> (parser:Parser coalton:String)))
  (define (take-parser n)
    (map into (sequence (list:repeat n parser:peek-char))))

  (declare string-parser (parser:Parser coalton:String))
  (define string-parser
    (let ((declare escaped-char-parser (parser:Parser coalton:String))
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
          (declare substring-parser (parser:Parser coalton:String))
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

  (declare digits-parser (parser:Parser coalton:String))
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

  (declare number-parser (parser:Parser Double-Float))
  (define number-parser
    (let ((declare integer-parser (coalton:String -> parser:Parser Double-Float))
          (integer-parser
            (fn (head)
              (match (str:parse-int head)
                ((None) (fail (message-with "Unexpected string" head)))
                ((Some int)
                 (match (tryInto int)
                   ((Ok d) (pure d))
                   ((Err _) (fail (message-with "Unexpected string" head))))))))

          (declare float-parser (coalton:String -> coalton:String -> coalton:String -> parser:Parser Double-Float))
          (float-parser
            (fn (head fraction exponent)
              (match (parse-float head fraction exponent)
                ((None)
                 (fail (message-with "Unexpected string" (mconcat
                                                          (make-list head "." fraction "e" exponent)))))
                ((Some float)
                 (pure float)))))

          (declare head-parser (parser:Parser coalton:String))
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

          (declare fraction-parser (coalton:String -> (parser:Parser Double-Float)))
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

          (declare exponent-parser (coalton:String -> coalton:String -> (parser:Parser Double-Float)))
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

  (define-type State
    Start-Parse-Array
    Continue-Parse-Array
    Start-Parse-Object
    Continue-Parse-Object)

  (declare zipper-parser (parser:Parser Zipper))
  (define zipper-parser
    (let ((parse-atom
            (fn (parser)
              (the (parser:Parser JSON) (map into parser))))
          (parse-json
            (do whitespace-parser
                (c <- parser:peek-char)
                (cond
                  ((== c #\n) (parse-atom null-parser))
                  ((== c #\t) (parse-atom true-parser))
                  ((== c #\f) (parse-atom false-parser))
                  ((or (digit? c) (== c #\-)) (parse-atom number-parser))
                  ((== c #\") (parse-atom string-parser))
                  ((== c #\[)
                   (do parser:read-char
                       (pure (into (Array Nil)))))
                  ((== c #\{)
                   (do parser:read-char
                       (pure (into (Object map:empty)))))
                  (coalton:True
                   (fail (message-with "Unexpected Char" (singleton c)))))))
          (declare key-value-parser (parser:Parser (Tuple coalton:String JSON)))
          (key-value-parser
            (parser:delay
             (do (key <- string-parser)
                 whitespace-parser
                 (parser:from-guard
                  (parser:guard-char (== #\:)
                                     (do parser:read-char
                                         (value <- parse-json)
                                         whitespace-parser
                                         (pure (Tuple key value))))))))
          (declare parse (State -> Zipper -> parser:Parser (Tuple (Optional State) Zipper)))
          (parse
            (fn (op z)
              (match op
                ((Start-Parse-Array)
                 (do whitespace-parser
                     (ch <- parser:peek-char)
                     (cond
                       ((== ch #\])
                        (do parser:read-char
                            (match z
                              ((Zipper _ (CrumbTop))
                               (pure (Tuple None z)))
                              ((Zipper _ (CrumbArray _ _ _))
                               (pure (Tuple (Some Continue-Parse-Array) z)))
                              ((Zipper _ (CrumbObject _ _ _ _))
                               (pure (Tuple (Some Continue-Parse-Object) z))))))
                       (coalton:True
                        (do (let (Zipper _ cr) = z)
                            (j <- parse-json)
                            (let ((new-zipper (Zipper j (CrumbArray cr Nil Nil)))
                                  (continue (pure (Tuple (Some Continue-Parse-Array) new-zipper))))
                              (match j
                                ((Null) continue)
                                ((True) continue)
                                ((False) continue)
                                ((Number _) continue)
                                ((String _) continue)
                                ((Array _) (pure (Tuple (Some Start-Parse-Array) new-zipper)))
                                ((Object _) (pure (Tuple (Some Start-Parse-Object) new-zipper))))))))))
                ((Continue-Parse-Array)
                 (match z
                   ((Zipper x (CrumbArray cr l r))
                    (do whitespace-parser
                        (ch <- parser:peek-char)
                        (cond
                          ((== ch #\])
                           (do parser:read-char
                               (let ((new-zipper (Zipper (Array (append (reverse l) (Cons x r)))
                                                         cr)))
                                 (match cr
                                   ((CrumbTop) (pure (Tuple None new-zipper)))
                                   ((CrumbArray _ _ _) (pure (Tuple (Some Continue-Parse-Array) new-zipper)))
                                   ((CrumbObject _ _ _ _) (pure (Tuple (Some Continue-Parse-Object) new-zipper)))))))
                          ((== ch #\,)
                           (do parser:read-char
                               (j <- parse-json)
                               (let ((new-zipper (Zipper j (CrumbArray cr (Cons x l) r)))
                                     (continue (pure (Tuple (Some Continue-Parse-Array) new-zipper))))
                                 (match j
                                   ((Null) continue)
                                   ((True) continue)
                                   ((False) continue)
                                   ((Number _) continue)
                                   ((String _) continue)
                                   ((Array _) (pure (Tuple (Some Start-Parse-Array) new-zipper)))
                                   ((Object _) (pure (Tuple (Some Start-Parse-Object) new-zipper)))))))
                          (coalton:True
                           (fail (message-with "Unexpected char" (singleton ch)))))))
                   (_ (coalton-prelude:error "zipper-parser: program error (Continue-Parse-Array)"))))
                ((Start-Parse-Object)
                 (do whitespace-parser
                     (ch <- parser:peek-char)
                     (cond
                       ((== ch #\})
                        (do parser:read-char
                            (match z
                              ((Zipper _ (CrumbTop))
                               (pure (Tuple None z)))
                              ((Zipper _ (CrumbArray _ _ _))
                               (pure (Tuple (Some Continue-Parse-Array) z)))
                              ((Zipper _ (CrumbObject _ _ _ _))
                               (pure (Tuple (Some Continue-Parse-Object) z))))))
                       (coalton:True
                        (do (let (Zipper _ cr) = z)
                            ((Tuple key j) <- key-value-parser)
                            (let ((new-zipper (Zipper j (CrumbObject cr key Nil Nil)))
                                  (continue (pure (Tuple (Some Continue-Parse-Object) new-zipper))))
                              (match j
                                ((Null) continue)
                                ((True) continue)
                                ((False) continue)
                                ((Number _) continue)
                                ((String _) continue)
                                ((Array _) (pure (Tuple (Some Start-Parse-Array) new-zipper)))
                                ((Object _) (pure (Tuple (Some Start-Parse-Object) new-zipper))))))))))
                ((Continue-Parse-Object)
                 (match z
                   ((Zipper x (CrumbObject cr xk l r))
                    (do whitespace-parser
                        (ch <- parser:peek-char)
                        (cond
                          ((== ch #\})
                           (do parser:read-char
                               (let ((new-zipper (Zipper (Object
                                                          (map:collect!
                                                           (iter:into-iter (append (reverse l) (Cons (Tuple xk x) r)))))
                                                         cr)))
                                 (match cr
                                   ((CrumbTop) (pure (Tuple None new-zipper)))
                                   ((CrumbArray _ _ _) (pure (Tuple (Some Continue-Parse-Array) new-zipper)))
                                   ((CrumbObject _ _ _ _) (pure (Tuple (Some Continue-Parse-Object) new-zipper)))))))
                          ((== ch #\,)
                           (do parser:read-char
                               whitespace-parser
                               ((Tuple key j) <- key-value-parser)
                               (let ((new-zipper (Zipper j (CrumbObject cr key (Cons (Tuple xk x) l) r)))
                                     (continue (pure (Tuple (Some Continue-Parse-Object) new-zipper))))
                                 (match j
                                   ((Null) continue)
                                   ((True) continue)
                                   ((False) continue)
                                   ((Number _) continue)
                                   ((String _) continue)
                                   ((Array _) (pure (Tuple (Some Start-Parse-Array) new-zipper)))
                                   ((Object _) (pure (Tuple (Some Start-Parse-Object) new-zipper)))))))
                          (coalton:True
                           (fail (message-with "Unexpected char" (singleton ch)))))))
                   (_ (coalton-prelude:error "zipper-parser: program error (Continue-Parse-Object)"))))))))
      (do (j <- parse-json)
          (match j
            ((Null) (pure (into j)))
            ((True) (pure (into j)))
            ((False) (pure (into j)))
            ((Number _) (pure (into j)))
            ((String _) (pure (into j)))
            ((Array _)
             (parser:fold-while parse
                                Start-Parse-Array
                                (into j)))
            ((Object _)
             (parser:fold-while parse
                                Start-Parse-Object
                                (into j)))))))

  (declare json-parser (parser:Parser JSON))
  (define json-parser
    (map into zipper-parser))

  (declare parse! (iter:Iterator Char -> (Result coalton:String JSON)))
  (define (parse! iter)
    (result:map-err (fn (e)
                      (match e
                        ((parser:UnexpectedEof) "Unexpected eof")
                        ((parser:Message s) s)))
                    (parser:run! json-parser
                                 (parser:make-stream! iter))))

  (declare parse (coalton:String -> (Result coalton:String JSON)))
  (define (parse str)
    (parse! (iter:into-iter str)))

  (define-instance (TryInto coalton:String JSON)
    (define tryInto parse)))
