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
   (#:json #:tokyo.tojo.json/json)
   (#:parser #:tokyo.tojo.json/private/parser))
  (:export #:parse
           #:parse!))

(in-package #:tokyo.tojo.json/parser)

(named-readtables:in-readtable coalton:coalton)

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
    (>>= parser:peek-char-or-eof
         (fn (opt)
           (match opt
             ((None) (pure Unit))
             ((Some c)
              (if (whitespace? c)
                  (>> parser:read-char
                      (whitespace-parser_))
                  (pure Unit)))))))

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
    (>>= (non-empty-string word-parser)
         (fn (word)
           (if (== word "null")
               (pure Unit)
               (fail (message-with "Unexpected string" word))))))

  (declare true-parser (parser:Parser Boolean))
  (define true-parser
    (>>= (non-empty-string word-parser)
         (fn (word)
           (if (== word "true")
               (pure True)
               (fail (message-with "Unexpected empty string" word))))))

  (declare false-parser (parser:Parser Boolean))
  (define false-parser
    (>>= (non-empty-string word-parser)
         (fn (word)
           (if (== word "false")
               (pure False)
               (fail (message-with "Unexpected string" word))))))

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

  (declare take-parser (UFix -> (parser:Parser string)))
  (define (take-parser n)
    (map into
         (foldr (liftA2 Cons)
                (pure (make-list))
                (list:repeat n parser:peek-char))))

  (define string-parser (parser:delay (string-parser_)))

  (declare string-parser_ (Unit -> parser:Parser String))
  (define (string-parser_)
    (let ((declare start-parser (Unit -> (parser:Parser (List String))))
          (start-parser
            (fn ()
              (let ((escape-parser
                      (>>= parser:read-char
                           (fn (c)
                             (match (map:lookup escape-char-map c)
                               ((Some str) (liftA2 cons (pure str) (start-parser)))
                               ((None)
                                (if (== c #\u)
                                    (>>= (take-parser 4)
                                         (fn (str)
                                           (match (>>= (parse-hex str) char:code-char)
                                             ((None)
                                              (fail (message-with "Unexpected string" str)))
                                             ((Some c)
                                              (liftA2 Cons
                                                      (pure (into (make-list c)))
                                                      (start-parser))))))
                                    (fail (message-with "Unexpected char" (make-list c)))))))))
                    (str-parser
                      (take-until-parser
                       (fn (c)
                         (or (== c #\\)
                             (== c #\"))))))
                (>>= parser:peek-char
                     (fn (c)
                       (cond ((== c #\")
                              (>> parser:read-char
                                  (pure (make-list))))
                             ((== c #\\)
                              (>> parser:read-char
                                  escape-parser))
                             (True
                              (>>= str-parser
                                   (fn (str)
                                     (liftA2 Cons (pure str) (start-parser))))))))))))
      (>>= (>> parser:read-char
               (start-parser))
           (fn (lst)
             (pure (mconcat lst))))))

  (declare array-parser (parser:Parser (List json:JSON)))
  (define array-parser
    (let ((start-parser
            (fn ()
              (let ((continue-parser
                      (fn (value)
                        (>>= (>> whitespace-parser
                                 parser:read-char)
                             (fn (c)
                               (cond
                                 ((== c #\]) (pure (make-list value)))
                                 ((== c #\,) (liftA2 Cons (pure value) (start-parser)))
                                 (True (fail (message-with "Unexpected char" (make-list c))))))))))
                (>>= json-parser continue-parser)))))
      (>> parser:read-char
          (>> whitespace-parser
              (>>= parser:peek-char
                   (fn (c)
                     (if (== c #\])
                         (>> parser:read-char
                             (pure Nil))
                         (start-parser))))))))

  (define object-parser (parser:delay (object-parser_)))

  (declare object-parser_ (Unit -> parser:Parser (map:Map String json:JSON)))
  (define (object-parser_)
    (let key-parser = string-parser)
    (let value-parser = (>> whitespace-parser json-parser))
    (let key-value-parser =
      (>>= key-parser
           (fn (key)
             (>>= (>> whitespace-parser
                      parser:read-char)
                  (fn (c)
                    (if (== c #\:)
                        (>>= value-parser
                             (fn (value)
                               (pure (Tuple key value))))
                        (fail (message-with "Unexpected char" (make-list c)))))))))
    (let ((start-parser
            (fn ()
              (let ((continue-parser
                      (fn ((Tuple key value))
                        (>>= (>> whitespace-parser
                                 parser:read-char)
                             (fn (c)
                               (match c
                                 (#\,
                                  (>>= (start-parser)
                                       (fn (map)
                                         (pure (map:insert-or-replace map key value)))))
                                 (#\}
                                  (pure (map:insert-or-replace map:empty key value)))
                                 (_
                                  (fail (message-with "Unexpected char" (make-list c))))))))))
                (>>= (>> whitespace-parser
                         parser:peek-char)
                     (fn (c)
                       (match c
                         (#\"
                          (>>= key-value-parser continue-parser))
                         (_
                          (fail (message-with "Unexpected char" (make-list c)))))))))))
      (>>= (>> parser:read-char
               (>> whitespace-parser
                   parser:peek-char))
           (fn (c)
             (match c
               (#\}
                (>> parser:read-char
                    (pure map:empty)))
               (_ (start-parser)))))))


  (declare digits-parser (parser:Parser String))
  (define digits-parser
    (let length>0-check =
      (fn (str)
        (if (< 0 (str:length str))
            (pure str)
            (fail "Unexpected empty string"))))
    (>>= (take-until-parser (fn:complement digit?))
         (fn (str)
           (>>= parser:peek-char-or-eof
                (fn (opt)
                  (match opt
                    ((None) (length>0-check str))
                    ((Some c)
                     (if (or (sep? c)
                             (== c #\.)
                             (== c #\e)
                             (== c #\E))
                         (length>0-check str)
                         (fail (message-with "Unexpected char" (make-list c)))))))))))

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
    (let integer-parser =
      (fn (head)
        (match (str:parse-int head)
          ((None) (fail (message-with "Unexpected string" head)))
          ((Some int)
           (match (tryInto int)
             ((Ok d) (pure d))
             ((Err _) (fail (message-with "Unexpected string" head))))))))
    (let float-parser =
      (fn (head fraction exponent)
        (match (parse-float head fraction exponent)
          ((None)
           (fail (message-with "Unexpected string" (mconcat
                                                    (make-list head "." fraction "e" exponent)))))
          ((Some float)
           (pure float)))))
    (let head-parser =
      (progn
        (let sign-parser =
          (fn (continue-parser)
            (>>= parser:peek-char
                 (fn (c)
                   (cond
                     ((== c #\-)
                      (>> parser:read-char
                          (liftA2 <> (pure "-") continue-parser)))
                     (True continue-parser))))))
        (sign-parser
         (>>= parser:peek-char
              (fn (c)
                (cond
                  ((== c #\0)
                   (>>= digits-parser
                        (fn (digits)
                          (if (== digits "0")
                              (pure digits)
                              (fail (message-with "Unexpected string" digits))))))
                  ((digit1-9? c) digits-parser)
                  (True
                   (fail (message-with "Unexpected char" (make-list c))))))))))
    (let ((fraction-parser
            (fn (head)
              (>> parser:read-char
                  (>>= digits-parser
                       (fn (fraction)
                         (>>= parser:peek-char-or-eof
                              (fn (opt)
                                (match opt
                                  ((None) (float-parser head fraction "0"))
                                  ((Some c)
                                   (cond
                                     ((or (== c #\e) (== c #\E))
                                      (exponent-parser head fraction))
                                     ((sep? c) (float-parser head fraction "0"))
                                     (True
                                      (fail (message-with "Unexpected char" (make-list c))))))))))))))
          (exponent-parser
            (fn (head fraction)
              (>> parser:read-char
                  (>>= parser:peek-char
                       (fn (c)
                         (cond
                           ((== #\+ c)
                            (>> parser:read-char
                                (>>= digits-parser (float-parser head fraction))))
                           ((== #\- c)
                            (>> parser:read-char
                                (>>= (map (<> "-") digits-parser)
                                     (float-parser head fraction))))
                           (True
                            (>>= digits-parser
                                 (float-parser head fraction))))))))))
      (>>= head-parser
           (fn (head)
             (>>= parser:peek-char-or-eof
                  (fn (opt)
                    (match opt
                      ((None) (integer-parser head))
                      ((Some c)
                       (cond
                         ((== c #\.) (fraction-parser head))
                         ((or (== c #\e) (== c #\E)) (exponent-parser head "0"))
                         ((sep? c) (integer-parser head))
                         (True
                          (fail (message-with "Unexpected char" (make-list c)))))))))))))

  (declare json-parser (parser:Parser json:JSON))
  (define json-parser
    (>> whitespace-parser
        (into
         (asum
          (make-list
           (parser:guard (== #\n)
                         (>> null-parser (pure json:Null)))
           (parser:guard (== #\t)
                         (map into true-parser))
           (parser:guard (== #\f)
                         (map into false-parser))
           (parser:guard (disjoin digit? (== #\-))
                         (map into number-parser))
           (parser:guard (== #\")
                         (map into string-parser))
           (parser:guard (== #\[)
                         (map into array-parser))
           (parser:guard (== #\{)
                         (map into object-parser)))))))

  (declare parse! (iter:Iterator Char -> (Result parser:Error json:JSON)))
  (define (parse! iter)
    (parser:run! json-parser
                 (parser:make-stream! iter)))

  (declare parse (String -> (Result parser:Error json:JSON)))
  (define (parse str)
    (parse! (iter:into-iter str))))
