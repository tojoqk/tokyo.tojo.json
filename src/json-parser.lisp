(cl:defpackage #:tokyo.tojo.json-parser/json-parser
  (:use #:coalton
        #:coalton-library/builtin
        #:coalton-library/classes
        #:tokyo.tojo.json-parser/parser)
  (:nicknames #:tokyo.tojo.json-parser)
  (:local-nicknames
   (#:map #:coalton-library/ord-map)
   (#:iter #:coalton-library/iterator)
   (#:str #:coalton-library/string)
   (#:list #:coalton-library/list)
   (#:fn #:coalton-library/functions)
   (#:char #:coalton-library/char))
  (:export #:JSON
           #:JSON-Null
           #:JSON-String
           #:JSON-Number #:JSON-Integer #:JSON-Float
           #:JSON-Boolean
           #:JSON-Array
           #:JSON-Object
           #:parse-json
           #:parse-json!))

(cl:in-package #:tokyo.tojo.json-parser/json-parser)

(coalton-toplevel
  (define-type JSON-Number
    (JSON-Integer Integer)
    (JSON-Float Double-Float))

  (define-type JSON
    (JSON-Null)
    (JSON-String String)
    (JSON-Number JSON-Number)
    (JSON-Boolean Boolean)
    (JSON-Array (List JSON))
    (JSON-Object (map:Map String JSON)))

  (declare digit? (Char -> Boolean))
  (define (digit? c)
    (char:ascii-digit? c))

  (declare digit1-9? (Char -> Boolean))
  (define (digit1-9? c)
    (and (char:ascii-digit? c)
         (not (== c #\0))))

  (define-type JSON-Value-Type
    (JString)
    (JNumber)
    (JObject)
    (JArray)
    (JTrue)
    (JFalse)
    (JNull))

  (declare or-eof-error (Parser (Optional :a) -> Parser :a))
  (define (or-eof-error p)
    (>>= p
         (fn (opt)
           (match opt
             ((Some x) (pure x))
             ((None) (parser-error "Unexpected EOF"))))))

  (declare json-type-parser (Unit -> Parser JSON-Value-Type))
  (define (json-type-parser)
    (>>= (or-eof-error peek-char)
         (fn (c)
           (cond
             ((== c #\-) (pure JNumber))
             ((digit? c) (pure JNumber))
             ((== c #\") (pure JString))
             ((== c #\[) (pure JArray))
             ((== c #\t) (pure JTrue))
             ((== c #\f) (pure JFalse))
             ((== c #\n) (pure JNull))
             ((== c #\{) (pure JObject))
             (True (parser-error (<> "(json-parser) Unexpected Char: "
                                     (into (make-list c)))))))))

  (define (whitespace? c)
    (or (== c #\return)
        (== c #\tab)
        (== c #\newline)
        (== c #\space)))

  (define (whitespace-parser)
    (>>= peek-char
         (fn (opt)
           (match opt
             ((None) (pure Unit))
             ((Some c)
              (if (whitespace? c)
                  (>> (or-eof-error read-char)
                      (whitespace-parser))
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

  (define (empty-string-error parser msg)
    (>>= parser
         (fn (str)
           (if (< 0 (str:length str))
               (pure str)
               (parser-error msg)))))

  (declare take-until-parser ((Char -> Boolean) -> (Parser String)))
  (define (take-until-parser end?)
    (let ((chars
            (fn ()
              (>>= peek-char
                   (fn (opt)
                     (match opt
                       ((None) (pure Nil))
                       ((Some c)
                        (if (end? c)
                            (pure Nil)
                            (liftA2 Cons (or-eof-error read-char) (chars))))))))))
      (map into (chars))))

  (declare word-parser (Unit -> (Parser String)))
  (define (word-parser) (take-until-parser sep?))

  (declare null-parser (Unit -> (Parser Unit)))
  (define (null-parser)
    (>>= (empty-string-error (word-parser) "(null-parser) Empty")
         (fn (word)
           (if (== word "null")
               (pure Unit)
               (parser-error (<> "(null-parser) Unexpected word: " word))))))

  (declare true-parser (Unit -> (Parser Boolean)))
  (define (true-parser)
    (>>= (empty-string-error (word-parser) "(true-parser) Empty")
         (fn (word)
           (if (== word "true")
               (pure True)
               (parser-error (<> "(true-parser) Unexpected word: " word))))))

  (declare false-parser (Unit -> (Parser Boolean)))
  (define (false-parser)
    (>>= (empty-string-error (word-parser) "(false-parser) Empty")
         (fn (word)
           (if (== word "false")
               (pure False)
               (parser-error (<> "(false-parser) Unexpected word: " word))))))

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
            (Tuple #\f (into (make-list #\formfeed)))
            (Tuple #\n (into (make-list #\newline)))
            (Tuple #\r (into (make-list #\return)))
            (Tuple #\t (into (make-list #\tab))))))

  (declare take-parser (UFix -> (Parser string)))
  (define (take-parser n)
    (map into
         (foldr (liftA2 Cons)
                (pure (make-list))
                (list:repeat n (or-eof-error peek-char)))))

  (declare string-parser (Unit -> Parser String))
  (define (string-parser)
    (let ((declare start-parser (Unit -> (Parser (List String))))
          (start-parser
            (fn ()
              (let ((escape-parser
                      (>>= (or-eof-error read-char)
                           (fn (c)
                             (match (map:lookup escape-char-map c)
                               ((Some str) (liftA2 cons (pure str) (start-parser)))
                               ((None)
                                (if (== c #\u)
                                    (>>= (take-parser 4)
                                         (fn (str)
                                           (match (>>= (parse-hex str) char:code-char)
                                             ((None)
                                              (parser-error (<> "Invalid code: " str)))
                                             ((Some c)
                                              (liftA2 Cons
                                                      (pure (into (make-list c)))
                                                      (start-parser))))))
                                    (parser-error (<> "Unexpected escape char: "
                                                      (into (make-list c))))))))))
                    (str-parser
                      (take-until-parser
                       (fn (c)
                         (or (== c #\\)
                             (== c #\"))))))
                (>>= (or-eof-error peek-char)
                     (fn (c)
                       (cond ((== c #\")
                              (>> (or-eof-error read-char)
                                  (pure (make-list))))
                             ((== c #\\)
                              (>> (or-eof-error read-char)
                                  escape-parser))
                             (True
                              (>>= str-parser
                                   (fn (str)
                                     (liftA2 Cons (pure str) (start-parser))))))))))))
      (>>= (>> (or-eof-error read-char)
               (start-parser))
           (fn (lst)
             (pure (mconcat lst))))))

  (declare array-parser (Unit -> (Parser (List JSON))))
  (define (array-parser)
    (let ((start-parser
            (fn ()
              (let ((continue-parser
                      (fn (value)
                        (>>= (>> (whitespace-parser)
                                 (or-eof-error read-char))
                             (fn (c)
                               (cond
                                 ((== c #\]) (pure (make-list value)))
                                 ((== c #\,) (liftA2 Cons (pure value) (start-parser)))
                                 (True (parser-error
                                        (<> "(array-parser) Unexpected char: "
                                            (into (make-list c)))))))))))
                (>>= (json-parser) continue-parser)))))
      (>> (or-eof-error read-char)
          (>> (whitespace-parser)
              (>>= (or-eof-error peek-char)
                   (fn (c)
                     (if (== c #\])
                         (>> (or-eof-error read-char)
                             (pure Nil))
                         (start-parser))))))))

  (declare object-parser (Unit -> Parser (map:Map String JSON)))
  (define (object-parser)
    (let key-parser = (string-parser))
    (let value-parser = (>> (whitespace-parser) (json-parser)))
    (let key-value-parser =
      (>>= key-parser
           (fn (key)
             (>>= (>> (whitespace-parser)
                      (or-eof-error read-char))
                  (fn (c)
                    (if (== c #\:)
                        (>>= value-parser
                             (fn (value)
                               (pure (Tuple key value))))
                        (parser-error
                         (<> "(object-parser) Unexpected Char: "
                             (<> (into (make-list c))
                                 " (Expected: ':')")))))))))
    (let ((start-parser
            (fn ()
              (let ((continue-parser
                      (fn ((Tuple key value))
                        (>>= (>> (whitespace-parser)
                                 (or-eof-error read-char))
                             (fn (c)
                               (match c
                                 (#\,
                                  (>>= (start-parser)
                                       (fn (map)
                                         (pure (map:insert-or-replace map key value)))))
                                 (#\}
                                  (pure (map:insert-or-replace map:empty key value)))
                                 (_
                                  (parser-error
                                   (<> "(object-parser) Unexpected Char: "
                                       (<> (into (make-list c))
                                           " (Expected: ',' or '}')"))))))))))
                (>>= (>> (whitespace-parser)
                         (or-eof-error peek-char))
                     (fn (c)
                       (match c
                         (#\"
                          (>>= key-value-parser continue-parser))
                         (_
                          (parser-error
                           (<> "(object-parser) Unexpected Char: "
                               (<> (into (make-list c))
                                   " (Expected: '\"')")))))))))))
      (>>= (>> (or-eof-error read-char)
               (>> (whitespace-parser)
                   (or-eof-error peek-char)))
           (fn (c)
             (match c
               (#\}
                (>> (or-eof-error read-char)
                    (pure map:empty)))
               (_ (start-parser)))))))

  (define (sep? c)
    (or (whitespace? c)
        (== c #\{)
        (== c #\})
        (== c #\[)
        (== c #\])
        (== c #\,)
        (== c #\")))

  (declare digits-parser (Parser String))
  (define digits-parser
    (let length>0-check =
      (fn (str)
        (if (< 0 (str:length str))
            (pure str)
            (parser-error "Empty digits"))))
    (>>= (take-until-parser (fn:complement digit?))
         (fn (str)
           (>>= peek-char
                (fn (opt)
                  (match opt
                    ((None) (length>0-check str))
                    ((Some c)
                     (if (or (sep? c)
                             (== c #\.)
                             (== c #\e)
                             (== c #\E))
                         (length>0-check str)
                         (parser-error (<> "Unexpected Char:"
                                           (into (make-list c))))))))))))

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

  (declare number-parser (Parser JSON-Number))
  (define number-parser
    (let integer-parser =
      (fn (head)
        (match (str:parse-int head)
          ((None) (parser-error (<> "Integer parse fail: " head)))
          ((Some int) (pure (JSON-Integer int))))))
    (let float-parser =
      (fn (head fraction exponent)
        (match (parse-float head fraction exponent)
          ((None)
           (parser-error
            (mconcat
             (make-list "Fraction parse fail: (" head " " fraction " " exponent ")"))))
          ((Some float)
           (pure (JSON-Float float))))))
    (let head-parser =
      (progn
        (let sign-parser =
          (fn (continue-parser)
            (>>= (or-eof-error peek-char)
                 (fn (c)
                   (cond
                     ((== c #\-)
                      (>> (or-eof-error read-char)
                          (liftA2 <> (pure "-") continue-parser)))
                     (True continue-parser))))))
        (sign-parser
         (>>= (or-eof-error peek-char)
              (fn (c)
                (cond
                  ((== c #\0)
                   (>>= digits-parser
                        (fn (digits)
                          (if (== digits "0")
                              (pure digits)
                              (parser-error (<> "(number-parser) Unxepcted digits: "
                                                digits))))))
                  ((digit1-9? c) digits-parser)
                  (True
                   (parser-error (<> "(number-parser) Unexpected Char: "
                                     (into (make-list c)))))))))))
    (let ((fraction-parser
            (fn (head)
              (>> (or-eof-error read-char)
                  (>>= digits-parser
                       (fn (fraction)
                         (>>= peek-char
                              (fn (opt)
                                (match opt
                                  ((None) (float-parser head fraction "0"))
                                  ((Some c)
                                   (cond
                                     ((or (== c #\e) (== c #\E))
                                      (exponent-parser head fraction))
                                     ((sep? c) (float-parser head fraction "0"))
                                     (True
                                      (parser-error
                                       (<> "Unexpected Char"
                                           (into (make-list c)))))))))))))))
          (exponent-parser
            (fn (head fraction)
              (>> (or-eof-error read-char)
                  (>>= (or-eof-error peek-char)
                       (fn (c)
                         (cond
                           ((== #\+ c)
                            (>> (or-eof-error read-char)
                                (>>= digits-parser (float-parser head fraction))))
                           ((== #\- c)
                            (>> (or-eof-error read-char)
                                (>>= (map (<> "-") digits-parser)
                                     (float-parser head fraction))))
                           (True
                            (>>= digits-parser
                                 (float-parser head fraction))))))))))
      (>>= head-parser
           (fn (head)
             (>>= peek-char
                  (fn (opt)
                    (match opt
                      ((None) (integer-parser head))
                      ((Some c)
                       (cond
                         ((== c #\.) (fraction-parser head))
                         ((or (== c #\e) (== c #\E)) (exponent-parser head "0"))
                         ((sep? c) (integer-parser head))
                         (True
                          (parser-error
                           (<> "Unexpected Char"
                               (into (make-list c))))))))))))))

  (declare json-parser (Unit -> (Parser JSON)))
  (define (json-parser)
    (>>= (>> (whitespace-parser)
             (json-type-parser))
         (fn (type)
           (match type
             ((JNull) (>> (null-parser) (pure JSON-Null)))
             ((JTrue) (>>= (true-parser) (.> JSON-Boolean pure)))
             ((JFalse) (>>= (false-parser) (.> JSON-Boolean pure)))
             ((JString) (>>= (string-parser) (.> JSON-String pure)))
             ((JArray) (>>= (array-parser) (.> JSON-Array pure)))
             ((JObject) (>>= (object-parser) (.> JSON-Object pure)))
             ((JNumber) (>>= number-parser
                             (.> JSON-Number pure)))))))

  (declare parse-json! (iter:Iterator Char -> (Result String JSON)))
  (define (parse-json! iter)
    (run-parser! (json-parser)
                 (make-stream! iter)))

  (declare parse-json (String -> (Result String JSON)))
  (define (parse-json str)
    (parse-json! (iter:into-iter str)))

  ;; TODO: If the deriving mechanism is added to Coalton, use it.
  ;; @see: https://github.com/coalton-lang/coalton/issues/10
  (define-instance (Eq JSON)
    (define (== x y)
      (match x
        ((JSON-Null)
         (match y
           ((JSON-Null) True)
           (_ False)))
        ((JSON-Boolean b1)
         (match y
           ((JSON-Boolean b2) (== b1 b2))
           (_ False)))
        ((JSON-String s1)
         (match y
           ((JSON-String s2) (== s1 s2))
           (_ False)))
        ((JSON-Array a1)
         (match y
           ((JSON-Array a2) (== a1 a2))
           (_ False)))
        ((JSON-Object o1)
         (match y
           ((JSON-Object o2) (== o1 o2))
           (_ False)))
        ((JSON-Number n1)
         (match y
           ((JSON-Number n2) (== n1 n2))
           (_ False))))))

  (define-instance (Eq JSON-Number)
    (define (== x y)
      (match x
        ((JSON-Integer i1)
         (match y
           ((JSON-Integer i2) (== i1 i2))
           (_ False)))
        ((JSON-Float f1)
         (match y
           ((JSON-Float f2) (== f1 f2))
           (_ False)))))))
