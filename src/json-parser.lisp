(cl:defpackage #:tokyo.tojo.json-parser/json-parser
  (:use #:coalton
        #:coalton-prelude)
  (:nicknames #:tokyo.tojo.json-parser)
  (:local-nicknames
   (#:map #:coalton-library/ord-map)
   (#:iter #:coalton-library/iterator)
   (#:str #:coalton-library/string)
   (#:list #:coalton-library/list)
   (#:fn #:coalton-library/functions)
   (#:char #:coalton-library/char)
   (#:parser #:tokyo.tojo.json-parser/parser))
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

(named-readtables:in-readtable coalton:coalton)

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

  (define-type ErrorType
    (UnexpectedEof)
    (UnexpectedChar Char)
    (UnexpectedString String))

  (define-instance (Eq ErrorType)
    (define (== x y)
      (match x
        ((UnexpectedEof)
         (match y
           ((UnexpectedEof) True)
           (_ False)))
        ((UnexpectedChar cx)
         (match y
           ((UnexpectedChar cy) (== cx cy))
           (_ False)))
        ((UnexpectedString cx)
         (match y
           ((UnexpectedString cy) (== cx cy))
           (_ False))))))

  (declare or-eof-error (parser:Parser ErrorType (Optional :a) -> parser:Parser ErrorType :a))
  (define (or-eof-error p)
    (>>= p
         (fn (opt)
           (match opt
             ((Some x) (pure x))
             ((None) (parser:parser-error UnexpectedEof))))))

  (declare json-type-parser (Unit -> parser:Parser ErrorType JSON-Value-Type))
  (define (json-type-parser)
    (>>= (or-eof-error parser:peek-char)
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
             (True (parser:parser-error (UnexpectedChar c)))))))

  (define (whitespace? c)
    (or (== c #\return)
        (== c #\tab)
        (== c #\newline)
        (== c #\space)))

  (define (whitespace-parser)
    (>>= parser:peek-char
         (fn (opt)
           (match opt
             ((None) (pure Unit))
             ((Some c)
              (if (whitespace? c)
                  (>> (or-eof-error parser:read-char)
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
               (parser:parser-error msg)))))

  (declare take-until-parser ((Char -> Boolean) -> (parser:Parser :e String)))
  (define (take-until-parser end?)
    (parser:take-until-string end?))

  (declare word-parser (Unit -> (parser:Parser :e String)))
  (define (word-parser) (take-until-parser sep?))

  (declare null-parser (Unit -> (parser:Parser ErrorType Unit)))
  (define (null-parser)
    (>>= (empty-string-error (word-parser) (UnexpectedString ""))
         (fn (word)
           (if (== word "null")
               (pure Unit)
               (parser:parser-error (UnexpectedString word))))))

  (declare true-parser (Unit -> (parser:Parser ErrorType Boolean)))
  (define (true-parser)
    (>>= (empty-string-error (word-parser) (UnexpectedString ""))
         (fn (word)
           (if (== word "true")
               (pure True)
               (parser:parser-error (UnexpectedString word))))))

  (declare false-parser (Unit -> (parser:Parser ErrorType Boolean)))
  (define (false-parser)
    (>>= (empty-string-error (word-parser) (UnexpectedString ""))
         (fn (word)
           (if (== word "false")
               (pure False)
               (parser:parser-error (UnexpectedString word))))))

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

  (declare take-parser (UFix -> (parser:Parser ErrorType string)))
  (define (take-parser n)
    (map into
         (foldr (liftA2 Cons)
                (pure (make-list))
                (list:repeat n (or-eof-error parser:peek-char)))))

  (declare string-parser (Unit -> parser:Parser ErrorType String))
  (define (string-parser)
    (let ((declare start-parser (Unit -> (parser:Parser ErrorType (List String))))
          (start-parser
            (fn ()
              (let ((escape-parser
                      (>>= (or-eof-error parser:read-char)
                           (fn (c)
                             (match (map:lookup escape-char-map c)
                               ((Some str) (liftA2 cons (pure str) (start-parser)))
                               ((None)
                                (if (== c #\u)
                                    (>>= (take-parser 4)
                                         (fn (str)
                                           (match (>>= (parse-hex str) char:code-char)
                                             ((None)
                                              (parser:parser-error (UnexpectedString str)))
                                             ((Some c)
                                              (liftA2 Cons
                                                      (pure (into (make-list c)))
                                                      (start-parser))))))
                                    (parser:parser-error (UnexpectedChar c))))))))
                    (str-parser
                      (take-until-parser
                       (fn (c)
                         (or (== c #\\)
                             (== c #\"))))))
                (>>= (or-eof-error parser:peek-char)
                     (fn (c)
                       (cond ((== c #\")
                              (>> (or-eof-error parser:read-char)
                                  (pure (make-list))))
                             ((== c #\\)
                              (>> (or-eof-error parser:read-char)
                                  escape-parser))
                             (True
                              (>>= str-parser
                                   (fn (str)
                                     (liftA2 Cons (pure str) (start-parser))))))))))))
      (>>= (>> (or-eof-error parser:read-char)
               (start-parser))
           (fn (lst)
             (pure (mconcat lst))))))

  (declare array-parser (Unit -> (parser:Parser ErrorType (List JSON))))
  (define (array-parser)
    (let ((start-parser
            (fn ()
              (let ((continue-parser
                      (fn (value)
                        (>>= (>> (whitespace-parser)
                                 (or-eof-error parser:read-char))
                             (fn (c)
                               (cond
                                 ((== c #\]) (pure (make-list value)))
                                 ((== c #\,) (liftA2 Cons (pure value) (start-parser)))
                                 (True (parser:parser-error (UnexpectedChar c)))))))))
                (>>= (json-parser) continue-parser)))))
      (>> (or-eof-error parser:read-char)
          (>> (whitespace-parser)
              (>>= (or-eof-error parser:peek-char)
                   (fn (c)
                     (if (== c #\])
                         (>> (or-eof-error parser:read-char)
                             (pure Nil))
                         (start-parser))))))))

  (declare object-parser (Unit -> parser:Parser ErrorType (map:Map String JSON)))
  (define (object-parser)
    (let key-parser = (string-parser))
    (let value-parser = (>> (whitespace-parser) (json-parser)))
    (let key-value-parser =
      (>>= key-parser
           (fn (key)
             (>>= (>> (whitespace-parser)
                      (or-eof-error parser:read-char))
                  (fn (c)
                    (if (== c #\:)
                        (>>= value-parser
                             (fn (value)
                               (pure (Tuple key value))))
                        (parser:parser-error (UnexpectedChar c))))))))
    (let ((start-parser
            (fn ()
              (let ((continue-parser
                      (fn ((Tuple key value))
                        (>>= (>> (whitespace-parser)
                                 (or-eof-error parser:read-char))
                             (fn (c)
                               (match c
                                 (#\,
                                  (>>= (start-parser)
                                       (fn (map)
                                         (pure (map:insert-or-replace map key value)))))
                                 (#\}
                                  (pure (map:insert-or-replace map:empty key value)))
                                 (_
                                  (parser:parser-error (UnexpectedChar c)))))))))
                (>>= (>> (whitespace-parser)
                         (or-eof-error parser:peek-char))
                     (fn (c)
                       (match c
                         (#\"
                          (>>= key-value-parser continue-parser))
                         (_
                          (parser:parser-error (UnexpectedChar c))))))))))
      (>>= (>> (or-eof-error parser:read-char)
               (>> (whitespace-parser)
                   (or-eof-error parser:peek-char)))
           (fn (c)
             (match c
               (#\}
                (>> (or-eof-error parser:read-char)
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

  (declare digits-parser (parser:Parser ErrorType String))
  (define digits-parser
    (let length>0-check =
      (fn (str)
        (if (< 0 (str:length str))
            (pure str)
            (parser:parser-error (UnexpectedString "")))))
    (>>= (take-until-parser (fn:complement digit?))
         (fn (str)
           (>>= parser:peek-char
                (fn (opt)
                  (match opt
                    ((None) (length>0-check str))
                    ((Some c)
                     (if (or (sep? c)
                             (== c #\.)
                             (== c #\e)
                             (== c #\E))
                         (length>0-check str)
                         (parser:parser-error (UnexpectedChar c))))))))))

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

  (declare number-parser (parser:Parser ErrorType JSON-Number))
  (define number-parser
    (let integer-parser =
      (fn (head)
        (match (str:parse-int head)
          ((None) (parser:parser-error (UnexpectedString head)))
          ((Some int) (pure (JSON-Integer int))))))
    (let float-parser =
      (fn (head fraction exponent)
        (match (parse-float head fraction exponent)
          ((None)
           (parser:parser-error (UnexpectedString (mconcat
                                                   (make-list head "." fraction "e" exponent)))))
          ((Some float)
           (pure (JSON-Float float))))))
    (let head-parser =
      (progn
        (let sign-parser =
          (fn (continue-parser)
            (>>= (or-eof-error parser:peek-char)
                 (fn (c)
                   (cond
                     ((== c #\-)
                      (>> (or-eof-error parser:read-char)
                          (liftA2 <> (pure "-") continue-parser)))
                     (True continue-parser))))))
        (sign-parser
         (>>= (or-eof-error parser:peek-char)
              (fn (c)
                (cond
                  ((== c #\0)
                   (>>= digits-parser
                        (fn (digits)
                          (if (== digits "0")
                              (pure digits)
                              (parser:parser-error (UnexpectedString digits))))))
                  ((digit1-9? c) digits-parser)
                  (True
                   (parser:parser-error (UnexpectedChar c)))))))))
    (let ((fraction-parser
            (fn (head)
              (>> (or-eof-error parser:read-char)
                  (>>= digits-parser
                       (fn (fraction)
                         (>>= parser:peek-char
                              (fn (opt)
                                (match opt
                                  ((None) (float-parser head fraction "0"))
                                  ((Some c)
                                   (cond
                                     ((or (== c #\e) (== c #\E))
                                      (exponent-parser head fraction))
                                     ((sep? c) (float-parser head fraction "0"))
                                     (True
                                      (parser:parser-error (UnexpectedChar c)))))))))))))
          (exponent-parser
            (fn (head fraction)
              (>> (or-eof-error parser:read-char)
                  (>>= (or-eof-error parser:peek-char)
                       (fn (c)
                         (cond
                           ((== #\+ c)
                            (>> (or-eof-error parser:read-char)
                                (>>= digits-parser (float-parser head fraction))))
                           ((== #\- c)
                            (>> (or-eof-error parser:read-char)
                                (>>= (map (<> "-") digits-parser)
                                     (float-parser head fraction))))
                           (True
                            (>>= digits-parser
                                 (float-parser head fraction))))))))))
      (>>= head-parser
           (fn (head)
             (>>= parser:peek-char
                  (fn (opt)
                    (match opt
                      ((None) (integer-parser head))
                      ((Some c)
                       (cond
                         ((== c #\.) (fraction-parser head))
                         ((or (== c #\e) (== c #\E)) (exponent-parser head "0"))
                         ((sep? c) (integer-parser head))
                         (True
                          (parser:parser-error (UnexpectedChar c))))))))))))

  (declare json-parser (Unit -> (parser:Parser ErrorType JSON)))
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

  (declare parse-json! (iter:Iterator Char -> (Result ErrorType JSON)))
  (define (parse-json! iter)
    (parser:run-parser! (json-parser)
                        (parser:make-stream! iter)))

  (declare parse-json (String -> (Result ErrorType JSON)))
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
