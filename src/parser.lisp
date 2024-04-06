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
             ((None) (parser:error UnexpectedEof))))))

  (define-type JSON-Value-Type
    (JString)
    (JNumber)
    (JObject)
    (JArray)
    (JTrue)
    (JFalse)
    (JNull))

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
             (True (parser:error (UnexpectedChar c)))))))

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
               (parser:error msg)))))

  (declare take-until-parser ((Char -> Boolean) -> (parser:Parser :e String)))
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

  (declare word-parser (Unit -> (parser:Parser :e String)))
  (define (word-parser) (take-until-parser sep?))


  (declare null-parser (Unit -> (parser:Parser ErrorType Unit)))
  (define (null-parser)
    (>>= (empty-string-error (word-parser) (UnexpectedString ""))
         (fn (word)
           (if (== word "null")
               (pure Unit)
               (parser:error (UnexpectedString word))))))

  (declare true-parser (Unit -> (parser:Parser ErrorType Boolean)))
  (define (true-parser)
    (>>= (empty-string-error (word-parser) (UnexpectedString ""))
         (fn (word)
           (if (== word "true")
               (pure True)
               (parser:error (UnexpectedString word))))))

  (declare false-parser (Unit -> (parser:Parser ErrorType Boolean)))
  (define (false-parser)
    (>>= (empty-string-error (word-parser) (UnexpectedString ""))
         (fn (word)
           (if (== word "false")
               (pure False)
               (parser:error (UnexpectedString word))))))

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
                                              (parser:error (UnexpectedString str)))
                                             ((Some c)
                                              (liftA2 Cons
                                                      (pure (into (make-list c)))
                                                      (start-parser))))))
                                    (parser:error (UnexpectedChar c))))))))
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

  (declare array-parser (Unit -> (parser:Parser ErrorType (List json:JSON))))
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
                                 (True (parser:error (UnexpectedChar c)))))))))
                (>>= (json-parser) continue-parser)))))
      (>> (or-eof-error parser:read-char)
          (>> (whitespace-parser)
              (>>= (or-eof-error parser:peek-char)
                   (fn (c)
                     (if (== c #\])
                         (>> (or-eof-error parser:read-char)
                             (pure Nil))
                         (start-parser))))))))

  (declare object-parser (Unit -> parser:Parser ErrorType (map:Map String json:JSON)))
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
                        (parser:error (UnexpectedChar c))))))))
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
                                  (parser:error (UnexpectedChar c)))))))))
                (>>= (>> (whitespace-parser)
                         (or-eof-error parser:peek-char))
                     (fn (c)
                       (match c
                         (#\"
                          (>>= key-value-parser continue-parser))
                         (_
                          (parser:error (UnexpectedChar c))))))))))
      (>>= (>> (or-eof-error parser:read-char)
               (>> (whitespace-parser)
                   (or-eof-error parser:peek-char)))
           (fn (c)
             (match c
               (#\}
                (>> (or-eof-error parser:read-char)
                    (pure map:empty)))
               (_ (start-parser)))))))


  (declare digits-parser (parser:Parser ErrorType String))
  (define digits-parser
    (let length>0-check =
      (fn (str)
        (if (< 0 (str:length str))
            (pure str)
            (parser:error (UnexpectedString "")))))
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
                         (parser:error (UnexpectedChar c))))))))))

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

  (declare number-parser (parser:Parser ErrorType Double-Float))
  (define number-parser
    (let integer-parser =
      (fn (head)
        (match (str:parse-int head)
          ((None) (parser:error (UnexpectedString head)))
          ((Some int)
           (match (tryInto int)
             ((Ok d) (pure d))
             ((Err _) (parser:error (UnexpectedString head))))))))
    (let float-parser =
      (fn (head fraction exponent)
        (match (parse-float head fraction exponent)
          ((None)
           (parser:error (UnexpectedString (mconcat
                                            (make-list head "." fraction "e" exponent)))))
          ((Some float)
           (pure float)))))
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
                              (parser:error (UnexpectedString digits))))))
                  ((digit1-9? c) digits-parser)
                  (True
                   (parser:error (UnexpectedChar c)))))))))
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
                                      (parser:error (UnexpectedChar c)))))))))))))
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
                          (parser:error (UnexpectedChar c))))))))))))

  (declare json-parser (Unit -> (parser:Parser ErrorType json:JSON)))
  (define (json-parser)
    (>>= (>> (whitespace-parser)
             (json-type-parser))
         (fn (type)
           (match type
             ((JNull) (>> (null-parser) (pure json:Null)))
             ((JTrue) (>>= (true-parser) (.> into pure)))
             ((JFalse) (>>= (false-parser) (.> into pure)))
             ((JString) (>>= (string-parser) (.> into pure)))
             ((JArray) (>>= (array-parser) (.> into pure)))
             ((JObject) (>>= (object-parser) (.> into pure)))
             ((JNumber) (>>= number-parser (.> into pure)))))))

  (declare parse! (iter:Iterator Char -> (Result ErrorType json:JSON)))
  (define (parse! iter)
    (parser:run! (json-parser)
                 (parser:make-stream! iter)))

  (declare parse (String -> (Result ErrorType json:JSON)))
  (define (parse str)
    (parse! (iter:into-iter str))))
