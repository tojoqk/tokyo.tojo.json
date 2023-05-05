(cl:defpackage #:tokyo.tojo.json-parser/parser
  (:use #:coalton
        #:coalton-library/builtin
        #:coalton-library/classes)
  (:local-nicknames
   (#:iter #:coalton-library/iterator))
  (:export #:Parser
           #:Stream
           #:peek-char
           #:read-char
           #:parser-error
           #:make-stream!
           #:run-parser!))

(cl:in-package #:tokyo.tojo.json-parser/parser)

(coalton-toplevel
  (define-type Stream (%Stream (Optional Char) (iter:Iterator Char)))

  (declare peek (Stream -> Optional Char))
  (define (peek (%Stream c _)) c)

  (declare read! (Stream -> (Optional (Tuple Char Stream))))
  (define (read! p)
    (match p
      ((%Stream (None) _) None)
      ((%Stream (Some c) iter)
       (let c_ = (iter:next! iter))
       (Some (Tuple c (%Stream c_ iter))))))

  (define-type (Parser :a) (Parser (Stream -> Result String (Tuple :a Stream))))

  (define-instance (Functor Parser)
    (define (map f p)
      (Parser
       (fn (in)
         (match p
           ((Parser parse!)
            (>>= (parse! in)
                 (fn ((Tuple x in_))
                   (pure (Tuple (f x) in_))))))))))

  (define-instance (Applicative Parser)
    (define (pure p)
      (Parser (fn (in) (Ok (Tuple p in)))))

    (define (liftA2 op p1 p2)
      (Parser
       (fn (in)
         (match (Tuple p1 p2)
           ((Tuple (Parser parse1!) (Parser parse2!))
            (>>= (parse1! in)
                 (fn ((Tuple x in_))
                   (>>= (parse2! in_)
                        (fn ((Tuple y in__))
                          (pure (Tuple (op x y) in__))))))))))))

  (define-instance (Monad Parser)
    (define (>>= (Parser parse!) f)
      (Parser
       (fn (in)
         (>>= (parse! in)
              (fn ((Tuple x in_))
                (match (f x)
                  ((Parser parse_!)
                   (>>= (parse_! in_)
                        (fn ((Tuple x_ in__))
                          (pure (Tuple x_ in__))))))))))))

  (declare peek-char (Parser (Optional Char)))
  (define peek-char
    (Parser (fn (in) (Ok (Tuple (peek in) in)))))

  (declare read-char (Parser (Optional Char)))
  (define read-char
    (Parser
     (fn (in)
       (match (read! in)
         ((Some (Tuple c in_)) (Ok (Tuple (Some c) in_)))
         ((None) (Ok (Tuple None in)))))))

  (declare parser-error (String -> Parser :a))
  (define (parser-error msg)
    (Parser (fn (_) (Err msg))))

  (declare make-stream! (iter:Iterator Char -> Stream))
  (define (make-stream! iter)
    (%Stream (iter:next! iter) iter))

  (declare run-parser! (Parser :a -> Stream -> Result String :a))
  (define (run-parser! parser in)
    (match parser
      ((Parser parse!)
       (>>= (parse! in)
            (fn ((Tuple x _))
              (pure x)))))))
