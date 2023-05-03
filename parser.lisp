(cl:defpackage #:qkjson/parser
  (:use #:coalton
        #:coalton-library/builtin
        #:coalton-library/classes)
  (:local-nicknames
   (:iter #:coalton-library/iterator))
  (:export Parser
           Stream
           peek-char
           read-char
           parser-error
           make-stream!
           run-parser!))

(cl:in-package #:qkjson/parser)

(coalton-toplevel
  (define-type Stream (%Stream (Optional Char) (iter:Iterator Char)))

  (declare peek (Stream -> Optional Char))
  (define (peek (%Stream c _)) c)

  (declare read! (Stream -> (Optional (Tuple Char Stream))))
  (define (read! p)
    (match p
      ((%Stream (None) _) None)
      ((%Stream (Some c) iter)
       (progn
         (let c* = (iter:next! iter))
         (Some (Tuple c (%Stream c* iter)))))))

  (define-type (Parser :a) (Parser (Stream -> Result String (Tuple :a Stream))))

  (define-instance (Functor Parser)
    (define (map f p)
      (Parser
       (fn (in)
         (match p
           ((Parser parse!)
            (>>= (parse! in)
                 (fn ((Tuple x in*))
                   (pure (Tuple (f x) in*))))))))))

  (define-instance (Applicative Parser)
    (define (pure p)
      (Parser (fn (in) (Ok (Tuple p in)))))

    (define (liftA2 op p1 p2)
      (Parser
       (fn (in)
         (match (Tuple p1 p2)
           ((Tuple (Parser parse1!) (Parser parse2!))
            (>>= (parse1! in)
                 (fn ((Tuple x in*))
                   (>>= (parse2! in*)
                        (fn ((Tuple y in**))
                          (pure (Tuple (op x y) in**))))))))))))

  (define-instance (Monad Parser)
    (define (>>= (Parser parse!) f)
      (Parser
       (fn (in)
         (>>= (parse! in)
              (fn ((Tuple x in*))
                (match (f x)
                  ((Parser parse*!)
                   (>>= (parse*! in*)
                        (fn ((Tuple x* in**))
                          (pure (Tuple x* in**))))))))))))

  (declare peek-char (Parser (Optional Char)))
  (define peek-char
    (Parser (fn (in) (Ok (Tuple (peek in) in)))))

  (declare read-char (Parser (Optional Char)))
  (define read-char
    (Parser
     (fn (in)
       (match (read! in)
         ((Some (Tuple c in*)) (Ok (Tuple (Some c) in*)))
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
