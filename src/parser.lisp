(cl:defpackage #:tokyo.tojo.json-parser/parser
  (:use #:coalton
        #:coalton-prelude)
  (:shadow #:error)
  (:local-nicknames
   (#:iter #:coalton-library/iterator))
  (:export #:Parser
           #:Stream
           #:peek-char
           #:read-char
           #:take-until-string
           #:error
           #:make-stream!
           #:run!))

(cl:in-package #:tokyo.tojo.json-parser/parser)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel
  ;; JSON is LL(1) grammar, so it only requires lookahead of one character.
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

  (define-type (Parser :e :a) (Parser (Stream -> Result :e (Tuple :a Stream))))

  (define-instance (Functor (Parser :e))
    (define (map f (Parser parse!))
      (Parser
       (fn (in)
         (>>= (parse! in)
              (fn ((Tuple x in_))
                (pure (Tuple (f x) in_))))))))

  (define-instance (Applicative (Parser :e))
    (define (pure p)
      (Parser (fn (in) (Ok (Tuple p in)))))

    (define (liftA2 op (Parser parse1!) (Parser parse2!))
      (Parser
       (fn (in)
         (>>= (parse1! in)
              (fn ((Tuple x in_))
                (>>= (parse2! in_)
                     (fn ((Tuple y in__))
                       (pure (Tuple (op x y) in__))))))))))

  (define-instance (Monad (Parser :e))
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

  (declare peek-char (Parser :e (Optional Char)))
  (define peek-char
    (Parser (fn (in) (Ok (Tuple (peek in) in)))))

  (declare read-char (Parser :e (Optional Char)))
  (define read-char
    (Parser
     (fn (in)
       (match (read! in)
         ((Some (Tuple c in_)) (Ok (Tuple (Some c) in_)))
         ((None) (Ok (Tuple None in)))))))

  (declare take-until-string ((Char -> Boolean) -> Parser :e String))
  (define (take-until-string end?)
    (Parser
     (fn (in)
       (Ok
        (lisp (Tuple String Stream) (end? in)
          (cl:let ((in* in)
                   (out (cl:make-string-output-stream)))
            (cl:loop
               :while (coalton
                       (match (peek (lisp Stream () in*))
                         ((Some x)
                          (not ((lisp (Char -> Boolean) () end?) x)))
                         ((None) False)))
               :do (coalton
                    (match (read! (lisp Stream () in*))
                      ((Some (Tuple x in_))
                       (lisp Unit (x in_)
                         (cl:setf in* in_)
                         (cl:write-char x out)
                         Unit))
                      (_ Unit))))
            (coalton
             (Tuple (lisp String () (cl:get-output-stream-string out))
                    (lisp Stream () in*)))))))))

  (declare error (:e -> Parser :e :a))
  (define (error msg)
    (Parser (fn (_) (Err msg))))

  (declare make-stream! (iter:Iterator Char -> Stream))
  (define (make-stream! iter)
    (%Stream (iter:next! iter) iter))

  (declare run! (Parser :e :a -> Stream -> Result :e :a))
  (define (run! (Parser parse!) in)
    (>>= (parse! in)
         (fn ((Tuple x _))
           (pure x)))))
