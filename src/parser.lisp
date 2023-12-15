(defpackage #:tokyo.tojo.json-parser/parser
  (:use #:coalton
        #:coalton-prelude)
  (:shadow #:error)
  (:local-nicknames
   (#:iter #:coalton-library/iterator)
   (#:cell #:Coalton-library/cell))
  (:export #:Parser
           #:Stream
           #:peek-char
           #:read-char
           #:take-until-string
           #:error
           #:make-stream!
           #:run!))

(in-package #:tokyo.tojo.json-parser/parser)

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

  (repr :native cl:stream)
  (define-type Lisp-Stream)

  (declare lisp-write-char! (Char -> Lisp-Stream -> Unit))
  (define (lisp-write-char! c s)
    (lisp Unit (c s)
      (cl:write-char c s)
      Unit))

  (declare lisp-make-string-output-sream (Unit -> Lisp-Stream))
  (define (lisp-make-string-output-sream)
    (lisp Lisp-Stream ()
      (cl:make-string-output-stream)))

  (declare lisp-get-output-stream-string (Lisp-Stream -> String))
  (define (lisp-get-output-stream-string s)
    (lisp String (s)
      (cl:get-output-stream-string s)))

  (declare take-until-string ((Char -> Boolean) -> Parser :e String))
  (define (take-until-string end?)
    (Parser
     (fn (in)
       (let ((out (lisp-make-string-output-sream))
             (in* (cell:new in)))
         (while (match (peek (cell:read in*))
                  ((Some x) (not (end? x)))
                  ((None) False))
           (match (read! (cell:read in*))
             ((Some (Tuple x in_))
              (progn
                (cell:swap! in* in_)
                (lisp-write-char! x out)))
             (_ Unit)))
         (Ok
          (Tuple (lisp-get-output-stream-string out)
                 (cell:read in*)))))))

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
