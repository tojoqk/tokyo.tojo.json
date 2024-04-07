(defpackage #:tokyo.tojo.json/private/parser
  (:use #:coalton
        #:coalton-prelude)
  (:shadow #:error)
  (:local-nicknames
   (#:iter #:coalton-library/iterator)
   (#:cell #:Coalton-library/cell))
  (:export #:Parser
           #:Stream
           #:peek-char
           #:peek-char-or-eof
           #:read-char-or-eof
           #:read-char
           #:take-until-string
           #:guard
           #:Error
           #:Message
           #:UnexpectedEof
           #:make-stream!
           #:delay
           #:run!))

(in-package #:tokyo.tojo.json/private/parser)

(named-readtables:in-readtable coalton:coalton)

(cl:defmacro delay (expr)
  (cl:let ((in (cl:gensym))
           (parse! (cl:gensym)))
    `(Parser (fn (,in)
               (let (Parser ,parse!) = ,expr)
               (,parse! ,in)))))

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

  (declare end-or-read! ((Char -> Boolean) -> Stream -> Optional (Tuple Char Stream)))
  (define (end-or-read! end? (%Stream opt iter))
    (match opt
      ((Some c)
       (if (end? c)
           None
           (progn
             (let c_ = (iter:next! iter))
             (Some (Tuple c (%Stream c_ iter))))))
      ((None) None)))

  (repr :transparent)
  (define-type (Parser :a) (Parser (Stream -> Result Error (Tuple :a Stream))))

  (define-instance (Functor Parser)
    (define (map f (Parser parse!))
      (Parser
       (fn (in)
         (>>= (parse! in)
              (fn ((Tuple x in_))
                (pure (Tuple (f x) in_))))))))

  (define-instance (Applicative Parser)
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

  (define-instance (MonadFail Parser)
    (define (fail msg)
      (Parser (const (Err (Message msg))))))

  (repr :transparent)
  (define-type (Guard :a)
    (%Guard (Char -> (Optional (Parser :a)))))

  (declare guard ((Char -> Boolean) -> Parser :a -> Guard :a))
  (define (guard p? parser)
    (%Guard (fn (c)
              (if (p? c)
                  (Some parser)
                  None))))

  (define-instance (Functor Guard)
    (define (map f (%Guard g))
      (%Guard (fn (c)
                (map (map f) (g c))))))

  (define-instance (Applicative Guard)
    (define (pure x)
      (%Guard (fn (_) (Some (pure x)))))
    (define (liftA2 f (%Guard g1) (%Guard g2))
      (%Guard (fn (c)
                (liftA2 (liftA2 f) (g1 c) (g2 c))))))

  (define-instance (Alternative Guard)
    (define empty (%Guard (fn (_) None)))
    (define (alt (%Guard g1) (%Guard g2))
      (%Guard (fn (c)
                (let ((x1 (g1 c)))
                  (match x1
                    ((Some _) x1)
                    ((None)
                     (let ((x2 (g2 c)))
                       (match x2
                         ((Some _) x2)
                         ((None) None))))))))))

  (define-instance (Into (Guard :a) (Parser :a))
    (define (into (%Guard g))
      (do (c <- peek-char)
          (match (g c)
            ((Some p) p)
            ((None) (fail (<> "Unexpected char: " (into (make-list c)))))))))

  (declare peek-char-or-eof (Parser (Optional Char)))
  (define peek-char-or-eof
    (Parser (fn (in) (Ok (Tuple (peek in) in)))))

  (declare peek-char (Parser Char))
  (define peek-char
    (do (opt <- peek-char-or-eof)
        (match opt
          ((Some c) (pure c))
          ((None) (Parser (const (Err UnexpectedEof)))))))

  (declare read-char-or-eof (Parser (Optional Char)))
  (define read-char-or-eof
    (Parser
     (fn (in)
       (match (read! in)
         ((Some (Tuple c in_))  (Ok (Tuple (Some c) in_)))
         ((None) (Ok (Tuple None in)))))))

  (declare read-char (Parser Char))
  (define read-char
    (Parser
     (fn (in)
       (match (read! in)
         ((Some (Tuple c in_)) (Ok (Tuple c in_)))
         ((None) (Err UnexpectedEof))))))

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

  (declare take-until-string ((Char -> Boolean) -> Parser String))
  (define (take-until-string end?)
    (Parser
     (fn (in)
       (let ((out (lisp-make-string-output-sream))
             (cell (cell:new in)))
         (while-let (Some (Tuple c next)) = (end-or-read! end? (cell:read cell))
                    (cell:write! cell next)
                    (lisp-write-char! c out))
         (Ok
          (Tuple (lisp-get-output-stream-string out)
                 (cell:read cell)))))))

  (define-type Error
    UnexpectedEof
    (Message String))

  (define-instance (Eq Error)
    (define (== a b)
      (match (Tuple a b)
        ((Tuple (UnexpectedEof) (UnexpectedEof)) True)
        ((Tuple (Message x) (Message y)) (== x y))
        (_ False))))

  (declare make-stream! (iter:Iterator Char -> Stream))
  (define (make-stream! iter)
    (%Stream (iter:next! iter) iter))

  (declare run! (Parser :a -> Stream -> Result Error :a))
  (define (run! (Parser parse!) in)
    (>>= (parse! in)
         (fn ((Tuple x _))
           (pure x)))))
