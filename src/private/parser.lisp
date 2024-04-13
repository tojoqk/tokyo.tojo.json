(defpackage #:tokyo.tojo.json/private/parser
  (:use #:coalton
        #:coalton-prelude)
  (:local-nicknames
   (#:iter #:coalton-library/iterator)
   (#:cell #:coalton-library/cell)
   (#:optional #:coalton-library/optional)
   (#:map #:coalton-library/ord-map)
   (#:output #:tokyo.tojo.json/private/output-stream))
  (:export #:Parser
           #:peek-char
           #:peek-char-or-eof
           #:read-char-or-eof
           #:read-char
           #:take-until-string
           #:guard
           #:guard-char
           #:guard-eof
           #:guard-lookup
           #:guard-else
           #:from-guard
           #:Error
           #:Message
           #:UnexpectedEof
           #:delay
           #:run!
           #:collect-while

           #:Port
           #:make-port!))

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
  (define-type Port (%Port (Optional Char) (iter:Iterator Char)))

  (define (make-port! iter)
    (%Port (iter:next! iter) iter))

  (declare peek (Port -> Optional Char))
  (define (peek (%Port c _)) c)

  (declare read! (Port -> (Optional (Tuple Char Port))))
  (define (read! p)
    (match p
      ((%Port (None) _) None)
      ((%Port (Some c) iter)
       (let c_ = (iter:next! iter))
       (Some (Tuple c (%Port c_ iter))))))

  (declare end-or-read! ((Char -> Boolean) -> Port -> Optional (Tuple Char Port)))
  (define (end-or-read! end? (%Port opt iter))
    (match opt
      ((Some c)
       (if (end? c)
           None
           (progn
             (let c_ = (iter:next! iter))
             (Some (Tuple c (%Port c_ iter))))))
      ((None) None)))

  (repr :transparent)
  (define-type (Parser :s :a)
    (Parser ((Tuple Port (List :s)) -> Result String (Tuple3 :a Port (List :s)))))

  (declare push (:s -> Parser :s Unit))
  (define (push x)
    (Parser
     (fn ((Tuple in xs))
       (pure (Tuple3 Unit in (Cons x xs))))))

  (declare pop (Parser :s :s))
  (define pop
    (Parser
     (fn ((Tuple in s))
       (match s
         ((Nil) (Err "Stack underflow"))
         ((Cons x xs)
          (pure (Tuple3 x in xs)))))))

  (declare run! (Parser :s :a -> Port -> Result String :a))
  (define (run! (Parser parse!) in)
    (do ((Tuple3 x _ _) <- (parse! (Tuple in Nil)))
        (pure x)))

(define-instance (Functor (Parser :s))
  (define (map f (Parser parse!))
    (Parser
     (fn (input)
       (do ((Tuple3 x port stack) <- (parse! input))
           (pure (Tuple3 (f x) port stack)))))))

  (define-instance (Applicative (Parser :s))
    (define (pure x)
      (Parser (fn ((Tuple port stack))
                (Ok (Tuple3 x port stack)))))
    (define (liftA2 op (Parser parse1!) (Parser parse2!))
      (Parser
       (fn (input)
         (>>= (parse1! input)
              (fn ((Tuple3 x port stack))
                (>>= (parse2! (Tuple port stack))
                     (fn ((Tuple3 y port stack))
                       (pure (Tuple3 (op x y) port stack))))))))))

  (define-instance (Monad (Parser :s))
    (define (>>= (Parser parse!) f)
      (Parser
       (fn (input)
         (do ((Tuple3 x port stack) <- (parse! input))
             (let (Parser parse!) = (f x))
             (parse! (Tuple port stack)))))))

  (define-instance (MonadFail (Parser :s))
    (define (fail msg)
      (Parser (const (Err msg)))))

  (repr :transparent)
  (define-type (Guard :s :a)
    (%Guard ((Optional Char) -> (Optional (Parser :s :a)))))

  (declare guard ((Optional Char -> Optional :in) ->  (:in -> Parser :s :a) -> Guard :s :a))
  (define (guard f make-parser)
    (%Guard (.> f (map make-parser))))

  (declare alt-guard (Alternative :f => Boolean -> (:f Unit)))
  (define (alt-guard b)
    (if b
        (pure Unit)
        empty))

  (declare guard-char ((Char -> Boolean) -> Parser :s :a -> Guard :s :a))
  (define (guard-char p? parser)
    (guard (fn (opt)
             (do (c <- opt)
                 (alt-guard (p? c))))
           (fn ((Unit)) parser)))

  (define (guard-eof parser)
    (guard (fn (opt)
             (alt-guard (optional:none? opt)))
           (fn ((Unit)) parser)))

  (declare guard-lookup (map:Map Char :b -> (:b -> Parser :s :a) -> Guard :s :a))
  (define (guard-lookup m make-parser)
    (%Guard (fn (opt)
              (do (c <- opt)
                  (x <- (map:lookup m c))
                (pure (make-parser x))))))

  (declare guard-else (Parser :s :a -> Guard :s :a))
  (define (guard-else parser)
    (%Guard (const (pure parser))))

  (define-instance (Functor (Guard :s))
    (define (map f (%Guard g))
      (%Guard (fn (c)
                (map (map f) (g c))))))

  (define-instance (Applicative (Guard :s))
    (define (pure x)
      (%Guard (fn (_) (Some (pure x)))))
    (define (liftA2 f (%Guard g1) (%Guard g2))
      (%Guard (fn (c)
                (liftA2 (liftA2 f) (g1 c) (g2 c))))))

  (define-instance (Alternative (Guard :s))
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

  (declare from-guard (Guard :s :a -> Parser :s :a))
  (define (from-guard (%Guard g))
    (do (opt <- peek-char-or-eof)
        (match (g opt)
          ((Some p) p)
          ((None)
           (match opt
             ((Some c) (fail (<> "Unexpected char: " (into (make-list c)))))
             ((None) (fail "Unexpected eof")))))))

  (declare peek-char-or-eof (Parser :s (Optional Char)))
  (define peek-char-or-eof
    (Parser (fn ((Tuple port stack)) (Ok (Tuple3 (peek port) port stack)))))

  (declare peek-char (Parser :s Char))
  (define peek-char
    (do (opt <- peek-char-or-eof)
        (match opt
          ((Some c) (pure c))
          ((None) (Parser (const (Err "Unexpected eof")))))))

  (declare read-char-or-eof (Parser :s (Optional Char)))
  (define read-char-or-eof
    (Parser
     (fn ((Tuple port stack))
       (match (read! port)
         ((Some (Tuple c port))  (Ok (Tuple3 (Some c) port stack)))
         ((None) (Ok (Tuple3 None port stack)))))))

  (declare read-char (Parser :s Char))
  (define read-char
    (Parser
     (fn ((Tuple port stack))
       (match (read! port)
         ((Some (Tuple c port)) (Ok (Tuple3 c port stack)))
         ((None) (Err "Unexpected eof"))))))

  (declare take-until-string ((Char -> Boolean) -> Parser :s String))
  (define (take-until-string end?)
    (Parser
     (fn ((Tuple port stack))
       (let ((out (output:make-string-output-stream))
             (cell (cell:new port)))
         (while-let (Some (Tuple c next)) = (end-or-read! end? (cell:read cell))
                    (cell:write! cell next)
                    (output:write-char c out))
         (Ok
          (Tuple3 (output:get-output-stream-string out)
                  (cell:read cell)
                  stack))))))

  (declare collect-while ((Char -> Optional (Parser :s :a)) -> Parser :s (List :a)))
  (define (collect-while f)
    (Parser
     (fn ((Tuple port stack))
       (let result = (cell:new Nil))
       (let port* = (cell:new port))
       (let stack* = (cell:new stack))
       (loop
        (match (peek (cell:read port*))
          ((Some c)
           (match (f c)
             ((None) (break))
             ((Some (Parser parse!))
              (match (parse! (Tuple (cell:read port*) (cell:read stack*)))
                ((Ok (Tuple3 elem port stack))
                 (cell:write! port* port)
                 (cell:write! stack* stack)
                 (cell:write! result
                              (cons elem (cell:read result)))
                 Unit)
                ((Err e)
                 (return (Err e)))))))
          ((None)
           (return (Err "Unexpected eof")))))
       (pure (Tuple3 (reverse (cell:read result))
                     (cell:read port*)
                     (cell:read stack*)))))))
