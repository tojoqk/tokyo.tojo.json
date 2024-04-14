(defpackage #:tokyo.tojo.json/private/parser
  (:use #:coalton
        #:coalton-prelude)
  (:local-nicknames
   (#:iter #:coalton-library/iterator)
   (#:cell #:coalton-library/cell)
   (#:optional #:coalton-library/optional)
   (#:map #:coalton-library/ord-map)
   (#:output #:tokyo.tojo.json/private/output-stream))
  (:export #:Port
           #:make-port!

           #:Parser
           #:peek-char
           #:peek-char-or-eof
           #:read-char-or-eof
           #:read-char
           #:take-until-string
           #:delay
           #:run!
           #:collect-while
           #:fold-while

           #:guard
           #:guard-char
           #:guard-eof
           #:guard-lookup
           #:guard-else
           #:from-guard))

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
  (define-type (Parser :a) (Parser (Port -> Result String (Tuple :a Port))))

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
      (Parser (const (Err msg)))))

  (repr :transparent)
  (define-type (Guard :a)
    (%Guard ((Optional Char) -> (Optional (Parser :a)))))

  (declare guard ((Optional Char -> Optional :in) ->  (:in -> Parser :a) -> Guard :a))
  (define (guard f make-parser)
    (%Guard (.> f (map make-parser))))

  (declare alt-guard (Alternative :f => Boolean -> (:f Unit)))
  (define (alt-guard b)
    (if b
        (pure Unit)
        empty))

  (declare guard-char ((Char -> Boolean) -> Parser :a -> Guard :a))
  (define (guard-char p? parser)
    (guard (fn (opt)
             (do (c <- opt)
                 (alt-guard (p? c))))
           (fn ((Unit)) parser)))

  (define (guard-eof parser)
    (guard (fn (opt)
             (alt-guard (optional:none? opt)))
           (fn ((Unit)) parser)))

  (declare guard-lookup (map:Map Char :b -> (:b -> Parser :a) -> Guard :a))
  (define (guard-lookup m make-parser)
    (%Guard (fn (opt)
              (do (c <- opt)
                  (x <- (map:lookup m c))
                (pure (make-parser x))))))

  (declare guard-else (Parser :a -> Guard :a))
  (define (guard-else parser)
    (%Guard (const (pure parser))))

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

  (declare from-guard (Guard :a -> Parser :a))
  (define (from-guard (%Guard g))
    (do (opt <- peek-char-or-eof)
        (match (g opt)
          ((Some p) p)
          ((None)
           (match opt
             ((Some c) (fail (<> "Unexpected char: " (into (make-list c)))))
             ((None) (fail "Unexpected eof")))))))

  (declare peek-char-or-eof (Parser (Optional Char)))
  (define peek-char-or-eof
    (Parser (fn (in) (Ok (Tuple (peek in) in)))))

  (declare peek-char (Parser Char))
  (define peek-char
    (do (opt <- peek-char-or-eof)
        (match opt
          ((Some c) (pure c))
          ((None) (Parser (const (Err "Unexpected eof")))))))

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
         ((None) (Err "Unexpected eof"))))))

  (declare take-until-string ((Char -> Boolean) -> Parser String))
  (define (take-until-string end?)
    (Parser
     (fn (in)
       (let ((out (output:make-string-output-stream))
             (cell (cell:new in)))
         (while-let (Some (Tuple c next)) = (end-or-read! end? (cell:read cell))
                    (cell:write! cell next)
                    (output:write-char c out))
         (Ok
          (Tuple (output:get-output-stream-string out)
                 (cell:read cell)))))))

  (declare make-port! (iter:Iterator Char -> Port))
  (define (make-port! iter)
    (%Port (iter:next! iter) iter))

  (declare run! (Parser :a -> Port -> Result String :a))
  (define (run! (Parser parse!) in)
    (>>= (parse! in)
         (fn ((Tuple x _))
           (pure x))))

  (declare collect-while ((Char -> Optional (Parser :a)) -> Parser (List :a)))
  (define (collect-while f)
    (Parser
     (fn (in)
       (let result = (cell:new Nil))
       (let in* = (cell:new in))
       (loop
         (match (peek (cell:read in*))
           ((Some c)
            (match (f c)
              ((None) (break))
              ((Some (Parser parse!))
               (match (parse! (cell:read in*))
                 ((Ok (Tuple elem in_))
                  (cell:write! in* in_)
                  (cell:write! result
                               (cons elem (cell:read result)))
                  Unit)
                 ((Err e)
                  (return (Err e)))))))
           ((None)
            (return (Err "Unexpected eof")))))
       (pure (Tuple (reverse (cell:read result))
                    (cell:read in*))))))

  (declare fold-while ((:a -> :c -> Parser (Tuple :a (Optional :c))) -> :a -> :c -> Parser :a))
  (define (fold-while f acc state)
    (Parser
     (fn (port)
       (let port* = (cell:new port))
       (let acc* = (cell:new acc))
       (let state* = (cell:new state))
       (loop
         (let (Parser parse!) = (f (cell:read acc*) (cell:read state*)))
         (match (parse! (cell:read port*))
           ((Ok (Tuple (Tuple acc opt) port))
            (cell:write! port* port)
            (cell:write! acc* acc)
            (match opt
              ((Some state)
               (cell:write! state* state)
               Unit)
              ((None) (break))))
           ((Err e) (return (Err e)))))
       (Ok (Tuple (cell:read acc*) (cell:read port*)))))))
