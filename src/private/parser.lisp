(defpackage #:tokyo.tojo.json/private/parser
  (:use #:coalton
        #:coalton-prelude)
  (:local-nicknames
   (#:cell #:coalton-library/cell)
   (#:output #:tokyo.tojo.json/private/output-stream)
   (#:port #:tokyo.tojo.json/private/port))
  (:export #:Parser
           #:peek-char
           #:peek-char-or-eof
           #:read-char-or-eof
           #:read-char
           #:take-until-string
           #:run!
           #:fold-while
           #:do-while))

(in-package #:tokyo.tojo.json/private/parser)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel

  (repr :transparent)
  (define-type (Parser :a) (Parser (port:Port -> Result String (Tuple :a port:Port))))

  (define-instance (Functor Parser)
    (define (map f (Parser parse!))
      (Parser
       (fn (port)
         (do ((Tuple x port) <- (parse! port))
             (pure (Tuple (f x) port)))))))

  (define-instance (Applicative Parser)
    (define (pure x)
      (Parser (fn (port) (Ok (Tuple x port)))))

    (define (liftA2 op (Parser parse1!) (Parser parse2!))
      (Parser
       (fn (port)
         (do ((Tuple x port) <- (parse1! port))
             ((Tuple y port) <- (parse2! port))
             (pure (Tuple (op x y) port)))))))

  (define-instance (Monad Parser)
    (define (>>= (Parser parse!) f)
      (Parser
       (fn (port)
         (do ((Tuple x port) <- (parse! port))
             (let (Parser parse!) = (f x))
             ((Tuple x port) <- (parse! port))
             (pure (Tuple x port)))))))

  (define-instance (MonadFail Parser)
    (define (fail msg)
      (Parser (const (Err msg)))))

  (declare peek-char-or-eof (Parser (Optional Char)))
  (define peek-char-or-eof
    (Parser (fn (port) (Ok (Tuple (port:peek port) port)))))

  (declare peek-char (Parser Char))
  (define peek-char
    (do (opt <- peek-char-or-eof)
        (match opt
          ((Some c) (pure c))
          ((None) (Parser (const (Err "Unexpected eof")))))))

  (declare read-char-or-eof (Parser (Optional Char)))
  (define read-char-or-eof
    (Parser
     (fn (port)
       (match (port:read! port)
         ((Some (Tuple c port)) (Ok (Tuple (Some c) port)))
         ((None) (Ok (Tuple None port)))))))

  (declare read-char (Parser Char))
  (define read-char
    (Parser
     (fn (port)
       (match (port:read! port)
         ((Some (Tuple c port)) (Ok (Tuple c port)))
         ((None) (Err "Unexpected eof"))))))

  (declare take-until-string ((Char -> Boolean) -> Parser String))
  (define (take-until-string end?)
    (Parser
     (fn (port)
       (let ((out (output:make-string-output-stream))
             (cell (cell:new port)))
         (while-let (Some (Tuple c next)) = (port:peek-or-read! (complement end?) (cell:read cell))
                    (cell:write! cell next)
                    (output:write-char c out))
         (Ok
          (Tuple (output:get-output-stream-string out)
                 (cell:read cell)))))))

  (declare run! (Parser :a -> port:Port -> Result String :a))
  (define (run! (Parser parse!) port)
    (map fst (parse! port)))

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
       (Ok (Tuple (cell:read acc*) (cell:read port*))))))

  (declare do-while (Parser Boolean -> Parser Unit))
  (define (do-while p)
    (fold-while (fn ((Unit) (Unit))
                  (do (b <- p)
                      (if b
                          (pure (Tuple Unit (Some Unit)))
                          (pure (Tuple Unit None)))))
                Unit
                Unit)))
