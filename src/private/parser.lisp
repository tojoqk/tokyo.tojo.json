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
           #:run!
           #:fold-while
           #:do-while

           #:clear
           #:get-string
           #:write-char
           #:write-string))

(in-package #:tokyo.tojo.json/private/parser)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel

  (repr :transparent)
  (define-type (Parser :a)
    (Parser ((Tuple port:Port output:Stream) -> Result String (Tuple3 :a port:Port output:Stream))))


  (define-instance (Functor Parser)
    (define (map f (Parser parse!))
      (Parser
       (fn ((Tuple port stream))
         (do ((Tuple3 x port stream) <- (parse! (Tuple port stream)))
             (pure (Tuple3 (f x) port stream)))))))

  (define-instance (Applicative Parser)
    (define (pure x)
      (Parser (fn ((Tuple port stream)) (Ok (Tuple3 x port stream)))))

    (define (liftA2 op (Parser parse1!) (Parser parse2!))
      (Parser
       (fn ((Tuple port stream))
         (do ((Tuple3 x port stream) <- (parse1! (Tuple port stream)))
             ((Tuple3 y port stream) <- (parse2! (Tuple port stream)))
             (pure (Tuple3 (op x y) port stream)))))))

  (define-instance (Monad Parser)
    (define (>>= (Parser parse!) f)
      (Parser
       (fn ((Tuple port stream))
         (do ((Tuple3 x port stream) <- (parse! (Tuple port stream)))
             (let (Parser parse!) = (f x))
             ((Tuple3 x port stream) <- (parse! (Tuple port stream)))
             (pure (Tuple3 x port stream)))))))

  (define-instance (MonadFail Parser)
    (define (fail msg)
      (Parser (const (Err msg)))))

  (declare peek-char-or-eof (Parser (Optional Char)))
  (define peek-char-or-eof
    (Parser (fn ((Tuple port stream))
              (Ok (Tuple3 (port:peek port) port stream)))))

  (declare peek-char (Parser Char))
  (define peek-char
    (do (opt <- peek-char-or-eof)
        (match opt
          ((Some c) (pure c))
          ((None) (Parser (const (Err "Unexpected eof")))))))

  (declare read-char-or-eof (Parser (Optional Char)))
  (define read-char-or-eof
    (Parser
     (fn ((Tuple port stream))
       (match (port:read! port)
         ((Some (Tuple c port)) (Ok (Tuple3 (Some c) port stream)))
         ((None) (Ok (Tuple3 None port stream)))))))

  (declare read-char (Parser Char))
  (define read-char
    (Parser
     (fn ((Tuple port stream))
       (match (port:read! port)
         ((Some (Tuple c port)) (Ok (Tuple3 c port stream)))
         ((None) (Err "Unexpected eof"))))))

  (declare run! (Parser :a -> port:Port -> Result String :a))
  (define (run! (Parser parse!) port)
    (map (fn ((Tuple3 x _ _)) x)
         (parse! (Tuple port (output:make-string-output-stream)))))

  (declare fold-while ((:a -> :c -> Parser (Tuple :a (Optional :c))) -> :a -> :c -> Parser :a))
  (define (fold-while f acc state)
    (Parser
     (fn ((Tuple port stream))
       (let port* = (cell:new port))
       (let acc* = (cell:new acc))
       (let state* = (cell:new state))
       (let stream* = (cell:new stream))
       (loop
         (let (Parser parse!) = (f (cell:read acc*) (cell:read state*)))
         (match (parse! (Tuple (cell:read port*) (cell:read stream*)))
           ((Ok (Tuple3 (Tuple acc opt) port stream))
            (cell:write! port* port)
            (cell:write! stream* stream)
            (cell:write! acc* acc)
            (match opt
              ((Some state)
               (cell:write! state* state)
               Unit)
              ((None) (break))))
           ((Err e) (return (Err e)))))
       (Ok (Tuple3 (cell:read acc*)
                   (cell:read port*)
                   (cell:read stream*))))))

  (declare do-while (Parser Boolean -> Parser Unit))
  (define (do-while p)
    (fold-while (fn ((Unit) (Unit))
                  (do (b <- p)
                      (if b
                          (pure (Tuple Unit (Some Unit)))
                          (pure (Tuple Unit None)))))
                Unit
                Unit))

  ;;
  ;; String Buffer feature
  ;;

  (define clear
    (Parser
     (fn ((Tuple port _))
       (Ok (Tuple3 Unit port (output:make-string-output-stream))))))

  (define get-string
    (Parser
     (fn ((Tuple port stream))
       (Ok (Tuple3 (output:get-output-stream-string stream) port stream)))))

  (define (write-char ch)
    (Parser
     (fn ((Tuple port stream))
       (output:write-char ch stream)
       (Ok (Tuple3 Unit port stream)))))

  (define (write-string str)
    (Parser
     (fn ((Tuple port stream))
       (output:write-string str stream)
       (Ok (Tuple3 Unit port stream))))))
