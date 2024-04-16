(defpackage #:tokyo.tojo.json/private/parser
  (:use #:coalton
        #:coalton-prelude)
  (:shadow #:error)
  (:local-nicknames
   (#:cell #:coalton-library/cell)
   (#:result #:coalton-library/result)
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

           #:push-new-buffer
           #:pop-string
           #:write-char
           #:write-string))

(in-package #:tokyo.tojo.json/private/parser)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel

  (repr :transparent)
  (define-type (Parser :a)
    (Parser ((Tuple port:Port (List output:Stream)) -> Result String (Tuple3 :a port:Port (List output:Stream)))))

  (define-instance (Functor Parser)
    (define (map f (Parser parse!))
      (Parser
       (fn ((Tuple port streams))
         (do ((Tuple3 x port streams) <- (parse! (Tuple port streams)))
             (pure (Tuple3 (f x) port streams)))))))

  (define-instance (Applicative Parser)
    (define (pure x)
      (Parser (fn ((Tuple port streams)) (Ok (Tuple3 x port streams)))))

    (define (liftA2 op (Parser parse1!) (Parser parse2!))
      (Parser
       (fn ((Tuple port streams))
         (do ((Tuple3 x port streams) <- (parse1! (Tuple port streams)))
             ((Tuple3 y port streams) <- (parse2! (Tuple port streams)))
             (pure (Tuple3 (op x y) port streams)))))))

  (define-instance (Monad Parser)
    (define (>>= (Parser parse!) f)
      (Parser
       (fn ((Tuple port streams))
         (do ((Tuple3 x port streams) <- (parse! (Tuple port streams)))
             (let (Parser parse!) = (f x))
             ((Tuple3 x port streams) <- (parse! (Tuple port streams)))
             (pure (Tuple3 x port streams)))))))

  (define-instance (MonadFail Parser)
    (define (fail msg)
      (Parser (const (Err msg)))))

  (declare peek-char-or-eof (Parser (Optional Char)))
  (define peek-char-or-eof
    (Parser (fn ((Tuple port streams))
              (do (let opt-ch = (port:peek port))
                  (pure (Tuple3 opt-ch port streams))))))

  (declare peek-char (Parser Char))
  (define peek-char
    (do (opt <- peek-char-or-eof)
        (match opt
          ((Some c) (pure c))
          ((None) (Parser (const (Err "Unexpected eof")))))))

  (declare read-char-or-eof (Parser (Optional Char)))
  (define read-char-or-eof
    (Parser
     (fn ((Tuple port streams))
       (do (let opt-ch = (port:read! port))
           (match opt-ch
             ((Some (Tuple ch port)) (Ok (Tuple3 (Some ch) port streams)))
             ((None) (Ok (Tuple3 None port streams))))))))

  (declare read-char (Parser Char))
  (define read-char
    (Parser
     (fn ((Tuple port streams))
       (do (let opt-ch = (port:read! port))
           (match opt-ch
             ((Some (Tuple ch port)) (Ok (Tuple3 ch port streams)))
             ((None) (Err "Unexpected eof")))))))

  (declare run! (Parser :a -> port:Port -> Result String (Tuple :a port:Port)))
  (define (run! (Parser parse!) port)
    (map (fn ((Tuple3 x port _)) (Tuple x port))
         (parse! (Tuple port (singleton (output:make-string-output-stream))))))

  (declare fold-while ((:a -> :c -> Parser (Tuple :a (Optional :c))) -> :a -> :c -> Parser :a))
  (define (fold-while f acc state)
    (Parser
     (fn ((Tuple port streams))
       (let port* = (cell:new port))
       (let acc* = (cell:new acc))
       (let state* = (cell:new state))
       (let streams* = (cell:new streams))
       (loop
         (let (Parser parse!) = (f (cell:read acc*) (cell:read state*)))
         (match (parse! (Tuple (cell:read port*) (cell:read streams*)))
           ((Ok (Tuple3 (Tuple acc opt) port streams))
            (cell:write! port* port)
            (cell:write! streams* streams)
            (cell:write! acc* acc)
            (match opt
              ((Some state)
               (cell:write! state* state)
               Unit)
              ((None) (break))))
           ((Err e) (return (Err e)))))
       (Ok (Tuple3 (cell:read acc*)
                   (cell:read port*)
                   (cell:read streams*))))))

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

  (declare push-new-buffer (Parser Unit))
  (define push-new-buffer
    (Parser
     (fn ((Tuple port streams))
       (Ok (Tuple3 Unit
                   port
                   (Cons (output:make-string-output-stream)
                         streams))))))

  (declare pop-string (Parser String))
  (define pop-string
    (Parser
     (fn ((Tuple port streams))
       (match streams
         ((Cons s ss)
          (Ok (Tuple3 (output:get-output-stream-string s) port ss)))
         ((Nil)
          (Err "pop-buffer: String buffer stack undeflow"))))))

  (declare write-char (Char -> Parser Unit))
  (define (write-char ch)
    (Parser
     (fn ((Tuple port streams))
       (match streams
         ((Cons stream _)
          (output:write-char ch stream)
          (Ok (Tuple3 Unit port streams)))
         ((Nil)
          (Err "write-char: No string buffer"))))))

  (declare write-string (String -> Parser Unit))
  (define (write-string str)
    (Parser
     (fn ((Tuple port streams))
       (match streams
         ((Cons stream _)
          (output:write-string str stream)
          (Ok (Tuple3 Unit port streams)))
         ((Nil)
          (Err "write-string: No string buffer")))))))
