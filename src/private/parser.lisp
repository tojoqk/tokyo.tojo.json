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
           #:run!
           #:collect-while
           #:fold-while))

(in-package #:tokyo.tojo.json/private/parser)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel

  ;;
  ;; Port
  ;;

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

  ;;
  ;; Parser
  ;;

  (repr :transparent)
  (define-type (Parser :a) (Parser (Port -> Result String (Tuple :a Port))))

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
    (Parser (fn (port) (Ok (Tuple (peek port) port)))))

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
       (match (read! port)
         ((Some (Tuple c port)) (Ok (Tuple (Some c) port)))
         ((None) (Ok (Tuple None port)))))))

  (declare read-char (Parser Char))
  (define read-char
    (Parser
     (fn (port)
       (match (read! port)
         ((Some (Tuple c port)) (Ok (Tuple c port)))
         ((None) (Err "Unexpected eof"))))))

  (declare take-until-string ((Char -> Boolean) -> Parser String))
  (define (take-until-string end?)
    (Parser
     (fn (port)
       (let ((out (output:make-string-output-stream))
             (cell (cell:new port)))
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
  (define (run! (Parser parse!) port)
    (map fst (parse! port)))

  (declare collect-while ((Char -> Optional (Parser :a)) -> Parser (List :a)))
  (define (collect-while f)
    (Parser
     (fn (port)
       (let result = (cell:new Nil))
       (let port* = (cell:new port))
       (loop
         (match (peek (cell:read port*))
           ((Some c)
            (match (f c)
              ((None) (break))
              ((Some (Parser parse!))
               (match (parse! (cell:read port*))
                 ((Ok (Tuple elem port))
                  (cell:write! port* port)
                  (cell:write! result
                               (cons elem (cell:read result)))
                  Unit)
                 ((Err e)
                  (return (Err e)))))))
           ((None)
            (return (Err "Unexpected eof")))))
       (pure (Tuple (reverse (cell:read result))
                    (cell:read port*))))))

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
