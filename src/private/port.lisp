(defpackage #:tokyo.tojo.json/private/port
  (:use #:coalton
        #:coalton-prelude)
  (:shadow #:error)
  (:local-nicknames
   (#:iter #:coalton-library/iterator))
  (:export #:Port
           #:make!
           #:peek
           #:read!
           #:peek-or-read!

           #:Error
           #:ReadError))

(in-package #:tokyo.tojo.json/private/port)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel

  ;; JSON is LL(1) grammar, so it only requires lookahead of one character.
  (define-type (Port :e) (%Port (Optional Char) (iter:Iterator (Result :e Char))))

  (define-type (Error :e)
    (ReadError :e))

  (declare make! (iter:Iterator (Result :e Char) -> (Result (Error :e) (Port :e))))
  (define (make! iter)
    (match (iter:next! iter)
      ((Some (Ok c)) (Ok (%Port (Some c) iter)))
      ((None) (Ok (%Port None iter)))
      ((Some (Err e)) (Err (ReadError e)))))

  (declare peek (Port :e -> Result (Error :e) (Optional Char)))
  (define (peek (%Port opt _))
    (match opt
      ((Some c) (Ok (Some c)))
      ((None) (Ok None))))

  (declare read! (Port :e -> (Result (Error :e) (Optional (Tuple Char (Port :e))))))
  (define (read! p)
    (match p
      ((%Port (None) _) (Ok None))
      ((%Port (Some ch) iter)
       (match (iter:next! iter)
         ((Some (Ok next-ch))
          (Ok (Some (Tuple ch (%Port (Some next-ch) iter)))))
         ((None) (Ok (Some (Tuple ch (%Port None iter)))))
         ((Some (Err e))
          (Err (ReadError e)))))))

  (declare peek-or-read! ((char -> Boolean) -> Port :e -> Result (Error :e) (Optional (Tuple Char (Port :e)))))
  (define (peek-or-read! read? (%Port opt iter))
    (match opt
      ((Some ch)
       (cond ((read? ch)
              (match (iter:next! iter)
                ((Some (Ok next-ch))
                 (Ok (Some (Tuple ch (%Port (Some next-ch) iter)))))
                ((None) (Ok None))
                ((Some (Err e))
                 (Err (ReadError e)))))
             (True (Ok None))))
      ((None) (Ok None)))))
