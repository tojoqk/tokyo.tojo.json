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
           #:peek-or-read!))

(in-package #:tokyo.tojo.json/private/port)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel

  ;; JSON is LL(1) grammar, so it only requires lookahead of one character.
  (define-type Port (%Port (Optional Char) (iter:Iterator Char)))

  (declare make! (iter:Iterator Char -> Port))
  (define (make! iter)
    (%Port (iter:next! iter) iter))

  (declare peek (Port -> Optional Char))
  (define (peek (%Port opt _))
    (match opt
      ((Some c) (Some c))
      ((None)  None)))

  (declare read! (Port -> (Optional (Tuple Char Port))))
  (define (read! p)
    (match p
      ((%Port (None) _) None)
      ((%Port (Some ch) iter)
       (match (iter:next! iter)
         ((Some next-ch)
          (Some (Tuple ch (%Port (Some next-ch) iter))))
         ((None) (Some (Tuple ch (%Port None iter))))))))

  (declare peek-or-read! ((char -> Boolean) -> Port -> (Optional (Tuple Char Port))))
  (define (peek-or-read! read? (%Port opt iter))
    (match opt
      ((Some ch)
       (cond ((read? ch)
              (match (iter:next! iter)
                ((Some next-ch)
                 (Some (Tuple ch (%Port (Some next-ch) iter))))
                ((None) None)))))
      ((None) None))))
