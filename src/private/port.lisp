(defpackage #:tokyo.tojo.json/private/port
  (:use #:coalton
        #:coalton-prelude)
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
  (define (peek (%Port c _)) c)

  (declare read! (Port -> (Optional (Tuple Char Port))))
  (define (read! p)
    (match p
      ((%Port (None) _) None)
      ((%Port (Some c) iter)
       (let c_ = (iter:next! iter))
       (Some (Tuple c (%Port c_ iter))))))

  (declare peek-or-read! ((char -> Boolean) -> Port -> Optional (Tuple Char Port)))
  (define (peek-or-read! read? (%Port opt iter))
    (do (ch <- opt)
        (cond ((read? ch)
               (let next-ch = (iter:next! iter))
               (Some (Tuple ch (%Port next-ch iter))))
              (True None)))))
