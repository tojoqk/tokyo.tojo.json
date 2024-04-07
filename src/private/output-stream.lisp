(defpackage #:tokyo.tojo.json/private/output-stream
  (:use #:coalton
        #:coalton-prelude)
  (:export #:Stream
           #:write-string
           #:write-char
           #:make-string-output-stream
           #:get-output-stream-string))

(in-package #:tokyo.tojo.json/private/output-stream)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel
  (repr :native cl:stream)
  (define-type Stream)

  (declare write-string (String -> Stream -> Unit))
  (define (write-string str s)
    (lisp Unit (str s)
      (cl:write-string str s)
      Unit))

  (declare write-char (Char -> Stream -> Unit))
  (define (write-char c s)
    (lisp Unit (c s)
      (cl:write-char c s)
      Unit))

  (declare make-string-output-stream (Unit -> Stream))
  (define (make-string-output-stream)
    (lisp Stream ()
      (cl:make-string-output-stream)))

  (declare get-output-stream-string (Stream -> String))
  (define (get-output-stream-string s)
    (lisp String (s)
      (cl:get-output-stream-string s))))
