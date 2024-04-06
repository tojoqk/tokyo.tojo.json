(defpackage #:tokyo.tojo.json/test
  (:use #:coalton-testing)
  (:local-nicknames
   (#:map #:coalton-library/ord-map)
   (#:json #:tokyo.tojo.json/json)
   (#:parser #:tokyo.tojo.json/parser))
  (:export #:run-tests))

(in-package #:tokyo.tojo.json/test)

(named-readtables:in-readtable coalton:coalton)

(fiasco:define-test-package #:tokyo.tojo.json/fiasco-test-package)

(coalton-fiasco-init #:tokyo.tojo.json/fiasco-test-package)

(cl:defun run-tests ()
  (fiasco:run-package-tests
   :packages '(#:tokyo.tojo.json/fiasco-test-package)
   :interactive cl:t))

(define-test parse-symbol-test ()
  (matches (Ok json:Null)
      (parser:parse "null"))
  (matches (Ok json:True)
      (parser:parse "true"))
  (matches (Ok json:False)
      (parser:parse "false"))
  (matches (Err _)
      (parser:parse "nil")))

(define-test parse-number-test ()
  (matches (Ok (json:Number 42.0d0))
      (parser:parse "42"))
  (matches (Ok (json:Number 42.0d0))
      (parser:parse "  42.0  "))
  (matches (Ok (json:Number 42.0d0))
      (parser:parse "   42e0"))
  (matches (Ok (json:Number 420d0))
      (parser:parse "42e001"))
  (matches (Ok (json:Number 4200d0))
      (parser:parse "42e2"))
  (matches (Ok (json:Number 123456789d0))
      (parser:parse "123456789"))
  (matches (Ok (json:Number 4200d0))
      (parser:parse "42e+2"))
  (matches (Ok (json:Number 0.42d0))
      (parser:parse "42e-2"))
  (matches (Err _)
      (parser:parse "0123"))
  (matches (Err _)
      (parser:parse "133."))
  (matches (Err _)
      (parser:parse "133a")))

(define-test parse-string-test ()
  (matches (Ok (json:String "hello"))
      (parser:parse "\"hello\"")))

(define-test parse-array-test ()
  (is (pipe "[\"hello\", 3, true, false  ]"
            parser:parse
            (== (Ok (json:Array
                     (make-list (json:String "hello")
                                (json:Number 3d0)
                                json:True
                                json:False))))))
  (matches (Err _)
      (parser:parse "[133, ]"))
  (matches (Err _)
      (parser:parse "[133, , 3]"))
  (is (pipe (parser:parse "[133, [1, 2, 3], 3]")
            (== (Ok (json:Array
                     (make-list (json:Number 133d0)
                                (json:Array
                                 (make-list
                                  (json:Number 1d0)
                                  (json:Number 2d0)
                                  (json:Number 3d0)))
                                (json:Number 3d0))))))))

(coalton-toplevel
  (define (make-object pairs)
    (json:Object
     (foldr (fn ((Tuple k v) m) (map:insert-or-replace m k v))
            map:empty
            pairs))))

(define-test parse-object-test ()
  (is (pipe "{\"hello\" : \"world\" }"
            parser:parse
            (== (Ok (make-object
                     (make-list (Tuple "hello" (json:String "world"))))))))
  (is (pipe "
{\"test\" : 42, \"object\": { \"true\": true, \"false\" : false },
 \"array\": [10, 3.2, \"test\"], \"null\" : null }"
            parser:parse
            (== (Ok (make-object
                     (make-list (Tuple "test" (json:Number 42d0))
                                (Tuple "object"
                                       (make-object
                                        (make-list (Tuple "true" json:True)
                                                   (Tuple "false" json:False))))
                                (Tuple "array"
                                       (json:Array
                                        (make-list (json:Number 10d0)
                                                   (json:Number 3.2d0)
                                                   (json:String "test"))))
                                (Tuple "null" json:Null)))))))
  (matches (Ok (json:Object map:empty))
      (parser:parse "{}"))
  (matches (Err _)
      (parser:parse "{,}"))
  (matches (Err _)
      (parser:parse "{\"test\" : 42,}")))
