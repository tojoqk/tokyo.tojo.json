(in-package #:tokyo.tojo.json/test)

(named-readtables:in-readtable coalton:coalton)

(define-test parse-symbol-test ()
  (matches (Ok json:Null)
      (json:parse "null"))
  (matches (Ok json:True)
      (json:parse "true"))
  (matches (Ok json:False)
      (json:parse "false"))
  (matches (Err _)
      (json:parse "nil")))

(define-test parse-number-test ()
  (matches (Ok (json:Number 42.0d0))
      (json:parse "42"))
  (matches (Ok (json:Number 42.0d0))
      (json:parse "  42.0  "))
  (matches (Ok (json:Number 42.0d0))
      (json:parse "   42e0"))
  (matches (Ok (json:Number 420d0))
      (json:parse "42e001"))
  (matches (Ok (json:Number 4200d0))
      (json:parse "42e2"))
  (matches (Ok (json:Number 123456789d0))
      (json:parse "123456789"))
  (matches (Ok (json:Number 4200d0))
      (json:parse "42e+2"))
  (matches (Ok (json:Number 0.42d0))
      (json:parse "42e-2"))
  (matches (Err _)
      (json:parse "0123"))
  (matches (Err _)
      (json:parse "133."))
  (matches (Err _)
      (json:parse "133a")))

(define-test parse-string-test ()
  (matches (Ok (json:String "hello"))
      (json:parse "\"hello\"")))

(define-test parse-array-test ()
  (is (pipe "[\"hello\", 3, true, false  ]"
            json:parse
            (== (Ok (json:Array
                     (make-list (json:String "hello")
                                (json:Number 3d0)
                                json:True
                                json:False))))))
  (matches (Err _)
      (json:parse "[133, ]"))
  (matches (Err _)
      (json:parse "[133, , 3]"))
  (is (pipe (json:parse "[133, [1, 2, 3], 3]")
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
            json:parse
            (== (Ok (make-object
                     (make-list (Tuple "hello" (json:String "world"))))))))
  (is (pipe "
{\"test\" : 42, \"object\": { \"true\": true, \"false\" : false },
 \"array\": [10, 3.2, \"test\"], \"null\" : null }"
            json:parse
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
  (matches (Ok (json:Object Nil))
      (json:parse "{}"))
  (matches (Err _)
      (json:parse "{,}"))
  (matches (Err _)
      (json:parse "{\"test\" : 42,}")))
