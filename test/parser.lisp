(in-package #:tokyo.tojo.json/test)

(named-readtables:in-readtable coalton:coalton)

(define-test parse-symbol-test ()
  (matches (Ok json:Null)
      (the (Result String json:JSON)
           (tryInto "null")))
  (matches (Ok json:True)
      (the (Result String json:JSON)
           (tryInto "true")))
  (matches (Ok json:False)
      (the (Result String json:JSON)
           (tryInto "false")))
  (matches (Err _)
      (the (Result String json:JSON)
           (tryInto "nil"))))

(define-test parse-number-test ()
  (matches (Ok (json:Number 42.0d0))
      (the (Result String json:JSON)
           (tryInto "42")))
  (matches (Ok (json:Number 42.0d0))
      (the (Result String json:JSON)
           (tryInto "  42.0  ")))
  (matches (Ok (json:Number 42.0d0))
      (the (Result String json:JSON)
           (tryInto "   42e0")))
  (matches (Ok (json:Number 420d0))
      (the (Result String json:JSON)
           (tryInto "42e001")))
  (matches (Ok (json:Number 4200d0))
      (the (Result String json:JSON)
           (tryInto "42e2")))
  (matches (Ok (json:Number 123456789d0))
      (the (Result String json:JSON)
           (tryInto "123456789")))
  (matches (Ok (json:Number 4200d0))
      (the (Result String json:JSON)
           (tryInto "42e+2")))
  (matches (Ok (json:Number 0.42d0))
      (the (Result String json:JSON)
           (tryInto "42e-2")))
  (matches (Err _)
      (the (Result String json:JSON)
           (tryInto "0123")))
  (matches (Err _)
      (the (Result String json:JSON)
           (tryInto "133.")))
  (matches (Err _)
      (the (Result String json:JSON)
           (tryInto "133a"))))

(define-test parse-string-test ()
  (matches (Ok (json:String "hello"))
      (the (Result String json:JSON)
           (tryInto "\"hello\""))))

(define-test parse-array-test ()
  (is (pipe "[\"hello\", 3, true, false  ]"
            tryInto
            (== (Ok (json:Array
                     (make-list (json:String "hello")
                                (json:Number 3d0)
                                json:True
                                json:False))))))
  (matches (Err _)
      (the (Result String json:JSON)
           (tryInto "[133, ]")))
  (matches (Err _)
      (the (Result String json:JSON)
           (tryInto "[133, , 3]")))
  (is (pipe (tryInto "[133, [1, 2, 3], 3]")
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
            tryInto
            (== (Ok (make-object
                     (make-list (Tuple "hello" (json:String "world"))))))))
  (is (pipe "
{\"test\" : 42, \"object\": { \"true\": true, \"false\" : false },
 \"array\": [10, 3.2, \"test\"], \"null\" : null }"
            tryInto
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
      (tryInto "{}"))
  (matches (Err _)
      (the (Result String json:JSON)
           (tryInto "{,}")))
  (matches (Err _)
      (the (Result String json:JSON)
           (tryInto "{\"test\" : 42,}"))))

(define-test parse-multiple-jsons ()
  (matches (Ok (Cons (json:Array (Cons (json:Number 1d0)
                                       (Cons (json:Number 2d0)
                                             (Cons (json:Number 3d0) (Nil)))))
                     (Cons (json:Number 42d0)
                           (Cons (json:String "hello")
                                 (Nil)))))
      (sequence (iter:collect! (json:parse! (iter:into-iter "[1, 2, 3] 42

 \"hello\"

")))))

  (matches (Some (Ok (json:Array (Cons (json:Number 1d0)
                                       (Cons (json:Number 2d0)
                                             (Cons (json:Number 3d0) (Nil)))))))
      (head (iter:collect! (json:parse! (iter:into-iter "[1, 2, 3] 42 invalid

 \"hello\"

")))))

  (matches (Err _)
      (the (Result String (List json:JSON))
           (sequence (iter:collect! (json:parse! (iter:into-iter "[1, 2, 3] 42 invalid

 \"hello\"

")))))))
