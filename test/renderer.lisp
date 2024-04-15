(in-package #:tokyo.tojo.json/test)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel
  (define (make-object pairs)
    (json:Object
     (foldr (fn ((Tuple k v) m) (map:insert-or-replace m k v))
            map:empty
            pairs))))

(define-test render-null-test ()
  (matches "null"
      (into json:Null)))

(define-test render-boolean-test ()
  (matches "true"
      (into json:True))
  (matches "false"
      (into json:False)))

(define-test render-number-test ()
  (matches "1.0"
      (into (json:Number 1d0)))
  (matches "0.5"
      (into (json:Number 0.5d0)))
  (matches "1024.0"
      (into (json:Number 1024d0)))
  (matches "1024.1024"
      (into (json:Number 1024.1024d0))))

(define-test render-string-test ()
  (matches "\"\""
      (into (json:String "")))
  (matches "\"string\""
      (into (json:String "string")))
  (matches "\"\\\"string\\\"\""
      (into (json:String "\"string\"")))
  (matches "\"\\\\^ _ ^\\/\""
      (into (json:String "\\^ _ ^/")))
  (matches "\"\\nnewline\\n\""
      (into (json:String "
newline
"))))

(define-test render-array-test ()
  (matches "[]"
      (into (json:Array (make-list))))
  (matches "[1.0]"
      (into (json:Array (make-list (json:Number 1d0)))))
  (matches "[1.0,2.0]"
      (into (json:Array (make-list (json:Number 1d0)
                                   (json:Number 2d0)))))
  (matches "[true,null,false,5.5,\"test\"]"
      (into (json:Array (make-list json:True
                                   json:Null
                                   json:False
                                   (json:Number 5.5d0)
                                   (json:String "test")))))
  (matches "[[],[[1.0]],[[[]]]]"
      (into (json:Array
             (make-list (json:Array Nil)
                        (json:Array (singleton (json:Array (singleton (json:Number 1d0)))))
                        (json:Array (singleton (json:Array (singleton (json:Array Nil))))))))))

(define-test render-object-test ()
  (matches "{}"
      (into (make-object Nil)))
  (matches "{\"one\":1.0\}"
      (into (make-object
             (make-list (Tuple "one" (json:Number 1d0))))))
  (is (let ((obj
              (make-object (make-list (Tuple "one" (json:Number 1d0))
                                      (Tuple "two" (json:Number 2d0))
                                      (Tuple "three" (json:Number 3d0))))))
        (== (tryInto (the String (into obj))) (pure obj))))
  (is (let ((obj
              (make-object
               (make-list (Tuple "one" (json:Number 1d0))
                          (Tuple "array" (json:Array (make-list (json:Number 1)
                                                                json:True
                                                                json:False
                                                                json:Null)))
                          (Tuple "object" (make-object (make-list
                                                        (Tuple "a" (json:String "A")))))
                          (Tuple "three" (json:Number 3d0))))))
        (== (tryInto (the String (into obj))) (pure obj)))))
