(fiasco:define-test-package #:tokyo.tojo.json-parser-test-fiasco)

(defpackage #:tokyo.tojo.json-parser-test
  (:use #:coalton-testing
        #:tokyo.tojo.json-parser/json-parser)
  (:local-nicknames
   (#:map #:coalton-library/ord-map)
   (#:result #:coalton-library/result)))

(in-package #:tokyo.tojo.json-parser-test)

(coalton-fiasco-init #:tokyo.tojo.json-parser-test-fiasco)


(define-test parse-json-symbol-test ()
  (is (== (Ok JSON-Null) (parse-json "null")))
  (is (== (Ok (JSON-Boolean True)) (parse-json "true")))
  (is (== (Ok (JSON-Boolean False)) (parse-json "false")))
  (is (result:err? (parse-json "nil"))))

(define-test parse-json-number-test ()
  (is (== (Ok (JSON-Number (JSON-Integer 42)))
          (parse-json "42")))
  (is (== (Ok (JSON-Number (JSON-Float 42.0d0)))
          (parse-json "  42.0  ")))
  (is (== (Ok (JSON-Number (JSON-Float 42.0d0)))
          (parse-json "   42e0")))
  (is (== (Ok (JSON-Number (JSON-Float 420d0)))
          (parse-json "42e001")))
  (is (== (Ok (JSON-Number (JSON-Float 4200d0)))
          (parse-json "42e2")))
  (is (== (Ok (JSON-Number (JSON-Integer 123456789)))
          (parse-json "123456789")))
  (is (== (Ok (JSON-Number (JSON-Float 4200d0)))
          (parse-json "42e+2")))
  (is (== (Ok (JSON-Number (JSON-Float 0.42d0)))
          (parse-json "42e-2")))
  (is (result:err? (parse-json "0123")))
  (is (result:err? (parse-json "133.")))
  (is (result:err? (parse-json "133a"))))

(define-test parse-json-string-test ()
  (is (== (Ok (JSON-String "hello"))
          (parse-json "\"hello\""))))

(define-test parse-json-array-test ()
  (is (== (Ok (JSON-Array
               (make-list (JSON-String "hello")
                          (JSON-Number (JSON-Integer 3))
                          (JSON-Boolean True)
                          (JSON-Boolean False))))
          (parse-json "[\"hello\", 3, true, false  ]")))
  (is (result:err? (parse-json "[133, ]")))
  (is (result:err? (parse-json "[133, , 3]")))
  (is (== (Ok (JSON-Array
               (make-list (JSON-Number (JSON-Integer 133))
                          (JSON-Array
                           (make-list
                            (JSON-Number (JSON-Integer 1))
                            (JSON-Number (JSON-Integer 2))
                            (JSON-Number (JSON-Integer 3))))
                          (JSON-Number (JSON-Integer 3)))))
          (parse-json "[133, [1, 2, 3], 3]"))))

(coalton-toplevel
  (define (make-object pairs)
    (JSON-Object
     (foldr (fn ((Tuple k v) m) (map:insert-or-replace m k v))
            map:empty
            pairs))))

(define-test parse-json-object-test ()
  (is (== (Ok (make-object
               (make-list (Tuple "hello" (JSON-String "world")))))
          (parse-json "{\"hello\" : \"world\" }")))
  (is (== (Ok (make-object
               (make-list (Tuple "test" (JSON-Number (JSON-Integer 42)))
                          (Tuple "object"
                                 (make-object
                                  (make-list (Tuple "true" (JSON-Boolean True))
                                             (Tuple "false" (JSON-Boolean False)))))
                          (Tuple "array"
                                 (JSON-Array
                                  (make-list (JSON-Number (JSON-Integer 10))
                                             (JSON-Number (JSON-Float 3.2d0))
                                             (JSON-String "test"))))
                          (Tuple "null" JSON-Null))))
          (parse-json "
{\"test\" : 42, \"object\": { \"true\": true, \"false\" : false },
 \"array\": [10, 3.2, \"test\"], \"null\" : null }")))
  (is (== (Ok (JSON-Object map:empty))
          (parse-json "{}")))
  (is (result:err? (parse-json "{,}")))
  (is (result:err? (parse-json "{\"test\" : 42,}"))))
