(fiasco:define-test-package #:tokyo.tojo.json-parser-test-fiasco)

(defpackage #:tokyo.tojo.json-parser-test
  (:use #:coalton-testing
        #:tokyo.tojo.json-parser/json-parser)
  (:local-nicknames
   (#:map #:coalton-library/ord-map)
   (#:result #:coalton-library/result)))

(in-package #:tokyo.tojo.json-parser-test)

(named-readtables:in-readtable coalton:coalton)

(coalton-fiasco-init #:tokyo.tojo.json-parser-test-fiasco)

(define-test parse-json-symbol-test ()
  (is (pipe "null"
            parse-json
            (== (Ok JSON-Null))))
  (is (pipe "true"
            parse-json
            (== (Ok (JSON-Boolean True)))))
  (is (pipe "false"
            parse-json
            (== (Ok (JSON-Boolean False)))))
  (is (pipe (parse-json "nil")
            result:err?)))

(define-test parse-json-number-test ()
  (is (pipe "42"
            parse-json
            (== (Ok (JSON-Number (JSON-Integer 42))))))
  (is (pipe "  42.0  "
            parse-json
            (== (Ok (JSON-Number (JSON-Float 42.0d0))))))
  (is (pipe "   42e0"
            parse-json
            (== (Ok (JSON-Number (JSON-Float 42.0d0))))))
  (is (pipe "42e001"
            parse-json
            (== (Ok (JSON-Number (JSON-Float 420d0))))))
  (is (pipe "42e2"
            parse-json
            (== (Ok (JSON-Number (JSON-Float 4200d0))))))
  (is (pipe "123456789"
            parse-json
            (== (Ok (JSON-Number (JSON-Integer 123456789))))))
  (is (pipe "42e+2"
            parse-json
            (== (Ok (JSON-Number (JSON-Float 4200d0))))))
  (is (pipe "42e-2"
            parse-json
            (== (Ok (JSON-Number (JSON-Float 0.42d0))))))
  (is (pipe "0123"
            parse-json
            result:err?))
  (is (pipe "133."
            parse-json
            result:err?))
  (is (pipe (parse-json "133a")
            result:err?)))

(define-test parse-json-string-test ()
  (is (pipe "\"hello\""
            parse-json
            (== (Ok (JSON-String "hello"))))))

(define-test parse-json-array-test ()
  (is (pipe "[\"hello\", 3, true, false  ]"
            parse-json
            (== (Ok (JSON-Array
                     (make-list (JSON-String "hello")
                                (JSON-Number (JSON-Integer 3))
                                (JSON-Boolean True)
                                (JSON-Boolean False)))))))
  (is (pipe "[133, ]"
            parse-json
            result:err?))
  (is (pipe "[133, , 3]"
            parse-json
            result:err?))
  (is (pipe (parse-json "[133, [1, 2, 3], 3]")
            (== (Ok (JSON-Array
                     (make-list (JSON-Number (JSON-Integer 133))
                                (JSON-Array
                                 (make-list
                                  (JSON-Number (JSON-Integer 1))
                                  (JSON-Number (JSON-Integer 2))
                                  (JSON-Number (JSON-Integer 3))))
                                (JSON-Number (JSON-Integer 3)))))))))

(coalton-toplevel
  (define (make-object pairs)
    (JSON-Object
     (foldr (fn ((Tuple k v) m) (map:insert-or-replace m k v))
            map:empty
            pairs))))

(define-test parse-json-object-test ()
  (is (pipe "{\"hello\" : \"world\" }"
            parse-json
            (== (Ok (make-object
                     (make-list (Tuple "hello" (JSON-String "world"))))))))
  (is (pipe "
{\"test\" : 42, \"object\": { \"true\": true, \"false\" : false },
 \"array\": [10, 3.2, \"test\"], \"null\" : null }"
            parse-json
            (== (Ok (make-object
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
                                (Tuple "null" JSON-Null)))))))
  (is (pipe "{}"
            parse-json
            (== (Ok (JSON-Object map:empty)))))
  (is (pipe "{,}"
            parse-json
            result:err?))
  (is (pipe "{\"test\" : 42,}"
            parse-json
            result:err?)))
