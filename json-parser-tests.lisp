(defpackage #:tokyo.tojo.json-parser/json-parser-tests
  (:use #:coalton
        #:coalton-library/builtin
        #:coalton-library/classes
        #:tokyo.tojo.json-parser/json-parser)
  (:local-nicknames
   (#:map #:coalton-library/ord-map)
   (#:result #:coalton-library/result)))

(in-package #:tokyo.tojo.json-parser/json-parser-tests)

(5am:def-suite json-parser-suite)
(5am:in-suite json-parser-suite)

(5am:test parse-json-symbol-test
  (5am:is-true (coalton
                (== (Ok JSON-Null)
                    (parse-json "null"))))

  (5am:is-true (coalton
                (== (Ok (JSON-Boolean True))
                    (parse-json "true"))))

  (5am:is-true (coalton
                (== (Ok (JSON-Boolean False))
                    (parse-json "false"))))

  (5am:is-true (coalton
                (== (Ok (JSON-Boolean False))
                    (parse-json "false"))))


  (5am:is-true (coalton
                (parse-json "nil"))))

(5am:test parse-json-number-test
  (5am:is-true (coalton
                (== (Ok (JSON-Number (JSON-Integer 42)))
                    (parse-json "42"))))

  (5am:is-true (coalton
                (== (Ok (JSON-Number (JSON-Float 42.0d0)))
                    (parse-json "  42.0  "))))

  (5am:is-true (coalton
                (== (Ok (JSON-Number (JSON-Float 42.0d0)))
                    (parse-json "   42e0"))))

  (5am:is-true (coalton
                (== (Ok (JSON-Number (JSON-Float 420d0)))
                    (parse-json "42e001"))))

  (5am:is-true (coalton
                (== (Ok (JSON-Number (JSON-Float 4200d0)))
                    (parse-json "42e2"))))

  (5am:is-true (coalton
                (== (Ok (JSON-Number (JSON-Integer 123456789)))
                    (parse-json "123456789"))))

  (5am:is-true (coalton
                (== (Ok (JSON-Number (JSON-Float 4200d0)))
                    (parse-json "42e+2"))))

  (5am:is-true (coalton
                (== (Ok (JSON-Number (JSON-Float 0.42d0)))
                    (parse-json "42e-2"))))

  (5am:is-true (coalton
                (result:err?
                 (parse-json "0123"))))

  (5am:is-true (coalton
                (result:err?
                 (parse-json "133."))))

  (5am:is-true (coalton
                (result:err?
                 (parse-json "133a")))))

(5am:test parse-json-string-test
  (5am:is-true (coalton
                (== (Ok (JSON-String "hello"))
                    (parse-json "\"hello\"")))))

(5am:test parse-json-array-test
  (5am:is-true (coalton
                (== (Ok (JSON-Array
                         (make-list (JSON-String "hello")
                                    (JSON-Number (JSON-Integer 3))
                                    (JSON-Boolean True)
                                    (JSON-Boolean False))))
                    (parse-json "[\"hello\", 3, true, false  ]"))))

  (5am:is-true (coalton
                (result:err? (parse-json "[133, ]"))))

  (5am:is-true (coalton
                (result:err? (parse-json "[133, , 3]"))))

  (5am:is-true (coalton
                (== (Ok (JSON-Array
                         (make-list (JSON-Number (JSON-Integer 133))
                                    (JSON-Array
                                     (make-list
                                      (JSON-Number (JSON-Integer 1))
                                      (JSON-Number (JSON-Integer 2))
                                      (JSON-Number (JSON-Integer 3))))
                                    (JSON-Number (JSON-Integer 3)))))
                    (parse-json "[133, [1, 2, 3], 3]")))))


(coalton-toplevel
  (define (make-object pairs)
    (JSON-Object
     (foldr (fn ((Tuple k v) m) (map:insert-or-replace m k v))
            map:empty
            pairs))))

(5am:test parse-json-object-test
  (5am:is-true (coalton
                (== (Ok (make-object
                         (make-list (Tuple "hello" (JSON-String "world")))))
                    (parse-json "{\"hello\" : \"world\" }"))))

  (5am:is-true (coalton
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
                                    (Tuple "null" JSON-Null))))
                    (parse-json "
{\"test\" : 42, \"object\": { \"true\": true, \"false\" : false },
 \"array\": [10, 3.2, \"test\"], \"null\" : null }"))))

  (5am:is-true (coalton
                (== (Ok (JSON-Object map:empty))
                    (parse-json "{}"))))

  (5am:is-true (coalton
                (result:err? (parse-json "{,}"))))

  (5am:is-true (coalton
                (result:err? (parse-json "{\"test\" : 42,}")))))
