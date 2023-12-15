(defpackage #:tokyo.tojo.json-parser/test
  (:use #:coalton-testing
        #:tokyo.tojo.json-parser/json-parser)
  (:local-nicknames
   (#:map #:coalton-library/ord-map))
  (:export #:run-tests))

(in-package #:tokyo.tojo.json-parser/test)

(named-readtables:in-readtable coalton:coalton)

(fiasco:define-test-package #:tokyo.tojo.json-parser/fiasco-test-package)

(coalton-fiasco-init #:tokyo.tojo.json-parser/fiasco-test-package)

(cl:defun run-tests ()
  (fiasco:run-package-tests
   :packages '(#:tokyo.tojo.json-parser/fiasco-test-package)
   :interactive cl:t))

(define-test parse-json-symbol-test ()
  (matches (Ok JSON-Null)
      (parse-json "null"))
  (matches (Ok (JSON-Boolean True))
      (parse-json "true"))
  (matches (Ok (JSON-Boolean False))
      (parse-json "false"))
  (matches (Err _)
      (parse-json "nil")))

(define-test parse-json-number-test ()
  (matches (Ok (JSON-Number (JSON-Integer 42)))
      (parse-json "42"))
  (matches (Ok (JSON-Number (JSON-Float 42.0d0)))
      (parse-json "  42.0  "))
  (matches (Ok (JSON-Number (JSON-Float 42.0d0)))
      (parse-json "   42e0"))
  (matches (Ok (JSON-Number (JSON-Float 420d0)))
      (parse-json "42e001"))
  (matches (Ok (JSON-Number (JSON-Float 4200d0)))
      (parse-json "42e2"))
  (matches (Ok (JSON-Number (JSON-Integer 123456789)))
      (parse-json "123456789"))
  (matches (Ok (JSON-Number (JSON-Float 4200d0)))
      (parse-json "42e+2"))
  (matches (Ok (JSON-Number (JSON-Float 0.42d0)))
      (parse-json "42e-2"))
  (matches (Err _)
      (parse-json "0123"))
  (matches (Err _)
      (parse-json "133."))
  (matches (Err _)
      (parse-json "133a")))

(define-test parse-json-string-test ()
  (matches (Ok (JSON-String "hello"))
      (parse-json "\"hello\"")))

(define-test parse-json-array-test ()
  (is (pipe "[\"hello\", 3, true, false  ]"
            parse-json
            (== (Ok (JSON-Array
                     (make-list (JSON-String "hello")
                                (JSON-Number (JSON-Integer 3))
                                (JSON-Boolean True)
                                (JSON-Boolean False)))))))
  (matches (Err _)
      (parse-json "[133, ]"))
  (matches (Err _)
      (parse-json "[133, , 3]"))
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
  (matches (Ok (JSON-Object map:empty))
      (parse-json "{}"))
  (matches (Err _)
      (parse-json "{,}"))
  (matches (Err _)
      (parse-json "{\"test\" : 42,}")))
