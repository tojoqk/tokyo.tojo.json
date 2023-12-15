(defpackage #:tokyo.tojo.json-parser/test
  (:use #:coalton-testing)
  (:local-nicknames
   (#:map #:coalton-library/ord-map)
   (#:json-parser #:tokyo.tojo.json-parser/json-parser))
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
  (matches (Ok json-parser:JSON-Null)
      (json-parser:parse-json "null"))
  (matches (Ok (json-parser:JSON-Boolean True))
      (json-parser:parse-json "true"))
  (matches (Ok (json-parser:JSON-Boolean False))
      (json-parser:parse-json "false"))
  (matches (Err _)
      (json-parser:parse-json "nil")))

(define-test parse-json-number-test ()
  (matches (Ok (json-parser:JSON-Number (json-parser:JSON-Integer 42)))
      (json-parser:parse-json "42"))
  (matches (Ok (json-parser:JSON-Number (json-parser:JSON-Float 42.0d0)))
      (json-parser:parse-json "  42.0  "))
  (matches (Ok (json-parser:JSON-Number (json-parser:JSON-Float 42.0d0)))
      (json-parser:parse-json "   42e0"))
  (matches (Ok (json-parser:JSON-Number (json-parser:JSON-Float 420d0)))
      (json-parser:parse-json "42e001"))
  (matches (Ok (json-parser:JSON-Number (json-parser:JSON-Float 4200d0)))
      (json-parser:parse-json "42e2"))
  (matches (Ok (json-parser:JSON-Number (json-parser:JSON-Integer 123456789)))
      (json-parser:parse-json "123456789"))
  (matches (Ok (json-parser:JSON-Number (json-parser:JSON-Float 4200d0)))
      (json-parser:parse-json "42e+2"))
  (matches (Ok (json-parser:JSON-Number (json-parser:JSON-Float 0.42d0)))
      (json-parser:parse-json "42e-2"))
  (matches (Err _)
      (json-parser:parse-json "0123"))
  (matches (Err _)
      (json-parser:parse-json "133."))
  (matches (Err _)
      (json-parser:parse-json "133a")))

(define-test parse-json-string-test ()
  (matches (Ok (json-parser:JSON-String "hello"))
      (json-parser:parse-json "\"hello\"")))

(define-test parse-json-array-test ()
  (is (pipe "[\"hello\", 3, true, false  ]"
            json-parser:parse-json
            (== (Ok (json-parser:JSON-Array
                     (make-list (json-parser:JSON-String "hello")
                                (json-parser:JSON-Number (json-parser:JSON-Integer 3))
                                (json-parser:JSON-Boolean True)
                                (json-parser:JSON-Boolean False)))))))
  (matches (Err _)
      (json-parser:parse-json "[133, ]"))
  (matches (Err _)
      (json-parser:parse-json "[133, , 3]"))
  (is (pipe (json-parser:parse-json "[133, [1, 2, 3], 3]")
            (== (Ok (json-parser:JSON-Array
                     (make-list (json-parser:JSON-Number (json-parser:JSON-Integer 133))
                                (json-parser:JSON-Array
                                 (make-list
                                  (json-parser:JSON-Number (json-parser:JSON-Integer 1))
                                  (json-parser:JSON-Number (json-parser:JSON-Integer 2))
                                  (json-parser:JSON-Number (json-parser:JSON-Integer 3))))
                                (json-parser:JSON-Number (json-parser:JSON-Integer 3)))))))))

(coalton-toplevel
  (define (make-object pairs)
    (json-parser:JSON-Object
     (foldr (fn ((Tuple k v) m) (map:insert-or-replace m k v))
            map:empty
            pairs))))

(define-test parse-json-object-test ()
  (is (pipe "{\"hello\" : \"world\" }"
            json-parser:parse-json
            (== (Ok (make-object
                     (make-list (Tuple "hello" (json-parser:JSON-String "world"))))))))
  (is (pipe "
{\"test\" : 42, \"object\": { \"true\": true, \"false\" : false },
 \"array\": [10, 3.2, \"test\"], \"null\" : null }"
            json-parser:parse-json
            (== (Ok (make-object
                     (make-list (Tuple "test" (json-parser:JSON-Number (json-parser:JSON-Integer 42)))
                                (Tuple "object"
                                       (make-object
                                        (make-list (Tuple "true" (json-parser:JSON-Boolean True))
                                                   (Tuple "false" (json-parser:JSON-Boolean False)))))
                                (Tuple "array"
                                       (json-parser:JSON-Array
                                        (make-list (json-parser:JSON-Number (json-parser:JSON-Integer 10))
                                                   (json-parser:JSON-Number (json-parser:JSON-Float 3.2d0))
                                                   (json-parser:JSON-String "test"))))
                                (Tuple "null" json-parser:JSON-Null)))))))
  (matches (Ok (json-parser:JSON-Object map:empty))
      (json-parser:parse-json "{}"))
  (matches (Err _)
      (json-parser:parse-json "{,}"))
  (matches (Err _)
      (json-parser:parse-json "{\"test\" : 42,}")))
