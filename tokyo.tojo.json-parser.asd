(asdf:defsystem #:tokyo.tojo.json-parser
  :description "JSON parser"
  :author "Masaya Tojo"
  :license  "MIT"
  :version "0.0.0"
  :depends-on (#:coalton)
  :serial t
  :pathname "src/"
  :components ((:file "parser")
               (:file "json-parser"))
  :in-order-to ((test-op (test-op "tokyo.tojo.json-parser/test"))))

(asdf:defsystem #:tokyo.tojo.json-parser/test
  :description "Tests of JSON parser"
  :author "Masaya Tojo"
  :license  "MIT"
  :version "0.0.0"
  :depends-on (#:tokyo.tojo.json-parser
               #:coalton/testing)
  :perform (test-op (o s)
                    (symbol-call '#:tokyo.tojo.json-parser/test '#:run-tests))
  :serial t
  :pathname "test/"
  :components ((:file "json-parser")))
