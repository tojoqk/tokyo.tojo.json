(asdf:defsystem #:tokyo.tojo.json-parser
  :description "JSON parser"
  :author "Masaya Tojo"
  :license  "BSD"
  :version "0.0.0"
  :depends-on (#:coalton)
  :serial t
  :components ((:file "parser")
               (:file "json-parser")))

(asdf:defsystem #:tokyo.tojo.json-parser-tests
  :description "Tests of JSON parser"
  :author "Masaya Tojo"
  :license  "BSD"
  :version "0.0.0"
  :depends-on (#:tokyo.tojo.json-parser
               #:coalton
               #:fiveam)
  :serial t
  :components ((:file "json-parser-tests")))
