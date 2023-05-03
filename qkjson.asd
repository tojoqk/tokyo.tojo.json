(asdf:defsystem #:qkjson
  :description "JSON parser"
  :author "Masaya Tojo"
  :license  "BSD"
  :version "0.0.0"
  :depends-on (#:coalton)
  :serial t
  :components ((:file "parser")
               (:file "qkjson")))

(asdf:defsystem #:qkjson-tests
  :description "Tests of JSON parser"
  :author "Masaya Tojo"
  :license  "BSD"
  :version "0.0.0"
  :depends-on (#:qkjson
               #:coalton
               #:fiveam)
  :serial t
  :components ((:file "qkjson-tests")))
