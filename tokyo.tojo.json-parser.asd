(asdf:defsystem #:tokyo.tojo.json-parser
  :description "JSON parser"
  :author "Masaya Tojo"
  :license  "BSD"
  :version "0.0.0"
  :depends-on (#:coalton)
  :serial t
  :pathname "src/"
  :components ((:file "parser")
               (:file "json-parser")))

(asdf:defsystem #:tokyo.tojo.json-parser/test
  :description "Tests of JSON parser"
  :author "Masaya Tojo"
  :license  "BSD"
  :version "0.0.0"
  :depends-on (#:tokyo.tojo.json-parser
               #:coalton/testing)
  :perform (asdf:test-op (o s)
                         (unless (symbol-call
                                  :fiasco
                                  :run-package-tests
                                  :package
                                  '#:tokyo.tojo.json-parser-test-fiasco)
                           (error "Tests failed")))
  :serial t
  :pathname "test/"
  :components ((:file "json-parser")))
