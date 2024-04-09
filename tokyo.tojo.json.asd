(defsystem #:tokyo.tojo.json
  :description "JSON parser"
  :author "Masaya Tojo"
  :license  "MIT"
  :version "0.0.0"
  :depends-on (#:coalton)
  :serial t
  :pathname "src/"
  :components ((:file "private/output-stream")
               (:file "private/parser")
               (:file "json")
               (:file "json/renderer")
               (:file "json/parser"))
  :in-order-to ((test-op (test-op "tokyo.tojo.json/test"))))

(defsystem #:tokyo.tojo.json/test
  :description "Tests of JSON parser"
  :author "Masaya Tojo"
  :license  "MIT"
  :version "0.0.0"
  :depends-on (#:tokyo.tojo.json
               #:coalton/testing)
  :perform (test-op (o s)
                    (symbol-call '#:tokyo.tojo.json/test '#:run-tests))
  :serial t
  :pathname "test/"
  :components ((:file "package")
               (:file "parser")
               (:file "renderer")))
