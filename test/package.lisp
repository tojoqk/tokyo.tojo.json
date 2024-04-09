(defpackage #:tokyo.tojo.json/test
  (:use #:coalton-testing)
  (:local-nicknames
   (#:map #:coalton-library/ord-map)
   (#:json #:tokyo.tojo.json/json))
  (:export #:run-tests))

(in-package #:tokyo.tojo.json/test)

(fiasco:define-test-package #:tokyo.tojo.json/fiasco-test-package)

(coalton-fiasco-init #:tokyo.tojo.json/fiasco-test-package)

(cl:defun run-tests ()
  (fiasco:run-package-tests
   :packages '(#:tokyo.tojo.json/fiasco-test-package)
   :interactive cl:t))
