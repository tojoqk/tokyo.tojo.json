# json-parser

This is a JSON parser implemented in Coalton.

## Installation

Since it depends on Coalton, please refer to the link below to install Coalton.

https://github.com/coalton-lang/coalton

Next, place json-parser in your local repository (`~/common-lisp`, etc.).

```shell:~/common-lisp
git clone https://github.com/tojoqk/json-parser.git
```

If you are using Quicklisp, you can load the system with the following.

```lisp
(ql:quickload :tokyo.tojo.json-parser)
```

## Examples

```lisp
(defpackage #:json-parser-example
  (:use #:coalton
        #:coalton-library/builtin
        #:coalton-library/classes
        #:tokyo.tojo.json-parser)
  (:local-nicknames
   (#:map #:coalton-library/ord-map)))

(in-package #:json-parser-example)

(coalton-toplevel
  (declare get-number (JSON -> (Optional Double-Float)))
  (define (get-number x)
    (match x
      ((JSON-Number (JSON-Integer n)) (as-optional (tryinto n)))
      ((JSON-Number (JSON-Float n)) (Some n))
      (_ None)))

  (declare get-string (JSON -> (Optional String)))
  (define (get-string x)
    (match x
      ((JSON-String str) (Some str))
      (_ None)))

  (declare get-object (JSON -> (Optional (map:Map String JSON))))
  (define (get-object x)
    (match x
      ((JSON-Object m) (Some m))
      (_ None)))

  (declare eval (String -> (Optional Double-Float)))
  (define (eval str)
    (do (json <- (as-optional (parse-json str)))
        (obj <- (get-object json))
      (left <- (>>= (map:lookup obj "left")
                    get-number))
      (op <- (>>= (map:lookup obj "op")
                  get-string))
      (right <- (>>= (map:lookup obj "right")
                     get-number))
      (cond ((== op "+") (pure (+ left right)))
            (True None)))))
```

in REPL:

```lisp
JSON-PARSER-EXAMPLE> (eval "{\"left\": 10, \"op\": \"+\", \"right\": 32.0}")
#.(SOME 42.0d0)
JSON-PARSER-EXAMPLE> (eval "{\"left\": 10, \"op\": \"-\", \"right\": 32.0}")
#.NONE
JSON-PARSER-EXAMPLE> 
```
