(defpackage #:tokyo.tojo.json/json
  (:use #:coalton
        #:coalton-prelude)
  (:shadow #:String
           #:True
           #:False)
  (:local-nicknames
   (#:map #:coalton-library/ord-map)
   (#:iter #:coalton-library/iterator)
   (#:char #:coalton-library/char)
   (#:str #:coalton-library/string)
   (#:optional #:coalton-library/optional)
   (#:list #:coalton-library/list)
   (#:output #:tokyo.tojo.json/private/output-stream)
   (#:parser #:tokyo.tojo.json/private/parser))
  (:export #:JSON
           #:Null
           #:True
           #:False
           #:Number
           #:String
           #:Array
           #:Object

           #:parse
           #:parse!))

(in-package #:tokyo.tojo.json/json)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel
  (define-type JSON
    Null
    True False
    (Number Double-Float)
    (String coalton:String)
    (Array (List JSON))
    (Object (map:Map coalton:String JSON)))

  (define-instance (Into Boolean JSON)
    (define (into b)
      (match b
        ((coalton:True) True)
        ((coalton:False) False))))

  (define-instance (Into coalton:String JSON)
    (define (into x)
      (String x)))

  (define-instance (Into Double-Float JSON)
    (define (into x)
      (Number x)))

  (define-instance (Into (List JSON) JSON)
    (define (into x)
      (Array x)))

  (define-instance (Into (map:Map coalton:String JSON) JSON)
    (define (into x)
      (Object x)))

  (define-instance (Eq JSON)
    (define (== a b)
      (match (Tuple a b)
        ((Tuple (Null) (Null)) coalton:True)
        ((Tuple (True) (True)) coalton:True)
        ((Tuple (False) (False)) coalton:True)
        ((Tuple (Number x) (Number y)) (== x y))
        ((Tuple (String x) (String y)) (== x y))
        ((Tuple (Array x) (Array y)) (== x y))
        ((Tuple (Object x) (Object y)) (== x y))
        (_ coalton:False))))

  (define-instance (Into JSON coalton:String)
    (define (into x) (render x)))

  (declare render (JSON -> Coalton:String))
  (define (render x)
    (let out = (output:make-string-output-stream))
    (render_ x out)
    (output:get-output-stream-string out))

  (declare render_ (JSON -> output:Stream -> Unit))
  (define (render_ x out)
    (match x
      ((Null) (output:write-string "null" out))
      ((True) (output:write-string "true" out))
      ((False) (output:write-string "false" out))
      ((Number n)
       (progn
         (lisp Unit (n out)
           (cl:format out "~f" n)
           Unit)))
      ((String s)
       (render-string s out))
      ((Array l)
       (output:write-char #\[ out)
       (match l
         ((Cons h t)
          (render_ h out)
          (for x in t
               (output:write-char #\, out)
               (render_ h out)))
         ((Nil) Unit))
       (output:write-char #\] out))
      ((Object m)
       (output:write-char #\{ out)
       (let iter = (iter:into-iter m))
       (match (iter:next! iter)
         ((Some (Tuple k v))
          (render-string k out)
          (output:write-char #\: out)
          (render_ v out)
          (for (Tuple k v) in iter
               (output:write-char #\, out)
               (render-string k out)
               (output:write-char #\: out)
               (render_ v out)))
         ((None) Unit))
       (output:write-char #\} out))))

  (declare render-string (coalton:String -> output:Stream -> Unit))
  (define (render-string x out)
    (output:write-char #\" out)
    (for c in x
         (match c
           (#\" (output:write-string "\\\"" out))
           (#\\ (output:write-string "\\\\" out))
           (#\/ (output:write-string "\\/" out))
           (#\Backspace (output:write-string "\\b" out))
           (#\Page (output:write-string "\\f" out))
           (#\Newline (output:write-string "\\n" out))
           (#\Return (output:write-string "\\r" out))
           (#\Tab (output:write-string "\\t" out))
           (_
            (let code = (char:char-code c))
            (if (<= code #x001f)
                (progn
                  (output:write-char #\\ out)
                  (output:write-char #\u out)
                  (lisp Unit (code out)
                    (cl:format out "~4,'0x" code)
                    Unit))
                (output:write-char c out)))))
    (output:write-char #\" out)))
