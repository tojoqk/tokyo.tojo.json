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
   (#:result #:coalton-library/result)
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
    (define (into x) (render x))))
