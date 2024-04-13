(in-package #:tokyo.tojo.json/json)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel
  (define-type Crumb
    CrumbTop
    (CrumbArray Crumb (List JSON) (List JSON))
    (CrumbObject Crumb
                 coalton:String
                 (List (Tuple coalton:String JSON))
                 (List (Tuple coalton:String JSON))))

  (define-type Zipper
    (Zipper JSON Crumb))

  (define-instance (Into JSON Zipper)
    (define (into x)
      (Zipper x CrumbTop)))

  (define-instance (Into Zipper JSON)
    (define (into z)
      (match z
        ((Zipper x (CrumbTop)) x)
        ((Zipper x (CrumbArray c l r))
         (into (Zipper (Array (append (reverse l) (Cons x r)))
                       c)))
        ((Zipper x (CrumbObject c k l r))
         (into (Zipper (Object (map:collect! (iter:into-iter (append (reverse l) (Cons (Tuple k x) r)))))
                       c))))))
  )
