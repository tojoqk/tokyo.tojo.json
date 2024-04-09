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
