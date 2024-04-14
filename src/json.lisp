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

(cl:defmacro alt* (cl:&rest xs)
  (cl:check-type xs cl:list)
  (cl:if (cl:null xs)
         'empty
         `(alt ,(cl:first xs)
               (alt* ,@(cl:rest xs)))))

(coalton-toplevel

  ;;
  ;; JSON Type
  ;;
  
  (define-type JSON
    Null
    True False
    (Number Double-Float)
    (String coalton:String)
    (Array (List JSON))
    (Object (map:Map coalton:String JSON)))

  (define-instance (Into Unit JSON)
    (define (into _) Null))

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

  ;;
  ;; Zipper
  ;;

  (define-type Zipper
    (Zipper JSON Crumb))

  (define-type Crumb
    CrumbTop
    (CrumbArray Crumb (List JSON) (List JSON))
    (CrumbObject Crumb
                 coalton:String
                 (List (Tuple coalton:String JSON))
                 (List (Tuple coalton:String JSON))))

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
         (into (Zipper (make-object (append (reverse l) (Cons (Tuple k x) r)))
                       c))))))

  ;; 
  ;; JSON Parser
  ;;

  (declare digit? (Char -> Boolean))
  (define (digit? c)
    (char:ascii-digit? c))

  (declare digit1-9? (Char -> Boolean))
  (define (digit1-9? c)
    (and (char:ascii-digit? c)
         (not (== c #\0))))

  (define (message-with msg c)
    (<> msg (<> ": " (into c))))

  (define (fail-unexpected-char c)
    (fail (message-with "Unexpected char" (singleton c))))

  (define (fail-unexpected-string str)
    (fail (message-with "Unexpected string" str)))

  (define (whitespace? c)
    (or (== c #\return)
        (== c #\tab)
        (== c #\newline)
        (== c #\space)))

  (declare skip-whitespaces (parser:Parser Unit))
  (define skip-whitespaces
    (>> (parser:collect-while
         (fn (c)
           (if (whitespace? c)
               (Some parser:read-char)
               None)))
        (pure Unit)))

  (declare parse-hex (coalton:String -> (Optional UFix)))
  (define (parse-hex str)
    (>>= (lisp (Optional Integer) (str)
           (cl:handler-case
               (cl:let ((value (cl:parse-integer str :radix 16)))
                 (cl:check-type value cl:integer)
                 (coalton (Some (lisp Integer () value))))
             (cl:error (e)
               (cl:declare (cl:ignore e))
               (coalton None))))
         (.> tryInto as-optional)))

  (define (non-empty-string parser)
    (>>= parser
         (fn (str)
           (if (< 0 (str:length str))
               (pure str)
               (fail "Unexpected empty symbol")))))

  (declare take-until-parser ((Char -> Boolean) -> (parser:Parser coalton:String)))
  (define (take-until-parser end?)
    (parser:take-until-string end?))

  (define (sep? c)
    (or (whitespace? c)
        (== c #\{)
        (== c #\})
        (== c #\[)
        (== c #\])
        (== c #\,)
        (== c #\")))

  (declare word-parser (parser:Parser coalton:String))
  (define word-parser (parser:delay (take-until-parser sep?)))

  (declare null-parser (parser:Parser Unit))
  (define null-parser
    (do (word <- (non-empty-string word-parser))
        (if (== word "null")
            (pure Unit)
            (fail-unexpected-string word))))

  (declare true-parser (parser:Parser Boolean))
  (define true-parser
    (do (word <- (non-empty-string word-parser))
        (if (== word "true")
            (pure coalton:True)
            (fail-unexpected-string word))))

  (declare false-parser (parser:Parser Boolean))
  (define false-parser
    (do (word <- (non-empty-string word-parser))
        (if (== word "false")
            (pure coalton:False)
            (fail-unexpected-string word))))

  (declare escape-char-map (map:Map Char Char))
  (define escape-char-map
    (foldr (fn ((Tuple k v) acc)
             (map:insert-or-replace acc k v))
           map:empty
           (make-list
            (Tuple #\" #\")
            (Tuple #\\ #\\)
            (Tuple #\/ #\/)
            (Tuple #\b #\backspace)
            (Tuple #\f #\page)
            (Tuple #\n #\newline)
            (Tuple #\r #\return)
            (Tuple #\t #\tab))))

  (define (escape-char? c)
    (optional:some? (map:lookup escape-char-map c)))

  (declare take-parser (UFix -> (parser:Parser coalton:String)))
  (define (take-parser n)
    (map into (sequence (list:repeat n parser:peek-char))))

  (declare string-parser (parser:Parser coalton:String))
  (define string-parser
    (let ((declare escaped-char-parser (parser:Parser coalton:String))
          (escaped-char-parser
            (parser:from-guard
             (alt* (parser:guard-lookup escape-char-map
                                        (fn (c)
                                          (>> parser:read-char
                                              (pure (into (make-list c))))))
                   (parser:guard-char (== #\u)
                                      (do parser:read-char
                                          (str <- (take-parser 4))
                                          (match (>>= (parse-hex str) char:code-char)
                                            ((None)
                                             (fail-unexpected-string str))
                                            ((Some c)
                                             (pure (into (make-list c))))))))))
          (declare substring-parser (parser:Parser coalton:String))
          (substring-parser
            (parser:from-guard
             (alt* (parser:guard-char (== #\\)
                                      (>> parser:read-char
                                          escaped-char-parser))
                   (parser:guard-char (const coalton:True)
                                      (take-until-parser (disjoin (== #\\) (== #\"))))))))
      (>> parser:read-char
          (>>= (parser:collect-while
                (fn (c)
                  (if (== c #\")
                      None
                      (Some substring-parser))))
               (fn (lst)
                 (>> parser:read-char
                     (pure (mconcat lst))))))))

  (declare digits-parser (parser:Parser coalton:String))
  (define digits-parser
    (let length>0-check =
      (fn (str)
        (if (< 0 (str:length str))
            (pure str)
            (fail "Unexpected empty symbol"))))
    (>>= (take-until-parser (complement digit?))
         (fn (str)
           (parser:from-guard
            (alt*
             (parser:guard-eof (parser:delay (length>0-check str)))
             (parser:guard-char (fn (c)
                                  (or (sep? c)
                                      (== c #\.)
                                      (== c #\e)
                                      (== c #\E)))
                                (length>0-check str)))))))

  (declare parse-float (coalton:String -> coalton:String -> coalton:String -> (Optional Double-Float)))
  (define (parse-float head fraction exponent)
    (let str = (mconcat (make-list head "." fraction "d" exponent)))
    (lisp (Optional Double-Float) (str)
      (cl:handler-case
          (cl:let ((cl:*read-eval* cl:nil))
            (cl:let ((value (cl:read-from-string str)))
              (cl:check-type value cl:double-float)
              (coalton (Some (lisp Double-Float () value)))))
        (cl:error (e)
          (cl:declare (cl:ignore e))
          (coalton None)))))

  (declare number-parser (parser:Parser Double-Float))
  (define number-parser
    (let ((declare integer-parser (coalton:String -> parser:Parser Double-Float))
          (integer-parser
            (fn (head)
              (match (str:parse-int head)
                ((None) (fail-unexpected-string head))
                ((Some int)
                 (match (tryInto int)
                   ((Ok d) (pure d))
                   ((Err _) (fail-unexpected-string head)))))))

          (declare float-parser (coalton:String -> coalton:String -> coalton:String -> parser:Parser Double-Float))
          (float-parser
            (fn (head fraction exponent)
              (match (parse-float head fraction exponent)
                ((None)
                 (fail-unexpected-string (mconcat (make-list head "." fraction "e" exponent))))
                ((Some float)
                 (pure float)))))

          (declare head-parser (parser:Parser coalton:String))
          (head-parser
            (let ((sign-parser
                    (fn (continue-parser)
                      (parser:from-guard
                       (alt* (parser:guard-char (== #\-)
                                                (>> parser:read-char
                                                    (liftA2 <> (pure "-") continue-parser)))
                             (parser:guard-else continue-parser)))))
                  (main-parser
                    (parser:from-guard
                     (alt* (parser:guard-char (== #\0)
                                              (do (digits <- digits-parser)
                                                  (if (== digits "0")
                                                      (pure digits)
                                                      (fail-unexpected-string digits))))
                           (parser:guard-char digit1-9? digits-parser)))))
              (sign-parser main-parser)))

          (declare fraction-parser (coalton:String -> (parser:Parser Double-Float)))
          (fraction-parser
            (fn (head)
              (>> parser:read-char
                  (>>= digits-parser
                       (fn (fraction)
                         (parser:from-guard
                          (alt*
                           (parser:guard-eof (parser:delay (float-parser head fraction "0")))
                           (parser:guard-char (disjoin (== #\e) (== #\E))
                                              (parser:delay (exponent-parser head fraction)))
                           (parser:guard-char sep?
                                              (parser:delay (float-parser head fraction "0"))))))))))

          (declare exponent-parser (coalton:String -> coalton:String -> (parser:Parser Double-Float)))
          (exponent-parser
            (fn (head fraction)
              (>> parser:read-char
                  (parser:from-guard
                   (alt*
                    (parser:guard-char (== #\+)
                                       (>> parser:read-char
                                           (>>= digits-parser (float-parser head fraction))))
                    (parser:guard-char (== #\-)
                                       (>> parser:read-char
                                           (>>= (map (<> "-") digits-parser)
                                                (float-parser head fraction))))
                    (parser:guard-char (const coalton:True)
                                       (>>= digits-parser
                                            (float-parser head fraction)))))))))

      (>>= head-parser
           (fn (head)
             (parser:from-guard
              (alt*
               (parser:guard-eof (parser:delay (integer-parser head)))
               (parser:guard-char (== #\.)
                                  (parser:delay (fraction-parser head)))
               (parser:guard-char (disjoin (== #\e) (== #\E))
                                  (parser:delay (exponent-parser head "0")))
               (parser:guard-char sep?
                                  (parser:delay (integer-parser head)))))))))

  (define-type State
    Start-Parse-Array
    Continue-Parse-Array
    Start-Parse-Object
    Continue-Parse-Object)

  (define (make-object xs)
    (Object (map:collect! (iter:into-iter xs))))

  (declare zipper-parser (parser:Parser Zipper))
  (define zipper-parser
    (let ((shallow-json-parser
            (let ((atom-parser (fn (parser) (map into parser))))
              (do skip-whitespaces
                  (c <- parser:peek-char)
                  (cond
                    ((== c #\n) (atom-parser null-parser))
                    ((== c #\t) (atom-parser true-parser))
                    ((== c #\f) (atom-parser false-parser))
                    ((or (digit? c) (== c #\-)) (atom-parser number-parser))
                    ((== c #\") (atom-parser string-parser))
                    ((== c #\[)
                     (do parser:read-char
                         (pure (into (Array Nil)))))
                    ((== c #\{)
                     (do parser:read-char
                         (pure (into (Object map:empty)))))
                    (coalton:True
                     (fail-unexpected-char c))))))
          (declare deep-parser (Zipper -> State -> parser:Parser (Tuple Zipper (Optional State))))
          (deep-parser
            (fn (z state)
              (let ((declare key-value-parser (parser:Parser (Tuple coalton:String JSON)))
                    (key-value-parser
                      (parser:delay
                       (do (key <- string-parser)
                           skip-whitespaces
                           (parser:from-guard
                            (parser:guard-char (== #\:)
                                               (do parser:read-char
                                                   (value <- shallow-json-parser)
                                                   skip-whitespaces
                                                   (pure (Tuple key value))))))))
                    (empty-next
                      (fn ()
                        (match z
                          ((Zipper _ (CrumbTop))
                           (pure (Tuple z None)))
                          ((Zipper _ (CrumbArray _ _ _))
                           (pure (Tuple z (Some Continue-Parse-Array))))
                          ((Zipper _ (CrumbObject _ _ _ _))
                           (pure (Tuple z (Some Continue-Parse-Object)))))))
                    (end-next
                      (fn (new-zipper)
                        (match new-zipper
                          ((Zipper _ (CrumbTop))
                           (pure (Tuple new-zipper None)))
                          ((Zipper _ (CrumbArray _ _ _))
                           (pure (Tuple new-zipper (Some Continue-Parse-Array))))
                          ((Zipper _ (CrumbObject _ _ _ _))
                           (pure (Tuple new-zipper (Some Continue-Parse-Object)))))))
                    (comma-next
                      (fn (json new-zipper next-state)
                        (let ((continue (pure (Tuple new-zipper (Some next-state)))))
                          (match json
                            ((Null) continue)
                            ((True) continue)
                            ((False) continue)
                            ((Number _) continue)
                            ((String _) continue)
                            ((Array _) (pure (Tuple new-zipper (Some Start-Parse-Array))))
                            ((Object _) (pure (Tuple new-zipper (Some Start-Parse-Object)))))))))
                (match state
                  ((Start-Parse-Array)
                   (do skip-whitespaces
                       (ch <- parser:peek-char)
                       (cond
                         ((== ch #\])
                          (do parser:read-char
                              (empty-next)))
                         (coalton:True
                          (do (let (Zipper _ cr) = z)
                              (j <- shallow-json-parser)
                              (comma-next j (Zipper j (CrumbArray cr Nil Nil)) Continue-Parse-Array))))))
                  ((Continue-Parse-Array)
                   (match z
                     ((Zipper x (CrumbArray cr l r))
                      (do skip-whitespaces
                          (ch <- parser:peek-char)
                          (cond
                            ((== ch #\])
                             (do parser:read-char
                                 (end-next (Zipper (Array (append (reverse l) (Cons x r))) cr))))
                            ((== ch #\,)
                             (do parser:read-char
                                 (j <- shallow-json-parser)
                                 (comma-next j (Zipper j (CrumbArray cr (Cons x l) r)) Continue-Parse-Array)))
                            (coalton:True
                             (fail-unexpected-char ch)))))
                     (_ (coalton-prelude:error "zipper-parser: program error (Continue-Parse-Array)"))))
                  ((Start-Parse-Object)
                   (do skip-whitespaces
                       (ch <- parser:peek-char)
                       (cond
                         ((== ch #\})
                          (do parser:read-char
                              (empty-next)))
                         (coalton:True
                          (do (let (Zipper _ cr) = z)
                              ((Tuple key j) <- key-value-parser)
                              (comma-next j
                                          (Zipper j (CrumbObject cr key Nil Nil))
                                          Continue-Parse-Object))))))
                  ((Continue-Parse-Object)
                   (match z
                     ((Zipper x (CrumbObject cr xk l r))
                      (do skip-whitespaces
                          (ch <- parser:peek-char)
                          (cond
                            ((== ch #\})
                             (do parser:read-char
                                 (end-next (Zipper (make-object (append (reverse l) (Cons (Tuple xk x) r))) cr))))
                            ((== ch #\,)
                             (do parser:read-char
                                 skip-whitespaces
                                 ((Tuple key j) <- key-value-parser)
                                 (comma-next j
                                             (Zipper j (CrumbObject cr key (Cons (Tuple xk x) l) r))
                                             Continue-Parse-Object)))
                            (coalton:True
                             (fail-unexpected-char ch)))))
                     (_ (coalton-prelude:error "zipper-parser: program error (Continue-Parse-Object)")))))))))
      (do (z <- (map into shallow-json-parser))
          (match z
            ((Zipper (Null) _) (pure z))
            ((Zipper (True) _) (pure z))
            ((Zipper (False) _) (pure z))
            ((Zipper (Number _) _) (pure z))
            ((Zipper (String _) _) (pure z))
            ((Zipper (Array _) _)
             (parser:fold-while deep-parser z Start-Parse-Array))
            ((Zipper (Object _) _)
             (parser:fold-while deep-parser z Start-Parse-Object))))))

  (declare json-parser (parser:Parser JSON))
  (define json-parser
    (map into zipper-parser))

  (declare parse! (iter:Iterator Char -> (Result coalton:String JSON)))
  (define (parse! iter)
    (parser:run! json-parser
                 (parser:make-port! iter)))

  (declare parse (coalton:String -> (Result coalton:String JSON)))
  (define (parse str)
    (parse! (iter:into-iter str)))

  (define-instance (TryInto coalton:String JSON)
    (define tryInto parse))

  ;;
  ;; Renderer
  ;;

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
               (render_ x out)))
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
