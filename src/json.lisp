(defpackage #:tokyo.tojo.json
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
   (#:cell #:coalton-library/cell)
   (#:output #:tokyo.tojo.json/private/output-stream)
   (#:port #:tokyo.tojo.json/private/port)
   (#:parser #:tokyo.tojo.json/private/parser))
  (:export #:JSON
           #:Null
           #:True
           #:False
           #:Number
           #:String
           #:Array
           #:Object

           #:parse!))

(in-package #:tokyo.tojo.json)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel

  ;;
  ;; JSON Type
  ;;

  (define-type JSON
    "A representation of a JSON object."
    Null
    True False
    (Number Double-Float)
    (String coalton:String)
    (Array (List JSON))
    (Object (map:Map coalton:String JSON)))

  (define (boolean-to-json b)
    (match b
      ((coalton:True) True)
      ((coalton:False) False)))

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

  (define (to-zipper x)
    (Zipper x CrumbTop))

  (define (from-zipper z)
    (match z
      ((Zipper x (CrumbTop)) x)
      ((Zipper x (CrumbArray c l r))
       (from-zipper (Zipper (Array (append (reverse l) (Cons x r)))
                            c)))
      ((Zipper x (CrumbObject c k l r))
       (from-zipper (Zipper (make-object (append (reverse l) (Cons (Tuple k x) r)))
                            c)))))

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

  (define (unexpected-char c)
    (message-with "Unexpected char" (singleton c)))

  (define (fail-unexpected-char c)
    (fail (unexpected-char c)))

  (define (unexpected-string str)
    (message-with "Unexpected string" str))

  (define (fail-unexpected-string str)
    (fail (unexpected-string str)))

  (define (whitespace? c)
    (or (== c #\return)
        (== c #\tab)
        (== c #\newline)
        (== c #\space)))

  (declare skip-whitespaces (parser:Parser Unit))
  (define skip-whitespaces
    (parser:do-while (do (opt-ch <- parser:peek-char-or-eof)
                         (match opt-ch
                           ((Some ch)
                            (if (whitespace? ch)
                                (do parser:read-char
                                    (pure coalton:True))
                                (pure coalton:False)))
                           ((None) (pure coalton:False))))))

  (declare parse-hex (coalton:String -> (Optional UFix)))
  (define (parse-hex str)
    (let m = (the UFix coalton-library/math:maxbound))
    (lisp (Optional UFix) (str m)
      (alexandria:if-let (n (cl:ignore-errors
                             (cl:parse-integer str :radix 16)))
        (cl:if (cl:<= n m)
               (Some n)
               None)
        None)))

  (define (non-empty-string parser)
    (do (str <- parser)
        (guard "Unexpected empty symbol"
               (< 0 (str:length str)))
        (pure str)))

  (define (write-take-until end?)
    (parser:do-while (do (opt-ch <- parser:peek-char-or-eof)
                         (match opt-ch
                           ((Some ch)
                            (if (end? ch)
                                (pure coalton:False)
                                (do parser:read-char
                                    (parser:write-char ch)
                                    (pure coalton:True))))
                           ((None) (pure coalton:False))))))

  (define (sep? c)
    (or (whitespace? c)
        (== c #\{)
        (== c #\})
        (== c #\[)
        (== c #\])
        (== c #\,)
        (== c #\")))

  (declare word-parser (parser:Parser coalton:String))
  (define word-parser
    (do parser:push-new-buffer
        (write-take-until sep?)
        parser:pop-string))

  (declare null-parser (parser:Parser Unit))
  (define null-parser
    (do (word <- (non-empty-string word-parser))
        (guard (unexpected-string word)
               (== word "null"))
        (pure Unit)))

  (declare true-parser (parser:Parser Boolean))
  (define true-parser
    (do (word <- (non-empty-string word-parser))
        (guard (unexpected-string word) (== word "true"))
        (pure coalton:True)))

  (declare false-parser (parser:Parser Boolean))
  (define false-parser
    (do (word <- (non-empty-string word-parser))
        (guard (unexpected-string word) (== word "false"))
        (pure coalton:False)))
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
    (map into (sequence (list:repeat n parser:read-char))))

  (define (from-optional str opt)
    (match opt
      ((Some x) (pure x))
      ((None) (fail str))))

  (define (guard msg b)
    (if b
        (pure Unit)
        (fail msg)))

  (declare ufix-to-str (UFix -> coalton:String))
  (define (ufix-to-str hex)
    (into (the Integer (into hex))))

  (declare code-point-out-of-range (UFix -> coalton:String))
  (define (code-point-out-of-range x)
    (lisp coalton:String (x)
      (cl:format nil "Code point out of range: \\u~x" x)))

  (define (write-surrogate-pair-parser heigh)
    (do (let msg = (code-point-out-of-range heigh))
        (u <- (take-parser 2))
        (guard msg (== u "\\u"))
        (low-str <- (take-parser 4))
        (low <- (from-optional msg (parse-hex low-str)))
        (guard msg (and (<= #xDC00 low) (<= low #xDFFF)))
        (let code = (surrogate-pair-code heigh low))
        (let msg = (code-point-out-of-range code))
        (guard msg (and (<= #x10000 code) (<= code #x10FFFF)))
        (ch <- (from-optional msg (char:code-char code)))
        (parser:write-char ch)))

  (declare string-parser (parser:Parser coalton:String))
  (define string-parser
    (let ((declare write-escaped-char-parser (parser:Parser Unit))
          (write-escaped-char-parser
            (do (ch <- parser:read-char)
                (cond ((== ch #\u)
                       (do (str <- (take-parser 4))
                           (code <- (from-optional "" (parse-hex str)))
                           (let msg = (code-point-out-of-range code))
                           (guard msg (not (and (<= #xDC00 code) (<= code #xDFFF)))) ; low of surrogate pair
                           (if (and (<= #xD800 code) (<= code #xDBFF))
                               (write-surrogate-pair-parser code)
                               (do (ch <- (from-optional msg (char:code-char code)))
                                   (parser:write-char ch)))))
                      (coalton:True
                       (do (ch <- (from-optional (unexpected-char ch) (map:lookup escape-char-map ch)))
                           (parser:write-char ch))))))
          (declare write-substring (parser:Parser Unit))
          (write-substring
            (do (ch <- parser:peek-char)
                (cond ((== ch #\\)
                       (do parser:read-char
                           write-escaped-char-parser))
                      (coalton:True
                       (write-take-until (disjoin (== #\\) (== #\"))))))))
      (do parser:read-char
          parser:push-new-buffer
          (parser:do-while (do (ch <- parser:peek-char)
                               (if (== ch #\")
                                   (do parser:read-char
                                       (pure coalton:False))
                                   (do write-substring
                                       (pure coalton:True)))))
          parser:pop-string)))

  (declare digits-parser (parser:Parser coalton:String))
  (define digits-parser
    (do parser:push-new-buffer
        (write-take-until (complement digit?))
        (str <- parser:pop-string)
        (opt-ch <- parser:peek-char-or-eof)
        (match opt-ch
          ((None)
           (do (guard "Unexpected empty symbol" (< 0 (str:length str)))
               (pure str)))
          ((Some ch)
           (do (guard (unexpected-char ch)
                      (or (sep? ch)
                          (== ch #\.)
                          (== ch #\e)
                          (== ch #\E)))
               (guard "Unexpected empty symbol" (< 0 (str:length str)))
               (pure str))))))

  (declare parse-float (coalton:String -> (Optional Double-Float)))
  (define (parse-float str)
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
    (let ((declare float-parser (coalton:String -> coalton:String -> coalton:String -> parser:Parser Double-Float))
          (float-parser
            (fn (head fraction exponent)
              (do parser:push-new-buffer
                  (parser:write-string head)
                  (parser:write-string ".")
                  (parser:write-string fraction)
                  (parser:write-string "d")
                  (parser:write-string exponent)
                  (float-string <- parser:pop-string)
                  (from-optional (unexpected-string (mconcat (make-list head "." fraction "e" exponent)))
                                 (parse-float float-string)))))

          (declare head-parser (parser:Parser coalton:String))
          (head-parser
            (let ((sign-parser
                    (fn (continue-parser)
                      (do (ch <- parser:peek-char)
                          (match ch
                            (#\-
                             (do parser:read-char
                                 (head <- continue-parser)
                                 (pure (<> "-" head))))
                            (_ continue-parser)))))
                  (main-parser
                    (do (ch <- parser:peek-char)
                        (cond
                          ((== ch #\0)
                           (do (digits <- digits-parser)
                               (guard (unexpected-string digits)
                                      (== digits "0"))
                               (pure digits)))
                          ((digit1-9? ch) digits-parser)
                          (coalton:True
                           (fail-unexpected-char ch))))))
              (sign-parser main-parser)))

          (declare fraction-parser (coalton:String -> (parser:Parser Double-Float)))
          (fraction-parser
            (fn (head)
              (do parser:read-char
                  (fraction <- digits-parser)
                  (opt-ch <- parser:peek-char-or-eof)
                  (match opt-ch
                    ((None) (float-parser head fraction "0"))
                    ((Some ch)
                     (cond ((or (== ch #\e) (== ch #\E)) (exponent-parser head fraction))
                           ((sep? ch) (float-parser head fraction "0"))
                           (coalton:True
                            (fail-unexpected-char ch))))))))

          (declare exponent-parser (coalton:String -> coalton:String -> (parser:Parser Double-Float)))
          (exponent-parser
            (fn (head fraction)
              (do parser:read-char
                  (ch <- parser:peek-char)
                  (cond ((== ch #\+)
                         (do parser:read-char
                             (exponent <- digits-parser)
                             (float-parser head fraction exponent)))
                        ((== ch #\-)
                         (do parser:read-char
                             (exponent <- (map (<> "-") digits-parser))
                             (float-parser head fraction exponent)))
                        (coalton:True
                         (do (exponent <- digits-parser)
                             (float-parser head fraction exponent))))))))
      (do (head <- head-parser)
          (opt-ch <- parser:peek-char-or-eof)
          (match opt-ch
            ((None) (float-parser head "0" "0"))
            ((Some ch)
             (cond ((== ch #\.) (fraction-parser head))
                   ((or (== ch #\e) (== ch #\E)) (exponent-parser head "0"))
                   ((sep? ch) (float-parser head "0" "0"))
                   (coalton:True
                    (fail-unexpected-char ch))))))))

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
            (do skip-whitespaces
                (c <- parser:peek-char)
                (cond
                  ((== c #\n) (>> null-parser (pure Null)))
                  ((== c #\t) (map boolean-to-json true-parser))
                  ((== c #\f) (map boolean-to-json false-parser))
                  ((or (digit? c) (== c #\-)) (map Number number-parser))
                  ((== c #\") (map String string-parser))
                  ((== c #\[)
                   (do parser:read-char
                       (pure (into (Array Nil)))))
                  ((== c #\{)
                   (do parser:read-char
                       (pure (into (Object map:empty)))))
                  (coalton:True
                   (fail-unexpected-char c)))))
          (declare deep-parser (Zipper -> State -> parser:Parser (Tuple Zipper (Optional State))))
          (deep-parser
            (fn (z state)
              (let ((declare key-value-parser (parser:Parser (Tuple coalton:String JSON)))
                    (key-value-parser
                      (do (key <- string-parser)
                          skip-whitespaces
                          (ch <- parser:peek-char)
                          (match ch
                            (#\:
                             (do parser:read-char
                                 (value <- shallow-json-parser)
                                 skip-whitespaces
                                 (pure (Tuple key value))))
                            (_ (fail-unexpected-char ch)))))
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
                     (_ (error "zipper-parser: program error (Continue-Parse-Array)"))))
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
                     (_ (error "zipper-parser: program error (Continue-Parse-Object)")))))))))
      (do (z <- (map to-zipper shallow-json-parser))
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
    (map from-zipper zipper-parser))

  (declare parse (coalton:String -> (Result coalton:String JSON)))
  (define (parse str)
    "Parse the JSON string `STR`.

Returns a JSON type object if successful, otherwise returns an error message."
    (let parser = (do (json <- json-parser)
                      skip-whitespaces
                      (opt-ch <- parser:peek-char-or-eof)
                      (match opt-ch
                        ((Some _) (fail "Contains extra characters at the end"))
                        ((None) (pure json)))))
    (map fst (parser:run! parser (port:make! (iter:into-iter str)))))

  (declare parse! (iter:Iterator Char -> Iterator (Result coalton:String JSON)))
  (define (parse! iter)
    "Create an iterator for JSON objects from the character iterator ITER, which contains JSON data.

Includes a one-character lookahead process when called."
    (let done?* = (cell:new coalton:False))
    (let port* = (cell:new (port:make! iter)))
    (let parser = (do skip-whitespaces
                      (opt-ch <- parser:peek-char-or-eof)
                      (match opt-ch
                        ((Some _) (map Some json-parser))
                        ((None) (pure None)))))
    (iter:new (fn ()
                (if (cell:read done?*)
                    None
                    (match (parser:run! parser (cell:read port*))
                      ((Ok (Tuple (Some x) port))
                       (cell:write! port* port)
                       (Some (Ok x)))
                      ((Ok (Tuple (None) _))
                       (cell:write! done?* coalton:True)
                       None)
                      ((Err e)
                       (cell:write! done?* coalton:True)
                       (Some (Err e))))))))

  (define-instance (TryInto coalton:String JSON)
    (define tryInto parse))

  ;;
  ;; JSON to String conversion
  ;;

  (define-instance (Into JSON coalton:String)
    (define (into x)
      (let out = (output:make-string-output-stream))
      (write-json x out)
      (output:get-output-stream-string out)))

  (declare write-json (JSON -> output:Stream -> Unit))
  (define (write-json x out)
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
       (write-json-string s out))
      ((Array l)
       (output:write-char #\[ out)
       (match l
         ((Cons h t)
          (write-json h out)
          (for x in t
               (output:write-char #\, out)
               (write-json x out)))
         ((Nil) Unit))
       (output:write-char #\] out))
      ((Object m)
       (output:write-char #\{ out)
       (let iter = (iter:into-iter m))
       (match (iter:next! iter)
         ((Some (Tuple k v))
          (write-json-string k out)
          (output:write-char #\: out)
          (write-json v out)
          (for (Tuple k v) in iter
               (output:write-char #\, out)
               (write-json-string k out)
               (output:write-char #\: out)
               (write-json v out)))
         ((None) Unit))
       (output:write-char #\} out))))

  (declare write-json-string (coalton:String -> output:Stream -> Unit))
  (define (write-json-string x out)
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
