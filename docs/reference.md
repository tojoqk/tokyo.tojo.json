# Package `tokyo.tojo.json`<a name="tokyo.tojo.json-package"></a>

## [json.lisp](https://github.com/tojoqk/json/tree/main/src/json.lisp) <a name="tokyo.tojo.json-json-lisp-file"></a>

### Types

#### <code>JSON</code> <sup><sub>[TYPE]</sub></sup><a name="json-type"></a>

A representation of a JSON object.

<details>
<summary>Instances</summary>

- <code><a href="#runtimerepr-class">RUNTIMEREPR</a> <a href="#json-type">JSON</a></code>
- <code><a href="#eq-class">EQ</a> <a href="#json-type">JSON</a></code>
- <code><a href="#into-class">INTO</a> <a href="#json-type">JSON</a> <a href="#string-type">STRING</a></code>
- <code><a href="#tryinto-class">TRYINTO</a> <a href="#string-type">STRING</a> <a href="#json-type">JSON</a></code>

</details>


***

### Values

#### <code>(PARSE! SOURCE)</code> <sup><sub>FUNCTION</sub></sup><a name="parse!-value"></a>
<code>&forall; :A :B. <a href="#intoport-class">INTOPORT</a> :B :A &rArr; (:B &rarr; (<a href="#iterator-type">ITERATOR</a> (<a href="#result-type">RESULT</a> <a href="#string-type">STRING</a> <a href="#json-type">JSON</a>)))</code>

Create an iterator for JSON objects from the character iterator ITER, which contains JSON data.

Includes a one-character lookahead process when called.


***

