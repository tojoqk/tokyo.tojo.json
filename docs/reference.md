# Package `tokyo.tojo.json`<a name="tokyo.tojo.json-package"></a>

## [json.lisp](https://github.com/tojoqk/json/tree/main/src/json.lisp) <a name="tokyo.tojo.json-json-lisp-file"></a>

### Types

#### <code>ERROR :A</code> <sup><sub>[TYPE]</sub></sup><a name="error-type"></a>

<details>
<summary>Instances</summary>

- <code><a href="#runtimerepr-class">RUNTIMEREPR</a> (<a href="#error-type">ERROR</a> :A)</code>

</details>


***

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

#### <code>(PARSE! ITER)</code> <sup><sub>FUNCTION</sub></sup><a name="parse!-value"></a>
<code>&forall; :A. ((<a href="#iterator-type">ITERATOR</a> (<a href="#result-type">RESULT</a> :A <a href="#char-type">CHAR</a>)) &rarr; (<a href="#iterator-type">ITERATOR</a> (<a href="#result-type">RESULT</a> (<a href="#error-type">ERROR</a> :A) <a href="#json-type">JSON</a>)))</code>

Create an iterator for JSON objects from the character iterator ITER, which contains JSON data.

Includes a one-character lookahead process when called.


***

