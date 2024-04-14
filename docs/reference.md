# Package `tokyo.tojo.json`<a name="tokyo.tojo.json-package"></a>

## [json.lisp](https://github.com/tojoqk/json/tree/main/src/json.lisp) <a name="tokyo.tojo.json-json-lisp-file"></a>

### Types

#### <code>JSON</code> <sup><sub>[TYPE]</sub></sup><a name="json-type"></a>

A representation of a JSON object.

<details>
<summary>Instances</summary>

- <code><a href="#runtimerepr-class">RUNTIMEREPR</a> <a href="#json-type">JSON</a></code>
- <code><a href="#eq-class">EQ</a> <a href="#json-type">JSON</a></code>
- <code><a href="#into-class">INTO</a> <a href="#unit-type">UNIT</a> <a href="#json-type">JSON</a></code>
- <code><a href="#into-class">INTO</a> <a href="#boolean-type">BOOLEAN</a> <a href="#json-type">JSON</a></code>
- <code><a href="#into-class">INTO</a> <a href="#string-type">STRING</a> <a href="#json-type">JSON</a></code>
- <code><a href="#into-class">INTO</a> <a href="#double-float-type">DOUBLE-FLOAT</a> <a href="#json-type">JSON</a></code>
- <code><a href="#into-class">INTO</a> (<a href="#list-type">LIST</a> <a href="#json-type">JSON</a>) <a href="#json-type">JSON</a></code>
- <code><a href="#into-class">INTO</a> (<a href="#map-type">MAP</a> <a href="#string-type">STRING</a> <a href="#json-type">JSON</a>) <a href="#json-type">JSON</a></code>
- <code><a href="#into-class">INTO</a> <a href="#json-type">JSON</a> <a href="#string-type">STRING</a></code>
- <code><a href="#tryinto-class">TRYINTO</a> <a href="#string-type">STRING</a> <a href="#json-type">JSON</a></code>

</details>


***

### Values

#### <code>(PARSE STR)</code> <sup><sub>FUNCTION</sub></sup><a name="parse-value"></a>
<code>(<a href="#string-type">STRING</a> &rarr; (<a href="#result-type">RESULT</a> <a href="#string-type">STRING</a> <a href="#json-type">JSON</a>))</code>

Parse the given JSON string `str` and return a corresponding JSON type object.


***

#### <code>(PARSE* ITER)</code> <sup><sub>FUNCTION</sub></sup><a name="parse*-value"></a>
<code>((<a href="#iterator-type">ITERATOR</a> <a href="#char-type">CHAR</a>) &rarr; (<a href="#iterator-type">ITERATOR</a> (<a href="#result-type">RESULT</a> <a href="#string-type">STRING</a> <a href="#json-type">JSON</a>)))</code>

Parse the JSON data from the given iterator of char `iter` and return a corresponding iterator of JSON.


***

