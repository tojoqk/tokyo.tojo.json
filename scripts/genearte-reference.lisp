(ql:quickload :coalton/doc)
(ql:quickload :tokyo.tojo.json)
(coalton-doc:write-documentation-to-file
 "docs/reference.md"
 :packages '(tokyo.tojo.json)
 :asdf-system :tokyo.tojo.json
 :file-link-prefix "https://github.com/tojoqk/json/tree/main/src/")
