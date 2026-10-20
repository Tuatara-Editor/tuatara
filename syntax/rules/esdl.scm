; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The colouring rules for esdl: runtime/queries/esdl/highlights.scm of
; Helix (https://github.com/YardQuit/helix_editor) at commit 9bcdf4d79d14,
; unchanged below this notice.

; Keywords
[
  "module"
  "using"
  "single"
  "multi"
  "link"
  "property"
  "constraint"
  "tuple"
  "annotation"
  "abstract"
  "scalar"
  "type"
  "required"
  "optional"
  "extension"
  "function"
] @keyword

(identifier) @variable

(modifier) @keyword
(extending) @keyword

(module name: (identifier) @namespace)
(object_type) @type

(comment) @comment

; Properties (includes both property and link declarations, since link keyword is optional)
(property) @variable.other.member

(string) @string
(edgeql_fragment) @string

; Builtins
(type) @type
[
  "str"
  "bool"
  "int16"
  "int32"
  "int64"
  "float32"
  "float64"
  "bigint"
  "decimal"
  "json"
  "uuid"
  "bytes"
  "datetime"
  "duration"
  "sequence"
  "anytype"
] @type.builtin

(true) @constant.builtin.boolean
(false) @constant.builtin.boolean
(null) @constant.builtin

; Delimiters
[
  ";"
  ","
] @punctuation.delimiter

; Operators
[
  "->"
  ":="
] @operator
