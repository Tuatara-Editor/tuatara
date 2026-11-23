; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The colouring rules for less: runtime/queries/less/highlights.scm of
; Helix (https://github.com/helix-editor/helix) at commit 9bcdf4d79d14,
; unchanged below this notice.

(comment) @comment
(js_comment) @comment

[
  (tag_name)
  (nesting_selector)
  (universal_selector)
] @tag

[
  "~"
  ">"
  "+"
  "-"
  "*"
  "/"
  "="
  "^="
  "|="
  "~="
  "$="
  "*="
  ">="
  "<="
] @operator

[
  "and"
  "not"
  "only"
  "or"
] @keyword.operator

(attribute_selector (plain_value) @string)

(property_name) @variable.other.member
(plain_value) @string

((property_name) @variable
  (#match? @variable "^--"))
((plain_value) @variable
  (#match? @variable "^--"))

(class_name) @label
(feature_name) @variable.other.member
(function_name) @function
(id_name) @label
(namespace_name) @namespace

(identifier) @variable
(variable) @variable

(attribute_name) @attribute
(pseudo_element_selector (tag_name) @attribute)
(pseudo_class_selector (class_name) @attribute)

[
  "@charset"
  "@import"
  "@keyframes"
  "@media"
  "@namespace"
  "@supports"
  (at_keyword)
  (from)
  (important)
  (to)
  (keyword_query)
  (keyframes_name)
  (unit)
] @keyword

[
  "#"
  "."
] @punctuation

(string_value) @string
(color_value "#" @string.special)
(color_value) @string.special

(integer_value) @constant.numeric.integer
(float_value) @constant.numeric.float

[
  ")"
  "("
  "["
  "]"
  "{"
  "}"
] @punctuation.bracket

[
  ","
  ";"
  ":"
  "::"
] @punctuation.delimiter

(arguments
  (variable) @variable.parameter)

(import_statement
  (identifier) @function)
