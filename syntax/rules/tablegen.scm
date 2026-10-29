; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The colouring rules for tablegen: runtime/queries/tablegen/highlights.scm of
; Helix (https://github.com/helix-editor/helix) at commit 9bcdf4d79d14,
; unchanged below this notice.

[
  (comment)
  (multiline_comment)
] @comment

[
  "("
  ")"
  "["
  "]"
  "{"
  "}"
  "<"
  ">"
] @punctuation.bracket

[
  ","
  ";"
  "."
] @punctuation.delimiter

[
  "#"
  "-"
  "..."
  ":"
] @operator

[
  "="
  "!cond"
  (operator_keyword)
] @function

[
  "true"
  "false"
] @constant.builtin.boolean

[
  "?"
] @constant.builtin

(var) @variable

(template_arg (identifier) @variable.parameter)

(_ argument: (value (identifier) @variable.parameter))

(type) @type

"code" @type.builtin

(number) @constant.numeric.integer
[
  (string_string)
  (code_string)
] @string

(preprocessor) @keyword.directive

[
  "class"
  "field"
  "let"
  "defvar"
  "def"
  "defset"
  "defvar"
  "deftype"
  "assert"
  "dump"
] @keyword

[
  "let"
  "in"
  "foreach"
  "if"
  "then"
  "else"
] @keyword.operator

"include" @keyword.control.import

[
  "multiclass"
  "defm"
] @namespace
