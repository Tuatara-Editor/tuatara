; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The colouring rules for r: runtime/queries/r/highlights.scm of
; Helix (https://github.com/helix-editor/helix) at commit 9bcdf4d79d14,
; unchanged below this notice.

; highlights.scm

(identifier) @variable

; Member access: the name after `$`/`@` (`obj$field`, `s4@slot`).
(extract_operator rhs: (identifier) @variable.other.member)

; Literals

[
  (integer)
  (complex)
] @constant.numeric.integer

[
  (float)
  (nan)
  (inf)
] @constant.numeric.float

[
  (true)
  (false)
] @constant.builtin.boolean

[
  (na)
  (null)
  (dots)
  (dot_dot_i)
] @constant.builtin

(string) @string
(string (string_content (escape_sequence) @constant.character.escape))

; Comments

(comment) @comment

; Operators

[
  "!" "!=" "$" "&" "&&" "*" "**" "+" "-" "->" "->>" "/" ":" ":::" ":::" ":=" "<"
  "<-" "<<-" "<=" "=" "==" ">" ">=" "?" "@" "^" "special" "|" "|>" "||" "~"
] @operator

(function_definition name: "\\" @operator)

; Punctuation

[
  "(" ")"
  "[" "]"
  "{" "}"
  "[[" "]]"
] @punctuation.bracket

(comma) @punctuation.delimiter

; Functions

(binary_operator
  lhs: (identifier) @function
  operator: "<-"
  rhs: (function_definition))

(binary_operator
  lhs: (identifier) @function
  operator: "="
  rhs: (function_definition))

; Calls

(call function: (identifier) @function)
(call function: (namespace_operator rhs: (identifier) @function))

; Parameters

(parameters (parameter name: (identifier) @variable.parameter))
(arguments (argument name: (identifier) @variable.parameter))

; Namespaces

(namespace_operator lhs: (identifier) @namespace)

; Keywords

[
  "in"
  (next)
  (break)
] @keyword

(return) @keyword.control.return

[
  "if"
  "else"
] @keyword.control.conditional

[
  "while"
  "repeat"
  "for"
] @keyword.control.repeat

"function" @keyword.function

