; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The colouring rules for supercollider: runtime/queries/supercollider/highlights.scm of
; Helix (https://github.com/helix-editor/helix) at commit 9bcdf4d79d14,
; unchanged below this notice.

(line_comment) @comment.line
(block_comment) @comment.block

(argument name: (identifier) @variable.parameter)

(local_var name: (identifier) @variable)
(environment_var name:(identifier) @variable.builtin)
(builtin_var) @constant.builtin

(function_definition name: (variable) @function)

(named_argument name: (identifier) @variable.other.member)

(method_call name: (method_name) @function.method)

(class) @keyword.storage.type

(number) @constant.numeric
(float) @constant.numeric.float

(string) @string
(symbol) @string.special.symbol

[
"&&"
"||"
"&"
"|"
"^"
"=="
"!="
"<"
"<="
">"
">="
"<<"
">>"
"+"
"-"
"*"
"/"
"%"
"="
"|@|"
"@@"
"@|@"
] @operator

[
"arg"
"classvar"
"const"
"var"
] @keyword

[
  "("
  ")"
  "["
  "]"
  "{"
  "}"
  "|"
] @punctuation.bracket

[
  ";"
  "."
  ","
] @punctuation.delimiter

(control_structure) @keyword.control.conditional

(escape_sequence) @string.special

(duplicated_statement) @keyword.control.repeat
