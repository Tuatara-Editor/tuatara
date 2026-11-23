; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The colouring rules for awk: runtime/queries/awk/highlights.scm of
; Helix (https://github.com/helix-editor/helix) at commit 9bcdf4d79d14,
; unchanged below this notice.

; tree-sitter-awk v0.5.1

; Order matters

[
  ";"
  ","
  "("
  ")"
  "["
  "]"
  "{"
  "}"
] @operator

(piped_io_statement [
  "|"
  "|&"
] @operator)

(redirected_io_statement [
  ">"
  ">>"
] @operator)

(update_exp [
  "++"
  "--"
] @operator)

(ternary_exp [
  "?"
  ":"
] @operator)

(assignment_exp [
  "="
  "+="
  "-="
  "*="
  "/="
  "%="
  "^="
] @operator)

(unary_exp [
  "!"
  "+"
  "-"
] @operator)

(binary_exp [
  "^"
  "**"
  "*"
  "/"
  "%"
  "+"
  "-"
  "<"
  ">"
  "<="
  ">="
  "=="
  "!="
  "~"
  "!~"
  "in"
  "&&"
  "||"
] @operator)

[
  "@include"
  "@load"
  "@namespace"
  (pattern)
] @namespace

[
  "function"
  "func"
  "print"
  "printf"
  "if"
  "else"
  "do"
  "while"
  "for"
  "in"
  "delete"
  "return"
  "exit"
  "switch"
  "case"
  "default"
  (break_statement)
  (continue_statement)
  (next_statement)
  (nextfile_statement)
  (getline_input)
  (getline_file)
] @keyword

(comment) @comment
(regex) @string.regexp
(number) @constant.numeric
(string) @string

[
  (identifier)
  (field_ref)
] @variable

(func_call name: (identifier) @function)
(func_def name: (identifier) @function)

(field_ref (_) @variable)

(ns_qualified_name "::" @operator)
(ns_qualified_name (namespace) @namespace)
