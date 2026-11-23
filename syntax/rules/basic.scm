; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The colouring rules for basic: runtime/queries/basic/highlights.scm of
; Helix (https://github.com/helix-editor/helix) at commit 9bcdf4d79d14,
; unchanged below this notice.

; Keywords
[
  "PRINT"
  "LET"
  "IF"
  "THEN"
  "GOTO"
  "GOSUB"
  "RETURN"
  "FOR"
  "TO"
  "STEP"
  "NEXT"
  "INPUT"
  "END"
  "REM"
  "DATA"
  "READ"
  "DIM"
] @keyword

; Logical operators
[
  "AND"
  "and"
  "OR"
  "or"
  "NOT"
] @keyword.operator

; Comments
(comment) @comment
(rem_statement) @comment

; Function calls
(function_call) @function

; Numbers
(line_number) @constant.numeric
(number) @constant.numeric

; Strings
(string) @string

; Operators
[
  "="
  "<>"
  "<"
  ">"
  "<="
  ">="
  "+"
  "-"
  "*"
  "/"
  "^"
] @operator

; Punctuation
[
  "("
  ")"
] @punctuation.bracket

[
  ","
  ";"
] @punctuation.delimiter

; Variables
(identifier) @variable
