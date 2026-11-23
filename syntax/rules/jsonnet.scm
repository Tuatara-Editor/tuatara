; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The colouring rules for jsonnet: runtime/queries/jsonnet/highlights.scm of
; Helix (https://github.com/helix-editor/helix) at commit 9bcdf4d79d14,
; unchanged below this notice.

["if" "then" "else"] @keyword.control.conditional
[
  (local)
  "function"
] @keyword
(comment) @comment

(string) @string
(number) @constant.numeric
[
  (true)
  (false)
] @constant.builtin.boolean

; Binary operators are now an `operator:` field whose node is the operator
; category (additive/comparison/and/bitor/…); capture it directly.
(binary operator: _ @operator)
(unaryop) @operator

(id) @variable
(param identifier: (id) @variable.parameter)
(bind function: (id) @function)
(fieldname (id) @variable.other.member)
[
  "["
  "]"
  "{"
  "}"
] @punctuation.bracket
"for" @keyword.control.repeat
"in" @keyword.operator
[(self) (dollar)] @variable.builtin
"assert" @keyword
(null) @constant.builtin
[
  ":"
  "::"
  ";"
  "="
] @punctuation.delimiter
