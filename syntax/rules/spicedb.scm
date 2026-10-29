; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The colouring rules for spicedb: runtime/queries/spicedb/highlights.scm of
; Helix (https://github.com/helix-editor/helix) at commit 9bcdf4d79d14,
; unchanged below this notice.

; highlights.scm

[
  "definition"
  "caveat"
  "permission"
  "relation"
  "nil"
] @keyword

[
  ","
  ":"
] @punctuation.delimiter

[
  "("
  ")"
  "{"
  "}"
] @punctuation.bracket

[
  "|"
  "+"
  "-"
  "&"
  "#"
  "->"
  "="
] @operator
("with") @keyword.operator

[
  "nil"
  "*"
] @constant.builtin

(comment) @comment
(type_identifier) @type
(cel_type_identifier) @type
(cel_variable_identifier) @variable.parameter
(field_identifier) @variable.other.member
[
  (func_identifier)
  (method_identifier)
] @function.method
