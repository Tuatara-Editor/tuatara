; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The indentation rules for ocaml: runtime/queries/ocaml/indents.scm of
; Helix (https://github.com/YardQuit/helix_editor) at commit 9bcdf4d79d14,
; unchanged below this notice.

[
  (let_binding)
  (type_binding)
  (structure)
  (signature)
  (record_declaration)
  (function_expression)
  (fun_expression)
  (match_case)
  (then_clause)
  (else_clause)
] @indent

; `else if` chains nest else_clause -> if_expression -> else_clause, which would
; double-indent the trailing branch; `try`/`with` and explicit module `end`
; alignment are likewise left to the writer — see tests/indent/ocaml.ml.
[
  "}"
  ")"
  "]"
  "end"
] @outdent
