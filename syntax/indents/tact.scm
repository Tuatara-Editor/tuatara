; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The indentation rules for tact: runtime/queries/tact/indents.scm of
; Helix (https://github.com/YardQuit/helix_editor) at commit 9bcdf4d79d14,
; unchanged below this notice.

; indent
; ------

[
  ; (..., ...)
  (parameter_list)
  (argument_list)

  ; {..., ...}
  (instance_argument_list)

  ; {...; ...}
  (message_body)
  (struct_body)
  (contract_body)
  (trait_body)
  (function_body)
  (block_statement)

  ; misc.
  (binary_expression)
  (return_statement)
] @indent

; outdent
; -------

[
  "}"
  ")"
  ">"
] @outdent

; indent.always
; outdent.always
; align
; extend
; extend.prevent-once