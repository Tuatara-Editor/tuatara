; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The indentation rules for amber: runtime/queries/amber/indents.scm of
; Helix (https://github.com/YardQuit/helix_editor) at commit 9bcdf4d79d14,
; unchanged below this notice.

; Block structures that increase indent
[
  (function_definition)
  (block)
  (main_block)

  ; Control flow
  (if_cond)
  (if_chain)
  (for_loop)
  (while_loop)
  (loop_infinite)

  ; Collections
  (array)
  (parameter_list)
  (function_parameter_list)
  (parentheses)

  ; Amber-specific
  (command_modifier_block)
  (handler_failed)
  (handler_succeeded)
  (handler_exited)
] @indent

; Closing delimiters
[
  "}"
  "]"
  ")"
] @outdent

; Multi-line construct support
[
  (function_definition)
  (if_cond)
  (if_chain)
  (for_loop)
  (while_loop)
  (loop_infinite)
  (main_block)
] @extend

; Prevent premature outdent
[
  (function_control_flow)
  (loop_control_flow)
] @extend.prevent-once
