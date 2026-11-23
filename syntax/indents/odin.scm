; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The indentation rules for odin: runtime/queries/odin/indents.scm of
; Helix (https://github.com/helix-editor/helix) at commit 9bcdf4d79d14,
; unchanged below this notice.

[
  (block)
  (enum_declaration)
  (union_declaration)
  (struct_declaration)
  (struct)
  (parameters)
  (tuple_type)
  (struct_type)
  (call_expression)
  (switch_case)
] @indent

[
 ")"
 "]"
] @outdent

; Have to do all closing brackets separately because the one for switch statements shouldn't end.
(block "}" @outdent)
(enum_declaration "}" @outdent)
(union_declaration "}" @outdent)
(struct_declaration "}" @outdent)
(struct "}" @outdent)
(struct_type "}" @outdent)
