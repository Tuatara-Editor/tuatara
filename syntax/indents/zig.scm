; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The indentation rules for zig: runtime/queries/zig/indents.scm of
; Helix (https://github.com/YardQuit/helix_editor) at commit 9bcdf4d79d14,
; unchanged below this notice.

[
  (block)
  (switch_expression)
  (initializer_list)
  (struct_declaration)
  (enum_declaration)
  (union_declaration)
  (error_set_declaration)
] @indent

[
  "}"
  "]"
  ")"
] @outdent
