; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The indentation rules for v: runtime/queries/v/indents.scm of
; Helix (https://github.com/YardQuit/helix_editor) at commit 9bcdf4d79d14,
; unchanged below this notice.

[
  (struct_declaration)
  (function_declaration)
  (if_expression)
  (match_expression)
  (for_statement)
  (unsafe_expression)
  (var_declaration)
  (const_declaration)
] @indent

[
  "]"
  ")"
  "}"
] @outdent
