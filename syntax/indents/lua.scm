; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The indentation rules for lua: runtime/queries/lua/indents.scm of
; Helix (https://github.com/YardQuit/helix_editor) at commit 9bcdf4d79d14,
; unchanged below this notice.

[
  (function_definition)
  (function_declaration)
  (method_index_expression)
  (field)
  (if_statement)
  (for_statement)
  (repeat_statement)
  (while_statement)
  (table_constructor)
  (arguments)
  (do_statement)
] @indent

[
  "end"
  "until"
  "else"
  "elseif"
  "}"
  ")"
] @outdent
