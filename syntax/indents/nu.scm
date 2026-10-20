; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The indentation rules for nu: runtime/queries/nu/indents.scm of
; Helix (https://github.com/YardQuit/helix_editor) at commit 9bcdf4d79d14,
; unchanged below this notice.

[
  (expr_parenthesized)
  (parameter_bracks)
  (ctrl_match)

  (val_record)
  (val_list)
  (val_closure)
  (val_table)

  (block)
] @indent

[
  "}"
  "]"
  ")"
] @outdent
