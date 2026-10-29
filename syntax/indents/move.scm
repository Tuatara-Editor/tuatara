; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The indentation rules for move: runtime/queries/move/indents.scm of
; Helix (https://github.com/helix-editor/helix) at commit 9bcdf4d79d14,
; unchanged below this notice.

[
  (module_body)
  (block)
  (named_fields)
  (match_expression)
  (match_arm)
  (spec_block)
  (field_initialize_list)
  (expression_list)
] @indent

[
  "}"
  ")"
  "]"
] @outdent
