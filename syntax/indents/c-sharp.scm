; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The indentation rules for c-sharp: runtime/queries/c-sharp/indents.scm of
; Helix (https://github.com/helix-editor/helix) at commit 9bcdf4d79d14,
; unchanged below this notice.

[
  (declaration_list)
  (block)
  (enum_member_declaration_list)
  (accessor_list)
  (switch_body)
  (switch_section)
  (initializer_expression)
] @indent

[
  "}"
  ")"
  "]"
] @outdent
