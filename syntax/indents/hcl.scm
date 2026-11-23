; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The indentation rules for hcl: runtime/queries/hcl/indents.scm of
; Helix (https://github.com/helix-editor/helix) at commit 9bcdf4d79d14,
; unchanged below this notice.

[
  (object)
  (block)
  (tuple)
  (for_tuple_expr)
  (for_object_expr)
] @indent

[
  (object_end)
  (block_end)
  (tuple_end)
] @outdent
