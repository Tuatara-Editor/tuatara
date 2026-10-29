; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The indentation rules for wit: runtime/queries/wit/indents.scm of
; Helix (https://github.com/helix-editor/helix) at commit 9bcdf4d79d14,
; unchanged below this notice.

[
  (use_items)
  (fields)
  (variant_items)
  (variant_payload)
  (flags)
  (enum_items)
  (union_items)
  (args)
  (resource_items)
] @indent

[ "}" ")" ] @outdent
