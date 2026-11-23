; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The indentation rules for just: runtime/queries/just/indents.scm of
; Helix (https://github.com/helix-editor/helix) at commit 9bcdf4d79d14,
; unchanged below this notice.

; This query specifies how to auto-indent logical blocks.
;
; Better documentation with diagrams is in https://docs.helix-editor.com/guides/indent.html

[
  (recipe)
  (string)
  (external_command)
] @indent @extend
