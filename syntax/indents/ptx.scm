; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The indentation rules for ptx: runtime/queries/ptx/indents.scm of
; Helix (https://github.com/YardQuit/helix_editor) at commit 9bcdf4d79d14,
; unchanged below this notice.

; Indentation for PTX
[
  (func_body)
  (entry_declaration)
] @indent
; Do not indent on closing brace
[
  "}"
] @outdent