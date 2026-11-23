; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The selection rules for json5: runtime/queries/json5/textobjects.scm of
; Helix (https://github.com/helix-editor/helix) at commit 9bcdf4d79d14,
; unchanged below this notice.

(member
  (_) @entry.inside) @entry.around

(array
  (_) @entry.around)

(comment) @comment.inside
(comment)+ @comment.around
