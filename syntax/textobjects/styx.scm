; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The selection rules for styx: runtime/queries/styx/textobjects.scm of
; Helix (https://github.com/helix-editor/helix) at commit 9bcdf4d79d14,
; unchanged below this notice.

[(line_comment) (doc_comment)] @comment.inside

(line_comment)+ @comment.around
(doc_comment)+ @comment.around

(entry value: (_) @entry.inside) @entry.around
