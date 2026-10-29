; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The colouring rules for xit: runtime/queries/xit/highlights.scm of
; Helix (https://github.com/helix-editor/helix) at commit 9bcdf4d79d14,
; unchanged below this notice.

(headline) @markup.heading

(open_checkbox) @markup.list
(ongoing_checkbox) @keyword.control
(checked_checkbox) @diff.plus
(obsolete_checkbox) @comment.unused

(checked_task) @comment.unused
(obsolete_task) @comment.unused

(priority) @error
