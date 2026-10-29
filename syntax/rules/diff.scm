; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The colouring rules for diff: runtime/queries/diff/highlights.scm of
; Helix (https://github.com/helix-editor/helix) at commit 9bcdf4d79d14,
; unchanged below this notice.

[(addition) (new_file)] @diff.plus
[(deletion) (old_file)] @diff.minus

(commit) @constant
(location) @attribute
(command) @markup.bold

(comment) @comment
; Rename/copy similarity percentage (`similarity index 95%`).
(score) @constant.numeric
