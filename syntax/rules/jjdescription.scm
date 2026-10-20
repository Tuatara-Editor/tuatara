; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The colouring rules for jjdescription: runtime/queries/jjdescription/highlights.scm of
; Helix (https://github.com/YardQuit/helix_editor) at commit 9bcdf4d79d14,
; unchanged below this notice.

(text) @string
(filepath) @string.special.path

(change type: "A" @diff.plus)
(change type: "D" @diff.minus)
(change type: "M" @diff.delta)
(change type: "C" @diff.plus)
(change type: "R" @diff.delta)

(comment) @comment
