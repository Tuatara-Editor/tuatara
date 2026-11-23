; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The colouring rules for strace: runtime/queries/strace/highlights.scm of
; Helix (https://github.com/helix-editor/helix) at commit 9bcdf4d79d14,
; unchanged below this notice.

(syscall) @function.builtin
(integer) @constant.numeric
(pointer) @constant.numeric
(value) @label
(string) @string
(comment) @comment
(errorName) @error
(errorDescription) @error
