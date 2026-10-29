; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The colouring rules for po: runtime/queries/po/highlights.scm of
; Helix (https://github.com/helix-editor/helix) at commit 9bcdf4d79d14,
; unchanged below this notice.

[
  (msgctxt)
  (msgid)
  (msgid_plural)
  (msgstr)
]@keyword

(comment) @comment
(comment (comment_reference (text) @string.special.path))
(comment (comment_flag (text) @label))

(number) @constant.numeric
(string) @string
