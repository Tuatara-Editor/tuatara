; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The colouring rules for lua-format-string: runtime/queries/lua-format-string/highlights.scm of
; Helix (https://github.com/YardQuit/helix_editor) at commit 9bcdf4d79d14,
; unchanged below this notice.

(escaped_percent_sign) @constant.character.escape

(flag) @constant.builtin

(text) @string

(width) @constant.numeric.integer
(precision) @constant.numeric.float

(conversion_specifier) @type

"." @punctuation.delimiter
"%" @punctuation.special
