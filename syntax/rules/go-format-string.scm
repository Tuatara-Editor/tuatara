; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The colouring rules for go-format-string: runtime/queries/go-format-string/highlights.scm of
; Helix (https://github.com/YardQuit/helix_editor) at commit 9bcdf4d79d14,
; unchanged below this notice.

(escaped_percent_sign) @constant.character.escape

"." @punctuation.delimiter
"%" @punctuation.special

[
  "["
  "]"
] @punctuation.bracket

(explicit_argument_index) @constant.numeric

(flag) @constant.builtin

(width) @constant.numeric.integer
(precision) @constant.numeric.float
(asterisk) @string.special.symbol

(verb) @type

(text) @string
