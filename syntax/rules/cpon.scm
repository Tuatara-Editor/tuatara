; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The colouring rules for cpon: runtime/queries/cpon/highlights.scm of
; Helix (https://github.com/helix-editor/helix) at commit 9bcdf4d79d14,
; unchanged below this notice.

[
  (true)
  (false)
] @constant.builtin.boolean
(null) @constant.builtin
(number) @constant.numeric
(pair
  key: (_) @keyword)
(ipair
  key: (_) @keyword)
(mpair
  key: (_) @keyword)

(string) @string
(escape_sequence) @constant.character.escape

"," @punctuation.delimiter
[
  "["
  "]"
  "{"
  "}"
  "<"
  ">"
] @punctuation.bracket
