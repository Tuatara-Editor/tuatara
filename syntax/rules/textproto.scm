; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The colouring rules for textproto: runtime/queries/textproto/highlights.scm of
; Helix (https://github.com/YardQuit/helix_editor) at commit 9bcdf4d79d14,
; unchanged below this notice.

(string) @string

(field_name) @variable.other.member

(comment) @comment

(number) @constant.numeric
; covers e.g. booleans and "inf"
(scalar_value (identifier)) @constant
; Covers "-inf"
(scalar_value (signed_identifier)) @constant.numeric

[
  (open_squiggly)
  (close_squiggly)
  (open_square)
  (close_square)
  (open_arrow)
  (close_arrow)
] @punctuation.bracket

"," @punctuation.delimiter
