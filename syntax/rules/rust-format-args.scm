; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The colouring rules for rust-format-args: runtime/queries/rust-format-args/highlights.scm of
; Helix (https://github.com/helix-editor/helix) at commit 9bcdf4d79d14,
; unchanged below this notice.

; regular escapes like `\n` are detected using another grammar
; Here, we only detect `{{` and `}}` as escapes for `{` and `}`
(escaped) @constant.character.escape

[
  "#"
  (type)
] @special

[
  (sign)
  (fill)
  (align)
  (width)
] @operator

(number) @constant.numeric

(colon) @punctuation

(identifier) @variable

; SCREAMING_CASE is assumed to be constant
((identifier) @constant
 (#match? @constant "^[A-Z_]+$"))

[
  "{"
  "}"
] @punctuation.special
