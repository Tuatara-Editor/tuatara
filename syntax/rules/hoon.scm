; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The colouring rules for hoon: runtime/queries/hoon/highlights.scm of
; Helix (https://github.com/helix-editor/helix) at commit 9bcdf4d79d14,
; unchanged below this notice.

(number) @constant.numeric

(string) @string

[
  "("
  ")"
  "["
  "]"
]  @punctuation.bracket

[
  (coreTerminator)
  (seriesTerminator)
] @punctuation.delimiter


(rune) @keyword

(term) @constant

(aura) @constant.builtin

(Gap) @comment

(boolean) @constant.builtin

(date) @constant.builtin
(mold) @constant.builtin
(specialIndex) @constant.builtin
(lark) @operator
(fullContext) @constant.builtin
