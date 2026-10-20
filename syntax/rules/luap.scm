; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The colouring rules for luap: runtime/queries/luap/highlights.scm of
; Helix (https://github.com/YardQuit/helix_editor) at commit 9bcdf4d79d14,
; unchanged below this notice.

[
  (anchor_begin)
  (anchor_end)
] @punctuation.delimiter

(pattern
  (character
    "." @variable.builtin))

[
  "["
  "]"
  "("
  ")"
] @punctuation.bracket

[
  (zero_or_more)
  (shortest_zero_or_more)
  (one_or_more)
  (zero_or_one)
] @operator

(range
  from: (character) @constant
  "-" @operator
  to: (character) @constant)

(set
  (character) @constant)

(negated_set
  (character) @constant)

(class) @constant.character.escape

(class
  "%" @string.regexp
  (escape_char) @string.regexp)

(negated_set
  "^" @operator)

(balanced_match
  (character) @variable.parameter)
