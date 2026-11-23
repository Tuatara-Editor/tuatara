; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The colouring rules for pest: runtime/queries/pest/highlights.scm of
; Helix (https://github.com/helix-editor/helix) at commit 9bcdf4d79d14,
; unchanged below this notice.

(line_comment) @comment
(block_comment) @comment

[
  "("
  ")"
  "["
  "]"
  "{"
  "}"
] @punctuation.bracket

((identifier) @variable)
((builtin) @type.builtin)
((const) @constant)

[
  (string)
  (character)
] @string

[
  "_"
  "@"
  "$"
]@keyword.storage.modifier

[
  "~"
  "|"
  "="
  "+"
  "*"
  "&"
  "^"
  "!"
  "?"
  ".."
] @operator

[
  "ANY"
  "DROP"
  "EOI"
  "NEWLINE"
  "PEEK"
  "PEEK_ALL"
  "POP"
  "POP_ALL"
  "PUSH"
  "SOI"
] @keyword
