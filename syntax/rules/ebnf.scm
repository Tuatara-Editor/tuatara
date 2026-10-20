; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The colouring rules for ebnf: runtime/queries/ebnf/highlights.scm of
; Helix (https://github.com/YardQuit/helix_editor) at commit 9bcdf4d79d14,
; unchanged below this notice.

;; Simple tokens
(terminal) @string

(special_sequence) @string.special

(integer) @constant.numeric.integer

(comment) @comment.block

;; Identifiers
(identifier) @identifier

;; Punctuation
[
 ";"
 ","
] @punctuation.delimiter

[
 "|"
 "*"
 "-"
] @operator

"=" @keyword.operator

[
 "("
 ")"
 "["
 "]"
 "{"
 "}"
] @punctuation.bracket
