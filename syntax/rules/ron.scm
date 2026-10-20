; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The colouring rules for ron: runtime/queries/ron/highlights.scm of
; Helix (https://github.com/YardQuit/helix_editor) at commit 9bcdf4d79d14,
; unchanged below this notice.

; Structs
;------------

(enum_variant) @type.enum.variant
(struct_entry (_) @variable.other.member ":")
(struct_name (identifier)) @type
(unit_struct) @type.builtin

; Literals
;------------

(string) @string
(boolean) @constant.builtin.boolean
(integer) @constant.numeric.integer
(float) @constant.numeric.float
(char) @constant.character

; Comments
;------------

(line_comment) @comment.line
(block_comment) @comment.block


; Punctuation
;------------

"," @punctuation.delimiter
":" @punctuation.delimiter

"(" @punctuation.bracket
")" @punctuation.bracket
"[" @punctuation.bracket
"]" @punctuation.bracket
"{" @punctuation.bracket
"}" @punctuation.bracket

"-" @operator

; Special
;------------
(escape_sequence) @constant.character.escape
