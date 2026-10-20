; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The colouring rules for forth: runtime/queries/forth/highlights.scm of
; Helix (https://github.com/YardQuit/helix_editor) at commit 9bcdf4d79d14,
; unchanged below this notice.

; Definition keywords
[
  (start_definition)
  (end_definition)
] @keyword

; Control flow - highlighted as keywords for prominence
(control_flow) @keyword.control

; I/O operations
(io) @function.builtin

; Operators - arithmetic, logic, stack manipulation
(operator) @operator

; Core builtins - defining words, memory, etc.
(core) @type

; Numbers - all subtypes
(character_literal) @constant.character
(hex_number) @constant.numeric
(binary_number) @constant.numeric
(octal_number) @constant.numeric
(float_number) @constant.numeric
(double_cell_number) @constant.numeric
(decimal_number) @constant.numeric

; Strings
(string) @string

; Comments - different types
(line_comment) @comment.line
(block_comment) @comment.block
(stack_effect) @comment.block.documentation

; User-defined words
(word) @function
