; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The colouring rules for chuck: runtime/queries/chuck/highlights.scm of
; Helix (https://github.com/YardQuit/helix_editor) at commit 9bcdf4d79d14,
; unchanged below this notice.

; Copied from https://github.com/tymbalodeon/tree-sitter-chuck/blob/trunk/queries/highlights.scm

"@doc" @special
"do" @keyword.control.repeat
"fun" @keyword.function
"function" @keyword.function
"if" @keyword.control.conditional
"repeat" @keyword.control.repeat
"return" @keyword.control.return
"spork" @function.builtin
"until" @keyword.control.repeat
"while" @keyword.control.repeat

(block_comment) @comment.block
(boolean_literal_value) @constant.builtin.boolean
(chuck_operator) @operator
(class_identifier) @type
(duration_identifier) @type
(float) @constant.numeric.float

(function_definition name: [
  (class_identifier)
  (variable_identifier)
] @function)

(global_unit_generator) @variable.builtin
(hexidecimal) @constant.numeric
(int) @constant.numeric.integer
(keyword) @keyword
(line_comment) @comment.line
(operator) @operator
(primitive_type) @type.builtin
(special_literal_value) @constant.builtin
(string) @string
