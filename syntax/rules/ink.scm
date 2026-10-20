; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The colouring rules for ink: runtime/queries/ink/highlights.scm of
; Helix (https://github.com/YardQuit/helix_editor) at commit 9bcdf4d79d14,
; unchanged below this notice.

; tags and labels
(label) @label
(tag (identifier) @comment)
(tag) @comment

; values
(identifier) @function
(string) @string
(boolean) @constant.builtin.boolean
(number) @constant.numeric

; headers
(knot_header) @keyword
(stitch_header) @keyword
(function_header) @keyword

; marks (ink)
(option_mark) @keyword.directive
(gather_mark) @type.builtin
(glue) @type.builtin

; calls
(divert_or_thread) @function

; operators
(assignment) @operator

; special marks/operators (ink)
(arrow) @special
(double_arrow) @special
(back_arrow) @constant
(dot) @special
(mark_start) @special
(mark_end) @special
(hide_start) @special
(hide_end) @special

; declarations
(var_line) @attribute
(const_line) @constant
(list_line) @type

; comments
(line_comment) @comment
(block_comment) @comment

; unparsed code
(inline_block) @keyword
(condition_block) @keyword
(code_text) @keyword

; support injection
(program) @ui.text
