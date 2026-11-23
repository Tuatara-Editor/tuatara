; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The colouring rules for gas: runtime/queries/gas/highlights.scm of
; Helix (https://github.com/helix-editor/helix) at commit 9bcdf4d79d14,
; unchanged below this notice.

(comment) @comment
(number) @constant.numeric
(directive_name) @keyword.directive
(symbol) @variable
(label) @function
(label)
(instruction_prefix) @keyword
(instruction_name) @function.special
(register) @constant.builtin
(string) @string
(char) @constant.character
(type) @type
(constant "$" @constant)
(operand_modifier) @attribute

(expression
  ["-" "+" "*" "/" "="] @operator)

["(" ")"] @punctuation.bracket

["," ":"] @punctuation.delimiter
