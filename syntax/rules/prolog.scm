; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The colouring rules for prolog: runtime/queries/prolog/highlights.scm of
; Helix (https://github.com/helix-editor/helix) at commit 9bcdf4d79d14,
; unchanged below this notice.

(comment) @comment

(atom) @constant

((atom) @constant.builtin.boolean
  (#any-of? @constant.builtin.boolean "true" "false"))

(functional_notation
  function: (atom) @function)

(integer) @constant.numeric.integer

(float_number) @constant.numeric.float

(directive_head) @operator

(operator_notation
  operator: _ @operator)

[
 (open)
 (open_ct)
 (close)
 (open_list)
 "|"
 (close_list)
 (open_curly)
 (close_curly)
] @punctuation.bracket

[
 (arg_list_separator)
 (comma)
 (end)
 (list_notation_separator)
] @punctuation.delimiter

(operator_notation
  operator: (semicolon) @punctuation.delimiter)

(double_quoted_list_notation) @string

(variable_term) @variable
