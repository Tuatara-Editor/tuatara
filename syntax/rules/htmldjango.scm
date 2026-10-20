; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The colouring rules for htmldjango: runtime/queries/htmldjango/highlights.scm of
; Helix (https://github.com/YardQuit/helix_editor) at commit 9bcdf4d79d14,
; unchanged below this notice.

[
  (unpaired_comment)
  (paired_comment)
] @comment

[
  "{{"
  "}}"
  "{%"
  "%}"
  (end_paired_statement)
] @punctuation.bracket

[
 (tag_name) 
] @function

(variable_name) @variable
(filter_name) @function
(filter_argument) @variable.parameter
(keyword) @keyword
(operator) @operator
(keyword_operator) @keyword.operator
(number) @constant.numeric
(boolean) @constant.builtin.boolean
(string) @string
