; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The colouring rules for prisma: runtime/queries/prisma/highlights.scm of
; Helix (https://github.com/helix-editor/helix) at commit 9bcdf4d79d14,
; unchanged below this notice.

(string) @string

(enumeral) @constant
(number) @constant.numeric

(variable) @variable
(column_type) @type

(arguments) @variable.other.member
(model_declaration (identifier) @type)
(view_declaration (identifier) @type)

[
 "datasource"
 "enum"
 "generator"
 "model"
 "type"
 "view"
] @keyword

[
 (comment)
 (developer_comment)
] @comment

[
 (attribute)
 (block_attribute_declaration)
 (call_expression)
] @function.builtin

[
 (true)
 (false)
] @constant.builtin.boolean
(null) @constant.builtin

[
 "("
 ")"
 "["
 "]"
 "{"
 "}"
] @punctuation.bracket

[
 ":" 
 ","
] @punctuation.delimiter

[
 "="
 "@"
 "@@"
 (binary_expression)
] @operator
