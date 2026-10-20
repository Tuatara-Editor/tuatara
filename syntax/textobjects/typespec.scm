; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The selection rules for typespec: runtime/queries/typespec/textobjects.scm of
; Helix (https://github.com/YardQuit/helix_editor) at commit 9bcdf4d79d14,
; unchanged below this notice.

; Classes

(enum_statement
  (enum_body) @class.inside) @class.around

(model_statement
  (model_expression) @class.inside) @class.around

(union_statement
  (union_body) @class.inside) @class.around

; Interfaces

(interface_statement
  (interface_body
    (interface_member) @function.around) @class.inside) @class.around

; Comments

[
  (single_line_comment)
  (multi_line_comment)
] @comment.inside

[
  (single_line_comment)
  (multi_line_comment)
]+ @comment.around

; Functions

[
  (decorator)
  (decorator_declaration_statement)
  (function_declaration_statement)
  (operation_statement)
] @function.around

(function_parameter_list
  (function_parameter)? @parameter.inside)* @function.inside

(decorator_arguments
  (expression_list
    (_) @parameter.inside)*) @function.inside

(operation_arguments
  (model_property)? @parameter.inside)* @function.inside

(template_parameters
  (template_parameter_list
    (template_parameter) @parameter.inside)) @function.inside
