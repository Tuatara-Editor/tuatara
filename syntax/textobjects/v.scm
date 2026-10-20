; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The selection rules for v: runtime/queries/v/textobjects.scm of
; Helix (https://github.com/YardQuit/helix_editor) at commit 9bcdf4d79d14,
; unchanged below this notice.

(function_declaration
  body: (block)? @function.inside) @function.around

((function_declaration
   name: (identifier) @_name
   body: (block)? @test.inside) @test.around
 (#match? @_name "^test"))

(function_literal
  body: (block)? @function.inside) @function.around

(parameter_list
  ((_) @parameter.inside . ","? @parameter.around) @parameter.around)

(call_expression
  (argument_list
    ((_) @parameter.inside) @parameter.around))

(struct_declaration
    (struct_field_declaration) @class.inside) @class.around

(struct_field_declaration
  ((_) @parameter.inside) @parameter.around)

[(line_comment) (block_comment)] @comment.inside
[(line_comment)+ (block_comment)+] @comment.around

