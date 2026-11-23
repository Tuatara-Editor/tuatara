; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The selection rules for julia: runtime/queries/julia/textobjects.scm of
; Helix (https://github.com/helix-editor/helix) at commit 9bcdf4d79d14,
; unchanged below this notice.

(function_definition (_)? @function.inside) @function.around

; Short-form `f(x) = …` is now an assignment with a call_expression LHS.
((assignment . (call_expression)) @function.inside) @function.around

(macro_definition (_)? @function.inside) @function.around

(struct_definition (_)? @class.inside) @class.around

(abstract_definition (_)? @class.inside) @class.around

(primitive_definition (_)? @class.inside) @class.around

; Parameters and call arguments both live in argument_list now.
(argument_list
  ((_) @parameter.inside . ","? @parameter.around) @parameter.around)

(curly_expression
  ((_) @parameter.inside . ","? @parameter.around) @parameter.around)

(line_comment) @comment.inside

(line_comment)+ @comment.around

(block_comment) @comment.inside

(block_comment)+ @comment.around

(macrocall_expression (macro_identifier
    (identifier) @_name
    (#match? @_name "^(test|test_throws|test_logs|inferred|test_deprecated|test_warn|test_nowarn|test_broken|test_skip)$")
  )
  .
  (macro_argument_list) @test.inside) @test.around
