; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The selection rules for go: runtime/queries/go/textobjects.scm of
; Helix (https://github.com/helix-editor/helix) at commit 9bcdf4d79d14,
; unchanged below this notice.

(function_declaration
  body: (block)? @function.inside) @function.around

(func_literal
  body: (block)? @function.inside) @function.around

(method_declaration
  body: (block)? @function.inside) @function.around

;; struct and interface declaration as class textobject?
(type_declaration
  (type_spec (type_identifier) (struct_type (field_declaration_list (_)?) @class.inside))) @class.around

(type_declaration
  (type_spec (type_identifier) (interface_type (method_elem)+ @class.inside))) @class.around

(type_parameter_list
  ((_) @parameter.inside . ","? @parameter.around) @parameter.around)

(parameter_list
  ((_) @parameter.inside . ","? @parameter.around) @parameter.around)

(argument_list
  ((_) @parameter.inside . ","? @parameter.around) @parameter.around)

(comment) @comment.inside

(comment)+ @comment.around

; Go test/benchmark/example/fuzz functions (all live in _test.go).
((function_declaration
   name: (identifier) @_name
   body: (block)? @test.inside) @test.around
 (#match? @_name "^(Test|Benchmark|Example|Fuzz)"))
