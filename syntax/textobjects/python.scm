; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The selection rules for python: runtime/queries/python/textobjects.scm of
; Helix (https://github.com/helix-editor/helix) at commit 9bcdf4d79d14,
; unchanged below this notice.

(function_definition
  body: (block)? @function.inside) @function.around

(class_definition
  body: (block)? @class.inside) @class.around

(parameters
  ((_) @parameter.inside . ","? @parameter.around) @parameter.around)
  
(lambda_parameters
  ((_) @parameter.inside . ","? @parameter.around) @parameter.around)

(argument_list
  ((_) @parameter.inside . ","? @parameter.around) @parameter.around)

(comment) @comment.inside

(comment)+ @comment.around

((function_definition
   name: (identifier) @_name
   body: (block)? @test.inside) @test.around
 (#match? @_name "^test_"))

(list
  (_) @entry.around)

(tuple
  (_) @entry.around)

(set
  (_) @entry.around)

(pair
  (_) @entry.inside) @entry.around
