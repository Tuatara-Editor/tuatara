; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The selection rules for gdscript: runtime/queries/gdscript/textobjects.scm of
; Helix (https://github.com/helix-editor/helix) at commit 9bcdf4d79d14,
; unchanged below this notice.


(class_definition
  (body) @class.inside) @class.around

(function_definition
  (body) @function.inside) @function.around

(lambda (body) @function.inside) @function.around

(parameters 
  [
    (identifier)
    (typed_parameter)
    (default_parameter)    
    (typed_default_parameter)  
  ] @parameter.inside @parameter.around)

(arguments (_expression) @parameter.inside @parameter.around)

[
  (const_statement)
  (variable_statement)
  (pair)
  (enumerator)
] @entry.around

(comment) @comment.inside
(comment)+ @comment.around
