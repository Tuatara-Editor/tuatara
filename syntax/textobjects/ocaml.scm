; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The selection rules for ocaml: runtime/queries/ocaml/textobjects.scm of
; Helix (https://github.com/helix-editor/helix) at commit 9bcdf4d79d14,
; unchanged below this notice.

; Value/function definitions
(value_definition) @function.around
(value_definition
  (let_binding
    body: (_) @function.inside))

(fun_expression
  body: (_) @function.inside) @function.around

(function_expression) @function.around

(method_definition
  body: (_) @function.inside) @function.around

; Types and modules
(type_definition) @class.around
(type_definition
  (type_binding
    body: (_) @class.inside))

(module_definition) @class.around
(module_definition
  (module_binding
    body: (_) @class.inside))

; Parameters
(parameter) @parameter.around
(parameter
  pattern: (_) @parameter.inside)

(comment) @comment.inside
(comment)+ @comment.around
