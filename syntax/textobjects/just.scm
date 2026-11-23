; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The selection rules for just: runtime/queries/just/textobjects.scm of
; Helix (https://github.com/helix-editor/helix) at commit 9bcdf4d79d14,
; unchanged below this notice.

; Specify how to navigate around logical blocks in code

(assert_parameters
  ((_) @parameter.inside . ","? @parameter.around)) @parameter.around

(recipe
  (recipe_body) @function.inside) @function.around

(recipe_parameters
  ((_) @parameter.inside . ","? @parameter.around)) @parameter.around

(recipe_dependency
  (_) @parameter.inside) @parameter.around

(function_call
  (function_parameters
    ((_) @parameter.inside . ","? @parameter.around)) @parameter.around) @function.around

(comment) @comment.around
