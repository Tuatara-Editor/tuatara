; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The selection rules for amber: runtime/queries/amber/textobjects.scm of
; Helix (https://github.com/helix-editor/helix) at commit 9bcdf4d79d14,
; unchanged below this notice.

; Functions - capture both definition and body
(function_definition
  body: (_) @function.inside) @function.around

; Function parameters in definitions
(function_parameter_list
  (function_parameter_list_item) @parameter.inside)

; Function call arguments
(parameter_list
  ((_) @parameter.inside . ","? @parameter.around) @parameter.around)

; Comments
(comment) @comment.inside
(comment)+ @comment.around

; Arrays
(array
  (_) @entry.around)

; Main Block looks like a function
(main_block
  (block) @function.inside) @function.around
