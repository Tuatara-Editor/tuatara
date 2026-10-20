; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The selection rules for penrose: runtime/queries/penrose/textobjects.scm of
; Helix (https://github.com/YardQuit/helix_editor) at commit 9bcdf4d79d14,
; unchanged below this notice.

; Functions/constructors/predicates in Domain
(constructor_decl) @function.around

(function_decl) @function.around

(predicate_decl) @function.around

; Parameters
(parameter_list
  ((_) @parameter.inside . ","? @parameter.around) @parameter.around)

; Arguments
(argument_list
  ((_) @parameter.inside . ","? @parameter.around) @parameter.around)

; Comments
(comment) @comment.inside
(comment) @comment.around
