; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The selection rules for wren: runtime/queries/wren/textobjects.scm of
; Helix (https://github.com/YardQuit/helix_editor) at commit 9bcdf4d79d14,
; unchanged below this notice.

(class_definition
  (class_body) @class.inside) @class.around

(call_expression
  (call_body
    (_) @function.inside) @function.around)

(method_definition
  body: (_) @function.inside) @function.around

(parameter_list
  ((_) @parameter.inside . ","? @parameter.around) @parameter.around)

(comment) @comment.inside

(comment)+ @comment.around
