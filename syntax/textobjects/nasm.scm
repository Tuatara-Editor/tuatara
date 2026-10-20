; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The selection rules for nasm: runtime/queries/nasm/textobjects.scm of
; Helix (https://github.com/YardQuit/helix_editor) at commit 9bcdf4d79d14,
; unchanged below this notice.

(preproc_multiline_macro
  body: (body) @function.inside) @function.around
(struc_declaration
  body: (struc_declaration_body) @class.inside) @class.around
(struc_instance
  body: (struc_instance_body) @class.inside) @class.around

(preproc_function_def_parameters
  (word) @parameter.inside)
(call_syntax_arguments
  (_) @parameter.inside)
(operand) @parameter.inside

(comment) @comment.inside
(comment)+ @comment.around
