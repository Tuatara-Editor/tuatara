; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The selection rules for wesl: runtime/queries/wesl/textobjects.scm of
; Helix (https://github.com/YardQuit/helix_editor) at commit 9bcdf4d79d14,
; unchanged below this notice.

(function_decl
  body: (_) @function.inside) @function.around

(struct_decl
  body: (_) @class.inside) @class.around

(param_list
  ((_) @parameter.inside . ","? @parameter.around) @parameter.around)

(argument_list
  ((_) @parameter.inside . ","? @parameter.around) @parameter.around)

(template_list
  ((_) @parameter.inside . ","? @parameter.around) @parameter.around)

[
  (line_comment)
  (block_comment)
] @comment.inside
  
(line_comment)+ @comment.around

(block_comment) @comment.around
