; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The selection rules for c: runtime/queries/c/textobjects.scm of
; Helix (https://github.com/YardQuit/helix_editor) at commit 9bcdf4d79d14,
; unchanged below this notice.

(function_definition
  body: (_) @function.inside) @function.around

(struct_specifier
  body: (_) @class.inside) @class.around

(enum_specifier
  body: (_) @class.inside) @class.around

(union_specifier
  body: (_) @class.inside) @class.around

(parameter_list 
  ((_) @parameter.inside . ","? @parameter.around) @parameter.around)

(argument_list
  ((_) @parameter.inside . ","? @parameter.around) @parameter.around)

(comment) @comment.inside

(comment)+ @comment.around

(enumerator
  (_) @entry.inside) @entry.around

(initializer_list
  (_) @entry.around)
