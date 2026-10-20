; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The selection rules for vala: runtime/queries/vala/textobjects.scm of
; Helix (https://github.com/YardQuit/helix_editor) at commit 9bcdf4d79d14,
; unchanged below this notice.

(method_declaration
  (block) @function.inside) @function.around

(creation_method_declaration
  (block) @function.inside) @function.around

(method_declaration
  ((parameter) @parameter.inside . ","? @parameter.around) @parameter.around)

[
  (class_declaration)
  (struct_declaration)
  (interface_declaration)
] @class.around

(type_arguments
  ((_) @parameter.inside . ","? @parameter.around) @parameter.around)

(creation_method_declaration
  ((parameter) @parameter.inside . ","? @parameter.around) @parameter.around)

(method_call_expression
  ((argument) @parameter.inside . ","? @parameter.around) @parameter.around)

(comment) @comment.inside

(comment)+ @comment.around
