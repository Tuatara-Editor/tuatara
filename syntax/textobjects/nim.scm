; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The selection rules for nim: runtime/queries/nim/textobjects.scm of
; Helix (https://github.com/YardQuit/helix_editor) at commit 9bcdf4d79d14,
; unchanged below this notice.

(proc_declaration
  body: (_) @function.inside) @function.around
(func_declaration
  body: (_) @function.inside) @function.around
(iterator_declaration
  body: (_) @function.inside) @function.around
(converter_declaration
  body: (_) @function.inside) @function.around
(method_declaration
  body: (_) @function.inside) @function.around
(template_declaration
  body: (_) @function.inside) @function.around
(macro_declaration
  body: (_) @function.inside) @function.around

(type_declaration (_) @class.inside) @class.around

(parameter_declaration
  (symbol_declaration_list) @parameter.inside) @parameter.around

[
  (comment)
  (block_comment)
  (documentation_comment)
  (block_documentation_comment)
] @comment.inside

[
  (comment)+
  (block_comment)
  (documentation_comment)+
  (block_documentation_comment)+
] @comment.around
