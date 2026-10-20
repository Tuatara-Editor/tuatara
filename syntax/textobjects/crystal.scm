; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The selection rules for crystal: runtime/queries/crystal/textobjects.scm of
; Helix (https://github.com/YardQuit/helix_editor) at commit 9bcdf4d79d14,
; unchanged below this notice.

(class_def
  name: (_)
  (_) @class.inside) @class.around

(struct_def
  name: (_)
  (_) @class.inside) @class.around

(module_def
  name: (_)
  (_) @class.inside) @class.around

(lib_def
  name: (_)
  (_) @class.inside) @class.around

(enum_def
  name: (_)
  (_) @class.inside) @class.around

(block
  params: (_) @parameter.inside) @parameter.around

(method_def
  params: (_) @parameter.inside) @parameter.around

(method_def
  name: (_)
  (_) @function.inside) @function.around

(block
  (_) @function.inside) @function.around

(comment) @comment.inside
(comment)+ @comment.around

(array
  (_) @entry.around)
