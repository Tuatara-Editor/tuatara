; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The selection rules for wgsl: runtime/queries/wgsl/textobjects.scm of
; Helix (https://github.com/helix-editor/helix) at commit 9bcdf4d79d14,
; unchanged below this notice.

(function_declaration
  body: (_) @function.inside) @function.around

(struct_declaration) @class.around

[
  (struct_member)
  (parameter)
  (variable_declaration)
] @parameter.around

(comment) @comment.inside

(comment)+ @comment.around
