; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The selection rules for c-sharp: runtime/queries/c-sharp/textobjects.scm of
; Helix (https://github.com/helix-editor/helix) at commit 9bcdf4d79d14,
; unchanged below this notice.

[
  (class_declaration body: (_) @class.inside)
  (struct_declaration body: (_) @class.inside)
  (interface_declaration body: (_) @class.inside)
  (enum_declaration body: (_) @class.inside)
  (delegate_declaration)
  (record_declaration body: (_) @class.inside)
] @class.around

(constructor_declaration body: (_) @function.inside) @function.around

(destructor_declaration body: (_) @function.inside) @function.around

(method_declaration body: (_) @function.inside) @function.around

(property_declaration (_) @function.inside) @function.around

(parameter (_) @parameter.inside) @parameter.around

(comment)+ @comment.around
