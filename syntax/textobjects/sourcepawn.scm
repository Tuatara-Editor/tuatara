; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The selection rules for sourcepawn: runtime/queries/sourcepawn/textobjects.scm of
; Helix (https://github.com/helix-editor/helix) at commit 9bcdf4d79d14,
; unchanged below this notice.

(function_definition
  body: (_) @function.inside) @function.around

(alias_declaration
  body: (_) @function.inside) @function.around

(enum_struct_method
  body: (_) @function.inside) @function.around

(methodmap_method
  body: (_) @function.inside) @function.around

(methodmap_method_constructor
  body: (_) @function.inside) @function.around

(methodmap_method_destructor
  body: (_) @function.inside) @function.around

(methodmap_property_method
  body: (_) @function.inside) @function.around

(enum_struct) @class.around

(methodmap) @class.around

(parameter_declarations 
  ((parameter_declaration) @parameter.inside . ","? @parameter.around) @parameter.around)

(comment) @comment.around
