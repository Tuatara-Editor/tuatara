; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The selection rules for rescript: runtime/queries/rescript/textobjects.scm of
; Helix (https://github.com/helix-editor/helix) at commit 9bcdf4d79d14,
; unchanged below this notice.

; Classes (modules)
;------------------

(module_binding definition: ((_) @class.inside)) @class.around

; Blocks
;-------

(block (_) @function.inside) @function.around

; Functions
;----------

(function body: (_) @function.inside) @function.around

; Calls
;------

(call_expression arguments: ((_) @parameter.inside)) @parameter.around

; Comments
;---------

(comment) @comment.inside
(comment)+ @comment.around

; Parameters
;-----------

(function parameter: (_) @parameter.inside @parameter.around)

(formal_parameters
  ","
  . (_) @parameter.inside
  @parameter.around)
(formal_parameters
  . (_) @parameter.inside
  . ","?
  @parameter.around)

(arguments
  "," @_arguments_start
  . (_) @parameter.inside
  @parameter.around)
(arguments
  . (_) @parameter.inside
  . ","?
  @parameter.around)

(function_type_parameters
  ","
  . (_) @parameter.inside
  @parameter.around)
(function_type_parameters
  . (_) @parameter.inside
  . ","?
  @parameter.around)

(functor_parameters
  ","
  . (_) @parameter.inside
  @parameter.around)
(functor_parameters
  . (_) @parameter.inside
  . ","?
  @parameter.around)

(type_parameters
  ","
  . (_) @parameter.inside
  @parameter.around)
(type_parameters
  . (_) @parameter.inside
  . ","?
  @parameter.around)

(type_arguments
  ","
  . (_) @parameter.inside
  @parameter.around)
(type_arguments
  . (_) @parameter.inside
  . ","?
  @parameter.around)

(decorator_arguments
  ","
  . (_) @parameter.inside
  @parameter.around)
(decorator_arguments
  . (_) @parameter.inside
  . ","?
  @parameter.around)

(variant_parameters
  ","
  . (_) @parameter.inside
  @parameter.around)
(variant_parameters
  . (_) @parameter.inside
  . ","?
  @parameter.around)

(polyvar_parameters
  ","
  . (_) @parameter.inside
  @parameter.around)
(polyvar_parameters
  . (_) @parameter.inside
  . ","?
  @parameter.around)

