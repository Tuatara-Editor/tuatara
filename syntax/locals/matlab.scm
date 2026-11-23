; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The rules for local names in matlab: runtime/queries/matlab/locals.scm of
; Helix (https://github.com/helix-editor/helix) at commit 9bcdf4d79d14,
; unchanged below this notice.

(function_definition name: (identifier) @local.definition.function ?) @local.scope
(function_arguments (identifier)* @local.definition.variable.parameter)

(lambda (arguments (identifier) @local.definition.variable.parameter)) @local.scope

(identifier) @local.reference

; A call's function name and a field member name are not variable references;
; cancel resolution so a same-named local doesn't capture them.
(function_call
  name: (identifier) @_)
(field_expression
  field: (identifier) @_)
