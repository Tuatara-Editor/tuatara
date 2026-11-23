; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The rules for local names in sourcepawn: runtime/queries/sourcepawn/locals.scm of
; Helix (https://github.com/helix-editor/helix) at commit 9bcdf4d79d14,
; unchanged below this notice.

; Scopes

[
  (function_definition)
  (function_declaration)
  (block)
] @local.scope

; Definitions

(parameter_declaration
  name: (identifier) @local.definition.variable.parameter)

(variable_declaration
  name: (identifier) @local.definition.variable)
(old_variable_declaration
  name: (identifier) @local.definition.variable)

; References

(identifier) @local.reference

; Member/field names are not variable references.
(field_access
  field: (identifier) @_)
(scope_access
  field: (identifier) @_)

; Function names in call position are not variable references.
(call_expression
  function: (identifier) @_)

; Named argument labels are not variable references.
(named_arg
  arg_name: (identifier) @_)
