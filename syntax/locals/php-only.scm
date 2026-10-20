; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The rules for local names in php-only: runtime/queries/php-only/locals.scm of
; Helix (https://github.com/YardQuit/helix_editor) at commit 9bcdf4d79d14,
; unchanged below this notice.

; Scopes

[
  (function_definition)
  (method_declaration)
  (anonymous_function)
  (arrow_function)
  (compound_statement)
] @local.scope

; Definitions

; PHP variables are `variable_name` ($foo); parameters share that node type so
; references match by text including the leading `$`.
(simple_parameter
  name: (variable_name) @local.definition.variable.parameter)
(variadic_parameter
  name: (variable_name) @local.definition.variable.parameter)
(property_promotion_parameter
  name: (variable_name) @local.definition.variable.parameter)

; References

(variable_name) @local.reference
