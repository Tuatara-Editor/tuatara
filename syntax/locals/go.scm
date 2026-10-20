; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The rules for local names in go: runtime/queries/go/locals.scm of
; Helix (https://github.com/YardQuit/helix_editor) at commit 9bcdf4d79d14,
; unchanged below this notice.

; Scopes

[
  (function_declaration)
  (method_declaration)
  (func_literal)
  (type_declaration)
  (block)
] @local.scope

; Definitions

(parameter_declaration (identifier) @local.definition.variable.parameter)
(variadic_parameter_declaration (identifier) @local.definition.variable.parameter)

(const_declaration
 (const_spec
  name: (identifier) @local.definition.constant))

(var_spec
 name: (identifier) @local.definition.variable)

(short_var_declaration
 left: (expression_list (identifier) @local.definition.variable))

(range_clause
 left: (expression_list (identifier) @local.definition.variable))

; Bound identifier of `switch v := x.(type)`.
(type_switch_statement
 alias: (expression_list (identifier) @local.definition.variable))

; References

(identifier) @local.reference

; Field names in struct literals are identifier rather than field_identifier,
; these cannot be locals.
(keyed_element . (literal_element (identifier) @_))
