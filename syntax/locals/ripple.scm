; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The rules for local names in ripple: runtime/queries/ripple/locals.scm of
; Helix (https://github.com/helix-editor/helix) at commit 9bcdf4d79d14,
; unchanged below this notice.

; Scopes
[
  (statement_block)
  (function_declaration)
  (arrow_function)
  (function_expression)
  (component_declaration)
  (fragment_declaration)
  (class_declaration)
  (for_statement)
  (for_of_statement)
  (for_in_statement)
  (while_statement)
  (catch_clause)
] @local.scope

; Definitions
(component_declaration
  name: (identifier) @local.definition.function)

(fragment_declaration
  name: (identifier) @local.definition.function)

(function_declaration
  name: (identifier) @local.definition.function)

(class_declaration
  name: (identifier) @local.definition.type)

(method_definition
  name: (property_name) @local.definition.function.method)

(variable_declarator
  name: (identifier) @local.definition.variable)

(required_parameter
  pattern: (identifier) @local.definition.variable.parameter)

(rest_parameter
  (identifier) @local.definition.variable.parameter)

; References
(identifier) @local.reference

; Imports
(import_specifier
  name: (identifier) @local.definition.namespace)

(namespace_import
  (identifier) @local.definition.namespace)

; Exports
(export_specifier
  name: (identifier) @local.definition.namespace)
