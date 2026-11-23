; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The rules for local names in cpp: runtime/queries/cpp/locals.scm of
; Helix (https://github.com/helix-editor/helix) at commit 9bcdf4d79d14,
; unchanged below this notice.

; inherits: c

; C++-specific scopes on top of c's function_definition / declaration scopes.
[
  (lambda_expression)
  (namespace_definition)
  (class_specifier)
  (for_range_loop)
] @local.scope

; C++-only parameter forms (c only has parameter_declaration).
(optional_parameter_declaration
  declarator: (identifier) @local.definition.variable.parameter)
(variadic_parameter_declaration
  declarator: (variadic_declarator (identifier) @local.definition.variable.parameter))

; Template type parameters.
(type_parameter_declaration
  (type_identifier) @local.definition.type)
