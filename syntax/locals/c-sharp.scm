; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The rules for local names in c-sharp: runtime/queries/c-sharp/locals.scm of
; Helix (https://github.com/YardQuit/helix_editor) at commit 9bcdf4d79d14,
; unchanged below this notice.

; Scopes

[
  (method_declaration)
  (local_function_statement)
  (constructor_declaration)
  (destructor_declaration)
  (operator_declaration)
  (lambda_expression)
  (anonymous_method_expression)
  (block)
] @local.scope

; Definitions

(parameter
  name: (identifier) @local.definition.variable.parameter)

(variable_declarator
  name: (identifier) @local.definition.variable)

(foreach_statement
  left: (identifier) @local.definition.variable)

; References

(identifier) @local.reference

; Discards: identifiers that look like references but aren't variables.

; `obj.Member` — the member name is not a local.
(member_access_expression
  name: (identifier) @_)

; Named argument `f(name: value)`.
(argument
  name: (identifier) @_)
