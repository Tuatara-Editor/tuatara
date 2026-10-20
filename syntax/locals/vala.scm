; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The rules for local names in vala: runtime/queries/vala/locals.scm of
; Helix (https://github.com/YardQuit/helix_editor) at commit 9bcdf4d79d14,
; unchanged below this notice.

; Scopes

[
  (method_declaration)
  (creation_method_declaration)
  (constructor_declaration)
  (destructor_declaration)
  (local_function_declaration)
  (lambda_expression)
  (block)
  (for_statement)
  (foreach_statement)
] @local.scope

; Definitions

(parameter
  (identifier) @local.definition.variable.parameter)

(lambda_expression
  (identifier) @local.definition.variable.parameter)

; `foreach (Type x in ...)` — the identifier after the element type.
(foreach_statement
  .
  (type)
  .
  (identifier) @local.definition.variable)

; `Type x = ...;` — first identifier of each assignment in a local declaration.
(local_declaration
  (assignment
    .
    (identifier) @local.definition.variable))

; References

(identifier) @local.reference

; In `a.b`, `b` is a member access rather than a variable reference. A bare name
; is a single-child member_access_expression, so only discard the trailing
; identifier when it follows a receiver expression.
(member_access_expression
  (_)
  .
  (identifier) @_)
