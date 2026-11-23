; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The rules for local names in haxe: runtime/queries/haxe/locals.scm of
; Helix (https://github.com/helix-editor/helix) at commit 9bcdf4d79d14,
; unchanged below this notice.

; Scopes
[
 (block)
 (class_declaration)
 (interface_declaration)
 (function_declaration)
 (switch_expression)
] @local.scope

; Definitions
(function_arg name: (identifier) @local.definition.variable.parameter)
(variable_declaration name: (identifier) @local.definition.variable)

; References
(identifier) @local.reference

; The `member` side of `a.b` is field access, not a variable reference.
(member_expression
  member: (identifier) @_)
; A bare-identifier callee is a function/method name, not a variable reference
; (matches the `@function` baseline in highlights.scm).
(call_expression
  object: (identifier) @_)
; Type names are type references, not value references.
(type
  type_name: (identifier) @_)
(type
  built_in: (identifier) @_)
