; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The rules for local names in d: runtime/queries/d/locals.scm of
; Helix (https://github.com/helix-editor/helix) at commit 9bcdf4d79d14,
; unchanged below this notice.

; Scopes

[
  (function_body)
  (function_literal)
  (block_statement)
  (foreach_statement)
  (for_statement)
] @local.scope

; Definitions

; In a `parameter`, the type is wrapped in a `(type)` node while the variable
; name is a direct `identifier` child, so this matches only the name.
(parameter
  (identifier) @local.definition.variable.parameter)

; `Type name = …;` — declarator's first child is the name; the initializer is
; nested under expression nodes, so anchor to the first child.
(variable_declaration
  (declarator
    . (identifier) @local.definition.variable))
; `auto name = …;`
(auto_declaration
  variable: (identifier) @local.definition.variable)

; `foreach (name; range)` — like parameters, the optional type is a `(type)`
; node and the loop variable is the trailing direct identifier.
(foreach_type
  (identifier) @local.definition.variable)

; `catch (Exception e)` — type is wrapped, the bound name is a direct identifier.
(catch_statement
  (identifier) @local.definition.variable)

; References

(identifier) @local.reference

; Discards: identifiers that look like references but aren't variables.

; `expr.member` — the trailing member name is not a local.
(property_expression
  (identifier) @_ .)
; Named argument `foo(name: value)`.
(named_argument
  (identifier) @_)
