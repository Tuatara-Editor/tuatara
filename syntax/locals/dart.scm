; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The rules for local names in dart: runtime/queries/dart/locals.scm of
; Helix (https://github.com/helix-editor/helix) at commit 9bcdf4d79d14,
; unchanged below this notice.

; Scopes
;-------

[
 (function_body)
 (function_expression_body)
 (block)
 (for_statement)
 (try_statement)
 (catch_clause)
 (finally_clause)
] @local.scope

; Definitions
;------------

(formal_parameter
 name: (identifier) @local.definition.variable.parameter)

; for-in / C-style loop variable.
(for_loop_parts
 name: (identifier) @local.definition.variable)

(initialized_variable_definition
 name: (identifier) @local.definition.variable)

; References
;------------

(identifier) @local.reference

; Member access selectors carry plain identifiers that are not local references.
(unconditional_assignable_selector
 (identifier) @_)
(conditional_assignable_selector
 (identifier) @_)
