; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The rules for local names in erlang: runtime/queries/erlang/locals.scm of
; Helix (https://github.com/helix-editor/helix) at commit 9bcdf4d79d14,
; unchanged below this notice.

; Specs and Callbacks
(attribute
  (stab_clause
    pattern: (arguments (variable)? @local.definition.variable.parameter)
    ; If a spec uses a variable as the return type (and later a `when` clause to type it):
    body: (variable)? @local.definition.variable.parameter)) @local.scope

; parametric `-type`s
((attribute
    name: (atom) @_keyword
    (arguments
      (binary_operator
        left: (call (arguments (variable) @local.definition.variable.parameter))
        operator: "::") @local.scope))
 (#any-of? @_keyword "type" "opaque" "nominal"))

; `fun`s
(anonymous_function (stab_clause pattern: (arguments (variable) @local.definition.variable.parameter))) @local.scope

; Ordinary functions
((function_clause
   pattern: (arguments (variable) @local.definition.variable.parameter)) @local.scope
 (#not-match? @local.definition.variable.parameter "^_"))

(variable) @local.reference
(macro (variable)) @_
