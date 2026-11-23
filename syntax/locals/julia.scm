; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The rules for local names in julia: runtime/queries/julia/locals.scm of
; Helix (https://github.com/helix-editor/helix) at commit 9bcdf4d79d14,
; unchanged below this notice.

; -----------
; Definitions
; -----------

; Constants
(const_statement
  (assignment
    . (identifier) @local.definition.constant))

; Parameters (now in the signature's argument_list)
(argument_list
  (identifier) @local.definition.variable.parameter)

(argument_list
  (assignment
    . (identifier) @local.definition.variable.parameter))

(argument_list
  (splat_expression
    (identifier) @local.definition.variable.parameter))

(argument_list
  (typed_expression
    . (identifier) @local.definition.variable.parameter))

; Single parameter arrow function
(arrow_function_expression
  .
  (identifier) @local.definition.variable.parameter)

; ----------
; References
; ----------

(identifier) @local.reference

; A call's function name, field member access, and keyword-argument names are
; not variable references; cancel resolution so they keep their highlights.scm
; class even when a same-named local is in scope.
(call_expression
  . (identifier) @_)
(field_expression
  (_)
  (identifier) @_)
(call_expression
  (argument_list
    (assignment
      . (identifier) @_)))

; ------
; Scopes
; ------

[
  (for_statement)
  (while_statement)
  (try_statement)
  (catch_clause)
  (finally_clause)
  (let_statement)
  (quote_statement)
  (do_clause)
  (function_definition)
  (macro_definition)
] @local.scope

