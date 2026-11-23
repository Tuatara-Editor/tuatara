; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The rules for local names in teal: runtime/queries/teal/locals.scm of
; Helix (https://github.com/helix-editor/helix) at commit 9bcdf4d79d14,
; unchanged below this notice.


(var_declaration
  declarators: (var_declarators
  (var (identifier)) @local.definition.variable))

(var_assignment
  variables: (assignment_variables
    (var (identifier) @local.definition.variable)))

(arg name: (identifier) @local.definition.variable.parameter)

(anon_function) @local.scope
((function_statement
  (function_name) @local.definition.function) @local.scope)

(program) @local.scope
(if_statement) @local.scope
(generic_for_statement (for_body) @local.scope)
(numeric_for_statement (for_body) @local.scope)
(repeat_statement) @local.scope
(while_statement (while_body) @local.scope)
(do_statement) @local.scope

(identifier) @local.reference

