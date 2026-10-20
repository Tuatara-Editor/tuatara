; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The rules for local names in lua: runtime/queries/lua/locals.scm of
; Helix (https://github.com/YardQuit/helix_editor) at commit 9bcdf4d79d14,
; unchanged below this notice.

; Scopes

[
  (chunk)
  (function_declaration)
  (function_definition)
  (do_statement)
  (while_statement)
  (repeat_statement)
  (if_statement)
  (for_statement)
] @local.scope

; Definitions

(parameters
  (identifier) @local.definition.variable.parameter)

; `self` in a method is an implicit parameter.
(parameters
  (identifier) @local.definition.variable.builtin
  (#eq? @local.definition.variable.builtin "self"))

(for_numeric_clause
  name: (identifier) @local.definition.variable)

(for_generic_clause
  (variable_list
    (variable
      (identifier) @local.definition.variable)))

; `local x, y = ...`
(variable_declaration
  (assignment_statement
    (variable_list
      (variable
        (identifier) @local.definition.variable))))
(variable_declaration
  (variable_list
    (variable
      (identifier) @local.definition.variable)))

; References

(identifier) @local.reference

; Field/member names are not variable references.
(dot_index_expression
  field: (identifier) @_)
(method_index_expression
  method: (identifier) @_)
(field
  name: (identifier) @_)
