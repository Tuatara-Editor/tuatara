; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The rules for local names in openscad: runtime/queries/openscad/locals.scm of
; Helix (https://github.com/YardQuit/helix_editor) at commit 9bcdf4d79d14,
; unchanged below this notice.

; Scopes

[
  (function_item)
  (function_lit)
  (module_item)
  (let_expression)
  (let_block)
  (assign_block)
  (for_block)
  (intersection_for_block)
  (list_comprehension)
] @local.scope

; Definitions

; Parameters: a bare `identifier`, or `name = default` via an `assignment`.
(parameters
  (parameter
    (identifier) @local.definition.variable.parameter))
(parameters
  (parameter
    (assignment
      name: (identifier) @local.definition.variable.parameter)))

; let(x = ...) / assign(x = ...) / for(x = ...) bindings
(assignments
  (assignment
    name: (identifier) @local.definition.variable))

; References

(identifier) @local.reference

; Call targets are function/module names, not variable references.
(function_call
  name: (identifier) @_)
(module_call
  name: (identifier) @_)
