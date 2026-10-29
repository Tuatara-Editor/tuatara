; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The rules for local names in picat: runtime/queries/picat/locals.scm of
; Helix (https://github.com/helix-editor/helix) at commit 9bcdf4d79d14,
; unchanged below this notice.

; locals

[
  (predicate_definition)
  (function_definition)
  (actor_definition)
  (actor_definition)
] @local.scope

(import_declaration (_) @local.definition.namespace)

(module_declaration (_) @local.definition.namespace)

(binary_relational_expression left: (atom) @local.definition.variable)

(parameters
  [(variable) @local.definition.variable.parameter
   (atom) @local.definition.variable.parameter
   (array_expression [(variable) (atom)] @local.definition.variable.parameter)
   (list_expression [(variable) (atom)] @local.definition.variable.parameter)
   (as_pattern_expression left: [(variable) (atom)] @local.definition.variable.parameter)])

(arguments (argument [(variable) @local.reference (atom) @local.reference]))

[(variable) (atom)] @local.reference
