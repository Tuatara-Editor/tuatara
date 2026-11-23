; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The rules for local names in just: runtime/queries/just/locals.scm of
; Helix (https://github.com/helix-editor/helix) at commit 9bcdf4d79d14,
; unchanged below this notice.

; This file tells us about the scope of variables so e.g. local
; variables override global functions with the same name

; Scope

(recipe) @local.scope

; Definitions

(alias
  alias_name: (identifier) @local.definition.variable)

(assignment
  name: (identifier) @local.definition.variable)

(mod
  name: (identifier) @local.definition.namespace)

(recipe_parameter
  name: (identifier) @local.definition.variable.parameter)

(recipe
  name: (identifier) @local.definition.function)

; References

(alias
  name: (identifier) @local.reference)

(function_call
  name: (identifier) @local.reference)

(module_path
  name: (identifier) @local.reference)

(recipe_dependency
  name: (identifier) @local.reference)

(value
  (identifier) @local.reference)
