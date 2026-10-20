; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The rules for local names in cairo: runtime/queries/cairo/locals.scm of
; Helix (https://github.com/YardQuit/helix_editor) at commit 9bcdf4d79d14,
; unchanged below this notice.

; Scopes

[
  (function_item)
  (struct_item)
  (enum_item)
  (type_item)
  (trait_item)
  (impl_item)
  (closure_expression)
  (block)
] @local.scope

; Definitions

(parameter
  (identifier) @local.definition.variable.parameter)

(type_parameters
  (type_identifier) @local.definition.type.parameter)
(constrained_type_parameter
  left: (type_identifier) @local.definition.type.parameter)

(closure_parameters (identifier) @local.definition.variable.parameter)

; References
(identifier) @local.reference
(type_identifier) @local.reference
