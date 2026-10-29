; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The rules for local names in capnp: runtime/queries/capnp/locals.scm of
; Helix (https://github.com/helix-editor/helix) at commit 9bcdf4d79d14,
; unchanged below this notice.

; Scopes

[
  (message)
  (annotation_targets)
  (const_list)
  (enum)
  (interface)
  (implicit_generics)
  (generics)
  (group)
  (method_parameters)
  (named_return_types)
  (struct)
  (struct_shorthand)
  (union)
] @local.scope

; References

[
  (extend_type)
  (field_type)
] @local.reference
(custom_type (type_identifier) @local.reference)
(custom_type
  (generics
    (generic_parameters 
      (generic_identifier) @local.reference)))

; Definitions

[
  (param_identifier)
  (return_identifier)
] @local.definition.variable.parameter
