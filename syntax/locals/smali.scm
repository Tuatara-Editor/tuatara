; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The rules for local names in smali: runtime/queries/smali/locals.scm of
; Helix (https://github.com/helix-editor/helix) at commit 9bcdf4d79d14,
; unchanged below this notice.

[
  (class_directive)
  (expression)
  (annotation_directive)
  (array_data_directive)
  (method_definition)
  (packed_switch_directive)
  (sparse_switch_directive)
  (subannotation_directive)
] @local.scope

[
  (identifier)
  (class_identifier)
  (label)
  (jmp_label)
] @local.reference

(method_definition
  (method_signature (method_identifier) @local.definition.function.method))

(param_identifier) @local.definition.variable.parameter
