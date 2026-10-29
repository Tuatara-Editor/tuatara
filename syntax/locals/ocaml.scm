; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The rules for local names in ocaml: runtime/queries/ocaml/locals.scm of
; Helix (https://github.com/helix-editor/helix) at commit 9bcdf4d79d14,
; unchanged below this notice.

; Scopes
;-------

[
  (let_binding)
  (class_binding)
  (class_function)
  (method_definition)
  (fun_expression)
  (object_expression)
  (for_expression)
  (match_case)
  (attribute_payload)
] @local.scope

; Definitions
;------------

(value_pattern) @local.definition.variable.parameter

; References
;-----------

(value_path . (value_name) @local.reference)
