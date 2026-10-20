; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The rules for local names in verilog: runtime/queries/verilog/locals.scm of
; Helix (https://github.com/YardQuit/helix_editor) at commit 9bcdf4d79d14,
; unchanged below this notice.

[
  (loop_generate_construct)
  (loop_statement)
  (conditional_statement)
  (case_item)
  (function_declaration)
  (always_construct)
  (module_declaration)
] @local.scope

(parameter_declaration
 (list_of_param_assignments
  (param_assignment
   (parameter_identifier
    (simple_identifier) @local.definition.variable.parameter))))

(local_parameter_declaration
 (list_of_param_assignments
  (param_assignment
   (parameter_identifier
    (simple_identifier) @local.definition.variable.parameter))))

;; TODO: fixme
;(function_declaration
 ;(function_identifier
  ;(simple_identifier) @local.definition.function))

(function_declaration
 (function_body_declaration
  (function_identifier
   (function_identifier
    (simple_identifier) @local.definition.function))))

(tf_port_item1
 (port_identifier
  (simple_identifier) @local.definition.variable.parameter))

; too broad, now includes types etc
(simple_identifier) @local.reference
