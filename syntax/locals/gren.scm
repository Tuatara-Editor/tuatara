; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The rules for local names in gren: runtime/queries/gren/locals.scm of
; Helix (https://github.com/YardQuit/helix_editor) at commit 9bcdf4d79d14,
; unchanged below this notice.

(value_declaration) @local.scope
(type_alias_declaration) @local.scope
(type_declaration) @local.scope
(type_annotation) @local.scope
(port_annotation) @local.scope
(infix_declaration) @local.scope
(let_in_expr) @local.scope

(function_declaration_left (lower_pattern (lower_case_identifier)) @local.definition.variable.parameter)
(function_declaration_left (lower_case_identifier) @local.definition.function)

(value_expr(value_qid(upper_case_identifier)) @local.reference)
(value_expr(value_qid(lower_case_identifier)) @local.reference)
(type_ref (upper_case_qid) @local.reference)
