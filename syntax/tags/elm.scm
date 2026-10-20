; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The rules naming the definitions and headings in elm: runtime/queries/elm/tags.scm of
; Helix (https://github.com/YardQuit/helix_editor) at commit 9bcdf4d79d14,
; unchanged below this notice.

(value_declaration (function_declaration_left (lower_case_identifier) @name)) @definition.function

(function_call_expr (value_expr (value_qid) @name)) @reference.function
(exposed_value (lower_case_identifier) @name) @reference.function
(type_annotation ((lower_case_identifier) @name) (colon)) @reference.function

(type_declaration ((upper_case_identifier) @name) ) @definition.type

(type_ref (upper_case_qid (upper_case_identifier) @name)) @reference.type
(exposed_type (upper_case_identifier) @name) @reference.type

(type_declaration (union_variant (upper_case_identifier) @name)) @definition.enum

(value_expr (upper_case_qid (upper_case_identifier) @name)) @reference.union


(module_declaration 
    (upper_case_qid (upper_case_identifier)) @name
) @definition.module
