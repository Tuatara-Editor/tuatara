; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The colouring rules for eiffel: runtime/queries/eiffel/highlights.scm of
; Helix (https://github.com/YardQuit/helix_editor) at commit 9bcdf4d79d14,
; unchanged below this notice.

[
 "alias"
 "convert"
 "inherit"
 "redefine"
 "undefine"
 "rename"
 "select"
 "note"
 "create"
] @keyword.control.import

["export"] @keyword.control.import

[
 "do"
 "end"
 "once"
 "attribute"
] @keyword.control

[
 "class"
 "local"
] @keyword.storage.type

[
 "feature"
 "agent"
] @keyword.function

[
 "frozen"
 "deferred"
 "detachable"
 "expanded"
 "attached"
 "old"
 "like"
] @keyword.storage.modifier

(conditional ["if" "elseif" "end"] @keyword.control.conditional)
(else_part ["else"] @keyword.control.conditional)
(then_part ["then"] @keyword.control.conditional)

(conditional_expression ["if" "else" "elseif" "end"] @keyword.control.conditional)
(else_part_expression ["else"] @keyword.control.conditional)
(then_part_expression ["then"] @keyword.control.conditional)

(multi_branch "inspect" @keyword.control.conditional)
(when_part ["when" "then"] @keyword.control.conditional)

(multi_branch_expression "inspect" @keyword.control.conditional)
(when_part_expression ["when" "then"] @keyword.control.conditional)

(quantifier_loop ["∀" "∃" ":" "¦"] @keyword.control.repeat)
(quantifier_loop_body ["all" "some"] @keyword.control.repeat)
(iteration ["across" "as"] @keyword.control.repeat)
(initialization "from" @keyword.control.repeat)
(exit_condition "until" @keyword.control.repeat)
(loop_body "loop" @keyword.control.repeat)
(variant "variant" @keyword.control.repeat)
(loop (invariant "invariant" @keyword.control.repeat))
(loop ["⟳" ":" "¦" "⟲"]@keyword.control.repeat)
(loop "end" @keyword.control.repeat)

[
 "require"
 "ensure"
 "invariant"
 "check"
] @keyword.control.exception

["(" ")" "[" "]" "<<" ">>"] @punctuation.bracket
["," ":" ";"] @punctuation.delimiter

[
 (unary)
 ":="
 (binary_caret)
 (binary_mul_div)
 (binary_plus_minus)
 (binary_comparison)
 (binary_and)
 (binary_or)
 (binary_implies)
 (comparison)
 (unary_not)
] @operator

(result) @variable.builtin
(anchored (call (_) @variable))
[(verbatim_string) (basic_manifest_string)] @string
[(integer_constant) (real_constant)] @constant.numeric
(boolean_constant) @constant.builtin.boolean
(void) @constant.builtin
(current) @variable.builtin
(extended_feature_name (identifier) @function.method)

(iteration (identifier) @variable)
(quantifier_loop (identifier) @variable)
(entity_declaration_group (identifier) @variable)

(class_name) @type
(formal_generic) @type.parameter

(comment) @comment.line
(header_comment) @comment.line.documentation
