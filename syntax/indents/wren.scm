; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The indentation rules for wren: runtime/queries/wren/indents.scm of
; Helix (https://github.com/YardQuit/helix_editor) at commit 9bcdf4d79d14,
; unchanged below this notice.

[
	(list)
	(map)
	(call_body)
	(parameter_list)
	(call_expression)
	(getter_definition)
	(setter_definition)
	(prefix_operator_definition)
	(subscript_operator_definition)
	(subscript_setter_definition)
	(infix_operator_definition)
	(method_definition)
	(constructor)
	(static_method_definition)
	(static_getter_definition)
	(attribute)
	(conditional)
	(class_body)
	(if_statement)
	(for_statement)
	(while_statement)
] @indent

[
	"}"
	"]"
	")"
] @outdent
