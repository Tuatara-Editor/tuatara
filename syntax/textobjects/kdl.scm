; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The selection rules for kdl: runtime/queries/kdl/textobjects.scm of
; Helix (https://github.com/helix-editor/helix) at commit 9bcdf4d79d14,
; unchanged below this notice.

(type (_) @test.inside) @test.around

(node
	children: (node_children)? @class.inside) @class.around

(node
	children: (node_children)? @function.inside) @function.around

(node (identifier) @function.movement)

[
	(single_line_comment)
	(multi_line_comment)
] @comment.inside

[
	(single_line_comment)+
	(multi_line_comment)+
] @comment.around

[
	(prop)
	(value)
] @parameter.inside

(value (type) ? (_) @parameter.inside @parameter.movement . ) @parameter.around

