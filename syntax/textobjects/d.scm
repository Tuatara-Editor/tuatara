; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The selection rules for d: runtime/queries/d/textobjects.scm of
; Helix (https://github.com/helix-editor/helix) at commit 9bcdf4d79d14,
; unchanged below this notice.

(function_declaration (function_body) @function.inside) @function.around
(comment) @comment.inside
(comment)+ @comment.around
(class_declaration (aggregate_body) @class.inside) @class.around
(interface_declaration (aggregate_body) @class.inside) @class.around
(struct_declaration (aggregate_body) @class.inside) @class.around
(unittest_declaration (block_statement) @test.inside) @test.around
(parameter) @parameter.inside
(template_parameter) @parameter.inside
