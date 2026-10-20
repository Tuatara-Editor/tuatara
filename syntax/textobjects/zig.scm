; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The selection rules for zig: runtime/queries/zig/textobjects.scm of
; Helix (https://github.com/YardQuit/helix_editor) at commit 9bcdf4d79d14,
; unchanged below this notice.

(function_declaration
  body: (_) @function.inside) @function.around

(test_declaration (_) (block) @test.inside) @test.around

; matches all of: struct, enum, union
; this unfortunately cannot be split up because
; of the way struct "container" types are defined
(variable_declaration (identifier) (struct_declaration
    (_) @class.inside)) @class.around

(variable_declaration (identifier) (enum_declaration
    (_) @class.inside)) @class.around

(variable_declaration (identifier) (enum_declaration
    (_) @class.inside)) @class.around

(parameters
  ((_) @parameter.inside . ","? @parameter.around) @parameter.around)

(arguments
  ((_) @parameter.inside . ","? @parameter.around) @parameter.around)

(comment) @comment.inside
(comment)+ @comment.around
