; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The selection rules for gleam: runtime/queries/gleam/textobjects.scm of
; Helix (https://github.com/YardQuit/helix_editor) at commit 9bcdf4d79d14,
; unchanged below this notice.

(function
  parameters: (function_parameters (function_parameter)? @parameter.inside)
  body: (block) @function.inside) @function.around

(anonymous_function
  body: (block) @function.inside) @function.around

((function
   name: (identifier) @_name
   body: (block) @test.inside) @test.around
 (#match? @_name "_test$"))
