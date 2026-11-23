; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The selection rules for hare: runtime/queries/hare/textobjects.scm of
; Helix (https://github.com/helix-editor/helix) at commit 9bcdf4d79d14,
; unchanged below this notice.

(function_declaration) @function.around

(function_declaration
  body: (_) @function.inside)

(function_declaration
  (fndec_attrs
    (fndec_attr "@test"))) @test.around

(function_declaration
  (fndec_attrs
    (fndec_attr "@test"))
  body: (_) @test.inside)

(parameter) @parameter.around

(switch_case) @parameter.around

(comment) @comment.inside

(comment)+ @comment.around

(type_declaration) @class.around
