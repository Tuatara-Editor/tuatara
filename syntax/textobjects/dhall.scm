; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The selection rules for dhall: runtime/queries/dhall/textobjects.scm of
; Helix (https://github.com/helix-editor/helix) at commit 9bcdf4d79d14,
; unchanged below this notice.

(lambda_expression
  (label) @parameter.inside
  (expression) @function.inside
) @function.around

(forall_expression
  (label) @parameter.inside
  (expression) @function.inside
) @function.around

(assert_expression
  (expression) @test.inside
) @test.around

[
  (block_comment_content)
  (line_comment_content)
] @comment.inside

[
  (block_comment)
  (line_comment)
] @comment.around
