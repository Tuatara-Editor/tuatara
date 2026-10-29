; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The selection rules for erlang: runtime/queries/erlang/textobjects.scm of
; Helix (https://github.com/helix-editor/helix) at commit 9bcdf4d79d14,
; unchanged below this notice.

(function_clause
  pattern: (arguments (_)? @parameter.inside)
  body: (_) @function.inside) @function.around

(anonymous_function
  (stab_clause body: (_) @function.inside)) @function.around

(comment (comment_content) @comment.inside) @comment.around

; EUnit test names.
; (CommonTest cases are not recognizable by syntax alone.)
((function_clause
   name: (atom) @_name
   pattern: (arguments (_)? @parameter.inside)
   body: (_) @test.inside) @test.around
 (#match? @_name "_test$"))
