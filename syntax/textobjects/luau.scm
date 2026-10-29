; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The selection rules for luau: runtime/queries/luau/textobjects.scm of
; Helix (https://github.com/helix-editor/helix) at commit 9bcdf4d79d14,
; unchanged below this notice.

(fn_stmt
  body: (_)? @function.inside) @function.around

(local_fn_stmt
  body: (_)? @function.inside) @function.around

(anon_fn
  body: (_)? @function.inside) @function.around

(param
  ((name) @parameter.inside . ","? @parameter.around) @parameter.around)

(arglist
  ((_) @parameter.inside . ","? @parameter.around) @parameter.around)

(comment) @comment.inside

(comment)+ @comment.around
