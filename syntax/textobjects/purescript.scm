; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The selection rules for purescript: runtime/queries/purescript/textobjects.scm of
; Helix (https://github.com/helix-editor/helix) at commit 9bcdf4d79d14,
; unchanged below this notice.

(comment) @comment.inside

[
  (data)
  (type)
  (newtype)
] @class.around

((signature)? (function rhs:(_) @function.inside)) @function.around 
(exp_lambda) @function.around

(data (type_variable) @parameter.inside)
(patterns (_) @parameter.inside)
