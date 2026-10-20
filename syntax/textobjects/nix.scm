; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The selection rules for nix: runtime/queries/nix/textobjects.scm of
; Helix (https://github.com/YardQuit/helix_editor) at commit 9bcdf4d79d14,
; unchanged below this notice.

(comment) @comment.inside
(comment)+ @comment.around

(formals
  ((_) @parameter.inside . ","? @parameter.around) @parameter.around)

(function_expression
  body: (_) @function.inside) @function.around

(binding
  (_) @entry.inside) @entry.around

