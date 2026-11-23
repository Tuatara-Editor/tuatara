; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The selection rules for awk: runtime/queries/awk/textobjects.scm of
; Helix (https://github.com/helix-editor/helix) at commit 9bcdf4d79d14,
; unchanged below this notice.


(func_def name: (identifier) (block) @function.inside) @function.around

(param_list (_) @parameter.inside) @parameter.around

(args (_) @parameter.inside) @parameter.around

(comment) @comment.inside

(comment)+ @comment.around
