; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The selection rules for matlab: runtime/queries/matlab/textobjects.scm of
; Helix (https://github.com/helix-editor/helix) at commit 9bcdf4d79d14,
; unchanged below this notice.

(arguments ((_) @parameter.inside . ","? @parameter.around) @parameter.around)
(function_arguments ((_) @parameter.inside . ","? @parameter.around) @parameter.around)

(lambda expression: (_) @function.inside) @function.around
(function_definition (block) @function.inside) @function.around

(class_definition) @class.inside @class.around

(comment) @comment.inside @comment.around
