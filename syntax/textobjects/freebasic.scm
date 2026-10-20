; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The selection rules for freebasic: runtime/queries/freebasic/textobjects.scm of
; Helix (https://github.com/YardQuit/helix_editor) at commit 9bcdf4d79d14,
; unchanged below this notice.

(function_declaration) @function.around

(sub_declaration) @function.around

(for_statement) @function.around

(comment) @comment.inside
(comment)+ @comment.around
