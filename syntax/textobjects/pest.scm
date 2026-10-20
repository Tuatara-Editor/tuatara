; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The selection rules for pest: runtime/queries/pest/textobjects.scm of
; Helix (https://github.com/YardQuit/helix_editor) at commit 9bcdf4d79d14,
; unchanged below this notice.

(grammar_rule (_) @class.inside) @class.around
(term (_) @entry.inside) @entry.around

(line_comment) @comment.inside
(line_comment)+ @comment.around

(block_comment) @comment.inside
(block_comment)+ @comment.around
