; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The selection rules for unison: runtime/queries/unison/textobjects.scm of
; Helix (https://github.com/YardQuit/helix_editor) at commit 9bcdf4d79d14,
; unchanged below this notice.

(term_declaration) @function.around

(type_declaration) @class.inside
(record) @class.inside

(comment) @comment.inside
(comment)+ @comment.around

(doc_block) @comment.around

(literal_list) @entry.around

(parenthesized_or_tuple_pattern) @entry.around

(pattern) @entry.around
