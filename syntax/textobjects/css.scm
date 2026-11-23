; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The selection rules for css: runtime/queries/css/textobjects.scm of
; Helix (https://github.com/helix-editor/helix) at commit 9bcdf4d79d14,
; unchanged below this notice.

; Rule sets and their declaration blocks
(rule_set) @function.around
(rule_set
  (block) @function.inside)

(keyframe_block_list) @function.around
(keyframe_block
  (block) @function.inside)

; At-rules with a body (e.g. @media, @supports)
(media_statement) @class.around
(media_statement
  (block) @class.inside)

; Declarations as key/value entries
(declaration) @entry.around
(declaration
  ":"
  (_) @entry.inside)

(comment) @comment.inside
(comment)+ @comment.around
