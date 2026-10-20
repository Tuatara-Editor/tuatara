; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The colouring rules for wat: runtime/queries/wat/highlights.scm of
; Helix (https://github.com/YardQuit/helix_editor) at commit 9bcdf4d79d14,
; unchanged below this notice.

[
  "module" "func" "param" "result" "type" "memory" "elem" "data" "table" "global"
  "if" "then" "else" "block" "loop" "end" "mut"
] @keyword

["import" "export"] @keyword.control.import

["local"] @keyword.storage.type

[(name) (string)] @string

(identifier) @function

[(comment_block) (comment_line)] @comment

[(nat) (float) (align_offset_value)] @constant.numeric.integer

(value_type) @type

["(" ")"] @punctuation.bracket
