; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The colouring rules for uxntal: runtime/queries/uxntal/highlights.scm of
; Helix (https://github.com/helix-editor/helix) at commit 9bcdf4d79d14,
; unchanged below this notice.

; highlights.scm

(identifier) @keyword
(number) @constant.numeric
(comment) @comment
(raw_character) @constant.character
(literal_hex) @constant.numeric.integer
(macro_definition) @function
(label_definition) @label
(sub_label_definition) @label
(relative_pad) @constant
(label) @label
(sub_label) @label
["[" "]" "{" "}"] @punctuation.bracket
