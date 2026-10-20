; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The colouring rules for xtc: runtime/queries/xtc/highlights.scm of
; Helix (https://github.com/YardQuit/helix_editor) at commit 9bcdf4d79d14,
; unchanged below this notice.

(parameter) @keyword

(change_port) @function.special

(template) @variable

[
  (hex_argument)
  (ipv4_argument)
] @attribute

(numeric_argument) @constant.numeric

(index) @tag

(string_literal_argument) @string

(string_argument) @constant.character

(comment) @comment

(port_comment) @label

[
("[") 
("]")
] @punctuation.bracket
