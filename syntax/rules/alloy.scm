; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The colouring rules for alloy: runtime/queries/alloy/highlights.scm of
; Helix (https://github.com/YardQuit/helix_editor) at commit 9bcdf4d79d14,
; unchanged below this notice.

; Literals
; --------

(boolean) @constant.builtin.boolean
(comment) @comment
(string) @string
(number) @constant.numeric
(null) @constant.builtin

; Punctuation
; -----------

[
  "("
  ")"
  "["
  "]"
  "{"
  "}"
]  @punctuation.bracket

[
  "."
  ","
] @punctuation.delimiter

[
  "="
] @operator

; Function definitions
;---------------------

(function
  name: (identifier) @function)


(attribute (identifier) @variable.other.member)
(block (identifier) @type.builtin)
