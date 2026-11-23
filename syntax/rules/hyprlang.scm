; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The colouring rules for hyprlang: runtime/queries/hyprlang/highlights.scm of
; Helix (https://github.com/helix-editor/helix) at commit 9bcdf4d79d14,
; unchanged below this notice.

(comment) @comment

[
  "source"
  "exec"
  "exec-once"
] @function.builtin

(keyword
  (name) @keyword)

(assignment
  (name) @variable.other.member)

(section
  (name) @namespace)

(section
  device: (device_name) @type)

(variable) @variable

"$" @punctuation.special

(boolean) @constant.builtin.boolean

(string) @string

(mod) @constant

[
  "rgb"
  "rgba"
] @function.builtin

[
  (number)
  (legacy_hex)
  (angle)
  (hex)
] @constant.numeric

"deg" @type

"," @punctuation.delimiter

[
  "("
  ")"
  "{"
  "}"
] @punctuation.bracket

[
  "="
  "-"
  "+"
] @operator
