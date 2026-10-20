; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The colouring rules for godot-resource: runtime/queries/godot-resource/highlights.scm of
; Helix (https://github.com/YardQuit/helix_editor) at commit 9bcdf4d79d14,
; unchanged below this notice.

(section (identifier) @type.builtin)

(attribute (identifier) @attribute)
(property (path) @variable.other.member)
(constructor (identifier) @constructor)

(string) @string
(integer) @constant.numeric.integer
(float) @constant.numeric.float

(true) @constant.builtin.boolean
(false) @constant.builtin.boolean

[
  "["
  "]"
] @tag

[
  "("
  ")"
  "{"
  "}"
] @punctuation.bracket

"=" @operator

(comment) @comment
