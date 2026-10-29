; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The colouring rules for adl: runtime/queries/adl/highlights.scm of
; Helix (https://github.com/helix-editor/helix) at commit 9bcdf4d79d14,
; unchanged below this notice.

; adl

[
"module"
"struct"
"union"
"type"
"newtype"
"annotation"
] @keyword

(adl (scoped_name)) @namespace
(comment) @comment
(doc_comment) @comment.block.documentation
(name) @type

(fname) @variable.other.member

(type_expr (scoped_name) @type)

(type_expr_params (param (scoped_name) @type.parameter))

; json
(key) @string.special

(string) @string

(number) @constant.numeric

[
  (true)
  (false)
] @constant.builtin.boolean
(null) @constant.builtin

(escape_sequence) @constant.character.escape

