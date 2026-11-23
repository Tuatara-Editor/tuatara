; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The colouring rules for fidl: runtime/queries/fidl/highlights.scm of
; Helix (https://github.com/helix-editor/helix) at commit 9bcdf4d79d14,
; unchanged below this notice.

[
  "ajar"
  "alias"
  "as"
  "bits"
  "closed"
  "compose"
  "const"
  "enum"
  "error"
  "flexible"
  "library"
  "open"
  ; "optional" we did not specify a node for optional yet
  "overlay"
  "protocol"
  "reserved"
  "resource"
  "service"
  "strict"
  "struct"
  "table"
  "type"
  "union"
  "using"
] @keyword

(primitives_type) @type.builtin

(builtin_complex_type) @type.builtin

(const_declaration
  (identifier) @constant)

[
  "="
  "|"
  "&"
  "->"
] @operator

(attribute
  "@" @attribute
  (identifier) @attribute)

(string_literal) @string

(numeric_literal) @constant.numeric

[
  (true)
  (false)
] @constant.builtin.boolean

(comment) @comment

[
  "("
  ")"
  "<"
  ">"
  "{"
  "}"
] @punctuation.bracket
