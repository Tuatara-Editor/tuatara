; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The colouring rules for smithy: runtime/queries/smithy/highlights.scm of
; Helix (https://github.com/helix-editor/helix) at commit 9bcdf4d79d14,
; unchanged below this notice.

; Queries are taken from: https://github.com/indoorvivants/tree-sitter-smithy/blob/main/queries/highlights.scm
; Preproc
(control_key) @keyword.directive

; Namespace
(namespace) @namespace

; Includes
[
  "use"
] @keyword.control.import

; Builtins
(primitive) @type.builtin
[
  "enum"
  "intEnum"
  "list"
  "map"
  "set"
] @type.builtin

; Fields (Members)
; (field) @variable.other.member

(key_identifier) @variable.other.member
(shape_member
  (field) @variable.other.member)
(operation_field) @variable.other.member
(operation_error_field) @variable.other.member

; Constants
(enum_member
  (enum_field) @type.enum)

; Types
(identifier) @type
(structure_resource
  (shape_id) @type)

; Attributes
(mixins
  (shape_id) @attribute)
(trait_statement
  (shape_id) @attribute)

; Operators
[
  "@"
  "-"
  "="
  ":="
] @operator

; Keywords
[
  "namespace"
  "service"
  "structure"
  "operation"
  "union"
  "resource"
  "metadata"
  "apply"
  "for"
  "with"
] @keyword

; Literals
(string) @string
(escape_sequence) @constant.character.escape

(number) @constant.numeric

(float) @constant.numeric.float

(boolean) @constant.builtin.boolean

(null) @constant.builtin

; Misc
[
  "$"
  "#"
] @punctuation.special

["{" "}"] @punctuation.bracket

["(" ")"] @punctuation.bracket

["[" "]"] @punctuation.bracket

[
  ":"
  "."
] @punctuation.delimiter

; Comments
[
  (comment)
  (documentation_comment)
] @comment
