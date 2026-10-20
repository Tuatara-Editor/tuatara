; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The colouring rules for protobuf: runtime/queries/protobuf/highlights.scm of
; Helix (https://github.com/YardQuit/helix_editor) at commit 9bcdf4d79d14,
; unchanged below this notice.

[
  "syntax"
  "edition"
  "package"
  "option"
  "import"
  "service"
  "rpc"
  "returns"
  "message"
  "map"
  "enum"
  "oneof"
  "repeated"
  "optional"
  "required"
  "reserved"
  "to"
  "stream"
  "extend"
] @keyword

[
  (key_type)
  (type)
  (message_or_enum_type)
] @type.builtin

[
  (enum_name)
  (message_name)
  (service_name)
  (rpc_name)
] @type

[
  (field_name)
  (option_name)
] @variable.other.member
(enum_variant_name) @type.enum.variant

(full_ident) @namespace

(int_lit) @constant.numeric.integer
(float_lit) @constant.numeric.float
(bool) @constant.builtin.boolean
(string) @string

(block_lit) @constant

(comment) @comment

[
  "("
  ")"
  "["
  "]"
  "{"
  "}"
]  @punctuation.bracket

"=" @operator

";" @punctuation.delimiter
