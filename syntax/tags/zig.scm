; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The rules naming the definitions and headings in zig: runtime/queries/zig/tags.scm of
; Helix (https://github.com/YardQuit/helix_editor) at commit 9bcdf4d79d14,
; unchanged below this notice.

(function_declaration
  name: (identifier) @name) @definition.function

; `test "name" { ... }`
(test_declaration
  (string (string_content) @name)) @definition.function

; `const Name = struct/enum/union/opaque/error { ... }`
(variable_declaration
  (identifier) @name
  (struct_declaration)) @definition.struct
(variable_declaration
  (identifier) @name
  (enum_declaration)) @definition.enum
(variable_declaration
  (identifier) @name
  (union_declaration)) @definition.struct
(variable_declaration
  (identifier) @name
  (opaque_declaration)) @definition.struct
(variable_declaration
  (identifier) @name
  (error_set_declaration)) @definition.enum

(container_field
  name: (identifier) @name) @definition.field
