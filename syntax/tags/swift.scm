; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The rules naming the definitions and headings in swift: runtime/queries/swift/tags.scm of
; Helix (https://github.com/helix-editor/helix) at commit 9bcdf4d79d14,
; unchanged below this notice.

(class_declaration "class" name: (type_identifier) @name) @definition.class
(class_declaration "struct" name: (type_identifier) @name) @definition.struct
(class_declaration "enum" name: (type_identifier) @name) @definition.enum
(class_declaration "extension" name: (user_type (type_identifier) @name)) @definition.class
(protocol_declaration name: (type_identifier) @name) @definition.interface
(typealias_declaration name: (type_identifier) @name) @definition.type
(function_declaration name: (simple_identifier) @name) @definition.function
(protocol_function_declaration name: (simple_identifier) @name) @definition.function
(property_declaration
  name: (pattern bound_identifier: (simple_identifier) @name)) @definition.constant
