; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The rules naming the definitions and headings in java: runtime/queries/java/tags.scm of
; Helix (https://github.com/helix-editor/helix) at commit 9bcdf4d79d14,
; unchanged below this notice.

(class_declaration
  name: (identifier) @definition.class)

(interface_declaration
  name: (identifier) @definition.interface)

(record_declaration
  name: (identifier) @definition.class)

(enum_declaration
  name: (identifier) @definition.class)

(method_declaration
  name: (identifier) @definition.function)

(constructor_declaration
  name: (identifier) @definition.function)

(compact_constructor_declaration
  name: (identifier) @definition.function)

(field_declaration
  declarator: (variable_declarator
    name: (identifier) @definition.constant))

(enum_constant
  name: (identifier) @definition.constant)
