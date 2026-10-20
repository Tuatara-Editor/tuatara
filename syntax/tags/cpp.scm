; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The rules naming the definitions and headings in cpp: runtime/queries/cpp/tags.scm of
; Helix (https://github.com/YardQuit/helix_editor) at commit 9bcdf4d79d14,
; unchanged below this notice.

; inherits: c

(function_declarator
  declarator: (qualified_identifier name: (identifier) @definition.function))

(class_specifier
  name: (type_identifier) @definition.class
  body: (field_declaration_list))

(namespace_definition
  name: (namespace_identifier) @definition.module)

(concept_definition
  name: (identifier) @definition.interface)

(alias_declaration
  name: (type_identifier) @definition.type)
