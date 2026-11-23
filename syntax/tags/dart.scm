; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The rules naming the definitions and headings in dart: runtime/queries/dart/tags.scm of
; Helix (https://github.com/helix-editor/helix) at commit 9bcdf4d79d14,
; unchanged below this notice.

(class_definition
  name: (identifier) @name) @definition.class
(enum_declaration
  name: (identifier) @name) @definition.enum
(mixin_declaration
  (identifier) @name) @definition.interface
(extension_declaration
  name: (identifier) @name) @definition.class
(function_signature
  name: (identifier) @name) @definition.function
(constructor_signature
  name: (identifier) @name) @definition.function
(type_alias
  "typedef" . (type_identifier) @name) @definition.type
