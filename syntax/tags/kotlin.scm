; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The rules naming the definitions and headings in kotlin: runtime/queries/kotlin/tags.scm of
; Helix (https://github.com/helix-editor/helix) at commit 9bcdf4d79d14,
; unchanged below this notice.

(class_declaration
  (type_identifier) @definition.class)

(object_declaration
  "object" (type_identifier) @definition.class)

(function_declaration
  (simple_identifier) @definition.function)

(property_declaration
  (variable_declaration
    (simple_identifier) @definition.constant))
