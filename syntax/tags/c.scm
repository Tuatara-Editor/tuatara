; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The rules naming the definitions and headings in c: runtime/queries/c/tags.scm of
; Helix (https://github.com/helix-editor/helix) at commit 9bcdf4d79d14,
; unchanged below this notice.

(function_declarator
  declarator: [(identifier) (field_identifier)] @definition.function)

(preproc_function_def name: (identifier) @definition.function)

(preproc_def name: (identifier) @definition.constant)

(type_definition
  declarator: (type_identifier) @definition.type)

(struct_specifier
  name: (type_identifier) @definition.struct)

(enum_specifier
  name: (type_identifier) @definition.enum)

(union_specifier
  name: (type_identifier) @definition.struct)
