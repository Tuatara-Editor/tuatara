; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The rules naming the definitions and headings in unison: runtime/queries/unison/tags.scm of
; Helix (https://github.com/YardQuit/helix_editor) at commit 9bcdf4d79d14,
; unchanged below this notice.

(term_definition
  name: (regular_identifier) @name) @definition.function

(type_declaration
  (type_kw)
  (type_constructor
    ((type_name (regular_identifier) @name)) @definition.type))

(ability_declaration
  (ability_name) @name _) @definition.type
