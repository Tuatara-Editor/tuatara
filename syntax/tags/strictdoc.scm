; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The rules naming the definitions and headings in strictdoc: runtime/queries/strictdoc/tags.scm of
; Helix (https://github.com/YardQuit/helix_editor) at commit 9bcdf4d79d14,
; unchanged below this notice.

; StrictDoc symbols, mapped to recognized `@definition.*` kinds
; (see book/src/guides/tags.md). The previous query used hierarchical
; `definition.<area>.<field>` scopes that the symbol picker does not
; recognize, so it produced no symbols. @definition.* captures the whole
; item; @name captures the identifier shown in the picker.

; Document
(document_config
  uid: (uid_string) @name) @definition.module

; Sections
(section_body
  title: (single_line_string) @name) @definition.section

; Requirements / nodes
(sdoc_node_field_uid
  uid: (uid_string) @name) @definition.struct
(sdoc_composite_node_opening
  node_type_opening: (sdoc_composite_node_type_name) @name) @definition.struct

; Grammar field definitions
(grammar_field_title
  title: (field_name) @name) @definition.field
