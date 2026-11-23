; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The rules naming the definitions and headings in godot-resource: runtime/queries/godot-resource/tags.scm of
; Helix (https://github.com/helix-editor/helix) at commit 9bcdf4d79d14,
; unchanged below this notice.

(section
  (identifier) @_type
  (attribute
    (identifier) @_attr_type
    (string) @definition.struct)
  (#eq? @_type "node")
  (#eq? @_attr_type "name"))

(section
  (identifier) @definition.struct
  (#not-eq? @definition.struct "node"))
