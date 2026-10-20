; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The indentation rules for concerto: runtime/queries/concerto/indents.scm of
; Helix (https://github.com/YardQuit/helix_editor) at commit 9bcdf4d79d14,
; unchanged below this notice.

; Concerto Language - Indent Queries (Helix)
; ============================================
; Helix-specific indentation rules. For use in helix-editor/helix at
; runtime/queries/concerto/indents.scm
;
; Helix uses @indent and @outdent captures, same as tree-sitter convention.
; See: https://docs.helix-editor.com/guides/indent.html

; Indent inside declaration bodies and decorator argument lists
[
  (class_body)
  (enum_body)
  (map_body)
  (decorator_arguments)
] @indent

; Outdent at closing braces and parentheses
[
  "}"
  ")"
] @outdent
