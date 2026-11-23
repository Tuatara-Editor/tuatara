; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The indentation rules for ohm: runtime/queries/ohm/indents.scm of
; Helix (https://github.com/helix-editor/helix) at commit 9bcdf4d79d14,
; unchanged below this notice.

; See: https://docs.helix-editor.com/guides/indent.html

; indent
; ------

[
  ; <..., ...>
  (formals)
  (params)

  ; (...| ...)
  (alt)
] @indent

; outdent
; -------

[
  "}"
  ")"
  ">"
] @outdent

; align
; -----

; | ... | ...
(rule_body
  . (top_level_term) @anchor
  ) @align

; N/A or unused:
; --------------
; indent.always
; outdent.always
; extend
; extend.prevent-once
