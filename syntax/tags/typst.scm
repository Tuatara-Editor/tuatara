; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The rules naming the definitions and headings in typst: runtime/queries/typst/tags.scm of
; Helix (https://github.com/helix-editor/helix) at commit 9bcdf4d79d14,
; unchanged below this notice.

; should be a heading
(heading (text) @definition.section)

; should be a label/reference/tag
(heading (label) @definition.function)
(content (label) @definition.function)
