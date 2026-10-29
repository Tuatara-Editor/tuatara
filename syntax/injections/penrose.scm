; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The rules for text in other languages in penrose: runtime/queries/penrose/injections.scm of
; Helix (https://github.com/helix-editor/helix) at commit 9bcdf4d79d14,
; unchanged below this notice.

; TeX/LaTeX in Substance/Style labels
((latex) @injection.content
 (#set! injection.language "latex"))

; String-based labels are often TeX as well
((label_stmt (string) @injection.content)
 (#set! injection.language "latex"))
