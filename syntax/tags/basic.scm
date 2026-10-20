; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The rules naming the definitions and headings in basic: runtime/queries/basic/tags.scm of
; Helix (https://github.com/YardQuit/helix_editor) at commit 9bcdf4d79d14,
; unchanged below this notice.

; Old line-numbered BASIC has no functions/types; tag the variables it
; introduces. There is no `variable` tag kind, so use the closest recognized
; one (`constant`)
(for_statement
  variable: (identifier) @name) @definition.constant

(let_statement
  variable: (identifier) @name) @definition.constant
