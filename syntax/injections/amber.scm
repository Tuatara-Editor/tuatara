; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The rules for text in other languages in amber: runtime/queries/amber/injections.scm of
; Helix (https://github.com/helix-editor/helix) at commit 9bcdf4d79d14,
; unchanged below this notice.

; Inject bash into command content
((command_content) @injection.content
 (#set! injection.language "bash"))

; Inject markdown into documentation comments (///)
((comment) @injection.content
 (#match? @injection.content "^///")
 (#set! injection.language "markdown")
 (#set! injection.combined))

; Regular comments (excluding doc comments)
((comment) @injection.content
 (#not-match? @injection.content "^///")
 (#set! injection.language "comment"))
