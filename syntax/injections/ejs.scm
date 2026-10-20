; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The rules for text in other languages in ejs: runtime/queries/ejs/injections.scm of
; Helix (https://github.com/YardQuit/helix_editor) at commit 9bcdf4d79d14,
; unchanged below this notice.

((content) @injection.content
 (#set! injection.language "html")
 (#set! injection.combined))

((code) @injection.content
 (#set! injection.language "javascript")
 (#set! injection.combined))
