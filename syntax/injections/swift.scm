; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The rules for text in other languages in swift: runtime/queries/swift/injections.scm of
; Helix (https://github.com/YardQuit/helix_editor) at commit 9bcdf4d79d14,
; unchanged below this notice.

; Upstream: https://github.com/alex-pinkus/tree-sitter-swift/blob/57c1c6d6ffa1c44b330182d41717e6fe37430704/queries/injections.scm

; Parse regex syntax within regex literals

((regex_literal) @injection.content
 (#set! injection.language "regex"))

((comment) @injection.content
 (#set! injection.language "comment")
 (#set! injection.include-children))
