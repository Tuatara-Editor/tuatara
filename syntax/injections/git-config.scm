; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The rules for text in other languages in git-config: runtime/queries/git-config/injections.scm of
; Helix (https://github.com/YardQuit/helix_editor) at commit 9bcdf4d79d14,
; unchanged below this notice.

((comment) @injection.content
 (#set! injection.language "comment"))

; https://git-scm.com/docs/gitattributes#_defining_a_custom_hunk_header
; https://git-scm.com/docs/gitattributes#_customizing_word_diff
; e.g.
; ```
; [diff "tex"]
; 	xfuncname = "^(\\\\(sub)*section\\{.*)$"
; 	wordRegex = "\\\\[a-zA-Z]+|[{}]|\\\\.|[^\\{}[:space:]]+"
; ```
(variable
 (name) @_var (#any-of? @_var "xfuncname" "wordRegex")
 value: (string) @injection.content
 (#set! injection.language "regex"))
