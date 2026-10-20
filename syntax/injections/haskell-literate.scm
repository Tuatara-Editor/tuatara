; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The rules for text in other languages in haskell-literate: runtime/queries/haskell-literate/injections.scm of
; Helix (https://github.com/YardQuit/helix_editor) at commit 9bcdf4d79d14,
; unchanged below this notice.

; Inject Haskell parser into bird-style code lines
((bird_line
  (haskell_code) @injection.content)
 (#set! injection.language "haskell"))

; Inject Haskell parser into LaTeX code blocks
((latex_code_line
  (haskell_code) @injection.content)
 (#set! injection.language "haskell"))

; Inject Haskell parser into Markdown code blocks
((markdown_code_line
  (haskell_code) @injection.content)
 (#set! injection.language "haskell"))
