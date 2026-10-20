; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The colouring rules for haskell-literate: runtime/queries/haskell-literate/highlights.scm of
; Helix (https://github.com/YardQuit/helix_editor) at commit 9bcdf4d79d14,
; unchanged below this notice.

; Bird track marker
(bird_line ">" @punctuation.special)

; LaTeX delimiters
(latex_begin) @keyword.directive
(latex_end) @keyword.directive

; Highlight LaTeX comments like comments
(latex_comment) @comment

; Markdown delimiters
(markdown_begin) @keyword.directive
(markdown_end) @keyword.directive

; Normal prose is not highlighted. Haskell code will be
; highlighted by the injected Haskell grammar
