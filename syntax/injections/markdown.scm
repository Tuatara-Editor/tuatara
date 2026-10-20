; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The rules for text in other languages in markdown: runtime/queries/markdown/injections.scm of
; Helix (https://github.com/YardQuit/helix_editor) at commit 9bcdf4d79d14,
; unchanged below this notice.

; From nvim-treesitter/nvim-treesitter

(fenced_code_block
  (code_fence_content) @injection.shebang @injection.content
  (#set! injection.include-unnamed-children))

(fenced_code_block
  (info_string
    (language) @injection.language)
  (code_fence_content) @injection.content (#set! injection.include-unnamed-children))

((html_block) @injection.content
 (#set! injection.language "html")
 (#set! injection.include-unnamed-children)
 (#set! injection.combined))

((pipe_table_cell) @injection.content (#set! injection.language "markdown.inline") (#set! injection.include-unnamed-children))

((minus_metadata) @injection.content (#set! injection.language "yaml") (#set! injection.include-unnamed-children))
((plus_metadata) @injection.content (#set! injection.language "toml") (#set! injection.include-unnamed-children))

((inline) @injection.content (#set! injection.language "markdown.inline") (#set! injection.include-unnamed-children))
