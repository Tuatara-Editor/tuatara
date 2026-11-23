; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The rules for text in other languages in markdown.inline: runtime/queries/markdown.inline/injections.scm of
; Helix (https://github.com/helix-editor/helix) at commit 9bcdf4d79d14,
; unchanged below this notice.


((html_tag) @injection.content 
  (#set! injection.language "html") 
  (#set! injection.include-unnamed-children)
  (#set! injection.combined))

((latex_block) @injection.content (#set! injection.language "latex") (#set! injection.include-unnamed-children))
