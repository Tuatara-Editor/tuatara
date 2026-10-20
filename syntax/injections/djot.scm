; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The rules for text in other languages in djot: runtime/queries/djot/injections.scm of
; Helix (https://github.com/YardQuit/helix_editor) at commit 9bcdf4d79d14,
; unchanged below this notice.

(comment (content) @injection.content
  (#set! injection.language "comment"))

(math (content) @injection.content
  (#set! injection.language "latex") (#set! injection.include-unnamed-children))

(code_block
  (language) @injection.language
  (code) @injection.content (#set! injection.include-unnamed-children))

(raw_block
  (raw_block_info
    (language) @injection.language)
  (content) @injection.content (#set! injection.include-unnamed-children))

(raw_inline
  (content) @injection.content (#set! injection.include-unnamed-children)
  (raw_inline_attribute
    (language) @injection.language))
