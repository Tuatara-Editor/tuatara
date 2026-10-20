; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The rules for text in other languages in edoc: runtime/queries/edoc/injections.scm of
; Helix (https://github.com/YardQuit/helix_editor) at commit 9bcdf4d79d14,
; unchanged below this notice.

((xhtml_tag) @injection.content
 (#set! injection.combined)
 (#set! injection.include-children)
 (#set! injection.language "html"))

((block_quote
   !language
   (quote_content) @injection.content)
  (#set! injection.language "erlang"))

(block_quote
  language: (language_identifier) @injection.language
  (quote_content) @injection.content)

((macro
   (tag) @_tag
   (argument) @injection.content)
 (#eq? @_tag "@type")
 (#set! injection.language "erlang")
 (#set! injection.include-children))
