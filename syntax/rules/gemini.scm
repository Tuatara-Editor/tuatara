; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The colouring rules for gemini: runtime/queries/gemini/highlights.scm of
; Helix (https://github.com/YardQuit/helix_editor) at commit 9bcdf4d79d14,
; unchanged below this notice.

(link) @punctuation.bracket
(link 
  label: (text) @markup.link.label)
(link
  uri: (uri) @markup.link.url)

[
  (start_pre)
  (pre)
  (end_pre)
] @markup.raw.block
(start_pre
  alt: (text) @label)

(heading1
  (text) @markup.heading.1) @markup.heading.marker
(heading2
  (text) @markup.heading.2) @markup.heading.marker
(heading3
  (text) @markup.heading.3) @markup.heading.marker

(ulist
  (indicator) @markup.list.unnumbered) 
(quote
  (indicator) @markup.quote
  (text) @markup.italic)
