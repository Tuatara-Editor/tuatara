; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The colouring rules for edoc: runtime/queries/edoc/highlights.scm of
; Helix (https://github.com/YardQuit/helix_editor) at commit 9bcdf4d79d14,
; unchanged below this notice.

((section
   (section_marker) @markup.heading.marker
   (section_content) @markup.heading.1
   (section_marker) @markup.heading.marker)
 (#eq? @markup.heading.marker "=="))

((section
   (section_marker) @markup.heading.marker
   (section_content) @markup.heading.2
   (section_marker) @markup.heading.marker)
 (#eq? @markup.heading.marker "==="))

((section
   (section_marker) @markup.heading.marker
   (section_content) @markup.heading.3
   (section_marker) @markup.heading.marker)
 (#eq? @markup.heading.marker "===="))

(tag) @keyword
(macro (tag) @function.macro)
(macro_escape) @constant.character.escape
(inline_quote) @markup.raw.inline
(email_address) @markup.link.url

(em_xhtml_tag
  (open_xhtml_tag) @tag
  (xhtml_tag_content) @markup.italic
  (close_xhtml_tag) @tag)

(strong_xhtml_tag
  (open_xhtml_tag) @tag
  (xhtml_tag_content) @markup.bold
  (close_xhtml_tag) @tag)

(module) @namespace
(function) @function
(type) @type

; could be @constant.numeric.integer but this looks similar to a capture
(arity) @operator

(expression [":" "/"] @operator)
(expression ["(" ")"] @punctuation.delimiter)
(macro ["{" "}"] @function.macro)

[
  (quote_marker)
  (language_identifier)
  (quote_content)
] @markup.raw.block

(parameter) @variable.parameter
