; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The colouring rules for wikitext: runtime/queries/wikitext/highlights.scm of
; Helix (https://github.com/YardQuit/helix_editor) at commit 9bcdf4d79d14,
; unchanged below this notice.

(heading1
  (heading_marker) @markup.heading.marker
  (text) @markup.heading.1
  (heading_marker) @markup.heading.marker
)
(heading2
  (heading_marker) @markup.heading.marker
  (text) @markup.heading.2
  (heading_marker) @markup.heading.marker
)
(heading3
  (heading_marker) @markup.heading.marker
  (text) @markup.heading.3
  (heading_marker) @markup.heading.marker
)
(heading4
  (heading_marker) @markup.heading.marker
  (text) @markup.heading.4
  (heading_marker) @markup.heading.marker
)
(heading5
  (heading_marker) @markup.heading.marker
  (text) @markup.heading.5
  (heading_marker) @markup.heading.marker
)
(heading6
  (heading_marker) @markup.heading.marker
  (text) @markup.heading.6
  (heading_marker) @markup.heading.marker
)

(wikilink
  (wikilink_page) @markup.link.url
  (page_name_segment)? @markup.link.label
)
(external_link
  (url) @markup.link.url
  (page_name_segment)? @markup.link.label
)

(template
  (template_name) @function
  (template_argument
  (template_param_name)? @attribute
  (template_param_value)? @string
  )
)

(comment) @comment

[
  "[["
  "]]"
  "{{"
  "}}"
  "{|"
  "|}"
  "["
  "]"
  "<"
  ">"
  "</"
] @punctuation.bracket

[
  "|"
  "|-"
  "|+"
  "!"
  "!!"
  "||"
] @punctuation.delimiter

(table_header_block
  (content) @markup.bold
)
(table_header_inline
  (content) @markup.bold
)

(html_tag_name) @tag
(html_attribute
  (html_attribute_name) @attribute
)
(html_attribute
  (html_attribute_name) @attribute
  (html_attribute_value) @string
)

(italic) @markup.italic
(bold) @markup.bold

