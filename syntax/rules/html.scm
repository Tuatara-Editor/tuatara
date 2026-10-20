; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The colouring rules for html: runtime/queries/html/highlights.scm of
; Helix (https://github.com/YardQuit/helix_editor) at commit 9bcdf4d79d14,
; unchanged below this notice.

(tag_name) @tag
(erroneous_end_tag_name) @error
(doctype) @constant
(attribute_name) @attribute
(entity) @string.special.symbol
(comment) @comment

((attribute
  (attribute_name) @attribute
  (quoted_attribute_value (attribute_value) @markup.link.url))
 (#any-of? @attribute "href" "src"))

((element
  (start_tag
    (tag_name) @tag)
  (text) @markup.link.label)
  (#eq? @tag "a"))

(attribute [(attribute_value) (quoted_attribute_value)] @string)

((element
  (start_tag
    (tag_name) @tag)
  (text) @markup.bold)
  (#any-of? @tag "strong" "b"))

((element
  (start_tag
    (tag_name) @tag)
  (text) @markup.italic)
  (#any-of? @tag "em" "i"))

((element
  (start_tag
    (tag_name) @tag)
  (text) @markup.strikethrough)
  (#any-of? @tag "s" "del"))

[
  "<"
  ">"
  "</"
  "/>"
  "<!"
] @punctuation.bracket

"=" @punctuation.delimiter
