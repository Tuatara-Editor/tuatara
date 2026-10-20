; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The colouring rules for heex: runtime/queries/heex/highlights.scm of
; Helix (https://github.com/YardQuit/helix_editor) at commit 9bcdf4d79d14,
; unchanged below this notice.

; https://github.com/connorlay/tree-sitter-heex/blob/592e22292a367312c35e13de7fdb888f029981d6/queries/highlights.scm
; HEEx delimiters
[
  "<!"
  "<"
  "<%#"
  ">"
  "</"
  "/>"
  ; These could be `@keyword`s but the closing `>` wouldn't be highlighted
  ; as `@keyword`
  "<:"
  "</:"
] @punctuation.bracket

; Non-comment or tag delimiters
[
  "{"
  "}"
  "<%"
  "<%="
  "<%%="
  "%>"
] @keyword

; HEEx operators are highlighted as such
"=" @operator

; HEEx inherits the DOCTYPE tag from HTML
(doctype) @constant

; HEEx comments are highlighted as such
[
  "<!--"
  "-->"
  "<%!--"
  "--%>"
  (comment)
] @comment

; HEEx tags are highlighted as HTML
(tag_name) @tag

; HEEx slots are highlighted as atoms (symbols)
(slot_name) @string.special.symbol

; HEEx attributes are highlighted as HTML attributes
(attribute_name) @attribute
[
  (attribute_value)
  (quoted_attribute_value)
] @string

; HEEx special attributes are keywords
(special_attribute_name) @keyword

; HEEx components are highlighted as Elixir modules and functions
(component_name
  [
    (module) @namespace
    (function) @function
    "." @punctuation.delimiter
  ])
