; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The colouring rules for templ: runtime/queries/templ/highlights.scm of
; Helix (https://github.com/helix-editor/helix) at commit 9bcdf4d79d14,
; unchanged below this notice.

; inherits: go

(css_declaration
  name: (css_identifier) @function)
(script_declaration
  name: (script_identifier) @function)

(component_declaration
  name: (component_identifier) @function)

(tag_start) @tag
(tag_end) @tag
(self_closing_tag) @tag
(style_element) @tag

(attribute
  name: (attribute_name) @attribute)
(attribute
  value: (quoted_attribute_value) @string)

(element_text) @string.special
(style_element_text) @string.special

(css_property
  name: (css_property_name) @attribute)
(css_property
  value: (css_property_value) @constant)

(expression) @function.method
(dynamic_class_attribute_value) @function.method

(component_import
  name: (component_identifier) @function)

(component_render) @function

[
  "@"
] @operator

[
  "templ"
  "css"
  "type"
  "script"
] @keyword.storage.type

[
  (interpreted_string_literal)
  (raw_string_literal)
  (rune_literal)
] @string

; Comments

(comment) @comment

(element_comment) @comment
