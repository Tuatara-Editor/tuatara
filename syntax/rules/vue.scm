; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The colouring rules for vue: runtime/queries/vue/highlights.scm of
; Helix (https://github.com/helix-editor/helix) at commit 9bcdf4d79d14,
; unchanged below this notice.

(tag_name) @tag
(end_tag) @tag

(directive_name) @keyword
(directive_argument) @constant

(attribute
  (attribute_name) @attribute
  [(attribute_value) (quoted_attribute_value)]? @string)
 
(directive_attribute
  (directive_name) @attribute
  (directive_argument)? @attribute
  (directive_modifiers)? @attribute
  [(attribute_value) (quoted_attribute_value)]? @string) 

(comment) @comment

[
  "<"
  ">"
  "</"
  "{{"
  "}}"
  "/>" 
] @punctuation.bracket
"=" @punctuation.delimiter

