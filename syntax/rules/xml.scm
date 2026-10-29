; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The colouring rules for xml: runtime/queries/xml/highlights.scm of
; Helix (https://github.com/helix-editor/helix) at commit 9bcdf4d79d14,
; unchanged below this notice.

(comment) @comment

[
    "DOCTYPE"
    "ELEMENT"
    "ATTLIST"
] @keyword

[
    "#REQUIRED"
    "#IMPLIED"
    "#FIXED"
    "#PCDATA"
] @keyword.directive

[
    "EMPTY"
    "ANY"
    "SYSTEM"
    "PUBLIC"
] @constant

(doctype) @variable
(element_name) @variable

"xml" @tag
(tag_name) @tag

[
    "encoding"
    "version"
    "standalone"
] @attribute
(attribute_name) @attribute

(system_literal) @string
(pubid_literal) @string
(attribute_value) @string

[
    "<" ">" "</" "/>" "<?" "?>" "<!"
] @punctuation.bracket
