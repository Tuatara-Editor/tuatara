; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The colouring rules for dtd: runtime/queries/dtd/highlights.scm of
; Helix (https://github.com/YardQuit/helix_editor) at commit 9bcdf4d79d14,
; unchanged below this notice.

; highlights.scm

(comment) @comment

[
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

(element_name) @module


(attribute_name) @attribute

(system_literal) @string
(pubid_literal) @string
(attribute_value) @string

[
    ">"
    "</"
    "<?"
    "?>"
    "<!"
] @punctuation.bracket
