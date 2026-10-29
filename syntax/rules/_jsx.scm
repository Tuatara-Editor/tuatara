; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The colouring rules for _jsx: runtime/queries/_jsx/highlights.scm of
; Helix (https://github.com/helix-editor/helix) at commit 9bcdf4d79d14,
; unchanged below this notice.

; Punctuation
; -----------

; Handle attribute delimiter (<Component color="red"/>)
(jsx_attribute "=" @punctuation.delimiter)

; <Component>
(jsx_opening_element ["<" ">"] @punctuation.bracket)

; </Component>
(jsx_closing_element ["</" ">"] @punctuation.bracket)

; <Component />
(jsx_self_closing_element ["<" "/>"] @punctuation.bracket)

; Attributes
; ----------

(jsx_attribute (property_identifier) @attribute)

; Opening elements
; ----------------

(jsx_opening_element (identifier) @tag)

(jsx_opening_element ((identifier) @constructor
 (#match? @constructor "^[A-Z]")))

; Closing elements
; ----------------

(jsx_closing_element (identifier) @tag)

(jsx_closing_element ((identifier) @constructor
 (#match? @constructor "^[A-Z]")))

; Self-closing elements
; ---------------------

(jsx_self_closing_element (identifier) @tag)

(jsx_self_closing_element ((identifier) @constructor
 (#match? @constructor "^[A-Z]")))
