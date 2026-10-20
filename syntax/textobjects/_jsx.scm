; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The selection rules for _jsx: runtime/queries/_jsx/textobjects.scm of
; Helix (https://github.com/YardQuit/helix_editor) at commit 9bcdf4d79d14,
; unchanged below this notice.

; See runtime/queries/ecma/README.md for more info.

(jsx_self_closing_element) @xml-element.around @xml-element.inside

(jsx_element (jsx_opening_element) (_)* @xml-element.inside (jsx_closing_element))

(jsx_element) @xml-element.around
