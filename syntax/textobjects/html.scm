; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The selection rules for html: runtime/queries/html/textobjects.scm of
; Helix (https://github.com/YardQuit/helix_editor) at commit 9bcdf4d79d14,
; unchanged below this notice.

(script_element (start_tag) (_) @xml-element.inside (end_tag))  @xml-element.around

(style_element (start_tag) (_) @xml-element.inside (end_tag)) @xml-element.around

(element (start_tag) (_)* @xml-element.inside (end_tag))

(element) @xml-element.around

(comment) @comment.around
