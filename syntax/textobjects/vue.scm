; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The selection rules for vue: runtime/queries/vue/textobjects.scm of
; Helix (https://github.com/helix-editor/helix) at commit 9bcdf4d79d14,
; unchanged below this notice.

; Vue SFC blocks and template elements as xml-element textobjects, mirroring
; html. <script>/<style> bodies are a single raw_text child; template elements
; can hold many children.
(script_element (start_tag) (_) @xml-element.inside (end_tag)) @xml-element.around

(style_element (start_tag) (_) @xml-element.inside (end_tag)) @xml-element.around

(template_element (start_tag) (_)* @xml-element.inside (end_tag)) @xml-element.around

(element (start_tag) (_)* @xml-element.inside (end_tag))

(element) @xml-element.around

(comment) @comment.around
