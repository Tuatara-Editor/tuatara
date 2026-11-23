; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The indentation rules for html: runtime/queries/html/indents.scm of
; Helix (https://github.com/helix-editor/helix) at commit 9bcdf4d79d14,
; unchanged below this notice.

; Indent the contents of an element; pull the end tag — and the close of a
; multi-line start/self-closing tag — back to the element's column. Mirrors the
; jsx rules. Void elements (`<br>`, `<img …>`) are single-line `element`s, so the
; tail-scoped indent is a no-op for them.
[
  (element)
  (script_element)
  (style_element)
] @indent
(end_tag) @outdent
(start_tag ">" @outdent)
(self_closing_tag "/>" @outdent)

; <script>/<style> bodies are embedded JS/CSS, handled by injection-aware indent
; (the engine indents them with the JS/CSS query). No @opaque needed.
