; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The indentation rules for vue: runtime/queries/vue/indents.scm of
; Helix (https://github.com/YardQuit/helix_editor) at commit 9bcdf4d79d14,
; unchanged below this notice.

; Vue single-file components are HTML-like: indent the contents of <template>
; and of nested elements, and pull the end tag — and the close of a multi-line
; start / self-closing tag — back to the element's column. Mirrors the html/jsx
; rules.
;
; <script> and <style> bodies are deliberately *not* indented: Vue convention
; (and Prettier's default) keeps them at column 0, and their embedded JS/CSS is
; indented by the injected language's own indent query.
[
  (element)
  (template_element)
] @indent

(end_tag) @outdent
(start_tag ">" @outdent)
(self_closing_tag "/>" @outdent)
