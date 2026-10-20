; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The rules for text in other languages in ripple: runtime/queries/ripple/injections.scm of
; Helix (https://github.com/YardQuit/helix_editor) at commit 9bcdf4d79d14,
; unchanged below this notice.

; Inject CSS into style elements
(style_element
  (raw_text) @injection.content
  (#set! injection.language "css"))

; Inject JavaScript/TypeScript into server blocks
; Note: statement is inlined, so we need to match specific statement types
; Commenting out for now as it requires matching all concrete statement types

; Template string interpolations
(template_substitution
  (expression) @injection.content
  (#set! injection.language "typescript"))
