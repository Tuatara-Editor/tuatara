; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The rules for text in other languages in styx: runtime/queries/styx/injections.scm of
; Helix (https://github.com/YardQuit/helix_editor) at commit 9bcdf4d79d14,
; unchanged below this notice.

([(line_comment) (doc_comment)] @injection.content
 (#set! injection.language "comment"))

; Heredocs can specify a language: <<SQL,sql
; The heredoc_lang node captures the language name (e.g., "sql")
; The heredoc_content node contains the actual content to highlight
(heredoc
  (heredoc_lang) @injection.language
  (heredoc_content) @injection.content)
