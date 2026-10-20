; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The rules for text in other languages in crystal: runtime/queries/crystal/injections.scm of
; Helix (https://github.com/YardQuit/helix_editor) at commit 9bcdf4d79d14,
; unchanged below this notice.

((heredoc_body
  (literal_content) @injection.content
  (heredoc_end) @name
  (#set! injection.language "sql"))
  (#eq? @name "SQL"))

((heredoc_body
  (literal_content) @injection.content
  (heredoc_end) @name
  (#set! injection.language "html"))
  (#eq? @name "HTML"))
