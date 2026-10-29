; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The rules for text in other languages in ruby: runtime/queries/ruby/injections.scm of
; Helix (https://github.com/helix-editor/helix) at commit 9bcdf4d79d14,
; unchanged below this notice.

((comment) @injection.content
 (#set! injection.language "comment"))

((heredoc_body 
  (heredoc_content) @injection.content
  (heredoc_end) @name
  (#set! injection.language "sql")) 
 (#eq? @name "SQL"))

((heredoc_body
  (heredoc_content) @injection.content
  (heredoc_end) @name
  (#set! injection.language "graphql"))
 (#any-of? @name
       "GQL"
       "GRAPHQL"))

((heredoc_body
  (heredoc_content) @injection.content
  (heredoc_end) @name
  (#set! injection.language "erb"))
 (#eq? @name "ERB"))

; `<command>`
; %x{<command>}
(subshell
  (string_content) @injection.content
  (#set! injection.language "bash"))

(call
  method: (identifier) @_method (#any-of? @_method "system" "spawn" "exec")
  arguments: (argument_list
    (string
      (string_content) @injection.content))
  (#set! injection.language "bash"))
