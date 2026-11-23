; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The rules for text in other languages in fish: runtime/queries/fish/injections.scm of
; Helix (https://github.com/helix-editor/helix) at commit 9bcdf4d79d14,
; unchanged below this notice.

((comment) @injection.content
 (#set! injection.language "comment"))

(command
  name: (word) @_command (#any-of? @_command "jq" "jaq")
  argument: [(double_quote_string) (single_quote_string)] @injection.content
  (#set! injection.language "jq"))

(command
  name: (word) @_command (#eq? @_command "nu")
  argument: (word) @_flag (#match? @_flag "^-.*c$")
  argument: [(single_quote_string) (double_quote_string)] @injection.content
  (#set! injection.language "nu")
)
