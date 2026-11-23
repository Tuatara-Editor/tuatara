; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The rules for text in other languages in bash: runtime/queries/bash/injections.scm of
; Helix (https://github.com/helix-editor/helix) at commit 9bcdf4d79d14,
; unchanged below this notice.

((comment) @injection.content
 (#set! injection.language "comment"))

(command
  name: (command_name (word) @_command)
  argument: (raw_string) @injection.content
 (#match? @_command "^[gnm]?awk$")
 (#set! injection.language "awk"))

((regex) @injection.content
  (#set! injection.language "regex"))

(command
  name: (command_name (word) @_command (#any-of? @_command "jq" "jaq"))
  argument: [
    (raw_string) @injection.content
    (string (string_content) @injection.content)
  ]
  (#set! injection.language "jq"))

(command
  name: (command_name (word) @_command (#eq? @_command "alias"))
  argument: (concatenation
    (word)
    [
      (raw_string) @injection.content
      (string (string_content) @injection.content)
    ])
  (#set! injection.language "bash"))

(command
  name: (command_name (word) @_command (#any-of? @_command "eval" "trap"))
  .
  argument: [
    (raw_string) @injection.content
    (string (string_content) @injection.content)
  ]
  (#set! injection.language "bash"))
