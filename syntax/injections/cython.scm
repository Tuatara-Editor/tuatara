; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The rules for text in other languages in cython: runtime/queries/cython/injections.scm of
; Helix (https://github.com/YardQuit/helix_editor) at commit 9bcdf4d79d14,
; unchanged below this notice.

(call
  function: (attribute
    object: (identifier) @_re)
  arguments: (argument_list
    .
    (string
      (string_content) @injection.content))
  (#eq? @_re "re")
  (#set! injection.language "regex"))

((binary_operator
  left: (string
    (string_content) @injection.content)
  operator: "%")
  (#set! injection.language "printf"))

((comment) @injection.content
  (#set! injection.language "comment"))
