; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The indentation rules for erlang: runtime/queries/erlang/indents.scm of
; Helix (https://github.com/YardQuit/helix_editor) at commit 9bcdf4d79d14,
; unchanged below this notice.

[
  (function_clause)
  (case)
  (clause)
  (anonymous_function)
  (if)
  (receive)
  (try)
  (block)
  (maybe)
  (list)
  (tuple)
  (map)
  (arguments)
  (bitstring)
] @indent

; `catch`/`after` are branch keywords inside `try`/`receive`; they align with
; the construct, not its body.
[
  "end"
  "catch"
  "after"
  "]"
  "}"
  ")"
  ">>"
] @outdent
