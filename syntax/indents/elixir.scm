; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The indentation rules for elixir: runtime/queries/elixir/indents.scm of
; Helix (https://github.com/YardQuit/helix_editor) at commit 9bcdf4d79d14,
; unchanged below this notice.

[
  (anonymous_function)
  (do_block)
  (stab_clause)
  (map)
  (list)
  (tuple)
] @indent

; The else/rescue/catch/after clause blocks nest *inside* the do_block, which
; already provides the body's one level of indent; the keyword itself outdents
; back to the construct's level.
[
  "end"
  "else"
  "rescue"
  "catch"
  "after"
  "]"
  "}"
  ")"
] @outdent
