; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The indentation rules for matlab: runtime/queries/matlab/indents.scm of
; Helix (https://github.com/helix-editor/helix) at commit 9bcdf4d79d14,
; unchanged below this notice.

[
  (arguments_statement)
  (if_statement)
  (for_statement)
  (while_statement)
  (switch_statement)
  (try_statement)
  (function_definition)
  (class_definition)
  (enumeration)
  (events)
  (methods)
  (properties)
] @indent

; switch arms add the second indent level (case keyword at switch level, body
; one deeper); they stay @indent.
[
  (case_clause)
  (otherwise_clause)
] @indent @extend

; if/try branch keywords align with the construct: the enclosing statement
; already indents their bodies, so the clauses are NOT @indent and the keyword
; outdents.
[
  "else"
  "elseif"
  "catch"
  "end"
] @outdent
