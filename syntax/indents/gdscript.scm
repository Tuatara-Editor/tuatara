; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The indentation rules for gdscript: runtime/queries/gdscript/indents.scm of
; Helix (https://github.com/YardQuit/helix_editor) at commit 9bcdf4d79d14,
; unchanged below this notice.

[
  (if_statement)
  (for_statement)
  (while_statement)
  (match_statement)
  (pattern_section)

  (function_definition)
  (lambda)
  (constructor_definition)
  (class_definition)
  (enum_definition)

  (dictionary (_))
  (array (_))
  (setget)
] @indent

[
  (if_statement)
  (for_statement)
  (while_statement)
  (match_statement)
  (pattern_section)

  (function_definition)
  (class_definition)
] @extend

[
  (return_statement)
  (break_statement)
  (continue_statement)
  (pass_statement)
] @extend.prevent-once


[
  ")"
  "]"
  "}"
] @outdent
(elif_clause
  "elif" @outdent)
(else_clause
  "else" @outdent)
