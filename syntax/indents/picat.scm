; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The indentation rules for picat: runtime/queries/picat/indents.scm of
; Helix (https://github.com/YardQuit/helix_editor) at commit 9bcdf4d79d14,
; unchanged below this notice.

[
  (array_expression)
  (list_expression)
  (braced_goal)
  (arguments)
  (parameters)

  (predicate_rule)
  (predicate_fact)
  (function_rule)
  (function_fact)
  (action_rule)
  (nonbacktrackable_predicate_rule)

  (if_statement)
  (while_statement)
  (foreach_statement)
  (do_statement)
] @indent

[
  ")"
  "}"
  "]"
] @outdent

(if_statement "elseif" (_) @outdent)
(if_statement "else" (_) @outdent)
(while_statement "do" (_) @outdent)
(do_statement "while" (_) @outdent)
