; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The indentation rules for perl: runtime/queries/perl/indents.scm of
; Helix (https://github.com/YardQuit/helix_editor) at commit 9bcdf4d79d14,
; unchanged below this notice.

[
  (block)
  (loop_statement)
  (cstyle_for_statement)
  (for_statement)
  (array_element_expression)
  (hash_element_expression)
  (coderef_call_expression)
  (anonymous_slice_expression)
  (slice_expression)
  (keyval_expression)
  (anonymous_array_expression)
  (anonymous_hash_expression)
  (stub_expression)
  (func0op_call_expression)
  (func1op_call_expression)
  (map_grep_expression)
  (function_call_expression)
  (method_call_expression)
  (attribute)
] @indent

[
  "}"
  "]"
  ")"
] @outdent
