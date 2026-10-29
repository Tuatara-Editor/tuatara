; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The indentation rules for inko: runtime/queries/inko/indents.scm of
; Helix (https://github.com/helix-editor/helix) at commit 9bcdf4d79d14,
; unchanged below this notice.

[
  (arguments)
  (array)
  (assign_field)
  (assign_local)
  (assign_receiver_field)
  (binary)
  (block)
  (bounds)
  (cast)
  (class)
  (class_pattern)
  (compound_assign_field)
  (compound_assign_local)
  (compound_assign_receiver_field)
  (define_constant)
  (define_variable)
  (grouped_expression)
  (implement_trait)
  (match)
  (or_pattern)
  (reopen_class)
  (replace_field)
  (replace_local)
  (symbols)
  (trait)
  (tuple)
  (tuple_pattern)
  (type_arguments)
] @indent

[
  ")"
  "]"
  "}"
] @outdent
