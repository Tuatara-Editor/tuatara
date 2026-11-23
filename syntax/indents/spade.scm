; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The indentation rules for spade: runtime/queries/spade/indents.scm of
; Helix (https://github.com/helix-editor/helix) at commit 9bcdf4d79d14,
; unchanged below this notice.


[
  (unit_definition)
  (struct_definition)
  (enum_definition)
  (enum_member)
  (impl)
  (mod)
  (argument_list)
  (let_binding)
  (block)
  (tuple_literal)
  (array_literal)
  (paren_expression)
  (turbofish)
  (generic_parameters)
  (named_unpack)
  (positional_unpack)
  (tuple_pattern)
] @indent

[
  "}"
  "]"
  ")"
] @outdent

