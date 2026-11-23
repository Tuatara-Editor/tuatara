; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The indentation rules for sway: runtime/queries/sway/indents.scm of
; Helix (https://github.com/helix-editor/helix) at commit 9bcdf4d79d14,
; unchanged below this notice.

[
  (use_list)
  (block)
  (match_block)
  (arguments)
  (parameters)
  (declaration_list)
  (field_declaration_list)
  (field_initializer_list)
  (struct_pattern)
  (tuple_pattern)
  (unit_expression)
  (enum_variant_list)
  (call_expression)
  (binary_expression)
  (field_expression)
  (tuple_expression)
  (array_expression)
  (where_clause)

  (token_tree)
] @indent

[
  "}"
  "]"
  ")"
] @outdent

; Indent the right side of assignments.
; The #not-same-line? predicate is required to prevent an extra indent for e.g.
; an else-clause where the previous if-clause starts on the same line as the assignment.
(assignment_expression
  .
  (_) @expr-start
  right: (_) @indent
  (#not-same-line? @indent @expr-start)

)
(compound_assignment_expr
  .
  (_) @expr-start
  right: (_) @indent
  (#not-same-line? @indent @expr-start)

)
(let_declaration
  .
  (_) @expr-start
  value: (_) @indent
  alternative: (_)? @indent
  (#not-same-line? @indent @expr-start)

)
(if_expression
  .
  (_) @expr-start
  condition: (_) @indent
  (#not-same-line? @indent @expr-start)

)

; Some field expressions where the left part is a multiline expression are not
; indented by cargo fmt.
; Because this multiline expression might be nested in an arbitrary number of
; field expressions, this can only be matched using a Regex.
(field_expression
  value: (_) @val
  "." @outdent
  (#match? @val "(\\A[^\\n\\r]+\\([\\t ]*(\\n|\\r).*)|(\\A[^\\n\\r]*\\{[\\t ]*(\\n|\\r))")
)
