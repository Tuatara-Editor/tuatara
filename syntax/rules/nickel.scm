; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The colouring rules for nickel: runtime/queries/nickel/highlights.scm of
; Helix (https://github.com/helix-editor/helix) at commit 9bcdf4d79d14,
; unchanged below this notice.

(types) @type
(type_builtin) @type.builtin
"Array" @type.builtin

(enum_tag) @constructor

"null" @constant.builtin
(bool) @constant.builtin.boolean
(str_esc_char) @constant.character.escape
(num_literal) @constant.numeric

(str_chunks) @string

; NOTE: Nickel has no block comments
(comment) @comment.line
; Nickel doesn't use comments for documentation, ideally this would be
; `@documentation` or something similar
(annot_atom
  doc: (static_string) @comment.block.documentation
)

(record_operand (atom (ident) @variable))
(let_in_block
  "let" @keyword
  "rec"? @keyword
  "in" @keyword
)

(let_binding
  pat: (pattern
    (ident) @variable
  )
)

(fun_expr
  "fun" @keyword.function
  pats:
    (pattern_fun (ident) @variable.parameter)+
  "=>" @operator
)
(record_field) @variable.other.member
; Field access `o.field`: the id of a record operation chain.
(record_operation_chain id: (ident) @variable.other.member)

[
  "."
] @punctuation.delimiter
[
  "{" "}"
  "(" ")"
  "[|" "|]"
  "[" "]"
] @punctuation.bracket
(multstr_start) @punctuation.bracket
(multstr_end) @punctuation.bracket
(interpolation_start) @punctuation.bracket
(interpolation_end) @punctuation.bracket

["forall" "default" "doc"] @keyword
["if" "then" "else" "match"] @keyword.control.conditional
"import" @keyword.control.import

(infix_expr
  op: (_) @operator
)

(applicative
  t1: (applicative
    (record_operand) @function
  )
)
(builtin) @function.builtin
