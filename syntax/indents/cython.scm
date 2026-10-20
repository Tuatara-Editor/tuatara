; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The indentation rules for cython: runtime/queries/cython/indents.scm of
; Helix (https://github.com/YardQuit/helix_editor) at commit 9bcdf4d79d14,
; unchanged below this notice.

[
  (list)
  (tuple)
  (dictionary)
  (set)
  (if_statement)
  (for_statement)
  (while_statement)
  (with_statement)
  (try_statement)
  (match_statement)
  (case_clause)
  (import_from_statement)

  (parenthesized_expression)
  (generator_expression)
  (list_comprehension)
  (set_comprehension)
  (dictionary_comprehension)

  (tuple_pattern)
  (list_pattern)
  (argument_list)
  (parameters)
  (binary_operator)

  (function_definition)
  (cdef_statement)
  (class_definition)
] @indent

; Workaround for the tree-sitter grammar creating large errors when a
; try_statement is missing the except/finally clause

(ERROR
  "try"
  .
  ":" @indent @extend)
(ERROR
  .
  "def") @indent @extend
(ERROR
  (block) @indent @extend
  )

(ERROR
  "try"
  .
  ":"
  (ERROR
    (block
      (expression_statement
        (identifier) @_except) @indent))
  (#eq? @_except "except"))

[
  (if_statement)
  (for_statement)
  (while_statement)
  (with_statement)
  (try_statement)
  (match_statement)
  (case_clause)

  (cdef_statement)
  (function_definition)
  (class_definition)
] @extend

[
  (return_statement)
  (break_statement)
  (continue_statement)
  (raise_statement)
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

(parameters
  .
  (identifier) @anchor
  ) @align

(argument_list
  .
  (_) @anchor
  ) @align
