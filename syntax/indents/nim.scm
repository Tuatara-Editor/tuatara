; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The indentation rules for nim: runtime/queries/nim/indents.scm of
; Helix (https://github.com/helix-editor/helix) at commit 9bcdf4d79d14,
; unchanged below this notice.

[
  (if)
  (when)
  (elif_branch)
  (else_branch)
  (of_branch) ; note: not case_statement
  (block)
  (while)
  (for)
  (try)
  (except_branch)
  (finally_branch)
  (defer)
  (static_statement)
  (proc_declaration)
  (func_declaration)
  (iterator_declaration)
  (converter_declaration)
  (method_declaration)
  (template_declaration)
  (macro_declaration)
  (symbol_declaration)
] @indent
;; increase the indentation level

[
  (if)
  (when)
  (elif_branch)
  (else_branch)
  (of_branch) ; note: not case_statement
  (block)
  (while)
  (for)
  (try)
  (except_branch)
  (finally_branch)
  (defer)
  (static_statement)
  (proc_declaration)
  (func_declaration)
  (iterator_declaration)
  (converter_declaration)
  (method_declaration)
  (template_declaration)
  (macro_declaration)
  (symbol_declaration)
] @extend
;; ???

[
  (return_statement)
  (raise_statement)
  (yield_statement)
  (break_statement)
  (continue_statement)
] @extend.prevent-once
;; end a level of indentation while staying indented

[
  ")" ; tuples
  "]" ; arrays, seqs
  "}" ; sets
] @outdent
;; end a level of indentation and unindent the line
