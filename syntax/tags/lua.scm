; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The rules naming the definitions and headings in lua: runtime/queries/lua/tags.scm of
; Helix (https://github.com/helix-editor/helix) at commit 9bcdf4d79d14,
; unchanged below this notice.

; Function definitions
(function_declaration
  name: [
    (identifier) @name
    (dot_index_expression field: (identifier) @name)
    (method_index_expression method: (identifier) @name)
  ]) @definition.function

; `x = function() ... end`, `M.x = function() ... end`
(assignment_statement
  (variable_list
    name: [
      (identifier) @name
      (dot_index_expression field: (identifier) @name)
    ])
  (expression_list
    value: (function_definition))) @definition.function
