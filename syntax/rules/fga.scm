; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The colouring rules for fga: runtime/queries/fga/highlights.scm of
; Helix (https://github.com/helix-editor/helix) at commit 9bcdf4d79d14,
; unchanged below this notice.

(condition_declaration
  name: (identifier) @function)

(condition_declaration
  (param
    name: (identifier) @variable.parameter))

(conditional
  condition: (identifier) @function)

(type_declaration
  name: (extended_identifier) @type)

(definition
  relation: (extended_identifier) @variable)

(indirect_relation
  relation: (extended_identifier) @variable.other.member
  tupleset: (extended_identifier) @variable)

(relation_ref) @type
(all) @type

((simple_type_identifier) @type.builtin)

((container_type_identifier) @type.builtin)

(version) @constant.numeric
(int) @constant.numeric.integer
(uint) @constant.numeric.integer
(float) @constant.numeric.float

(string) @string
(bytes) @string.special

(boolean) @constant.builtin.boolean
(null) @constant.builtin

(condition_body
  (identifier) @variable)

(parenthesized_condition
  (identifier) @variable)

(bracket_condition
  (identifier) @variable)

(braced_condition
  (identifier) @variable)

(operator) @operator
(condition_operator) @operator

(condition_body ["{" "}"] @punctuation.bracket)
(parenthesized_condition ["(" ")"] @punctuation.bracket)
(bracket_condition ["[" "]"] @punctuation.bracket)
(braced_condition ["{" "}"] @punctuation.bracket)

(model) @keyword
(module "module" @keyword)
(schema "schema" @keyword)
(contents "contents" @keyword)
(relations "relations" @keyword)
(type_declaration "extend" @keyword)
(type_declaration "type" @keyword)
(definition "define" @keyword)

(indirect_relation "from" @keyword.operator)
(conditional "with" @keyword.operator)
(condition_declaration "condition" @keyword.function)

(comment) @comment
