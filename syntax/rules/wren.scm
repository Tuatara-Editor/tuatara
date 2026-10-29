; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The colouring rules for wren: runtime/queries/wren/highlights.scm of
; Helix (https://github.com/helix-editor/helix) at commit 9bcdf4d79d14,
; unchanged below this notice.

(comment) @comment
(string) @string
(raw_string) @string
(number) @constant.numeric.integer
(name) @variable
(field) @variable
(static_field) @variable
(null) @constant.builtin
(boolean) @constant.builtin.boolean

((name) @variable.builtin
 (#match? @variable.builtin "^(Bool|Class|Fiber|Fn|List|Map|Null|Num|Object|Range|Sequence|String|System)$"))

(call_expression
  (name) @function)

(method_definition
  (name) @function.method)

((parameter) @variable.parameter)


(if_statement
[
  "if"
  "else"
] @keyword.control.conditional)

(for_statement
[
  "for"
  "in"
] @keyword.control.repeat)

(while_statement
[
  "while"
] @keyword.control.repeat)

[
  (break_statement)
  (continue_statement)
  (return_statement)
] @keyword.control.return

(class_definition
"is"
@keyword)

[
  "import"
  "for"
  "as"
] @keyword.control.import

[
  "is"
] @keyword

(operator) @operator

[
 "("
 ")"
 "["
 "]"
 "{"
 "}"
] @punctuation.bracket

["," "."] @punctuation.delimiter

[
  "class"
  "var"
] @keyword.storage.type

[
  "static"
] @keyword.storage.modifier

(constructor
  ["construct"] @constructor)
