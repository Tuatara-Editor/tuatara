; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The colouring rules for tql: runtime/queries/tql/highlights.scm of
; Helix (https://github.com/helix-editor/helix) at commit 9bcdf4d79d14,
; unchanged below this notice.

;; Upstream: https://github.com/tenzir/tree-sitter-tql/blob/main/queries/tql/highlights.scm

"move" @keyword

[
  "let"
] @keyword.storage

[
  "and"
  "or"
  "not"
  "in"
] @keyword.operator

[
  "if"
  "else"
  "match"
] @keyword.control.conditional

"this" @variable.builtin

[
  "."
  ":"
] @punctuation.delimiter

[
  "["
  "]"
  "{"
  "}"
] @punctuation.bracket

[
  "?"
  "..."
  "+"
  "-"
  "*"
  "/"
  "="
  "=>"
  "|"
  "::"
  "=="
  "!="
  ">"
  ">="
  "<"
  "<="
] @operator

[
  "("
  ")"
] @punctuation.bracket

[
  ","
] @punctuation.delimiter

"_" @variable.builtin

"null" @constant.builtin

[
  "true"
  "false"
] @constant.builtin.boolean

(dollar_var) @variable
(meta_selector) @attribute
(number) @constant.numeric
(string) @string
(format_expr) @string
(ip) @constant
(subnet) @constant
(time) @constant.numeric
(duration) @constant.numeric
(frontmatter_open) @comment
(frontmatter_close) @comment
(comment) @comment

(invocation
  operator: (entity) @function)
(call_expression
  (entity) @function)
(call_expression
  method: (entity) @function.method)
(identifier) @variable
