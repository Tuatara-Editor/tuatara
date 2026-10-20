; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The colouring rules for codeql: runtime/queries/codeql/highlights.scm of
; Helix (https://github.com/YardQuit/helix_editor) at commit 9bcdf4d79d14,
; unchanged below this notice.

[
  "and"
  "any"
  "as"
  "asc"
  "avg"
  "by"
  "class"
  "concat"
  "count"
  "desc"
  "else"
  "exists"
  "extends"
  "forall"
  "forex"
  "from"
  "if"
  "implements"
  "implies"
  "import"
  "in"
  "instanceof"
  "max"
  "min"
  "module"
  "newtype"
  "not"
  "or"
  "order"
  "rank"
  "select"
  "strictconcat"
  "strictcount"
  "strictsum"
  "sum"
  "then"
  "where"

  (predicate)
  (result)
  (specialId)
  (super)
  (this)
] @keyword

[
  "boolean"
  "float"
  "int"
  "date"
  "string"
] @type.builtin

(annotName) @attribute

[
  "<"
  "<="
  "="
  ">"
  ">="
  "-"
  "!="
  "/"
  "*"
  "%"
  "+"
  "::"
] @operator

[
  "("
  ")"
  "{"
  "}"
  "["
  "]"
] @punctuation.bracket

[
  ","
  "|"
] @punctuation.delimiter

(className) @type

(varName) @variable

(integer) @constant.numeric.integer
(float) @constant.numeric.float

[
  (false)
  (true)
] @constant.builtin.boolean

(string) @string

(aritylessPredicateExpr (literalId) @function)
(predicateName) @function

[
  (line_comment)
  (block_comment)
  (qldoc)
] @comment
