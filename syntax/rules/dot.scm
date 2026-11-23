; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The colouring rules for dot: runtime/queries/dot/highlights.scm of
; Helix (https://github.com/helix-editor/helix) at commit 9bcdf4d79d14,
; unchanged below this notice.

(identifier) @variable

(keyword) @keyword
(string_literal) @string
(number_literal) @constant.numeric

[
  (edgeop)
  (operator)
] @operator

[
  ","
  ";"
] @punctuation.delimiter

[
  "{"
  "}"
  "["
  "]"
  "<"
  ">"
] @punctuation.bracket

(subgraph
  id: (id
    (identifier) @namespace)
)

(attribute
  name: (id
    (identifier) @type)
  value: (id
    (identifier) @constant)
)

(comment) @comment

(preproc) @keyword.directive
