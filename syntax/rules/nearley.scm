; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The colouring rules for nearley: runtime/queries/nearley/highlights.scm of
; Helix (https://github.com/YardQuit/helix_editor) at commit 9bcdf4d79d14,
; unchanged below this notice.


(comment) @comment.line

(string) @string
(string "i" @keyword.modifier)

(identifier) @variable.other
(rule_name (identifier) @function)
(rule (generic (identifier) @function))

(directive_name) @keyword.directive
(directive_value (identifier) @constant)
(directive_value (string) @string)

(token) @constant

(generic
  "<" @punctuation.bracket
  (identifier) @type.parameter
  ">" @punctuation.bracket
)

(group "(" @punctuation.bracket ")" @punctuation.bracket)

(charset) @string.regexp
(wildcard) @keyword

(quantifier) @function.builtin

(macro_name
  "[" @punctuation.bracket
  (identifier) @variable.parameter
  "]" @punctuation.bracket
)
(macro_arg) @variable.parameter

(rule "->" @operator)
(rule_body "|" @operator)

(cont_block "@{%" @keyword.directive "%}" @keyword.directive)
(cont_inline "{%" @keyword.directive "%}" @keyword.directive)

(ifdef) @keyword.directive
