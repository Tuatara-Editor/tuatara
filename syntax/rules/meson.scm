; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The colouring rules for meson: runtime/queries/meson/highlights.scm of
; Helix (https://github.com/YardQuit/helix_editor) at commit 9bcdf4d79d14,
; unchanged below this notice.

(comment) @comment

(number) @constant.numeric.integer
(bool) @constant.builtin.boolean
(string) @string
(escape_sequence) @constant.character.escape

[
  "(" ")" "[" "]" "{" "}"
] @punctuation.bracket

[
  "," "." ":"
] @punctuation.delimiter

[
  "=" "==" "!=" "+" "+=" "-=" "/" "/=" "<" ">" ">=" "?"
] @operator

[
  "and" "or" "not" "in"
] @keyword.operator

[
  "if" "elif" "else" "endif"
] @keyword.control.conditional

[
  "foreach" "endforeach"
  (keyword_break)
  (keyword_continue)
] @keyword.control.repeat

; Format-string placeholder `@var@`.
"@" @punctuation.special

(identifier) @variable

; Command calls: `project(...)`, `executable(...)`.
(normal_command
  command: (identifier) @function)

; Member access vs. method call. A plain `obj.prop` is a member; an
; `obj.method(...)` additionally carries the call's `function:` field, so the
; later, more specific rule reclaims it as @function.method.
(expression_statement
  property: (identifier) @variable.other.member)
(expression_statement
  property: (identifier) @function.method
  function: (_))

; Dictionary / keyword-argument keys.
(pair
  key: (identifier) @variable.other.member)
