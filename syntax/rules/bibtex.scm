; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The colouring rules for bibtex: runtime/queries/bibtex/highlights.scm of
; Helix (https://github.com/YardQuit/helix_editor) at commit 9bcdf4d79d14,
; unchanged below this notice.

[
  (string_type)
  (preamble_type)
  (entry_type)
] @keyword

[
  (junk)
  (comment)
] @comment

[
  "="
  "#"
] @operator

(command) @function.builtin

(number) @constant.numeric

(field
  name: (identifier) @variable.builtin)

(token
  (identifier) @variable.parameter)

[
  (brace_word)
  (quote_word)
] @string

[
  (key_brace)
  (key_paren)
] @attribute

(string
  name: (identifier) @constant)

[
  "{"
  "}"
  "("
  ")"
] @punctuation.bracket

"," @punctuation.delimiter
