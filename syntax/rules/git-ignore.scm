; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The colouring rules for git-ignore: runtime/queries/git-ignore/highlights.scm of
; Helix (https://github.com/helix-editor/helix) at commit 9bcdf4d79d14,
; unchanged below this notice.

(pattern_char) @string
(pattern_char_escaped) @constant.character.escape

(directory_separator) @string
(directory_separator_escaped) @constant.character.escape

(negation) @operator

(wildcard_char_single) @operator
(wildcard_chars) @operator
(wildcard_chars_allow_slash) @operator

(bracket_char) @string
(bracket_char_escaped) @constant.character.escape

(bracket_negation) @operator
(bracket_range) @operator
(bracket_char_class) @keyword

[
  "["
  "]"
] @punctuation.bracket

(comment) @comment
