; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The colouring rules for regex: runtime/queries/regex/highlights.scm of
; Helix (https://github.com/YardQuit/helix_editor) at commit 9bcdf4d79d14,
; unchanged below this notice.

; upstream: https://github.com/tree-sitter/tree-sitter-regex/blob/b2ac15e27fce703d2f37a79ccd94a5c0cbe9720b/queries/highlights.scm

[
  "("
  ")"
  "(?"
  "(?:"
  "(?<"
  "(?P<"
  "(?P="
  ">"
  "["
  "]"
  "[:"
  ":]"
  "{"
  "}"
] @punctuation.bracket

; `<=`/`<!` lookbehind are now `<` + `=`/`!` (lookahead and lookbehind share
; the unified lookaround_assertion); the bare `<` is left unhighlighted.
[
  "*"
  "+"
  "|"
  "="
  "!"
  "?"
] @operator

[
  (identity_escape)
  (control_letter_escape)
  (character_class_escape)
  (control_escape)
  (start_assertion)
  (end_assertion)
  (boundary_assertion)
  (non_boundary_assertion)
] @constant.character.escape

(group_name) @label

(count_quantifier
  [
    (decimal_digits) @constant.numeric
    "," @punctuation.delimiter
  ])

; Inline flags: `(?i)` / `(?i-s:…)`.
(inline_flags_group
  "-"? @operator
  ":"? @punctuation.delimiter)
(flags) @constant.character

(character_class
  [
    "^" @operator
    (class_range "-" @operator)
  ])

; POSIX class name (`alpha` in `[[:alpha:]]`) alongside literal class chars.
[
  (class_character)
  (posix_class_name)
] @constant.character
