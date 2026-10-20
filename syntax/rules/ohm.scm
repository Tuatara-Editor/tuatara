; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The colouring rules for ohm: runtime/queries/ohm/highlights.scm of
; Helix (https://github.com/YardQuit/helix_editor) at commit 9bcdf4d79d14,
; unchanged below this notice.

; See: https://docs.helix-editor.com/master/themes.html#syntax-highlighting

; attribute
; ---------

(case_name) @attribute

; comment.line
; ------------

[
  (singleline_comment)
  (rule_descr)
] @comment.line

; comment.block
; -------------

(multiline_comment) @comment.block

; function.method
; ---------------

(rule
  name: (identifier) @function.method)

; function.builtin
; ----------------

; Lexical
((identifier) @function.builtin
  (#any-of? @function.builtin
    "any"
    "alnum"
    "end"
    "digit" "hexDigit"
    "letter"
    "space"
    "lower" "upper" "caseInsensitive"
    "listOf" "nonemptyListOf" "emptyListOf"
    "applySyntactic")
  (#is-not? local))

; Syntactic
((identifier) @function.builtin
  (#any-of? @function.builtin "ListOf" "NonemptyListOf" "EmptyListOf")
  (#is-not? local))

; function.method (continuing)
; ---------------

(term
  base: (identifier) @function.method)

; string.special
; --------------

(escape_char) @constant.character.escape

; string
; ------

[
  (terminal_string)
  (one_char_terminal)
] @string

; type
; ----

(super_grammar
  name: (identifier) @type)

(grammar
  name: (identifier) @type)

; operator
; --------

[
  ; "=" ":=" "+="
  (define) (override) (extend)

  ; "&" "~"
  (lookahead) (negative_lookahead)

  ; "#"
  (lexification)

  ; "*" "+" "?"
  (zero_or_more) (one_or_more) (zero_or_one)

  ; "..."
  (super_splice)

  "<:" ".." "|"
] @operator

; punctuation.bracket
; -------------------

[
  "<"
  ">"
  "{"
  "}"
] @punctuation.bracket

(alt
  "(" @punctuation.bracket
  ")" @punctuation.bracket)

; punctuation.delimiter
; ---------------------

"," @punctuation.delimiter

; variable.parameter
; ------------------

(formals
  (identifier) @variable.parameter)
