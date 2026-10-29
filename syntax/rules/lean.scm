; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The colouring rules for lean: runtime/queries/lean/highlights.scm of
; Helix (https://github.com/helix-editor/helix) at commit 9bcdf4d79d14,
; unchanged below this notice.

; Variables
(identifier) @variable

; Namespaces

(open
  namespace: (identifier) @namespace)
(namespace
  name: (identifier) @namespace)
(section
  name: (identifier) @namespace)

;; Identifier naming conventions
((identifier) @type
 (#match? @type "^[A-Z]"))

(arrow) @type
(product) @type

;; Declarations

[
  "abbrev"
  "def"
  "theorem"
  "constant"
  "instance"
  "axiom"
  "example"
  "inductive"
  "structure"
  "class"

  "deriving"

  "section"
  "namespace"
] @keyword

(attributes
  (identifier) @function)

(abbrev
  name: (identifier) @type)
(def
  name: (identifier) @function)
(theorem
  name: (identifier) @function)
(constant
  name: (identifier) @type)
(instance
  name: (identifier) @function)
(instance
  type: (identifier) @type)
(axiom
  name: (identifier) @function)
(structure
  name: (identifier) @type)
(structure
  extends: (identifier) @type)

(where_decl
  type: (identifier) @type)

(proj
  name: (identifier) @field)

(binders
  type: (identifier) @type)

["if" "then" "else"] @keyword.control.conditional

["for" "in" "do"] @keyword.control.repeat

"import" @keyword.control.import

; Tokens

[
  "!"
  "$"
  "%"
  "&&"
  "*"
  "*>"
  "+"
  "++"
  "-"
  "/"
  "::"
  ":="
  "<"
  "<$>"
  "<*"
  "<*>"
  "<="
  "<|"
  "<|>"
  "="
  "=="
  "=>"
  ">"
  ">"
  ">="
  ">>"
  ">>="
  "@"
  "^"
  "|>"
  "|>."
  "||"
  "←"
  "→"
  "↔"
  "∘"
  "∧"
  "∨"
  "≠"
  "≤"
  "≥"
] @operator

[
  "@&"
] @operator

[
  "attribute"
  "by"
  "end"
  "export"
  "extends"
  "fun"
  "let"
  "have"
  "match"
  "open"
  "return"
  "universe"
  "variable"
  "where"
  "with"
  "λ"
  (hash_command)
  (prelude)
  (sorry)
] @keyword

[
  "prefix"
  "infix"
  "infixl"
  "infixr"
  "postfix"
  "notation"
  "macro_rules"
  "syntax"
  "elab"
  "builtin_initialize"
] @keyword

[
  "noncomputable"
  "partial"
  "private"
  "protected"
  "unsafe"
] @keyword

[
  "apply"
  "exact"
  "rewrite"
  "rw"
  "simp"
  (trivial)
] @keyword

[
  "catch"
  "finally"
  "try"
] @exception

((apply
  name: (identifier) @exception)
 (#match? @exception "throw"))

[
  "unless"
  "mut"
] @keyword

[(true) (false)] @constant.builtin.boolean

(number) @constant.numeric.integer
(float) @constant.numeric.float

(comment) @comment
(char) @constant.character
(string) @string
(interpolated_string) @string
; (escape_sequence) @string.escape

; Reset highlighting in string interpolation
(interpolation) @none

(interpolation
  "{" @punctuation.special
  "}" @punctuation.special)

["(" ")" "[" "]" "{" "}" "⟨" "⟩"] @punctuation.bracket

["|" "," "." ":" ";"] @punctuation.delimiter

(sorry) @error
