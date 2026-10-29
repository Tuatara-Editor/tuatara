; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The colouring rules for git-attributes: runtime/queries/git-attributes/highlights.scm of
; Helix (https://github.com/helix-editor/helix) at commit 9bcdf4d79d14,
; unchanged below this notice.

; inherits: git-ignore

(attribute) @variable
(value) @string

(quoted_pattern ["\""] @string)

(attribute_unset) @operator
(attribute_set_to) @operator

; Highlight builtin diff configuration
; The list of languages is taken from here https://git-scm.com/docs/gitattributes#_defining_a_custom_hunk_header
(attribute_set_to
  (attribute) @operator (#eq? @operator "diff")
  (value) @variable.builtin (#any-of? @variable.builtin
    "ada"
    "bash"
    "bibtex"
    "cpp"
    "csharp"
    "css"
    "dts"
    "elixir"
    "fortran"
    "fountain"
    "golang"
    "html"
    "java"
    "kotlin"
    "markdown"
    "matlab"
    "objc"
    "pascal"
    "perl"
    "php"
    "python"
    "ruby"
    "rust"
    "scheme"
    "tex"
  ))
