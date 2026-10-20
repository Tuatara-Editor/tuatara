; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The colouring rules for shellcheckrc: runtime/queries/shellcheckrc/highlights.scm of
; Helix (https://github.com/YardQuit/helix_editor) at commit 9bcdf4d79d14,
; unchanged below this notice.

(comment) @comment
(string) @string
(boolean) @constant.builtin.boolean
(integer) @constant.numeric.integer

[
  "disable"
  "enable"
  "extended-analysis"
  "external-sources"
  "source"
  "source-path"
  "shell"
] @keyword

"=" @operator

[
  ","
  "-"
] @punctuation.delimiter

"SC" @special

(shell) @type.enum.variant

(all) @variable.builtin

(identifier) @label

(source_directive
  (identifier) @string.special.path)

(source_path_directive
  (identifier) @string.special.path)

(source_directive
  (identifier) @variable.builtin (#eq? @variable.builtin "SCRIPTDIR"))

(source_path_directive
  (identifier) @variable.builtin (#eq? @variable.builtin "SCRIPTDIR"))

(enable_directive
  (identifier) @diagnostic.error
  (#not-any-of? @diagnostic.error
    "add-default-case"
    "avoid-nullary-conditions"
    "check-extra-masked-returns"
    "check-set-e-suppressed"
    "check-unassigned-uppercase"
    "deprecate-which"
    "quote-safe-variables"
    "require-double-brackets"
    "require-variable-braces"
  ))
