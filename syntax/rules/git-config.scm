; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The colouring rules for git-config: runtime/queries/git-config/highlights.scm of
; Helix (https://github.com/helix-editor/helix) at commit 9bcdf4d79d14,
; unchanged below this notice.

(section_name) @markup.heading

((section_name) @keyword.directive
 (#eq? @keyword.directive "include"))

((section_header
   (section_name) @keyword.directive
   (subsection_name))
 (#eq? @keyword.directive "includeIf"))

(variable (name) @variable.other.member)
[(true) (false)] @constant.builtin.boolean
(integer) @constant.numeric.integer

[(string) (subsection_name)] @string

((string) @string.special.path
 (#match? @string.special.path "^(~|./|/)"))

[
  "["
  "]"
] @punctuation.bracket

["=" "\\"] @punctuation.delimiter

(escape_sequence) @constant.character.escape

(comment) @comment
