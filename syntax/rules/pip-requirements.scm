; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The colouring rules for pip-requirements: runtime/queries/pip-requirements/highlights.scm of
; Helix (https://github.com/YardQuit/helix_editor) at commit 9bcdf4d79d14,
; unchanged below this notice.

(comment) @comment

(requirement (package) @variable)
(extras (package) @variable.parameter)

; "==" | ">" | "<" | ">=" | "<="
(version_cmp) @operator

(version) @constant.numeric

(marker_var) @attribute

(marker_op) @keyword.operator

[
  "[" "]"
  "(" ")"
] @punctuation.bracket

[
  ","
  ";"
  "@"
] @punctuation.delimiter

[
  "${" "}"
] @punctuation.special

"=" @operator

(path) @string.special.path
(url) @string.special.url

(option) @function

(env_var) @constant

(quoted_string) @string

(linebreak) @constant.character.escape
