; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The colouring rules for git-rebase: runtime/queries/git-rebase/highlights.scm of
; Helix (https://github.com/YardQuit/helix_editor) at commit 9bcdf4d79d14,
; unchanged below this notice.

; a rough translation:
; * constant.builtin - git hash
; * constant - a git label
; * keyword - command that acts on commits commits
; * function - command that acts only on labels
; * comment - discarded commentary on a command, has no effect on the rebase
; * string - text used in the rebase operation
; * operator - a 'switch' (used in fixup and merge), either -c or -C at time of writing

(((command) @keyword
  (label) @constant.builtin
  (message)? @comment)
 (#match? @keyword "^(p|pick|r|reword|e|edit|s|squash|d|drop)$"))

(((command) @function
  (label) @constant
  (message)? @comment)
 (#match? @function "^(l|label|t|reset|u|update-ref)$"))

((command) @keyword
 (#match? @keyword "^(x|exec|b|break)$"))

(((command) @attribute
  (label) @constant.builtin
  (message)? @comment)
 (#match? @attribute "^(f|fixup)$"))

(((command) @keyword
  (label) @constant.builtin
  (label) @constant
  (message) @string)
 (#match? @keyword "^(m|merge)$"))

(option) @operator

(comment) @comment
