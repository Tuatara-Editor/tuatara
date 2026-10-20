; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The colouring rules for blade: runtime/queries/blade/highlights.scm of
; Helix (https://github.com/YardQuit/helix_editor) at commit 9bcdf4d79d14,
; unchanged below this notice.

; inherits: html

(directive) @keyword.directive
(directive_start) @keyword.directive
(directive_end) @keyword.directive
(comment) @comment

; merged with blade punctuation
[
  "{{"
  "}}"
  "{!!"
  "!!}"
  "("
  ")"
] @punctuation.bracket
