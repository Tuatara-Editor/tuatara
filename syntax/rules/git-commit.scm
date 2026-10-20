; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The colouring rules for git-commit: runtime/queries/git-commit/highlights.scm of
; Helix (https://github.com/YardQuit/helix_editor) at commit 9bcdf4d79d14,
; unchanged below this notice.

[(comment) (generated_comment) (scissor)] @comment
(subject) @markup.heading
(branch) @string.special.symbol
(filepath) @string.special.path
(arrow) @punctuation.delimiter
(subject (subject_prefix) @function)
(prefix (type) @keyword)
(prefix (scope) @variable.parameter)
(prefix [ "(" ")" ":" ] @punctuation.delimiter)
(prefix "!" @punctuation.special)
(trailer (token) @variable.other.member)
(trailer (value) @string)
(breaking_change (token) @special)

(change kind: (new)) @diff.plus
(change kind: (deleted)) @diff.minus
(change kind: (modified)) @diff.delta
(change kind: [(renamed) (typechange)]) @diff.delta.moved
