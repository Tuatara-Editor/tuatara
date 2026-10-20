; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The colouring rules for jinja: runtime/queries/jinja/highlights.scm of
; Helix (https://github.com/YardQuit/helix_editor) at commit 9bcdf4d79d14,
; unchanged below this notice.

(expression) @string
(statement) @variable.builtin
(keyword) @keyword
(comment) @comment
(identifier) @variable.parameter
(operator) @operator
(string) @string