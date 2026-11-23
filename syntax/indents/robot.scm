; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The indentation rules for robot: runtime/queries/robot/indents.scm of
; Helix (https://github.com/helix-editor/helix) at commit 9bcdf4d79d14,
; unchanged below this notice.

(keyword_definition) @indent
(test_case_definition) @indent

(for_statement) @indent
(for_statement "END" @outdent)
(for_statement
  right: (_ (arguments (continuation (ellipses) @outdent))))

(while_statement) @indent
(while_statement "END" @outdent)

(if_statement) @indent
(if_statement (elseif_statement) @outdent)
(if_statement (else_statement) @outdent)
(if_statement "END" @outdent)

(try_statement) @indent
(try_statement (except_statement) @outdent)
(try_statement (finally_statement) @outdent)
(try_statement (else_statement) @outdent)
(try_statement "END" @outdent)
