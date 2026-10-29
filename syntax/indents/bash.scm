; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The indentation rules for bash: runtime/queries/bash/indents.scm of
; Helix (https://github.com/helix-editor/helix) at commit 9bcdf4d79d14,
; unchanged below this notice.

[
  (function_definition)
  (if_statement)
  (for_statement)
  (while_statement)
  (case_statement)
  (case_item)
  (pipeline)
] @indent

[
  "}"
  "fi"
  "elif"
  "else"
  "done"
  "esac"
] @outdent
