; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The indentation rules for cmake: runtime/queries/cmake/indents.scm of
; Helix (https://github.com/helix-editor/helix) at commit 9bcdf4d79d14,
; unchanged below this notice.

[
  (if_condition)
  (foreach_loop)
  (while_loop)
  (function_def)
  (macro_def)
  (block_def)
  (normal_command)
] @indent

")" @outdent

[
  (else)
  (elseif)
  (endif)
  (endforeach)
  (endwhile)
  (endfunction)
  (endmacro)
  (endblock)
] @outdent
