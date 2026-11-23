; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The indentation rules for textproto: runtime/queries/textproto/indents.scm of
; Helix (https://github.com/helix-editor/helix) at commit 9bcdf4d79d14,
; unchanged below this notice.

[
  (message_value)
  (message_list)
  (scalar_list)
] @indent

[
  (close_arrow)
  (close_square)
  (close_squiggly)
] @outdent
