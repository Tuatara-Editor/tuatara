; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The indentation rules for toml: runtime/queries/toml/indents.scm of
; Helix (https://github.com/helix-editor/helix) at commit 9bcdf4d79d14,
; unchanged below this notice.

; TOML keeps table/array-of-table bodies flat (keys at column 0), so only
; multi-line value arrays and inline tables indent; their closers dedent.
[
  (array)
  (inline_table)
] @indent

[
  "]"
  "}"
] @outdent
