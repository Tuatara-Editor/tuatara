; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The indentation rules for tql: runtime/queries/tql/indents.scm of
; Helix (https://github.com/helix-editor/helix) at commit 9bcdf4d79d14,
; unchanged below this notice.

;; Upstream: https://github.com/tenzir/tree-sitter-tql/blob/main/queries/tql/indents.scm

[
  (then_block)
  (else_block)
  (pipeline_block)
  (match_statement)
  (match_arm)
  (list)
  (record)
] @indent

[
  "}"
  "]"
] @outdent
