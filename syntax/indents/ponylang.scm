; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The indentation rules for ponylang: runtime/queries/ponylang/indents.scm of
; Helix (https://github.com/helix-editor/helix) at commit 9bcdf4d79d14,
; unchanged below this notice.

; queries for helix to do automatic indentation upon hitting enter
; TODO: needs more work, cover more cases
[
  (entity)
  (method)
  (behavior)
  (constructor)
  (block)
  (tuple)
  (grouped)
] @indent
(match_case body: (block) @indent)
; ffi_call and call
(_ arguments: (_) @indent)
(assignment right: (_) @indent

)

[
  (params)
  (object)
  ("if")
] @extend
(lambda params: (_) @extend)

[
  "end"
  "}"
  "]"
  ")"
  "|"
] @outdent
