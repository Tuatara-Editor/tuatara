; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The indentation rules for svelte: runtime/queries/svelte/indents.scm of
; Helix (https://github.com/helix-editor/helix) at commit 9bcdf4d79d14,
; unchanged below this notice.

[
  (element)
  (if_block)
  (else_if_clause)
  (else_clause)
  (await_branch)
  (each_block)
  (key_block)
  (snippet_block)
  (await_block)
  (expression)
] @indent

[
  (end_tag)
  (block_end)
  (else_if_clause)
  (else_clause)
  (await_branch)
] @outdent
