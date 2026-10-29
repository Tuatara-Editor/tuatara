; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The indentation rules for unison: runtime/queries/unison/indents.scm of
; Helix (https://github.com/helix-editor/helix) at commit 9bcdf4d79d14,
; unchanged below this notice.

[
  (term_definition)
  (pattern)
  (exp_if)
  (constructor)
  (delay_block)
  (type_signature)
] @indent

[(kw_then) (kw_else) (cases)] @indent.always @extend
