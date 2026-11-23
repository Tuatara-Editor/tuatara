; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The indentation rules for d: runtime/queries/d/indents.scm of
; Helix (https://github.com/helix-editor/helix) at commit 9bcdf4d79d14,
; unchanged below this notice.

[
  (parameters)
  (template_parameters)
  (expression_statement)
  (aggregate_body)
  (function_body)
  (scope_statement)
  (block_statement)
  (case_statement)
] @indent

[
  (case)
  (default)
  "}"
  "]"
] @outdent
