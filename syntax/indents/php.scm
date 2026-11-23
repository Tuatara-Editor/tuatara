; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The indentation rules for php: runtime/queries/php/indents.scm of
; Helix (https://github.com/helix-editor/helix) at commit 9bcdf4d79d14,
; unchanged below this notice.

[
  (array_creation_expression)
  (arguments)
  (formal_parameters)
  (compound_statement)
  (declaration_list)
  (binary_expression)
  (return_statement)
  (expression_statement)
  (switch_block)
  ; statements after a `case`/`default` label (the node also holds the label,
  ; so the default tail scope indents only the body lines, not the label)
  (case_statement)
  (default_statement)
  (anonymous_function_use_clause)
  (property_hook_list)
] @indent

[
  "}"
  ")"
  "]"
] @outdent

; Heredoc / nowdoc bodies are literal content.
[
  (heredoc)
  (nowdoc)
] @opaque
