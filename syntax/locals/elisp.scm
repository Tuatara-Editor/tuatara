; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The rules for local names in elisp: runtime/queries/elisp/locals.scm of
; Helix (https://github.com/YardQuit/helix_editor) at commit 9bcdf4d79d14,
; unchanged below this notice.

; Scopes

[
  (function_definition)
  (macro_definition)
] @local.scope

; `lambda`/`let`/`let*` are `special_form`s; the leading keyword is an anonymous
; token, so scope on the specific form rather than all special forms.
(special_form
  .
  ["lambda" "let" "let*"]) @local.scope

; Definitions

(function_definition
  parameters: (list (symbol) @local.definition.variable.parameter))
(macro_definition
  parameters: (list (symbol) @local.definition.variable.parameter))

; (lambda (a b) ...)
(special_form
  .
  "lambda"
  .
  (list (symbol) @local.definition.variable.parameter))

; (let ((x v) (y w)) ...) / let* — each binding's first symbol
(special_form
  .
  ["let" "let*"]
  .
  (list
    (list
      .
      (symbol) @local.definition.variable)))

; References

(symbol) @local.reference
