; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The indentation rules for latex: runtime/queries/latex/indents.scm of
; Helix (https://github.com/YardQuit/helix_editor) at commit 9bcdf4d79d14,
; unchanged below this notice.

[
  (generic_environment)
  (math_environment)
  (displayed_equation)
  (listing_environment)
  (minted_environment)
  (pycode_environment)
  (verbatim_environment)
  (comment_environment)
  (curly_group)
  (brack_group)
] @indent
[
  (end)
  "}"
  "]"
] @outdent
