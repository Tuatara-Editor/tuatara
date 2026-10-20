; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The indentation rules for koto: runtime/queries/koto/indents.scm of
; Helix (https://github.com/YardQuit/helix_editor) at commit 9bcdf4d79d14,
; unchanged below this notice.

[
  (list)
  (map)
  (tuple)
] @indent

[
  (for)
  (else_if)
  (else)
  (match)
  (switch)
  (until)
  (while)
] @indent @extend

(assign
  "=" @indent @extend
  !rhs
)
(assign
  "=" @indent @extend
  rhs: (_) @anchor
  (#not-same-line? @indent @anchor)
)

(if
  condition: (_) @indent @extend
  !then
)
(if
  condition: (_) @indent @extend
  then: (_) @anchor
  (#not-same-line? @indent @anchor)
)

(function
  (args) @indent @extend
  !body
)
(function
  (args) @indent @extend
  body: (_) @anchor
  (#not-same-line? @indent @anchor)
)

(match_arm
  "then" @indent @extend
  !then
)
(match_arm
  "then" @indent @extend
  then: (_) @anchor
  (#not-same-line? @indent @anchor)
)

[
  "}"
  "]"
  ")"
] @outdent
