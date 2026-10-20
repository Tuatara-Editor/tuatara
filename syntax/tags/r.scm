; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The rules naming the definitions and headings in r: runtime/queries/r/tags.scm of
; Helix (https://github.com/YardQuit/helix_editor) at commit 9bcdf4d79d14,
; unchanged below this notice.

(binary_operator
  lhs: [(identifier) (string)] @name
  operator: "<-"
  rhs: (function_definition)) @definition.function

(binary_operator
  lhs: [(identifier) (string)] @name
  operator: "="
  rhs: (function_definition)) @definition.function
