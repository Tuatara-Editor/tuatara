; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The rules for local names in pascal: runtime/queries/pascal/locals.scm of
; Helix (https://github.com/YardQuit/helix_editor) at commit 9bcdf4d79d14,
; unchanged below this notice.

; Scopes

[
  (declProc)
  (block)
] @local.scope

; Definitions

(declArg
  name: (identifier) @local.definition.variable.parameter)

(declVar
  name: (identifier) @local.definition.variable)

; References

(identifier) @local.reference

; The rhs of `a.b` is a member access, not a variable reference.
(exprDot
  rhs: (identifier) @_)

; Procedure/function names in call position are not variable references.
(exprCall
  entity: (identifier) @_)
