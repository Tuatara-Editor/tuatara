; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The rules for local names in crystal: runtime/queries/crystal/locals.scm of
; Helix (https://github.com/helix-editor/helix) at commit 9bcdf4d79d14,
; unchanged below this notice.

((method_def) @local.scope
 (#set! local.scope-inherits false))
((fun_def) @local.scope
 (#set! local.scope-inherits false))

(block) @local.scope

(param
  name: (identifier) @local.definition.variable.parameter)

(assign
  lhs: (identifier) @local.definition.variable)

(identifier) @local.reference

; Call/keyword-argument names look like identifiers but aren't variable refs.
(call
  method: (identifier) @_)
(named_expr
  name: (identifier) @_)
