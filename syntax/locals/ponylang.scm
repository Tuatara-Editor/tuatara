; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The rules for local names in ponylang: runtime/queries/ponylang/locals.scm of
; Helix (https://github.com/YardQuit/helix_editor) at commit 9bcdf4d79d14,
; unchanged below this notice.

[
  (entity)
  (method)
  (behavior)
  (constructor)
  ("if")
  (elseif)
  (ifdef)
  (elseifdef)
  (iftype)
  (elseiftype)
  (match)
  (match_case)
  ("while")
  ("repeat")
  ("for")
  (lambda)
  (try_block)
  (with)
] @local.scope
(match else_block: (block) @local.scope)
(try_block else_block: (block) @local.scope)
(try_block then_block: (block) @local.scope)
(with else_block: (block) @local.scope)

(param name: (identifier) @local.definition.variable.parameter)
(lambdaparam name: (identifier) @local.definition.variable.parameter)

; only lower case identifiers are references
(
  (identifier) @local.reference
  (#match? @local.reference "^[a-z_][a-zA-Z_]*")
)
