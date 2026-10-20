; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The rules for local names in tact: runtime/queries/tact/locals.scm of
; Helix (https://github.com/YardQuit/helix_editor) at commit 9bcdf4d79d14,
; unchanged below this notice.

; Scopes       @local.scope
; -------------------------

[
  (static_function)
  (init_function)
  (bounced_function)
  (receive_function)
  (external_function)
  (function)
  (block_statement)
] @local.scope

; Definitions  @local.definition
; ------------------------------

(parameter
  name: (identifier) @local.definition.variable.parameter)

(constant
  name: (identifier) @local.definition.constant)

; References   @local.reference
; -----------------------------

(self) @local.reference

(value_expression (identifier) @local.reference)

(lvalue (identifier) @local.reference)
