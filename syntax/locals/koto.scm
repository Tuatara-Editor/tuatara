; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The rules for local names in koto: runtime/queries/koto/locals.scm of
; Helix (https://github.com/YardQuit/helix_editor) at commit 9bcdf4d79d14,
; unchanged below this notice.

; Scopes
(module (_) @local.scope)

(function
  body: (_) @local.scope)

; Definitions
(arg
  (identifier) @local.definition.variable.parameter)

(arg
  (variable (identifier)) @local.definition.variable.parameter)

(import_item
  (identifier) @local.definition.namespace)

(entry_block
  (identifier) @local.definition.variable.other.member)

(entry_inline
  (identifier) @local.definition.variable.other.member)

; References
(identifier) @local.reference
