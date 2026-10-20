; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The rules for local names in wesl: runtime/queries/wesl/locals.scm of
; Helix (https://github.com/YardQuit/helix_editor) at commit 9bcdf4d79d14,
; unchanged below this notice.

; Scopes

[
  (global_decl)
  (switch_body)
  (compound_statement)
] @local.scope

; Definitions

(param
  (identifier) @local.definition.variable.parameter)

; References

(identifier) @local.reference
; (type_specifier) @local.reference

