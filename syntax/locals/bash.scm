; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The rules for local names in bash: runtime/queries/bash/locals.scm of
; Helix (https://github.com/YardQuit/helix_editor) at commit 9bcdf4d79d14,
; unchanged below this notice.

; Scopes

[
  (function_definition)
  (subshell)
] @local.scope

; Definitions

(variable_assignment
  name: (variable_name) @local.definition.variable)

; `local`/`declare`/`export x` etc. bind a bare name.
(declaration_command
  (variable_name) @local.definition.variable)

(for_statement
  variable: (variable_name) @local.definition.variable)

; References

(variable_name) @local.reference
