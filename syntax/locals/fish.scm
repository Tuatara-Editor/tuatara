; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The rules for local names in fish: runtime/queries/fish/locals.scm of
; Helix (https://github.com/helix-editor/helix) at commit 9bcdf4d79d14,
; unchanged below this notice.

; Scopes

(function_definition) @local.scope

; Definitions

; `set`/`read` bindings aren't a dedicated node (the name is an untyped `word`
; argument), so only the for-loop variable is reliably a definition.
(for_statement
  variable: (variable_name) @local.definition.variable)

; References

(variable_name) @local.reference
