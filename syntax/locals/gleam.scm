; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The rules for local names in gleam: runtime/queries/gleam/locals.scm of
; Helix (https://github.com/YardQuit/helix_editor) at commit 9bcdf4d79d14,
; unchanged below this notice.

; Scopes
(function) @local.scope

(case_clause) @local.scope

; Definitions
(function_parameter name: (identifier) @local.definition.variable.parameter)

; References
(identifier) @local.reference
