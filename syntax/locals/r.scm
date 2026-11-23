; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The rules for local names in r: runtime/queries/r/locals.scm of
; Helix (https://github.com/helix-editor/helix) at commit 9bcdf4d79d14,
; unchanged below this notice.

; locals.scm

(function_definition) @local.scope

(parameters (parameter name: (identifier) @local.definition.variable.parameter))

(identifier) @local.reference
