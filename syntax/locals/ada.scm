; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The rules for local names in ada: runtime/queries/ada/locals.scm of
; Helix (https://github.com/helix-editor/helix) at commit 9bcdf4d79d14,
; unchanged below this notice.

(compilation) @local.scope
(package_declaration) @local.scope
(package_body) @local.scope
(subprogram_declaration) @local.scope
(subprogram_body) @local.scope
(block_statement) @local.scope

(parameter_specification . (identifier) @local.definition.variable.parameter)

(identifier) @local.reference
