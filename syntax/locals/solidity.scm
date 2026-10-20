; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The rules for local names in solidity: runtime/queries/solidity/locals.scm of
; Helix (https://github.com/YardQuit/helix_editor) at commit 9bcdf4d79d14,
; unchanged below this notice.

(function_definition) @local.scope
(constructor_definition) @local.scope
(block_statement) @local.scope

(function_definition (parameter name: (identifier) @local.definition.variable.parameter))
(constructor_definition (parameter name: (identifier) @local.definition.variable.parameter))

(identifier) @local.reference
