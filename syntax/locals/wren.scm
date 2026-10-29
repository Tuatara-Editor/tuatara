; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The rules for local names in wren: runtime/queries/wren/locals.scm of
; Helix (https://github.com/helix-editor/helix) at commit 9bcdf4d79d14,
; unchanged below this notice.

[
	(method_definition)
] @local.scope

(name) @local.reference
(field) @local.reference
(static_field) @local.reference

(parameter) @local.definition.variable.parameter
