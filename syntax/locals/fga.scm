; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The rules for local names in fga: runtime/queries/fga/locals.scm of
; Helix (https://github.com/YardQuit/helix_editor) at commit 9bcdf4d79d14,
; unchanged below this notice.

(condition_declaration) @local.scope

(param
  name: (identifier) @local.definition.variable.parameter)

(condition_body
  (identifier) @local.reference)

(parenthesized_condition
  (identifier) @local.reference)

(bracket_condition
  (identifier) @local.reference)

(braced_condition
  (identifier) @local.reference)
