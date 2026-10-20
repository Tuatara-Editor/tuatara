; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The rules for local names in tera: runtime/queries/tera/locals.scm of
; Helix (https://github.com/YardQuit/helix_editor) at commit 9bcdf4d79d14,
; unchanged below this notice.

(identifier) @local.reference
(assignment_expression
   left: (identifier) @local.definition.variable)
(macro_statement
  (parameter_list
    (identifier) @local.definition.variable.parameter))
(macro_statement) @local.scope
