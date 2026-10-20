; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The rules for local names in luau: runtime/queries/luau/locals.scm of
; Helix (https://github.com/YardQuit/helix_editor) at commit 9bcdf4d79d14,
; unchanged below this notice.

[
  (block)
  (fn_stmt)
  (local_fn_stmt)
  (anon_fn)
  (for_range_stmt)
  (for_in_stmt)
] @local.scope

(_
  parameter_name: (name) @local.definition.variable.parameter
)

(binding
  variable_name: (name) @local.definition.variable
)

(var 
  variable_name: (name) @local.reference
)

; (call_stmt
;   .
;   method_table: (name) @local.reference
; )
