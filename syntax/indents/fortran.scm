; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The indentation rules for fortran: runtime/queries/fortran/indents.scm of
; Helix (https://github.com/helix-editor/helix) at commit 9bcdf4d79d14,
; unchanged below this notice.

[
  (module)
  (submodule)
  (program)
  (subroutine)
  (module_procedure)
  (function)
  ; (interface)
  (if_statement)
  (do_loop)
  (where_statement)
  (derived_type_definition)
  (enum)
] @indent

[
  (end_module_statement)
  (end_submodule_statement)
  (end_program_statement)
  (end_subroutine_statement)
  (end_module_procedure_statement)
  (end_function_statement)
  ; (end_interface_statement)
  (end_if_statement)
  (end_do_loop_statement)
  (else_clause)
  (elseif_clause)
  (end_type_statement)
  (end_enum_statement)
  (end_where_statement)
] @outdent
