; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The rules for local names in vhdl: runtime/queries/vhdl/locals.scm of
; Helix (https://github.com/YardQuit/helix_editor) at commit 9bcdf4d79d14,
; unchanged below this notice.

; Scopes

[
  (subprogram_definition)
  (process_statement)
] @local.scope

; Definitions

; Subprogram parameters: each interface declaration's identifier_list holds the
; parameter name(s), reachable regardless of the constant:/generic: field label.
(parameter_list_specification
  (interface_list
    (_
      (identifier_list
        (identifier) @local.definition.variable.parameter))))

; Locally declared signals/variables/constants.
(signal_declaration
  (identifier_list
    (identifier) @local.definition.variable))
(variable_declaration
  (identifier_list
    (identifier) @local.definition.variable))
(constant_declaration
  (identifier_list
    (identifier) @local.definition.constant))

; References

; A use of a name is `(name (identifier) ...)`; the base identifier may resolve.
; Member identifiers live inside (selection ...) and are excluded by this shape.
(name
  (identifier) @local.reference)
