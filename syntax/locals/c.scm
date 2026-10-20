; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The rules for local names in c: runtime/queries/c/locals.scm of
; Helix (https://github.com/YardQuit/helix_editor) at commit 9bcdf4d79d14,
; unchanged below this notice.

;; Scopes
(function_definition) @local.scope
(declaration) @local.scope

;; Definitions

; Parameters
; Up to 6 layers of declarators
(parameter_declaration
  (identifier) @local.definition.variable.parameter)
(parameter_declaration
  (_
    (identifier) @local.definition.variable.parameter))
(parameter_declaration
  (_
    (_
      (identifier) @local.definition.variable.parameter)))
(parameter_declaration
  (_
    (_
      (_
        (identifier) @local.definition.variable.parameter))))
(parameter_declaration
  (_
    (_
      (_
        (_
          (identifier) @local.definition.variable.parameter)))))
(parameter_declaration
  (_
    (_
      (_
        (_
          (_
            (identifier) @local.definition.variable.parameter))))))

;; References

(identifier) @local.reference

; A call's function name is not a variable reference; keep its class
; even when a same-named local is in scope.
(call_expression
  function: (identifier) @_)
