; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The rules for local names in _typescript: runtime/queries/_typescript/locals.scm of
; Helix (https://github.com/helix-editor/helix) at commit 9bcdf4d79d14,
; unchanged below this notice.

; Scopes
;-------

[
  (type_alias_declaration)
  (class_declaration)
  (interface_declaration)
] @local.scope

; Definitions
;------------

(type_parameter
  name: (type_identifier) @local.definition.type.parameter)

; Javascript and Typescript Treesitter grammars deviate when defining the
; tree structure for parameters, so we need to address them in each specific
; language instead of ecma.

; (i: t)
; (i: t = 1)
(required_parameter
  (identifier) @local.definition.variable.parameter)

; (i?: t)
; (i?: t = 1) // Invalid but still possible to highlight.
(optional_parameter
  (identifier) @local.definition.variable.parameter)

; References
;-----------

(type_identifier) @local.reference
(identifier) @local.reference
