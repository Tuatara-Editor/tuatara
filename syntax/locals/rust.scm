; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The rules for local names in rust: runtime/queries/rust/locals.scm of
; Helix (https://github.com/YardQuit/helix_editor) at commit 9bcdf4d79d14,
; unchanged below this notice.

; Scopes

[
  (function_item)
  (struct_item)
  (enum_item)
  (union_item)
  (type_item)
  (trait_item)
  (impl_item)
  (closure_expression)
  (block)
] @local.scope

; Definitions

(function_item
  (parameters
    (parameter
      pattern: (identifier) @local.definition.variable.parameter)))

(closure_parameters (identifier) @local.definition.variable.parameter)

; Mutable variables

(let_declaration
  (mutable_specifier)
  pattern: (identifier) @local.definition.variable.mutable)
(mut_pattern
  (mutable_specifier)
  (identifier) @local.definition.variable.mutable)

(parameter
  (mutable_specifier)
  pattern: (identifier) @local.definition.variable.parameter.mutable)

(self_parameter
  (mutable_specifier)
  (self) @local.definition.variable.builtin.mutable)

; References
(self) @local.reference
(identifier) @local.reference

; lifetimes / labels
(lifetime (identifier) @_)
(label (identifier) @_)

; == scoped function calls and function defs ==
; avoid coloring functions as variables
(call_expression
  function: (scoped_identifier
    name: (identifier) @_))
(generic_function
  function: (scoped_identifier
    name: (identifier) @_))

(function_item
  name: (identifier) @_)
(function_signature_item
  name: (identifier) @_)

; == other ==
(enum_variant (identifier) @_)
