; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The rules for local names in ruby: runtime/queries/ruby/locals.scm of
; Helix (https://github.com/YardQuit/helix_editor) at commit 9bcdf4d79d14,
; unchanged below this notice.

; Method, class, module and singleton-class bodies don't see locals from the
; enclosing scope in Ruby, so they must not inherit.
([
  (method)
  (singleton_method)
  (class)
  (module)
  (singleton_class)
] @local.scope
 (#set! local.scope-inherits false))

[
  (lambda)
  (block)
  (do_block)
] @local.scope

(block_parameter (identifier) @local.definition.variable.parameter)
(block_parameters (identifier) @local.definition.variable.parameter)
(destructured_parameter (identifier) @local.definition.variable.parameter)
(hash_splat_parameter (identifier) @local.definition.variable.parameter)
(lambda_parameters (identifier) @local.definition.variable.parameter)
(method_parameters (identifier) @local.definition.variable.parameter)
(splat_parameter (identifier) @local.definition.variable.parameter)
(keyword_parameter name: (identifier) @local.definition.variable.parameter)
(optional_parameter name: (identifier) @local.definition.variable.parameter)

(identifier) @local.reference

; A method-call name is not a variable reference (the grammar only forms `call`
; when it's syntactically a call), so a same-named local must not capture it.
(call
  method: (identifier) @_)
