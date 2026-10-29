; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The rules naming the definitions and headings in picat: runtime/queries/picat/tags.scm of
; Helix (https://github.com/helix-editor/helix) at commit 9bcdf4d79d14,
; unchanged below this notice.

; tags.scm

(
  (comment)* @doc
  .
  [
    (function_definition
      [(function_rule name: (_) @name) @definition.function
       (function_fact name: (_) @name) @definition.function])
    (predicate_definition
      [(predicate_rule name: (_) @name) @definition.function
       (predicate_fact name: (_) @name) @definition.function])
    (actor_definition
      [(action_rule name: (_) @name) @definition.function
       (nonbacktrackable_predicate_rule name: (_) @name) @definition.function])
  ]
  (#strip! @doc "^%\\s*")
  (#select-adjacent! @doc @definition.function)
)

(function_call function: (_) @name @reference.call)

(dot_expression right: (_) @reference.call)

(import_declaration (_) @name) @reference.module

(module_declaration (_) @name) @definition.module
