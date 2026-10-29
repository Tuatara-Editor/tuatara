; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The selection rules for picat: runtime/queries/picat/textobjects.scm of
; Helix (https://github.com/helix-editor/helix) at commit 9bcdf4d79d14,
; unchanged below this notice.

[
  (function_definition
    [(function_rule) (function_fact)] @function.inside)
  (predicate_definition
    [(predicate_rule) (predicate_fact)] @function.inside)
  (actor_definition
    [(action_rule) (nonbacktrackable_predicate_rule)] @function.inside)
] @function.around

(import_declaration
  (atom) @function.around)

(parameters
  ((_) @parameter.inside . ","? @parameter.around) @parameter.around)

(arguments
  ((_) @parameter.inside . ","? @parameter.around) @parameter.around)

(comment) @comment.inside
(comment)+ @comment.around

(array_expression
  (_) @entry.around)

(list_expression
  (_) @entry.around)
