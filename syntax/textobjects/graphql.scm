; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The selection rules for graphql: runtime/queries/graphql/textobjects.scm of
; Helix (https://github.com/helix-editor/helix) at commit 9bcdf4d79d14,
; unchanged below this notice.

(type_definition) @class.around

(executable_definition) @function.around

(arguments_definition
  (input_value_definition) @parameter.inside @parameter.movement)

(arguments
  (argument) @parameter.inside @parameter.movement)

(selection
  [(field) (fragment_spread)] @entry.around)

(selection
  (field (selection_set) @entry.inside))

(field_definition
  (_) @entry.inside) @entry.around

(input_fields_definition
  (input_value_definition ) @entry.around)

(enum_value) @entry.around
