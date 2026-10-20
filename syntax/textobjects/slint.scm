; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The selection rules for slint: runtime/queries/slint/textobjects.scm of
; Helix (https://github.com/YardQuit/helix_editor) at commit 9bcdf4d79d14,
; unchanged below this notice.

(function_definition
  (imperative_block) @function.inside) @function.around

(callback_event
  (imperative_block) @function.inside) @function.around

(property
  (imperative_block) @function.inside) @function.around

(struct_definition
  (struct_block) @class.inside) @class.around

(enum_definition
  (enum_block) @class.inside) @class.around

(global_definition
  (global_block) @class.inside) @class.around

(component_definition
  (block) @class.inside) @class.around

(component_definition
  (block) @class.inside) @class.around

(comment) @comment.around

(typed_identifier
  name: (_) @parameter.inside) @parameter.around

(callback
  arguments: (_) @parameter.inside)

(string_value
  "\"" . (_) @entry.inside . "\"") @entry.around

