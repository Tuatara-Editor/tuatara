; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The selection rules for eiffel: runtime/queries/eiffel/textobjects.scm of
; Helix (https://github.com/YardQuit/helix_editor) at commit 9bcdf4d79d14,
; unchanged below this notice.

[
  (comment)+
  (header_comment)+
] @comment.around
[
  (comment)
  (header_comment)
] @comment.inside
(formal_arguments) @parameter.around
(entity_declaration_group) @parameter.inside
(attribute_or_routine) @function.around
(feature_body) @function.inside
(class_declaration) @class.around
(feature_clause) @class.inside

