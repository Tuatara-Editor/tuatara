; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The selection rules for perl: runtime/queries/perl/textobjects.scm of
; Helix (https://github.com/helix-editor/helix) at commit 9bcdf4d79d14,
; unchanged below this notice.

(subroutine_declaration_statement
  body: (_) @function.inside) @function.around
(anonymous_subroutine_expression
  body: (_) @function.inside) @function.around

(package_statement) @class.around
(package_statement
  (block) @class.inside)

(list_expression
  (_) @parameter.inside)

(comment) @comment.around
(pod) @comment.around
