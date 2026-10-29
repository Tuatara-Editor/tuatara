; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The selection rules for prisma: runtime/queries/prisma/textobjects.scm of
; Helix (https://github.com/helix-editor/helix) at commit 9bcdf4d79d14,
; unchanged below this notice.

(model_declaration
  ((statement_block) @class.inside)) @class.around

(call_expression
  (arguments (_) @parameter.inside . ","? @parameter.around) @parameter.around)

(column_declaration) @entry.around

(array (_) @entry.around)

(assignment_expression
  (_) @entry.inside) @entry.around

(developer_comment) @comment.inside

(developer_comment)+ @comment.around

