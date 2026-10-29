; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The selection rules for cylc: runtime/queries/cylc/textobjects.scm of
; Helix (https://github.com/helix-editor/helix) at commit 9bcdf4d79d14,
; unchanged below this notice.

(comment) @comment.inside

(comment)+ @comment.around

(_
  brackets_open: _
  name: _?
  brackets_close: _
  _* @class.inside) @class.around

(setting
  value: _? @function.inside) @function.around

(graph_setting
  value: _? @function.inside) @function.around

(graph_string_content
  (graph_task) @entry.inside)

(task_parameter
  ((_) @parameter.inside
    .
    ","? @parameter.around) @parameter.around)
