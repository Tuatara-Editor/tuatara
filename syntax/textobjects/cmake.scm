; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The selection rules for cmake: runtime/queries/cmake/textobjects.scm of
; Helix (https://github.com/YardQuit/helix_editor) at commit 9bcdf4d79d14,
; unchanged below this notice.

[
  (macro_def)
  (function_def)
] @function.around

(argument) @parameter.inside

[
  (bracket_comment)
  (line_comment)
] @comment.inside

(line_comment)+ @comment.around

(bracket_comment) @comment.around