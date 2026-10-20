; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The selection rules for koto: runtime/queries/koto/textobjects.scm of
; Helix (https://github.com/YardQuit/helix_editor) at commit 9bcdf4d79d14,
; unchanged below this notice.

(comment) @comment.inside

(comment)+ @comment.around

(function
  body: (_) @function.inside) @function.around

(args
  ((arg) @parameter.inside . ","? @parameter.around) @parameter.around)

(call_args
  ((call_arg) @parameter.inside . ","? @parameter.around) @parameter.around)

(map
  ((entry_inline) @entry.inside . ","? @entry.around) @entry.around)

(map_block
  ((entry_block) @entry.inside) @entry.around)

(list
  ((element) @entry.inside . ","? @entry.around) @entry.around)

(tuple
  (_) @entry.around)

(assign
  (meta (test))
  (function body: (_) @test.inside)
) @test.around

(entry_block
  key: (meta (test))
  value: (function body: (_) @test.inside)
) @test.around
