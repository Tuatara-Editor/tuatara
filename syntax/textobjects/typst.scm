; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The selection rules for typst: runtime/queries/typst/textobjects.scm of
; Helix (https://github.com/YardQuit/helix_editor) at commit 9bcdf4d79d14,
; unchanged below this notice.

(let
  pattern: (call)
  value: (_) @function.inside) @function.around

(call
  (group
    ((_) @parameter.inside . ","? @parameter.around) @parameter.around))

(lambda
  pattern: 
    (group
      ((_) @parameter.inside . ","? @parameter.around) @parameter.around)
  value: (_) @function.inside) @function.around

(group
  [
    (tagged (_) @entry.inside)
    (_)
  ] @entry.around)

(comment) @comment.inside

(comment)+ @comment.around
