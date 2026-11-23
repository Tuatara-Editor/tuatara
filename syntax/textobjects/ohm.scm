; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The selection rules for ohm: runtime/queries/ohm/textobjects.scm of
; Helix (https://github.com/helix-editor/helix) at commit 9bcdf4d79d14,
; unchanged below this notice.

; See: https://docs.helix-editor.com/guides/textobject.html

; function.inside & around
; ------------------------

(rule
  body: (_) @function.inside) @function.around

; class.inside & around
; ---------------------

(grammar
  body: (_) @class.inside) @class.around

; parameter.inside & around
; -------------------------

(formals
  ((_) @parameter.inside . ","? @parameter.around) @parameter.around)

(rule_body
  ((_) @parameter.inside . "|"? @parameter.around) @parameter.around)

(params
  ((_) @parameter.inside . ","? @parameter.around) @parameter.around)

(alt
  ((_) @parameter.inside . "|"? @parameter.around) @parameter.around)

; comment.inside
; --------------

(multiline_comment)+ @comment.inside
(singleline_comment)+ @comment.inside

; comment.around
; --------------

(multiline_comment)+ @comment.around
(singleline_comment)+ @comment.around
