; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The selection rules for tact: runtime/queries/tact/textobjects.scm of
; Helix (https://github.com/helix-editor/helix) at commit 9bcdf4d79d14,
; unchanged below this notice.

; function.inside & around
; ------------------------

(static_function
  body: (_) @function.inside) @function.around

(init_function
  body: (_) @function.inside) @function.around

(bounced_function
  body: (_) @function.inside) @function.around

(receive_function
  body: (_) @function.inside) @function.around

(external_function
  body: (_) @function.inside) @function.around

(function
  body: (_) @function.inside) @function.around

; class.inside & around
; ---------------------

(struct
  body: (_) @class.inside) @class.around

(message
  body: (_) @class.inside) @class.around

(contract
  body: (_) @class.inside) @class.around

; NOTE: Marked as @definition.interface in tags, as it's semantically correct
(trait
  body: (_) @class.inside) @class.around

; parameter.inside & around
; -------------------------

(parameter_list
  ((_) @parameter.inside . ","? @parameter.around) @parameter.around)

(argument_list
  ((_) @parameter.inside . ","? @parameter.around) @parameter.around)

(instance_argument_list
  ((_) @parameter.inside . ","? @parameter.around) @parameter.around)

; comment.inside
; --------------

(comment) @comment.inside

; comment.around
; --------------

(comment)+ @comment.around