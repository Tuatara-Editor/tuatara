; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The selection rules for llvm: runtime/queries/llvm/textobjects.scm of
; Helix (https://github.com/helix-editor/helix) at commit 9bcdf4d79d14,
; unchanged below this notice.

(define
  body: (_) @function.inside) @function.around

(struct_type
  (struct_body) @class.inside) @class.around

(packed_struct_type
  (struct_body) @class.inside) @class.around

(array_type
  (array_vector_body) @class.inside) @class.around

(vector_type
  (array_vector_body) @class.inside) @class.around

(argument) @parameter.inside

(comment) @comment.inside

(comment)+ @comment.around
