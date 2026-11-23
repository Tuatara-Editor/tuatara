; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The selection rules for swift: runtime/queries/swift/textobjects.scm of
; Helix (https://github.com/helix-editor/helix) at commit 9bcdf4d79d14,
; unchanged below this notice.

(class_declaration
  body: (_) @class.inside) @class.around

(protocol_declaration
  body: (_) @class.inside) @class.around

(function_declaration
  body: (_) @function.inside) @function.around

(init_declaration
  body: (_) @function.inside) @function.around

(deinit_declaration
  body: (_) @function.inside) @function.around

(subscript_declaration
  (computed_property) @function.inside) @function.around

; Closures: the body lives in a `statements` child (the `{ … in }` signature is
; separate), so `*.inside` is the body alone; optional so empty closures match.
(lambda_literal
  (statements)? @function.inside) @function.around

(parameter
  (_) @parameter.inside) @parameter.around

(lambda_parameter
  (_) @parameter.inside) @parameter.around

(value_argument
  (_) @parameter.inside) @parameter.around

[
  (comment)
  (multiline_comment)
] @comment.inside

(comment)+ @comment.around

(multiline_comment) @comment.around
