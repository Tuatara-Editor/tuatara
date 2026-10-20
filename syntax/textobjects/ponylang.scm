; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The selection rules for ponylang: runtime/queries/ponylang/textobjects.scm of
; Helix (https://github.com/YardQuit/helix_editor) at commit 9bcdf4d79d14,
; unchanged below this notice.

;; Queries for helix to select textobjects: https://docs.helix-editor.com/usage.html#textobjects
;;  function.inside
;; function.around
;; class.inside
;; class.around
;; test.inside
;; test.around
;; parameter.inside
;; comment.inside
;; comment.around

;; Queries for navigating using textobjects

[
  (line_comment)
  (block_comment)
] @comment.inside

(line_comment)+ @comment.around
(block_comment) @comment.around

(entity members: (members)? @class.inside) @class.around
(object members: (members)? @class.inside) @class.around

(method
  body: (block)? @function.inside
) @function.around
(behavior
  body: (block)? @function.inside
) @function.around
(constructor
  body: (block)? @function.inside
) @function.around
(lambda
  body: (block)? @function.inside
) @function.around

(params
  ((_) @parameter.inside . ","? @parameter.around) @parameter.around
)
(lambda
  params: ((_) @parameter.inside . ","? @parameter.around) @parameter.around
)
(typeargs
  ((_) @parameter.inside . ","? @parameter.around) @parameter.around
)
(typeparams
  ((_) @parameter.inside . ","? @parameter.around) @parameter.around
)
(arguments
  positional: (positional_args
                ((_) @parameter.inside . ","? @parameter.around)? @parameter.around)
  ; TODO: get named args right
  named: (named_args ((_) @parameter.inside . ","? @parameter.around)? @parameter.around)
)

(
  (entity
    provides: (type (nominal_type name: (identifier) @_provides))
    members: (members) @test.inside
  ) @test.around
  (#eq? @_provides "UnitTest")
)

