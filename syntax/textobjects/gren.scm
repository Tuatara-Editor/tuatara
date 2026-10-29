; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The selection rules for gren: runtime/queries/gren/textobjects.scm of
; Helix (https://github.com/helix-editor/helix) at commit 9bcdf4d79d14,
; unchanged below this notice.


(line_comment) @comment.inside
(line_comment)+ @comment.around
(block_comment) @comment.inside
(block_comment)+ @comment.around

((type_annotation)?
  (value_declaration
    (function_declaration_left (lower_case_identifier))
    (eq)
    (_) @function.inside
  )
) @function.around

(parenthesized_expr
  (anonymous_function_expr
    (
      (arrow)
      (_) @function.inside
    )
  )
) @function.around

(value_declaration
  (function_declaration_left
    (lower_pattern
      (lower_case_identifier) @parameter.inside @parameter.around
    )
  )
)

(value_declaration
  (function_declaration_left
    (pattern) @parameter.inside @parameter.around
  )
)

(value_declaration
  (function_declaration_left
    (record_pattern
      (lower_pattern
        (lower_case_identifier) @parameter.inside
      )
    ) @parameter.around
  )
)

(parenthesized_expr
  (anonymous_function_expr
    (
      (backslash)
      (pattern) @parameter.inside
      (arrow)
    )
  )
)
