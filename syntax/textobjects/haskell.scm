; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The selection rules for haskell: runtime/queries/haskell/textobjects.scm of
; Helix (https://github.com/helix-editor/helix) at commit 9bcdf4d79d14,
; unchanged below this notice.

(comment) @comment.inside
(comment)+ @comment.around

(newtype
	(newtype_constructor
		(_) @class.inside)) @class.around
(data_type
	constructors: (_) @class.inside) @class.around
(decl/function
	(match expression:(_) @function.inside)) @function.around
(lambda
	expression:(_) @function.inside) @function.around

(decl/function
	patterns: (patterns
		(_) @parameter.inside))

(expression/lambda
	patterns: (patterns
	(_) @parameter.inside))

(decl/function
	(infix
		(pattern) @parameter.inside))
