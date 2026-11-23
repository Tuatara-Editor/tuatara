; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The selection rules for ruby: runtime/queries/ruby/textobjects.scm of
; Helix (https://github.com/helix-editor/helix) at commit 9bcdf4d79d14,
; unchanged below this notice.

; Class and Modules
(class
  body: (_)? @class.inside) @class.around

(singleton_class
  value: (_)
  (_)+ @class.inside) @class.around

(call
  receiver: (constant) @class_const
  method: (identifier) @class_method
  (#match? @class_const "Class")
  (#match? @class_method "new")
  (do_block (_)+ @class.inside)) @class.around
  
(module
  body: (_)? @class.inside) @class.around

; Functions and Blocks
(singleton_method
  body: (_)? @function.inside) @function.around

(method
  body: (_)? @function.inside) @function.around

(do_block
  body: (_)? @function.inside) @function.around

(block
  body: (_)? @function.inside) @function.around

; Parameters      
(method_parameters
  (_) @parameter.inside) @parameter.around
        
(block_parameters 
  (_) @parameter.inside) @parameter.around
        
(lambda_parameters 
  (_) @parameter.inside) @parameter.around

; Comments
(comment) @comment.inside 
(comment)+ @comment.around

(pair
  (_) @entry.inside) @entry.around

(array
  (_) @entry.around)

(string_array
  (_) @entry.around)

(symbol_array
  (_) @entry.around)
