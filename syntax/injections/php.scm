; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The rules for text in other languages in php: runtime/queries/php/injections.scm of
; Helix (https://github.com/helix-editor/helix) at commit 9bcdf4d79d14,
; unchanged below this notice.

((text) @injection.content
 (#set! injection.language "html")
 (#set! injection.combined))

((comment) @injection.content
 (#set! injection.language "comment"))

((function_call_expression
 function: (name) @_function
 arguments: (arguments . (argument (_ (string_content) @injection.content))))
 (#match? @_function "^preg_")
 (#set! injection.language "regex"))

((function_call_expression
 function: (name) @_function
 arguments: (arguments (_) (argument (_ (string_content) @injection.content))))
 (#match? @_function "^mysqli_")
 (#set! injection.language "sql"))

((member_call_expression
 object: (_)
 name: (name) @_function
 arguments: (arguments . (argument (_ (string_content) @injection.content))))
 (#match? @_function "^(prepare|query)$")
 (#set! injection.language "sql"))
