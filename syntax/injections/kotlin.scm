; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The rules for text in other languages in kotlin: runtime/queries/kotlin/injections.scm of
; Helix (https://github.com/helix-editor/helix) at commit 9bcdf4d79d14,
; unchanged below this notice.

([
  (line_comment)
  (multiline_comment)
 ] @injection.content
	(#set! injection.language "comment"))

; There are 3 ways to define a regex
;    - "[abc]?".toRegex()
((call_expression
	(navigation_expression
		(string_literal
		   (string_content) @injection.content)
		(navigation_suffix
			((simple_identifier) @_function
			(#eq? @_function "toRegex")))))
	(#set! injection.language "regex"))

;    - Regex("[abc]?")
((call_expression
	((simple_identifier) @_function
	(#eq? @_function "Regex"))
	(call_suffix
		(value_arguments
			(value_argument
				(string_literal
					(string_content) @injection.content)))))
	(#set! injection.language "regex"))

;    - Regex.fromLiteral("[abc]?")
((call_expression
	(navigation_expression
		((simple_identifier) @_class
		(#eq? @_class "Regex"))
		(navigation_suffix
			((simple_identifier) @_function
			(#eq? @_function "fromLiteral"))))
	(call_suffix
		(value_arguments
			(value_argument
				(string_literal
					(string_content) @injection.content)))))
	(#set! injection.language "regex"))
