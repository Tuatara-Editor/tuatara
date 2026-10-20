; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The colouring rules for nginx: runtime/queries/nginx/highlights.scm of
; Helix (https://github.com/YardQuit/helix_editor) at commit 9bcdf4d79d14,
; unchanged below this notice.

(comment) @comment

(block_directive
	(directive) @type)

[
	"{"
	"}"
	"("
	")"
	"["
	"]"
] @punctuation.bracket

(simple_directive
	(directive) @function)

[
	";"
] @punctuation.delimiter

((generic) @keyword
 (#any-of? @keyword
 	"on"
 	"off"
 	"any"
 	"auto"))

(modifier) @operator

(generic) @variable

(string) @string

(number) @constant.numeric
(metric) @constant.numeric

(variable) @variable.parameter

(regex) @string

(modifier) @keyword.operator

(lua_block_directive
	"access_by_lua_block" @function)
