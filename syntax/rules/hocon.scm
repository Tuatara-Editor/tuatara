; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The colouring rules for hocon: runtime/queries/hocon/highlights.scm of
; Helix (https://github.com/helix-editor/helix) at commit 9bcdf4d79d14,
; unchanged below this notice.

(comment) @comment

(null) @constant.builtin
[(true) (false)] @constant.builtin.boolean
(number) @constant.numeric
(string) @string
(multiline_string) @string
(string (escape_sequence) @constant.character.escape)
(unquoted_string) @string

(value [":" "=" "+=" ] @operator)

[ "(" ")" "[" "]" "{" "}" ]  @punctuation.bracket

(substitution (_) @string)
(substitution ["${" "${?" "}"] @punctuation.special)

[ 
  "url"
  "file"
  "classpath"
  "required"
] @function.builtin

(include) @keyword.directive

(unit) @keyword
(path (_) @keyword)
(unquoted_path "." @punctuation.delimiter)
[ "," ] @punctuation.delimiter

