; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The colouring rules for toml: runtime/queries/toml/highlights.scm of
; Helix (https://github.com/helix-editor/helix) at commit 9bcdf4d79d14,
; unchanged below this notice.

; Properties
;-----------

(table [
  (bare_key)
  (dotted_key)
  (quoted_key)
] @type)

(table_array_element [
  (bare_key)
  (dotted_key)
  (quoted_key)
] @type)

(pair [
  (bare_key)
  (dotted_key)
  (quoted_key)
] @variable.other.member)

; Literals
;---------

(boolean) @constant.builtin.boolean
(comment) @comment
(string) @string
(integer) @constant.numeric.integer
(float) @constant.numeric.float
(offset_date_time) @string.special
(local_date_time) @string.special
(local_date) @string.special
(local_time) @string.special

; Punctuation
;------------

"." @punctuation.delimiter
"," @punctuation.delimiter

"=" @operator

"[" @punctuation.bracket
"]" @punctuation.bracket
"[[" @punctuation.bracket
"]]" @punctuation.bracket
"{" @punctuation.bracket
"}" @punctuation.bracket
