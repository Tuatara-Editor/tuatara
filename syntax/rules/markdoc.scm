; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The colouring rules for markdoc: runtime/queries/markdoc/highlights.scm of
; Helix (https://github.com/helix-editor/helix) at commit 9bcdf4d79d14,
; unchanged below this notice.

tag_name: (identifier) @tag
(tag_self_closing "/" @tag)
(tag_close "/" @tag)
([(tag_start) (tag_end) "="] @tag)
(attribute [key : (identifier)] @attribute)
(attribute [shorthand : (identifier)]  @attribute)
(variable [variable : (identifier) (variable_sigil)] @variable)
(variable_tail property : (identifier) @variable.other.member)
(function function_name : (identifier) @function)
(function_parameter_named parameter : (identifier) @variable.parameter)

(hash_key key: (identifier) @variable.other.member)
(string) @string
(number) @constant.numeric
(boolean) @constant.builtin.boolean
(null) @constant.builtin
