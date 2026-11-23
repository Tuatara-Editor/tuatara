; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The colouring rules for kdl: runtime/queries/kdl/highlights.scm of
; Helix (https://github.com/helix-editor/helix) at commit 9bcdf4d79d14,
; unchanged below this notice.

[
    (single_line_comment)
    (multi_line_comment)

    (node_comment)
    (node_field_comment)

    ; these do not show up as comments in Helix as they are also highlighted as
    ; normal nodes
    (node . (node_comment))
    (node_field . (node_field_comment))
] @comment

(node
    (identifier) @variable)

(prop (identifier) @attribute)

(type (_) @type) @punctuation.bracket

(keyword) @keyword

(string) @string
(number) @constant.numeric
(boolean) @constant.builtin.boolean

"." @punctuation.delimiter

"=" @operator

"{" @punctuation.bracket
"}" @punctuation.bracket
