; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The colouring rules for twig: runtime/queries/twig/highlights.scm of
; Helix (https://github.com/helix-editor/helix) at commit 9bcdf4d79d14,
; unchanged below this notice.

(comment) @comment

(filter_identifier) @function.method
(function_identifier) @function.method
(test) @function.builtin
(variable) @variable
(string) @string
(interpolated_string) @string
(operator) @operator
(number) @constant.numeric.integer
(boolean) @constant.builtin.boolean
(null) @constant.builtin
(keyword) @keyword
(attribute) @attribute
(tag) @tag
(conditional) @keyword.control.conditional
(repeat) @keyword.control.repeat
(method) @function.method
(parameter) @variable.parameter

[
    "{{"
    "}}"
    "{{-"
    "-}}"
    "{{~"
    "~}}"
    "{%"
    "%}"
    "{%-"
    "-%}"
    "{%~"
    "~%}"
] @keyword

[
    ","
    "."
    "?"
    ":"
    "="
] @punctuation.delimiter

[
    "("
    ")"
    "["
    "]"
    "{"
] @punctuation.bracket

(hash [
    "}"
] @punctuation.bracket)

(interpolated_string [
    "#{"
    "}"
] @punctuation.delimiter)

