; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The colouring rules for amber: runtime/queries/amber/highlights.scm of
; Helix (https://github.com/YardQuit/helix_editor) at commit 9bcdf4d79d14,
; unchanged below this notice.

(comment) @comment

[
    "if"
    "loop"
    "for"
    "return"
    "fun"
    "else"
    "then"
    "break"
    "continue"
    "and"
    "or"
    "not"
    "let"
    "pub"
    "main"
    "echo"
    "exit"
    "fun"
    "import"
    "from"
    "as"
    "in"
    "fail"
    "failed"
    "silent"
    "nameof"
    "is"
    "unsafe"
    "trust"
] @keyword

; Literals
(boolean) @constant.builtin.boolean
(number) @constant.numeric
(null) @constant.builtin
(string) @string
(status) @keyword
; Highlight only command delimiters, not content (bash injection handles content)
(command
  ["$"] @string)
(handler) @keyword
(block) @punctuation.delimiter
(variable_init) @keyword
(variable_assignment) @punctuation.delimiter
(variable) @variable
(escape_sequence) @constant.character.escape
(type_name_symbol) @type
(interpolation) @punctuation.delimiter
(reference) @keyword
(preprocessor_directive) @comment
(shebang) @comment
(function_definition
    name: (variable) @function.method)
(function_call
    name: (variable) @function.method)
(import_statement
    "pub" @keyword
    "import" @keyword
    "from" @keyword)
