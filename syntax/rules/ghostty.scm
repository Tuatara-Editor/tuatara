; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The colouring rules for ghostty: runtime/queries/ghostty/highlights.scm of
; Helix (https://github.com/YardQuit/helix_editor) at commit 9bcdf4d79d14,
; unchanged below this notice.

; Comments
(comment) @comment

; Keys
(property) @variable

; Values
(boolean) @constant.builtin.boolean


[
 (number)
 (adjustment)
] @constant.numeric

[
 "+"
 "="
 (keybind_trigger ">")
 (chain_operator)
] @operator

(":") @punctuation.delimiter

[
 (string)
 (color)
] @string

; (color) are hex values
(color "#" @punctuation.special
 (#eq? @punctuation.special "#"))

(path_value "?" @keyword.control.conditional
    (#eq? @keyword.control.conditional "?"))

; `palette`
(palette_index) @variable.other.member

; `path_directive`
(path_directive (property) @keyword.control.import)
(path_directive (path_value (string) @string.special.path ))


(action_name) @function.builtin
(action_argument (string) @variable.parameter ) 

; (tuple)
(tuple "," @punctuation.delimiter.special
       (#eq? @punctuation.delimiter.special ","))

; `keybind`
(keybind_value) @string.special

; clear is a special keyword that clear all existing keybind up to that point
((keybind_value) @keyword 
 (#eq? @keyword "clear"))

; NOTE: The order here matters!
[
 (key_qualifier)
 (keybind_modifier)
 (theme_variant)
 (command_modifier)
] @attribute

[
 (modifier_key)
 (key)
] @constant.builtin

(keybind_table) @namespace
