; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The colouring rules for graphql: runtime/queries/graphql/highlights.scm of
; Helix (https://github.com/YardQuit/helix_editor) at commit 9bcdf4d79d14,
; unchanged below this notice.

; Types
;------

(scalar_type_definition
  (name) @type)

(object_type_definition
  (name) @type)

(interface_type_definition
  (name) @type)

(union_type_definition
  (name) @type)

(enum_type_definition
  (name) @type)

(input_object_type_definition
  (name) @type)

(directive_definition
  (name) @type)

(directive_definition
  "@" @type)

(scalar_type_extension
  (name) @type)

(object_type_extension
  (name) @type)

(interface_type_extension
  (name) @type)

(union_type_extension
  (name) @type)

(enum_type_extension
  (name) @type)

(input_object_type_extension
  (name) @type)

(named_type
  (name) @type)

(directive) @type

; Properties
;-----------

(field
  (name) @variable.other.member)

(field
  (alias
    (name) @variable.other.member))

(field_definition
  (name) @variable.other.member)

(object_value
  (object_field
    (name) @variable.other.member))

(enum_value
  (name) @variable.other.member)

; Variable Definitions and Arguments 
;-----------------------------------

(operation_definition
  (name) @variable)

(fragment_name
  (name) @variable)

(input_fields_definition
  (input_value_definition
    (name) @variable.parameter))

(argument
  (name) @variable.parameter)

(arguments_definition
  (input_value_definition
    (name) @variable.parameter))

(variable_definition
  (variable) @variable.parameter)

(argument
  (value
    (variable) @variable))

; Constants
;----------

(string_value) @string

(int_value) @constant.numeric.integer

(float_value) @constant.numeric.float

(boolean_value) @constant.builtin.boolean

; Literals
;---------

(description) @comment

(comment) @comment

(directive_location
  (executable_directive_location) @type.builtin)

(directive_location
  (type_system_directive_location) @type.builtin)

; Keywords
;----------

[
  "query"
  "mutation"
  "subscription"
  "fragment"
  "scalar"
  "type"
  "interface"
  "union"
  "enum"
  "input"
  "extend"
  "directive"
  "schema"
  "on"
  "repeatable"
  "implements"
] @keyword

; Punctuation
;------------

[
 "("
 ")"
 "["
 "]"
 "{"
 "}"
] @punctuation.bracket

"=" @operator

"|" @punctuation.delimiter
"&" @punctuation.delimiter
":" @punctuation.delimiter

"..." @punctuation.special
"!" @punctuation.special
