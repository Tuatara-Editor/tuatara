; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The colouring rules for qml: runtime/queries/qml/highlights.scm of
; Helix (https://github.com/YardQuit/helix_editor) at commit 9bcdf4d79d14,
; unchanged below this notice.

(comment) @comment

(ui_import
  source: _ @namespace
  version: _? @constant
  alias: _? @namespace)

(ui_pragma
  name: (identifier) @attribute
  value: (identifier)? @constant)

(ui_annotation
  "@" @punctuation
  type_name: _ @type)

;;; Declarations

(enum_declaration
  name: (identifier) @type)

(enum_assignment
  name: (identifier) @constant
  value: _ @constant)

(enum_body
  name: (identifier) @constant)

(ui_inline_component
  name: (identifier) @type)

(ui_object_definition
  type_name: _ @type)

(ui_object_definition_binding
  type_name: _ @type
  name: _ @variable.other.member)

(ui_property
  type: _ @type
  name: (identifier) @variable.other.member)

(ui_signal
  name: (identifier) @function)

(ui_signal_parameter
  name: (identifier) @variable.parameter
  type: _ @type)

(ui_signal_parameter
  type: _ @type
  name: (identifier) @variable.parameter);;; Properties and bindings

;;; Bindings

(ui_binding
  name: _ @variable.other.member)

;;; Other

[
  "("
  ")"
  "{"
  "}"
] @punctuation.bracket

(ui_list_property_type [
  "<"
  ">"
] @punctuation.bracket)

[
  ","
  "."
  ":"
] @punctuation.delimiter

[
  "as"
  "component"
  "default"
  "enum"
  "import"
  "on"
  "pragma"
  "property"
  "readonly"
  "required"
  "signal"
] @keyword
