; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The colouring rules for thrift: runtime/queries/thrift/highlights.scm of
; Helix (https://github.com/YardQuit/helix_editor) at commit 9bcdf4d79d14,
; unchanged below this notice.

; Variables

((identifier) @variable)

; Includes

[
  "include"
  "cpp_include"
] @keyword

; Function

(function_definition
  (identifier) @function)

; Fields

(field (identifier) @variable.other.member)

; Parameters

(function_definition
  (parameters
    (parameter (identifier) @variable.parameter)))

(throws
  (parameters
    (parameter (identifier) @keyword.control.exception)))

; Types

(typedef_identifier) @type
(struct_definition
  "struct" (identifier) @type)

(union_definition
  "union" (identifier) @type)

(exception_definition
  "exception" (identifier) @type)

(service_definition
  "service" (identifier) @type)

(interaction_definition
  "interaction" (identifier) @type)

(type
  type: (identifier) @type)

(definition_type
  type: (identifier) @type)

; Constants

(const_definition (identifier) @constant)

(enum_definition "enum"
  . (identifier) @type
  "{" (identifier) @constant "}")

; Builtin Types

(primitive) @type.builtin

[
  "list"
  "map"
  "set"
  "sink"
  "stream"
  "void"
] @type.builtin

; Namespace

(namespace_declaration
  (namespace_scope) @tag
  [(namespace) @namespace (_ (identifier) @namespace)])

; Attributes

(annotation_definition
  (annotation_identifier (identifier) @attribute))
(fb_annotation_definition
  "@" @attribute (annotation_identifier (identifier) @attribute)
  (identifier)? @attribute)
(namespace_uri (string) @attribute)

; Operators

[
  "="
  "&"
] @operator

; Exceptions

[
  "throws"
] @keyword.control.exception

; Keywords

[
  "enum"
  "exception"
  "extends"
  "interaction"
  "namespace"
  "senum"
  "service"
  "struct"
  "typedef"
  "union"
  "uri"
] @keyword

; Deprecated Keywords

[
  "cocoa_prefix"
  "cpp_namespace"
  "csharp_namespace"
  "delphi_namespace"
  "java_package"
  "perl_package"
  "php_namespace"
  "py_module"
  "ruby_namespace"
  "smalltalk_category"
  "smalltalk_prefix"
  "xsd_all"
  "xsd_attrs"
  "xsd_namespace"
  "xsd_nillable"
  "xsd_optional"
] @keyword

; Extended Keywords
[
  "package"
  "performs"
] @keyword

[
  "async"
  "oneway"
] @keyword

; Qualifiers

[
  "client"
  "const"
  "idempotent"
  "optional"
  "permanent"
  "readonly"
  "required"
  "safe"
  "server"
  "stateful"
  "transient"
] @type.directive

; Literals

(string) @string

(escape_sequence) @constant.character.escape

(namespace_uri
  (string) @string.special)

(number) @constant.numeric.integer

(double) @constant.numeric.float

(boolean) @constant.builtin.boolean

; Typedefs

(typedef_identifier) @type.definition

; Punctuation

[
  "*"
] @punctuation.special

["{" "}"] @punctuation.bracket

["(" ")"] @punctuation.bracket

["[" "]"] @punctuation.bracket

["<" ">"] @punctuation.bracket

[
  "."
  ","
  ";"
  ":"
] @punctuation.delimiter

; Comments

(comment) @comment

