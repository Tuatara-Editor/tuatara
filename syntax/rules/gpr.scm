; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The colouring rules for gpr: runtime/queries/gpr/highlights.scm of
; Helix (https://github.com/helix-editor/helix) at commit 9bcdf4d79d14,
; unchanged below this notice.

[ "abstract" "all" "at"
  "case"
  "end" "extends" "external" "external_as_list"
  "for"
  "is"
  "limited"
  "null"
  "others"
  "package"
  ;; "project"
  "renames"
  "type"
  "use"
  "when"
  "with"
  ] @keyword

;; Avoid highlighting Project in Project'Project_Dir
(project_declaration "project" @keyword)

;; highlight qualifiers as keywords (not all qualifiers are actual keywords)
(project_qualifier _ @keyword)

[":=" "&" "|" "=>"] @operator

(comment) @comment
(string_literal) @string
(numeric_literal) @constant.numeric

;; Type
(typed_string_declaration name: (identifier) @type)
(variable_declaration type: (name (identifier) @type .))

;; Variable
(variable_declaration name: (identifier) @variable)
(variable_reference (name (identifier) @variable .) .)

;; Function
(builtin_function_call name: _ @function.builtin)

;; Attribute
(attribute_declaration name: (identifier) @attribute)
(attribute_reference (identifier) @attribute)

;; Package
(variable_reference (name (identifier) @function .) "'")
(package_declaration
 [ name: (identifier) @function
   endname: (identifier) @function
   origname: (name (identifier) @function .)
   basename: (name (identifier) @function .)])
