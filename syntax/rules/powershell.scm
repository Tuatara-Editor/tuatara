; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The colouring rules for powershell: runtime/queries/powershell/highlights.scm of
; Helix (https://github.com/helix-editor/helix) at commit 9bcdf4d79d14,
; unchanged below this notice.

[
  "if"
  "elseif"
  "else"
  "switch"
] @keyword.control.conditional

[
  "foreach"
  "for"
  "while"
  "do"
  "until"
] @keyword.control.repeat

[
  "break"
  "continue"
  "return"
] @keyword.control.return

"in" @keyword.operator

"function" @keyword.function

[
  "class"
  "enum"
] @keyword.storage.type

[
  "param"
  "dynamicparam"
  "begin"
  "process"
  "end"
  "filter"
  "workflow"
  "throw"
  "exit"
  "trap"
  "try"
  "catch"
  "finally"
  "data"
  "inlinescript"
  "parallel"
  "sequence"
] @keyword

[
  "-as"
  "-ccontains"
  "-ceq"
  "-cge"
  "-cgt"
  "-cle"
  "-clike"
  "-clt"
  "-cmatch"
  "-cne"
  "-cnotcontains"
  "-cnotlike"
  "-cnotmatch"
  "-contains"
  "-creplace"
  "-csplit"
  "-eq"
  "-ge"
  "-gt"
  "-icontains"
  "-ieq"
  "-ige"
  "-igt"
  "-ile"
  "-ilike"
  "-ilt"
  "-imatch"
  "-in"
  "-ine"
  "-inotcontains"
  "-inotlike"
  "-inotmatch"
  "-ireplace"
  "-is"
  "-isnot"
  "-isplit"
  "-join"
  "-le"
  "-like"
  "-lt"
  "-match"
  "-ne"
  "-not"
  "-notcontains"
  "-notin"
  "-notlike"
  "-notmatch"
  "-replace"
  "-shl"
  "-shr"
  "-split"
  "-and"
  "-or"
  "-xor"
  "-band"
  "-bor"
  "-bxor"
  "+"
  "-"
  "*"
  "/"
  "%"
  "++"
  "--"
  "!"
  "\\"
  ".."
  "|"
] @operator

(assignement_operator) @operator

[
  "("
  ")"
  "{"
  "}"
  "["
  "]"
] @punctuation.bracket

[
  ";"
  ","
  "::"
] @punctuation.delimiter

(string_literal) @string

(integer_literal) @constant.numeric
(real_literal) @constant.numeric

(command
  command_name: (command_name) @function)

(function_name) @function

(invokation_expression
  (member_name) @function)

(member_access
  (member_name) @variable.other.member)

(command_invokation_operator) @operator

(type_spec) @type

(variable) @variable

(comment) @comment

(array_expression) @punctuation.bracket

(assignment_expression
  value: (pipeline) @variable)

(format_operator) @operator

(command_parameter) @variable.parameter

(command_elements) @variable.builtin

(generic_token) @variable
