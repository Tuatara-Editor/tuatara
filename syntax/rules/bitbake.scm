; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The colouring rules for bitbake: runtime/queries/bitbake/highlights.scm of
; Helix (https://github.com/YardQuit/helix_editor) at commit 9bcdf4d79d14,
; unchanged below this notice.


; variables
(variable_assignment (identifier) @variable.other.member)
(variable_assignment (concatenation (identifier) @variable.other.member))
(unset_statement (identifier) @variable.other.member)
(export_statement (identifier) @variable.other.member)
(variable_expansion (identifier) @variable.other.member)
(python_function_definition (parameters (python_identifier) @variable.other.member))

(variable_assignment (override) @keyword.storage.modifier)
(overrides_statement (identifier) @keyword.storage.modifier)
(flag) @keyword.storage.modifier

[
  "="
  "?="
  "??="
  ":="
  "=+"
  "+="
  ".="
  "=."

] @operator

[ "(" ")" "{" "}" "[" "]" ] @punctuation.bracket
(variable_expansion [ "${" "}" ] @punctuation.special)

[
  "noexec"
  "INHERIT"
  "OVERRIDES"
  "$BB_ENV_PASSTHROUGH"
  "$BB_ENV_PASSTHROUGH_ADDITIONS"
] @variable.builtin

; functions

(python_function_definition (python_identifier) @function)
(anonymous_python_function (identifier) @function)
(function_definition (identifier) @function)
(export_functions_statement (identifier) @function)
(addtask_statement (identifier) @function)
(deltask_statement (identifier) @function)
(addhandler_statement (identifier) @function)
(function_definition (override) @keyword.storage.modifier)

[
  "addtask"
  "deltask"
  "addhandler"
  "unset"
  "EXPORT_FUNCTIONS"
  "python"
  "def"
] @keyword.function

[
  "append"
  "prepend"
  "remove"

  "before"
  "after"
] @keyword.operator

; imports

[
  "inherit"
  "include"
  "require"
  "export"
  "import"
] @keyword.control.import

(inherit_path) @namespace
(include_path) @namespace


(string) @string
(comment) @comment
