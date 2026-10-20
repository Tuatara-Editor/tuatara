; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The rules for text in other languages in julia: runtime/queries/julia/injections.scm of
; Helix (https://github.com/YardQuit/helix_editor) at commit 9bcdf4d79d14,
; unchanged below this notice.

(
  (source_file
    (string_literal) @injection.content
    .
    [
      (module_definition)
      (function_definition)
      (macro_definition)
      (primitive_definition)
      (abstract_definition)
      (struct_definition)
      (assignment)
      (const_statement)
    ])
  (#set! injection.language "markdown"))

(
  [
    (line_comment) 
    (block_comment)
  ] @injection.content
  (#set! injection.language "comment"))

(
  [
    (command_literal)
    (prefixed_command_literal)
  ] @injection.content
  (#set! injection.language "bash"))

(
  (prefixed_string_literal
    prefix: (identifier) @function.macro) @injection.content
  (#eq? @function.macro "r")
  (#set! injection.language "regex"))

(
  (prefixed_string_literal
    prefix: (identifier) @function.macro) @injection.content
  (#eq? @function.macro "md")
  (#set! injection.language "markdown"))
