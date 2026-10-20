; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The rules for text in other languages in qml: runtime/queries/qml/injections.scm of
; Helix (https://github.com/YardQuit/helix_editor) at commit 9bcdf4d79d14,
; unchanged below this notice.

((comment) @injection.content
  (#set! injection.language "comment"))

([
    (empty_statement)
    (expression_statement)
    (function_declaration)
    (generator_function_declaration)
    (statement_block)
    (switch_statement)
    (try_statement)
    (variable_declaration)
    (with_statement)
  ] @injection.content
  (#set! injection.include-children)
  (#set! injection.language "javascript"))
