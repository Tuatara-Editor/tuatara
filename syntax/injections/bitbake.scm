; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The rules for text in other languages in bitbake: runtime/queries/bitbake/injections.scm of
; Helix (https://github.com/YardQuit/helix_editor) at commit 9bcdf4d79d14,
; unchanged below this notice.

((python_function_definition) @injection.content
  (#set! injection.language "python")
  (#set! injection.include-children))

((anonymous_python_function (block) @injection.content)
  (#set! injection.language "python")
  (#set! injection.include-children))

((inline_python) @injection.content
  (#set! injection.language "python")
  (#set! injection.include-children))

((function_definition) @injection.content
  (#set! injection.language "bash")
  (#set! injection.include-children))

((comment) @injection.content
  (#set! injection.language "comment"))
