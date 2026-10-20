; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The rules naming the definitions and headings in amber: runtime/queries/amber/tags.scm of
; Helix (https://github.com/YardQuit/helix_editor) at commit 9bcdf4d79d14,
; unchanged below this notice.

; Function definitions
(function_definition
  name: (variable) @name) @definition.function

; Function calls
(function_call
  name: (variable) @name) @reference.call

; Main block as entry point
(main_block) @definition.function
