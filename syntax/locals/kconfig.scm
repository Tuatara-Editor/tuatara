; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The rules for local names in kconfig: runtime/queries/kconfig/locals.scm of
; Helix (https://github.com/YardQuit/helix_editor) at commit 9bcdf4d79d14,
; unchanged below this notice.

[
  (symbol)
  (string)
] @local.reference

[
  (config)
  (menuconfig)
  (choice)
  (comment_entry)
  (menu)
  (if)
] @local.scope

(type_definition (string) @local.definition.type)
(type_definition (input_prompt (string) @local.definition.type))
(type_definition_default (expression (string) @local.definition.type))
