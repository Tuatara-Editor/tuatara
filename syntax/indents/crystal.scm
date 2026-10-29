; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The indentation rules for crystal: runtime/queries/crystal/indents.scm of
; Helix (https://github.com/helix-editor/helix) at commit 9bcdf4d79d14,
; unchanged below this notice.

[
  (param_list)
  (array)
  (block)
  (call)
  (class_def)
  (struct_def)
  (module_def)
  (enum_def)
  (lib_def)
  (fun_def)
  (method_def)
  (case)
  (select)
  (if)
  (unless)
  (hash)
  (tuple)
] @indent

[
  ")"
  "}"
  "]"
  "end"
  "when"
  "in"
  "else"
  "elsif"
] @outdent
