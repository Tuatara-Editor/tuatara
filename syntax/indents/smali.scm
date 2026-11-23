; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The indentation rules for smali: runtime/queries/smali/indents.scm of
; Helix (https://github.com/helix-editor/helix) at commit 9bcdf4d79d14,
; unchanged below this notice.

[
  (annotation_directive)
  (array_data_directive)
  (field_definition)
  (method_definition)
  (packed_switch_directive)
  (param_directive)
  (parameter_directive)
  (sparse_switch_directive)
  (subannotation_directive)
  (list)
  "\{"
] @indent

[
  ".end annotation"
  ".end array-data"
  ".end field"
  ".end method"
  ".end packed-switch"
  ".end param"
  ".end parameter"
  ".end sparse-switch"
  ".end subannotation"
  "\}"
] @outdent

[
  (ERROR)
  (comment)
] @indent.always
