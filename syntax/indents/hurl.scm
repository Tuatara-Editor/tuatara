; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The indentation rules for hurl: runtime/queries/hurl/indents.scm of
; Helix (https://github.com/helix-editor/helix) at commit 9bcdf4d79d14,
; unchanged below this notice.

[
  (json_object)
  (json_array)
  (xml_tag)
] @indent

[
  "}"
  "]"
  (xml_close_tag)
] @outdent
