; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The selection rules for _typescript: runtime/queries/_typescript/textobjects.scm of
; Helix (https://github.com/YardQuit/helix_editor) at commit 9bcdf4d79d14,
; unchanged below this notice.

[
  (interface_declaration 
    body:(_) @class.inside)
  (type_alias_declaration 
    value: (_) @class.inside)
] @class.around

(enum_body
  (_) @entry.around)

(enum_assignment (_) @entry.inside)

