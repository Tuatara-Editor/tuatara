; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The colouring rules for ldif: runtime/queries/ldif/highlights.scm of
; Helix (https://github.com/YardQuit/helix_editor) at commit 9bcdf4d79d14,
; unchanged below this notice.

(comment) @comment

(attributeType) @type.parameter

((distinguishedName
  (name
    (name_componet
      (attributeTypeAndValue
        (attributeType) @comment
        (string) @type.parameter
          )))) @comment)


(dn_spec) @constant
(changerecord) @constant
(mod_spec) @constant

(change_modify) @string

(value_spec) @keyword
