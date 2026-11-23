; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The selection rules for mail: runtime/queries/mail/textobjects.scm of
; Helix (https://github.com/helix-editor/helix) at commit 9bcdf4d79d14,
; unchanged below this notice.

(atom_block
  (atom) @entry.inside) @entry.around

(email_address) @entry.around
(header_other
  (header_unstructured) @entry.around)

(quoted_block)+ @comment.around

(body_block)+ @function.around

(header_subject
  (subject) @function.around)
