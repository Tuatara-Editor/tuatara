; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The selection rules for godot-resource: runtime/queries/godot-resource/textobjects.scm of
; Helix (https://github.com/helix-editor/helix) at commit 9bcdf4d79d14,
; unchanged below this notice.

(section 
  (identifier) 
  (_)
  (property) @class.inside
) @class.around

(attribute 
  (identifier)
  (_) @parameter.inside) @parameter.around

(property 
  (path)
  (_) @entry.inside) @entry.around

(pair 
  (_) @entry.inside) @entry.around

(array
  (_) @entry.around)

(comment) @comment.inside

(comment)+ @comment.around
