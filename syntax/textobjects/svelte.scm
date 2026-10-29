; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The selection rules for svelte: runtime/queries/svelte/textobjects.scm of
; Helix (https://github.com/helix-editor/helix) at commit 9bcdf4d79d14,
; unchanged below this notice.

(element (start_tag) (_)* @xml-element.inside (end_tag))

(element) @xml-element.around

(comment) @comment.around

(snippet_block
  parameters: (snippet_parameters) @parameter.inside) @function.around

(snippet_name) @function.inside
