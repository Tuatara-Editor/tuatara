; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The selection rules for caddyfile: runtime/queries/caddyfile/textobjects.scm of
; Helix (https://github.com/helix-editor/helix) at commit 9bcdf4d79d14,
; unchanged below this notice.

(comment) @comment.inside
(comment)+ @comment.around

(directive
  name: (directive_name) @parameter.inside) @parameter.around

(global_options
  "{" (_)* @class.inside "}") @class.around

(snippet_definition
  (block) @class.inside) @class.around

(named_route
  (block) @class.inside) @class.around

(site_block (block) @class.inside) @class.around
