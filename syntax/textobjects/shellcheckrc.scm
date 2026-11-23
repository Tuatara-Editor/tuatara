; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The selection rules for shellcheckrc: runtime/queries/shellcheckrc/textobjects.scm of
; Helix (https://github.com/helix-editor/helix) at commit 9bcdf4d79d14,
; unchanged below this notice.

(comment) @comment.inside
(comment)+ @comment.around


[
  (disable_directive)
  (enable_directive)
  (extended_analysis_directive)
  (external_sources_directive)
  (source_directive)
  (source_path_directive)
  (shell_directive)
] @entry.around
