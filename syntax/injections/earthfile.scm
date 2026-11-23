; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The rules for text in other languages in earthfile: runtime/queries/earthfile/injections.scm of
; Helix (https://github.com/helix-editor/helix) at commit 9bcdf4d79d14,
; unchanged below this notice.

((comment) @injection.content
  (#set! injection.language "comment"))

((line_continuation_comment) @injection.content
  (#set! injection.language "comment"))

((shell_fragment) @injection.content
  (#set! injection.language "bash")
  (#set! injection.include-children))
