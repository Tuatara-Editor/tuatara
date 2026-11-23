; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The colouring rules for lpf: runtime/queries/lpf/highlights.scm of
; Helix (https://github.com/helix-editor/helix) at commit 9bcdf4d79d14,
; unchanged below this notice.

[
  "SYSCONFIG"
  "BLOCK"
  "LOCATE"
  "COMP"
  "FREQUENCY"
  "PORT"
  "IOBUF"
] @keyword

["SITE"] @keyword.storage

["="] @operator

((number) @constant.numeric)

((string) @string)
((line_comment) @comment)

