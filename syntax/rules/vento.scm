; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The colouring rules for vento: runtime/queries/vento/highlights.scm of
; Helix (https://github.com/YardQuit/helix_editor) at commit 9bcdf4d79d14,
; unchanged below this notice.

(comment) @comment

(keyword) @keyword

(tag
  [
    "{{"
    "{{-"
    "}}"
    "-}}"
  ] @punctuation.bracket)

"|>" @operator
