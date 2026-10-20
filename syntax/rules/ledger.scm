; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The colouring rules for ledger: runtime/queries/ledger/highlights.scm of
; Helix (https://github.com/YardQuit/helix_editor) at commit 9bcdf4d79d14,
; unchanged below this notice.

[
    (comment)
    (note)
] @comment

[
    (date)
    (interval)
    (quantity)
] @constant.numeric.integer

((account) @variable.other.member)
((commodity) @text.literal)

"include" @keyword.control.import

[
    "account"
    "alias"
    "assert"
    "check"
    "commodity"
    "def"
    "default"
    "end"
    "eval"
    "format"
    "nomarket"
    "note"
    "payee"
    "check"
    "A"
    "Y"
    "N"
    "D"
    "C"
    "P"
] @keyword
