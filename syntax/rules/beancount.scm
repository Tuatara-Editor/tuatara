; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The colouring rules for beancount: runtime/queries/beancount/highlights.scm of
; Helix (https://github.com/helix-editor/helix) at commit 9bcdf4d79d14,
; unchanged below this notice.

(date) @variable.builtin
(txn) @variable.builtin

(account) @type

[
  (amount)
  (incomplete_amount)
  (amount_tolerance)
  (number)
] @constant.numeric


[(key_value) (key)] @variable.other.member
(string) @string

[
  (currency)
  (tag)
  (link)
] @constant

(comment) @comment

[
  (minus)
  (plus)
] @operator

[
  (balance) (open) (close) (commodity) (pad)
  (event) (price) (note) (document) (query)
  (custom) (pushtag) (poptag) (pushmeta)
  (popmeta) (option) (include) (plugin)
] @keyword


((headline item: (item) @markup.heading.6) @markup.heading.marker
 (#match? @markup.heading.marker "^\\*\\*\\*\\*\\*\\*"))
((headline item: (item) @markup.heading.5) @markup.heading.marker
 (#match? @markup.heading.marker "^\\*\\*\\*\\*\\*"))
((headline item: (item) @markup.heading.4) @markup.heading.marker
 (#match? @markup.heading.marker "^\\*\\*\\*\\*"))
((headline item: (item) @markup.heading.3) @markup.heading.marker
 (#match? @markup.heading.marker "^\\*\\*\\*"))
((headline item: (item) @markup.heading.2) @markup.heading.marker
 (#match? @markup.heading.marker "^\\*\\*"))
((headline item: (item) @markup.heading.1) @markup.heading.marker
 (#match? @markup.heading.marker "^\\*"))
