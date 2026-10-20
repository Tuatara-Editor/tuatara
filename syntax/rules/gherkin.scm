; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The colouring rules for gherkin: runtime/queries/gherkin/highlights.scm of
; Helix (https://github.com/YardQuit/helix_editor) at commit 9bcdf4d79d14,
; unchanged below this notice.

[
  (feature_keyword)
  (rule_keyword)
  (background_keyword)
  (scenario_keyword)
  (given_keyword)
  (when_keyword)
  (then_keyword)
  (and_keyword)
  (but_keyword)
  (asterisk_keyword)
] @keyword

(tag) @function
(doc_string) @string
(data_table) @special
(comment) @comment
