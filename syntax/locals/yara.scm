; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The rules for local names in yara: runtime/queries/yara/locals.scm of
; Helix (https://github.com/helix-editor/helix) at commit 9bcdf4d79d14,
; unchanged below this notice.

(rule_definition name: (identifier) @local.definition.string.special)
(string_identifier) @local.definition.string.special.symbol

(for_expression
  (string_identifier) @local.reference)

(for_of_expression
  (string_identifier) @local.reference)

(of_expression
  (string_set
    (string_identifier) @local.reference))

(string_count
  (string_identifier) @local.reference)

(string_offset
  (string_identifier) @local.reference)

(string_length
  (string_identifier) @local.reference)
