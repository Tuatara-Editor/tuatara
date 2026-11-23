; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The indentation rules for snakemake: runtime/queries/snakemake/indents.scm of
; Helix (https://github.com/helix-editor/helix) at commit 9bcdf4d79d14,
; unchanged below this notice.

; inherits: python


[
  (rule_definition)
  (checkpoint_definition)
  (rule_inheritance)
  (module_definition)
] @indent

[
  (rule_definition)
  (checkpoint_definition)
  (rule_inheritance)
  (module_definition)
] @extend


(directive) @indent
(directive) @extend

(rule_import
  "with"
  ":") @indent
(rule_import
  "with"
  ":") @extend
