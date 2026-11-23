; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The colouring rules for woodpecker-ci: runtime/queries/woodpecker-ci/highlights.scm of
; Helix (https://github.com/helix-editor/helix) at commit 9bcdf4d79d14,
; unchanged below this notice.

; yaml highlights from runtime/queries/yaml/highlights.scm
(boolean_scalar) @constant.builtin.boolean
(null_scalar) @constant.builtin
(double_quote_scalar) @string
(single_quote_scalar) @string
(block_scalar) @string
(string_scalar) @string
(escape_sequence) @constant.character.escape
(integer_scalar) @constant.numeric.integer
(float_scalar) @constant.numeric.float
(comment) @comment
(anchor_name) @type
(alias_name) @type
(tag) @type
(yaml_directive) @keyword

(block_mapping_pair
  key: (flow_node [(double_quote_scalar) (single_quote_scalar)] @variable.other.member))
(block_mapping_pair
  key: (flow_node (plain_scalar (string_scalar) @variable.other.member)))

(flow_mapping
  (_ key: (flow_node [(double_quote_scalar) (single_quote_scalar)] @variable.other.member)))
(flow_mapping
  (_ key: (flow_node (plain_scalar (string_scalar) @variable.other.member))))

[
","
"-"
":"
">"
"?"
"|"
] @punctuation.delimiter

[
"["
"]"
"{"
"}"
] @punctuation.bracket

["*" "&" "---" "..."] @punctuation.special

; Highlight the toplevel keys differently as keywords
; https://woodpecker-ci.org/docs/usage/workflow-syntax
(block_mapping_pair
  key: (flow_node (plain_scalar (string_scalar) @keyword (#any-of? @keyword "steps" "workspace" "labels" "matrix" "services" "variables" "clone" "skip_clone" "depends_on" "runs_on"))) )
