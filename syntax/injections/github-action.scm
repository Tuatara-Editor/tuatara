; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The rules for text in other languages in github-action: runtime/queries/github-action/injections.scm of
; Helix (https://github.com/helix-editor/helix) at commit 9bcdf4d79d14,
; unchanged below this notice.

; inherits: yaml

; JavaScript for workflow scripting (inline)
(block_mapping
  (block_mapping_pair
    key: (flow_node) @_uses (#eq? @_uses "uses")
    value: (flow_node) @_actions_ghs (#match? @_actions_ghs "^actions/github-script"))
  (block_mapping_pair
    key: (flow_node) @_with (#eq? @_with "with")
    value: (block_node
             (block_mapping
               (block_mapping_pair
                 key: (flow_node) @_run (#eq? @_run "script")
                 value: (flow_node
                          (plain_scalar
                            (string_scalar) @injection.content
                            (#set! injection.language "javascript"))))))))

; JavaScript for workflow scripting (block)
(block_mapping
  (block_mapping_pair
    key: (flow_node) @_uses (#eq? @_uses "uses")
    value: (flow_node) @_actions_ghs (#match? @_actions_ghs "^actions/github-script"))
  (block_mapping_pair
    key: (flow_node) @_with (#any-of? @_with "with")
    value: (block_node
             (block_mapping
               (block_mapping_pair
                 key: (flow_node) @_run (#any-of? @_run "script")
                 value: (block_node
                          (block_scalar) @injection.content
                          (#set! injection.language "javascript")))))))
