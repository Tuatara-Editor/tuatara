; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The rules naming the definitions and headings in gitlab-ci: runtime/queries/gitlab-ci/tags.scm of
; Helix (https://github.com/YardQuit/helix_editor) at commit 9bcdf4d79d14,
; unchanged below this notice.

; select jobs
(block_mapping
  (block_mapping_pair
    value: (block_node
             (block_mapping
               (block_mapping_pair
                 key: (flow_node) @_key (#eq? @_key "stage"))))) @definition.struct)

; select defined variables under `variables:`
(block_mapping
  (block_mapping_pair
    key: (flow_node) @_key (#eq? @_key "variables")
    value: (block_node
             (block_mapping
               (block_mapping_pair
                 key: (flow_node) @name
                 value: (_) @definition.constant)))))
