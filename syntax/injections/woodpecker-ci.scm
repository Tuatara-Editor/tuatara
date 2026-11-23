; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The rules for text in other languages in woodpecker-ci: runtime/queries/woodpecker-ci/injections.scm of
; Helix (https://github.com/helix-editor/helix) at commit 9bcdf4d79d14,
; unchanged below this notice.

((comment) @injection.content
 (#set! injection.language "comment"))

; https://woodpecker-ci.org/docs/usage/workflow-syntax#commands
; e.g.
; ```
; steps:
;   - name: backend
;     image: golang
;     commands:
;       - go build
;       - go test
; ```
(block_mapping_pair
  key: (flow_node) @_key (#eq? @_key "commands")
  value: (block_node
           (block_sequence
             (block_sequence_item
                (flow_node
                  (plain_scalar
                    (string_scalar) @injection.content))
                (#set! injection.language "bash")))))

(block_mapping_pair
  key: (flow_node) @_key (#any-of? @_key "commands")
  value: (block_node
           (block_sequence
             (block_sequence_item
               (block_node
                  (block_scalar) @injection.content
                  (#set! injection.language "bash"))))))

; https://woodpecker-ci.org/docs/usage/workflow-syntax#entrypoint
; e.g.
; ```
; job1:
;   services:
;     entrypoint: ["/usr/local/bin/docker-entrypoint.sh", "-c", 'max_connections=100']
; ```
(block_mapping_pair
  key: (flow_node) @_key (#any-of? @_key "entrypoint")
  value: (flow_node
           (flow_sequence
             (flow_node
               [
                 (double_quote_scalar)
                 (single_quote_scalar)
               ] @injection.content)))
  (#set! injection.language "bash"))
