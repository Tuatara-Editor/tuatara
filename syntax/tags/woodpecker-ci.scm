; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The rules naming the definitions and headings in woodpecker-ci: runtime/queries/woodpecker-ci/tags.scm of
; Helix (https://github.com/YardQuit/helix_editor) at commit 9bcdf4d79d14,
; unchanged below this notice.

; select steps
; e.g.
; ```
; steps:
;   - name: build-image
;     image: docker
;     commands:
;       - docker build --rm -t local/project-image .
;     volumes:
;       - /var/run/docker.sock:/var/run/docker.sock
; ```
(block_mapping_pair
  key: (_) @_key (#eq? @_key "steps")
  value: (block_node
           (block_sequence
             (block_sequence_item
               (block_node) @definition.struct))))
