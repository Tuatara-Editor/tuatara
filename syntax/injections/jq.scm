; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The rules for text in other languages in jq: runtime/queries/jq/injections.scm of
; Helix (https://github.com/helix-editor/helix) at commit 9bcdf4d79d14,
; unchanged below this notice.

;; From nvim-treesitter, contributed by @ObserverOfTime et al.

((comment) @injection.content
  (#set! injection.language "comment"))

; test(val)
(query
  ((funcname) @_function
    (#any-of? @_function "test" "match" "capture" "scan" "split" "splits" "sub" "gsub"))
  (args
    .
    (query
      (string) @injection.content
      (#set! injection.language "regex"))))

; test(regex; flags)
(query
  ((funcname) @_function
    (#any-of? @_function "test" "match" "capture" "scan" "split" "splits" "sub" "gsub"))
  (args
    .
    (args
      (query
        (string) @injection.content
        (#set! injection.language "regex")))))
