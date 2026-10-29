; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The rules for text in other languages in erlang: runtime/queries/erlang/injections.scm of
; Helix (https://github.com/helix-editor/helix) at commit 9bcdf4d79d14,
; unchanged below this notice.

((line_comment (comment_content) @injection.content)
 (#set! injection.language "edoc")
 (#set! injection.include-children)
 (#set! injection.combined))

((comment (comment_content) @injection.content)
 (#set! injection.language "comment"))

; EEP-59 doc attributes use markdown by default.
(attribute
  name: (atom) @_attribute
  (arguments [
    (string (quoted_content) @injection.content)
    (sigil (quoted_content) @injection.content)
  ])
 (#set! injection.language "markdown")
 (#any-of? @_attribute "doc" "moduledoc"))
