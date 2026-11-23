; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The rules for text in other languages in eex: runtime/queries/eex/injections.scm of
; Helix (https://github.com/helix-editor/helix) at commit 9bcdf4d79d14,
; unchanged below this notice.

; https://github.com/connorlay/tree-sitter-eex/blob/f742f2fe327463335e8671a87c0b9b396905d1d1/queries/injections.scm

((directive (expression) @injection.content)
 (#set! injection.language "elixir"))

((partial_expression) @injection.content
 (#set! injection.language "elixir")
 (#set! injection.include-children)
 (#set! injection.combined))
