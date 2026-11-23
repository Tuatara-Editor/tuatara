; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The rules for text in other languages in rust-format-args-macro: runtime/queries/rust-format-args-macro/injections.scm of
; Helix (https://github.com/helix-editor/helix) at commit 9bcdf4d79d14,
; unchanged below this notice.

; inherits: rust

; HACK: This language is the same as Rust but all strings are injected
; with rust-format-args. Rust injects this into known macros which use
; the format args syntax. This can cause false-positive highlights but
; those are expected to be rare.

([
   (string_literal (string_content) @injection.content)
   (raw_string_literal (string_content) @injection.content)
 ]
 (#set! injection.language "rust-format-args")
 (#set! injection.include-children))
