; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The rules for text in other languages in markdown-rustdoc: runtime/queries/markdown-rustdoc/injections.scm of
; Helix (https://github.com/helix-editor/helix) at commit 9bcdf4d79d14,
; unchanged below this notice.

; inherits: markdown

; In Rust, it is common to have documentation code blocks not specify the
; language, and it is assumed to be Rust if it is not specified.

(fenced_code_block
  (code_fence_content) @injection.content
  (#set! injection.language "rust")
  (#set! injection.include-unnamed-children))

(fenced_code_block
  (info_string
    (language) @injection.language)
  (code_fence_content) @injection.content (#set! injection.include-unnamed-children))

(fenced_code_block
  (info_string
    (language) @__language)
  (code_fence_content) @injection.content
  ; list of attributes for Rust syntax highlighting:
  ; https://doc.rust-lang.org/rustdoc/write-documentation/documentation-tests.html#attributes
  (#match? @__language
  "(ignore|should_panic|no_run|compile_fail|standalone_crate|custom|edition*)")
  (#set! injection.language "rust")
  (#set! injection.include-unnamed-children))
