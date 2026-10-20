; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The rules for text in other languages in dockerfile: runtime/queries/dockerfile/injections.scm of
; Helix (https://github.com/YardQuit/helix_editor) at commit 9bcdf4d79d14,
; unchanged below this notice.

((comment) @injection.content
 (#set! injection.language "comment"))

((shell_command (shell_fragment) @injection.content)
 (#set! injection.language "bash")
 (#set! injection.combined))

((run_instruction
 (heredoc_block (heredoc_line) @injection.content . "\n" @injection.content))
 (#set! injection.language "bash")
 (#set! injection.combined))

((copy_instruction
 (path (heredoc_marker)) . (path) @injection.filename
 (heredoc_block (heredoc_line) @injection.content . "\n" @injection.content))
 (#set! injection.combined))
