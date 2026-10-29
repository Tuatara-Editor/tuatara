; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The colouring rules for ptx: runtime/queries/ptx/highlights.scm of
; Helix (https://github.com/helix-editor/helix) at commit 9bcdf4d79d14,
; unchanged below this notice.

; highlights.scm - Syntax highlighting for PTX

; Comments
(comment) @comment

; Directives
(version_directive) @keyword
(target_directive) @keyword
(address_size_directive) @keyword
(file_directive) @keyword
(section_directive) @keyword
(visibility_directive) @keyword
(pragma_directive) @keyword

; Keywords
[
  ".global"
  ".const"
  ".param"
  ".local"
  ".shared"
  ".tex"
  ".func"
  ".entry"
] @keyword

; Types
(data_type) @type

; Instructions
(opcode) @function

; Identifiers
(identifier) @variable

; Registers
(register) @variable

; Numbers
(number) @constant.numeric.integer
(float_literal) @constant.numeric.float

; Strings
(string) @string

; Labels
(label) @label
