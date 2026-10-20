; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The rules naming the definitions and headings in just: runtime/queries/just/tags.scm of
; Helix (https://github.com/YardQuit/helix_editor) at commit 9bcdf4d79d14,
; unchanged below this notice.

; Symbols that can be considered definitions in a Just file.

(alias
  alias_name: (identifier) @definition.function)

(assignment
  name: (identifier) @definition.constant)

(import
  (path) @definition.module)

(mod
  name: (identifier) @definition.module)

(recipe
  name: (identifier) @definition.function)

(unexport
  name: (identifier) @definition.constant)
