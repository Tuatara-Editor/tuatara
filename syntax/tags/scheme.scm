; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The rules naming the definitions and headings in scheme: runtime/queries/scheme/tags.scm of
; Helix (https://github.com/YardQuit/helix_editor) at commit 9bcdf4d79d14,
; unchanged below this notice.

; function definitions
(list
  .
  (symbol) @_define
  .
  (list
    .
    [
      (symbol) @name
      ; for curried functions
      (list . (symbol) @name)
      (list . (list . [(symbol) (list)] @name))
    ])
  (#eq? @_define "define")) @definition.function

(list
  .
  (symbol) @_define
  .
  (symbol) @name
  (#eq? @_define "define")) @definition.constant
