; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The indentation rules for ruby: runtime/queries/ruby/indents.scm of
; Helix (https://github.com/helix-editor/helix) at commit 9bcdf4d79d14,
; unchanged below this notice.

[
  (argument_list)
  (array)
  (begin)
  (block)
  (call)
  (class)
  (case)
  (if)
  (hash)
  (method)
  (module)
  (singleton_class)
  (singleton_method)
] @indent

[
  ")"
  "}"
  "]"
  "end"
  "when"
  "else"
  "elsif"
  "rescue"
  "ensure"
] @outdent
