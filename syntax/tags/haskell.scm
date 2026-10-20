; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The rules naming the definitions and headings in haskell: runtime/queries/haskell/tags.scm of
; Helix (https://github.com/YardQuit/helix_editor) at commit 9bcdf4d79d14,
; unchanged below this notice.

(function name: (variable) @name) @definition.function
(signature name: (variable) @name) @definition.function
(data_type name: (name) @name) @definition.struct
(newtype name: (name) @name) @definition.struct
(type_synomym name: (name) @name) @definition.type
(class name: (name) @name) @definition.interface
(header module: (module (module_id) @name)) @definition.module
