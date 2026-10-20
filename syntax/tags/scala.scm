; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The rules naming the definitions and headings in scala: runtime/queries/scala/tags.scm of
; Helix (https://github.com/YardQuit/helix_editor) at commit 9bcdf4d79d14,
; unchanged below this notice.

(class_definition name: (identifier) @name) @definition.class
(object_definition name: (identifier) @name) @definition.module
(trait_definition name: (identifier) @name) @definition.interface
(enum_definition name: (identifier) @name) @definition.enum
(function_definition name: (identifier) @name) @definition.function
(val_definition pattern: (identifier) @name) @definition.constant
(var_definition pattern: (identifier) @name) @definition.constant
(type_definition name: (type_identifier) @name) @definition.type
(given_definition name: (identifier) @name) @definition.constant
