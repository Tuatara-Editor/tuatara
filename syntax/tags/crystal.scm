; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The rules naming the definitions and headings in crystal: runtime/queries/crystal/tags.scm of
; Helix (https://github.com/YardQuit/helix_editor) at commit 9bcdf4d79d14,
; unchanged below this notice.

(class_def name: (constant) @name) @definition.class
(struct_def name: (constant) @name) @definition.struct
(c_struct_def name: (constant) @name) @definition.struct
(union_def name: (constant) @name) @definition.struct
(module_def name: (constant) @name) @definition.module
(lib_def name: (constant) @name) @definition.module
(enum_def name: (constant) @name) @definition.enum
(annotation_def name: (constant) @name) @definition.interface
(type_def name: (constant) @name) @definition.type
(method_def name: [(identifier) (operator)] @name) @definition.function
(abstract_method_def name: [(identifier) (operator)] @name) @definition.function
(fun_def name: [(constant) (identifier)] @name) @definition.function
(macro_def name: (identifier) @name) @definition.macro
(const_assign lhs: (constant) @name) @definition.constant
