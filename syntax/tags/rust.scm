; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The rules naming the definitions and headings in rust: runtime/queries/rust/tags.scm of
; Helix (https://github.com/helix-editor/helix) at commit 9bcdf4d79d14,
; unchanged below this notice but where a comment says what was changed.

(struct_item
  name: (type_identifier) @name) @definition.struct

(const_item
  name: (identifier) @name) @definition.constant

(trait_item
  name: (type_identifier) @name) @definition.interface

(function_item
  name: (identifier) @name) @definition.function

(function_signature_item
  name: (identifier) @name) @definition.function

(enum_item
  name: (type_identifier) @name) @definition.enum

(enum_variant
  name: (identifier) @name) @definition.struct

(type_item
  name: (type_identifier) @name) @definition.type

(mod_item
  name: (identifier) @name) @definition.module

(macro_definition
  name: (identifier) @name) @definition.macro

; changed: added, impl blocks, so that their functions are outlined under
; them
(impl_item
  type: (_) @name) @definition.impl
