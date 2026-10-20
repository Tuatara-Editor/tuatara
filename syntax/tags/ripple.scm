; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The rules naming the definitions and headings in ripple: runtime/queries/ripple/tags.scm of
; Helix (https://github.com/YardQuit/helix_editor) at commit 9bcdf4d79d14,
; unchanged below this notice.

; Code outline/structure for symbol navigation

; Components
(component_declaration
  name: (identifier) @name) @definition.function

; Fragments
(fragment_declaration
  name: (identifier) @name) @definition.function

; Functions
(function_declaration
  name: (identifier) @name) @definition.function

; Classes
(class_declaration
  name: (identifier) @name) @definition.class

; Methods
(method_definition
  name: (property_name) @name) @definition.function
