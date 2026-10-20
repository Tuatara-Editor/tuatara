; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The rules for text in other languages in godot-resource: runtime/queries/godot-resource/injections.scm of
; Helix (https://github.com/YardQuit/helix_editor) at commit 9bcdf4d79d14,
; unchanged below this notice.

((comment) @injection.content
 (#set! injection.language "comment"))

; ((section) @injection.content
;  (#set! injection.language "comment"))

((section 
  (attribute 
    (identifier) @_type
    (string) @_is_shader)
  (property 
    (path) @_is_code
    (string) @injection.content))
  (#eq? @_type "type")
  (#match? @_is_shader "Shader")
  (#eq? @_is_code "code")
  (#set! injection.language "glsl")
)

((section 
  (identifier) @_is_resource
  (property 
    (path) @_is_code
    (string) @injection.content))
  (#eq? @_is_resource "resource")
  (#eq? @_is_code "code")
  (#set! injection.language "glsl")
)

((section 
  (identifier) @_id
  (property 
    (path) @_is_expression
    (string) @injection.content))
  (#eq? @_id "sub_resource")
  (#eq? @_is_expression "expression")
  (#set! injection.language "glsl")
)

((section 
  (attribute 
    (identifier) @_type
    (string) @_is_shader)
  (property 
    (path) @_is_code
    (string) @injection.content))
  (#eq? @_type "type")
  (#match? @_is_shader "GDScript")
  (#eq? @_is_code "script/source")
  (#set! injection.language "gdscript")
)
