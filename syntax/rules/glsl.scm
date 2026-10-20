; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The colouring rules for glsl: runtime/queries/glsl/highlights.scm of
; Helix (https://github.com/YardQuit/helix_editor) at commit 9bcdf4d79d14,
; unchanged below this notice.

; inherits: c

[
  "in"
  "out"
  "inout"
  "uniform"
  "shared"
  "layout"
  "attribute"
  "varying"
  "buffer"
  "coherent"
  "readonly"
  "writeonly"
  "precision"
  "highp"
  "mediump"
  "lowp"
  "centroid"
  "sample"
  "patch"
  "smooth"
  "flat"
  "noperspective"
  "invariant"
  "precise"
  "require"
  "enable"
  "warn"
  "disable"
] @keyword

"subroutine" @keyword.function

(extension_storage_class) @attribute

(
  (identifier) @variable.builtin
  (#match? @variable.builtin "^gl_")
)
