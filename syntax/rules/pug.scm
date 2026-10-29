; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The colouring rules for pug: runtime/queries/pug/highlights.scm of
; Helix (https://github.com/helix-editor/helix) at commit 9bcdf4d79d14,
; unchanged below this notice.

(comment) @comment

(
  doctype
  (("doctype") @keyword.storage.type)
  ((doctype_name) @type.enum.variant)
)

(tag_name) @constant

; Attributes
(id) @attribute
(class) @attribute
(attribute_name) @attribute

(quoted_attribute_value) @string

; Controls
(
  conditional
  ((keyword) @keyword.control.conditional)
)
(
  case
  ((keyword) @keyword.control)
  (
    when
    ((keyword) @keyword.control)
  )
)
(
  each
  ((keyword) @keyword.control.repeat)
)
(
  else
  ((keyword) @keyword.control.conditional)
)
(
  while
  ((keyword) @keyword.control.repeat)
)

; Mixins
(
  mixin_definition
  ((keyword) @keyword.function)
  ((mixin_name) @function.method)
)
(
  mixin_use
  (("+") @operator)
  ((mixin_name) @function.method)
)

; Includes
(
  include
  ((keyword) @keyword.directive)
  ((filename) @string.special.path)
)

; Inheritance
(
  extends
  ((keyword) @keyword.directive)
  ((filename) @string.special.path)
)
(
  block_definition
  ((keyword) @keyword.directive)
  ((block_name) @function.method)
)
(
  block_append
  ((keyword) @keyword.directive)
  ((block_name) @function.method)
)
(
  block_prepend
  ((keyword) @keyword.directive)
  ((block_name) @function.method)
)

; Filters
(
  filter
  (":" @function.macro)
  ((filter_name) @function.macro)
  ((content) @special)
)

; Inline JavaScript
(
  unbuffered_code
  (("-") @special)
)
