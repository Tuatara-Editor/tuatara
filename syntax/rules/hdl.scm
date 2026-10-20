; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The colouring rules for hdl: runtime/queries/hdl/highlights.scm of
; Helix (https://github.com/YardQuit/helix_editor) at commit 9bcdf4d79d14,
; unchanged below this notice.

;; Keywords
[
  "CHIP"
  "IN"
  "OUT"
  "PARTS"
  "BUILTIN"
  "CLOCKED"
] @keyword

(identifier) @variable

(chip_definition
  name: (identifier) @function)

(in_section
  input_pin_name: (identifier) @variable.parameter)

(out_section
  output_pin_name: (identifier) @variable.parameter)

(builtin_body
  chip_name: (identifier) @function)

(clocked_body
  (identifier) @variable.parameter)

(part
  chip_name: (identifier) @function)

(connection
  part_pin: (identifier) @variable.other.member
  chip_pin: [
    (identifier) @variable.parameter
    (bus_identifier
      (identifier) @variable.parameter
      (number) @constant.numeric)
  ])

(bus_identifier
  (number) @constant.numeric)

;; Comments
(comment) @comment
