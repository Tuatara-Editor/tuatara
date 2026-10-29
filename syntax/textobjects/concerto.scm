; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The selection rules for concerto: runtime/queries/concerto/textobjects.scm of
; Helix (https://github.com/helix-editor/helix) at commit 9bcdf4d79d14,
; unchanged below this notice.

; Concerto Language - Text Object Queries (Helix)
; =================================================
; Helix-specific text objects. For use in helix-editor/helix at
; runtime/queries/concerto/textobjects.scm
;
; Helix uses @<object>.around / @<object>.inside suffixes.
; See: https://docs.helix-editor.com/guides/textobject.html

; ---------------------------------------------------------------------------
; Classes / declarations
; ---------------------------------------------------------------------------
; mac / mic — select around/inside class
; ]c / [c   — jump to next/prev class boundary

(concept_declaration
  (class_body
    .
    "{"
    _+ @class.inside
    "}")) @class.around

(asset_declaration
  (class_body
    .
    "{"
    _+ @class.inside
    "}")) @class.around

(participant_declaration
  (class_body
    .
    "{"
    _+ @class.inside
    "}")) @class.around

(transaction_declaration
  (class_body
    .
    "{"
    _+ @class.inside
    "}")) @class.around

(event_declaration
  (class_body
    .
    "{"
    _+ @class.inside
    "}")) @class.around

(enum_declaration
  (enum_body
    .
    "{"
    _+ @class.inside
    "}")) @class.around

(map_declaration
  (map_body
    .
    "{"
    _+ @class.inside
    "}")) @class.around

; Scalar declarations have no body braces — around only
(scalar_declaration) @class.around

; ---------------------------------------------------------------------------
; Comments
; ---------------------------------------------------------------------------
; ]C / [C — jump to next/prev comment
; maC / miC — select around/inside comment

(line_comment) @comment.inside
(block_comment) @comment.inside

(line_comment) @comment.around
(block_comment) @comment.around

; ---------------------------------------------------------------------------
; Parameters — fields, enum values, map entries
; ---------------------------------------------------------------------------
; ]a / [a — jump to next/prev parameter
; maa / mia — select around/inside parameter

(string_field) @parameter.inside
(boolean_field) @parameter.inside
(datetime_field) @parameter.inside
(integer_field) @parameter.inside
(long_field) @parameter.inside
(double_field) @parameter.inside
(object_field) @parameter.inside
(relationship_field) @parameter.inside
(enum_property) @parameter.inside
(map_key_type) @parameter.inside
(map_value_type) @parameter.inside
