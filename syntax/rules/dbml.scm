; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The colouring rules for dbml: runtime/queries/dbml/highlights.scm of
; Helix (https://github.com/YardQuit/helix_editor) at commit 9bcdf4d79d14,
; unchanged below this notice.


; comments highlighting
(comment) @comment

; keyword highlighting
(keyword_def) @keyword
(keyword_enum) @keyword
(keyword_ref) @keyword

; identify blocks and definitions
(definition) @function

; for identifiers
(identifier) @variable
(type) @keyword

; Highlight special types for database/data types
("Project" ) @type
("Table" ) @type
("TableGroup" ) @type
("database_type" ) @variable

; string and number constants
("'''") @constant.character.escape
(string) @string
(number) @constant.numeric

; brackets
[
  "("
  ")"
  "{"
  "}"
  "["
  "]"
] @punctuation.bracket

; brackets
[
  ":"
  "."
  ","
] @punctuation.delimiter

