; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The rules for text in other languages in scala: runtime/queries/scala/injections.scm of
; Helix (https://github.com/YardQuit/helix_editor) at commit 9bcdf4d79d14,
; unchanged below this notice.

([(comment) (block_comment)] @injection.content
 (#set! injection.language "comment"))


; Matches these SQL interpolators:
;  - Doobie: 'sql', 'fr'
;  - Quill: 'sql', 'infix'
;  - Slick: 'sql', 'sqlu'
(interpolated_string_expression 
  interpolator: 
    ((identifier) @interpolator 
     (#any-of? @interpolator "fr" "infix" "sql" "sqlu"))
  (interpolated_string) @injection.content
  (#set! injection.language "sql"))

