; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The rules for text in other languages in rescript: runtime/queries/rescript/injections.scm of
; Helix (https://github.com/YardQuit/helix_editor) at commit 9bcdf4d79d14,
; unchanged below this notice.

((comment) @injection.content (#set! injection.language "comment"))

; %re
(extension_expression
  (extension_identifier) @_name
  (#eq? @_name "re")
  (expression_statement (_) @injection.content (#set! injection.language "regex")))

; %raw
(extension_expression
  (extension_identifier) @_name
  (#eq? @_name "raw")
  (expression_statement
    (_ (_)  @injection.content (#set! injection.language "javascript"))))

; %graphql
(extension_expression
  (extension_identifier) @_name
  (#eq? @_name "graphql")
  (expression_statement
    (_ (_) @injection.content (#set! injection.language "graphql"))))

; %relay
(extension_expression
  (extension_identifier) @_name
  (#eq? @_name "relay")
  (expression_statement
    (_ (_) @injection.content (#set! injection.language "graphql") )))

