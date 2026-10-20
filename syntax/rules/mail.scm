; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The colouring rules for mail: runtime/queries/mail/highlights.scm of
; Helix (https://github.com/YardQuit/helix_editor) at commit 9bcdf4d79d14,
; unchanged below this notice.

; header fields
[
  (header_field_email)
  (header_field_subject)
  (header_field)
] @keyword

; delimited punctuation
(header_separator) @punctuation.delimiter
(email_delimiter) @punctuation.delimiter

; email subject contents
(header_subject
  (subject) @markup.bold)
; extra metadata headers
(header_other
  (header_unstructured) @comment)

; Addressee Name (Firstname, Lastname, etc.)
(atom) @variable

; Email Address
(email) @string

; Quoted Reply
(quote_marker) @punctuation.special
(quote_contents) @markup.quote

