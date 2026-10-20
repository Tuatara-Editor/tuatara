; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The colouring rules for robots.txt: runtime/queries/robots.txt/highlights.scm of
; Helix (https://github.com/YardQuit/helix_editor) at commit 9bcdf4d79d14,
; unchanged below this notice.

(comment) @comment

(directive) @attribute

(rule (unknownDirective) @attribute) @diagnostic.error

(userAgent) @namespace

(value) @string

((value) @string.special.path
  (#match? @string.special.path "^/"))

((value) @ui.text.directory
  (#match? @ui.text.directory "^/.+/$"))

((value) @operator
  (#match? @operator "\\*"))

(rule
  (directive (sitemap))
  (value) @string.special.url)

(rule
  (directive (crawlDelay))
  (value) @constant.numeric.integer)

":" @punctuation.delimiter
