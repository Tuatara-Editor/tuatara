; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The rules naming the definitions and headings in markdown: runtime/queries/markdown/tags.scm of
; Helix (https://github.com/helix-editor/helix) at commit 9bcdf4d79d14,
; unchanged below this notice but where a comment says what was changed.

; changed: the section a heading opens, so that a heading under another is outlined under it, and underlined (setext) headings too; it read (atx_heading (inline) @name) @definition.section
(section . (atx_heading (inline) @name)) @definition.section
(setext_heading (paragraph) @name) @definition.section
