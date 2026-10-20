; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The colouring rules for comment: runtime/queries/comment/highlights.scm of
; Helix (https://github.com/YardQuit/helix_editor) at commit 9bcdf4d79d14,
; unchanged below this notice.

(tag
 (name) @ui.text
 (user)? @constant)

; Hint level tags
((tag (name) @hint)
 (#any-of? @hint "HINT" "MARK" "PASSED" "STUB" "MOCK" "TIP"))

("text" @hint
 (#any-of? @hint "HINT" "MARK" "PASSED" "STUB" "MOCK" "TIP"))

; Info level tags
((tag (name) @info)
 (#any-of? @info "INFO" "NOTE" "TODO" "TO-DO" "PERF" "OPTIMIZE" "PERFORMANCE" "QUESTION" "ASK" "REVIEW" "PR" "CR"))

("text" @info
 (#any-of? @info "INFO" "NOTE" "TODO" "TO-DO" "PERF" "OPTIMIZE" "PERFORMANCE" "QUESTION" "ASK" "REVIEW" "PR" "CR"))

; Warning level tags
((tag (name) @warning)
 (#any-of? @warning "HACK" "WARN" "WARNING" "TEST" "TEMP"))

("text" @warning
 (#any-of? @warning "HACK" "WARN" "WARNING" "TEST" "TEMP"))

; Error level tags
((tag (name) @error)
 (#any-of? @error "BUG" "FIXME" "ISSUE" "XXX" "FIX" "SAFETY" "FIXIT" "FAILED" "DEBUG" "INVARIANT" "COMPLIANCE" "PANIC" "SECURITY"))

("text" @error
 (#any-of? @error "BUG" "FIXME" "ISSUE" "XXX" "FIX" "SAFETY" "FIXIT" "FAILED" "DEBUG" "INVARIANT" "COMPLIANCE" "PANIC" "SECURITY"))

; Issue number (#123)
("text" @constant.numeric
 (#match? @constant.numeric "^#[0-9]+$"))

; User mention (@user)
("text" @tag
 (#match? @tag "^[@][a-zA-Z0-9_-]+$"))

(uri) @markup.link.url
