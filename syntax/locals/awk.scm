; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The rules for local names in awk: runtime/queries/awk/locals.scm of
; Helix (https://github.com/YardQuit/helix_editor) at commit 9bcdf4d79d14,
; unchanged below this notice.

; Scopes

(func_def) @local.scope

; Definitions

; awk has no local declarations; extra params are the idiom for locals.
(func_def
  (param_list
    (identifier) @local.definition.variable.parameter))

; References

(identifier) @local.reference

; Function names in call position are not variable references.
(func_call
  name: (identifier) @_)
