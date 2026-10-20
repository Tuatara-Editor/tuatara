; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The rules for local names in _javascript: runtime/queries/_javascript/locals.scm of
; Helix (https://github.com/YardQuit/helix_editor) at commit 9bcdf4d79d14,
; unchanged below this notice.

; Definitions
;------------
; Javascript and Typescript Treesitter grammars deviate when defining the
; tree structure for parameters, so we need to address them in each specific
; language instead of ecma.

; (i)
(formal_parameters 
  (identifier) @local.definition.variable.parameter)

; (i = 1)
(formal_parameters 
  (assignment_pattern
    left: (identifier) @local.definition.variable.parameter))
