; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The colouring rules for kcl: runtime/queries/kcl/highlights.scm of
; Helix (https://github.com/helix-editor/helix) at commit 9bcdf4d79d14,
; unchanged below this notice.

;; Maps AST nodes (left) to highlighting classes (right)
;; See https://docs.helix-editor.com/themes.html#scopes
;; for the supported scopes.
;; Don't forget to run the command `hx --grammar fetch` to fetch the grammars,
;; and `hx --grammar build` to build any out-of-date grammars.

"fn" @keyword.function
"return" @keyword.control.return
"import" @keyword.control.import
"export" @keyword.control.import
[
 "if"
 "else"
 ] @keyword.control.conditional
(identifier) @variable

;; highlight type names
(type_name
  (identifier) @type
) @type

(fn_call
  callee: (identifier) @function
  (labeledArg
    label: (identifier) @variable.parameter
  )
)


;; operators
(binary_operator) @operator
(prefix_operator) @operator

;; punctuation

; ".." @punctuation.special

"(" @punctuation.bracket
")" @punctuation.bracket
"[" @punctuation.bracket
"]" @punctuation.bracket
"{" @punctuation.bracket
"}" @punctuation.bracket

; "." @punctuation.delimiter
"," @punctuation.delimiter
; ":" @punctuation.delimiter
; ";" @punctuation.delimiter

;; literals
(boolean) @constant.builtin.boolean
(string) @string
(number) @constant.numeric

;; comments
(shebang) @keyword.directive
(comment) @comment
