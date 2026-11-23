; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The indentation rules for janet: runtime/queries/janet/indents.scm of
; Helix (https://github.com/helix-editor/helix) at commit 9bcdf4d79d14,
; unchanged below this notice.

; aligns forms to the second position if there's two in a line:
; (-> 10
;     (* 2)
;     (print))
(par_tup_lit . (sym_lit) @first . (_) @anchor

  (#same-line? @first @anchor)
  ; anything that doesn't match should be indented normally
  ; from https://github.com/janet-lang/spork/blob/5601dc883535473bca28351cc6df04ed6c656c65/spork/fmt.janet#L87C12-L93C38
  (#not-match? @first "^(fn|match|with|with-dyns|def|def-|var|var-|defn|defn-|varfn|defmacro|defmacro-|defer|edefer|loop|seq|tabseq|catseq|generate|coro|for|each|eachp|eachk|case|cond|do|defglobal|varglobal|if|when|when-let|when-with|while|with-syms|with-vars|if-let|if-not|if-with|let|short-fn|try|unless|default|forever|upscope|repeat|forv|compwhen|compif|ev/spawn|ev/do-thread|ev/spawn-thread|ev/with-deadline|label|prompt|forever)$")) @align

; everything else should be indented normally:
;
; (let [foo 10]
;   (print foo))
;
; (foo
;   bar)
(par_tup_lit . (sym_lit)) @indent

; for `{}` and `[]`:
; {:foo 10
;  :bar 20}
(struct_lit . (_) @anchor) @align

; [foo
;  bar]
(sqr_tup_lit . (_) @anchor) @align
