; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The rules naming the definitions and headings in clojure: runtime/queries/clojure/tags.scm of
; Helix (https://github.com/helix-editor/helix) at commit 9bcdf4d79d14,
; unchanged below this notice.

; (defn name ...), (defmethod ...), etc.  The form symbol selects the kind;
; the following symbol is the defined name. @definition.* captures the whole
; form; @name captures the defined symbol within it.
((list_lit
  (sym_lit) @_kw
  .
  (sym_lit) @name) @definition.function
 (#any-of? @_kw "defn" "defn-" "defmethod" "defmulti"))

((list_lit
  (sym_lit) @_kw
  .
  (sym_lit) @name) @definition.macro
 (#eq? @_kw "defmacro"))

((list_lit
  (sym_lit) @_kw
  .
  (sym_lit) @name) @definition.interface
 (#any-of? @_kw "defprotocol" "definterface"))

((list_lit
  (sym_lit) @_kw
  .
  (sym_lit) @name) @definition.struct
 (#any-of? @_kw "defrecord" "deftype" "defstruct"))

((list_lit
  (sym_lit) @_kw
  .
  (sym_lit) @name) @definition.module
 (#eq? @_kw "ns"))

((list_lit
  (sym_lit) @_kw
  .
  (sym_lit) @name) @definition.constant
 (#any-of? @_kw "def" "defonce"))
