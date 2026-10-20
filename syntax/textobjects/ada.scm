; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The selection rules for ada: runtime/queries/ada/textobjects.scm of
; Helix (https://github.com/YardQuit/helix_editor) at commit 9bcdf4d79d14,
; unchanged below this notice.

;; Support for high-level text objects selections.
;; For instance:
;;    maf     (v)isually select (a) (f)unction or subprogram
;;    mif     (v)isually select (i)nside a (f)unction or subprogram
;;    mai     (v)isually select (a) (i)f statement (or loop)
;;    mii     (v)isually select (i)nside an (i)f statement (or loop)
;;
;; For navigations using textobjects, check link below:
;; https://docs.helix-editor.com/master/usage.html#navigating-using-tree-sitter-textobjects
;;
;; For Textobject queries explanation, check out link below:
;; https://docs.helix-editor.com/master/guides/textobject.html

(subprogram_body) @function.around
(subprogram_body (non_empty_declarative_part) @function.inside)
(subprogram_body (handled_sequence_of_statements) @function.inside)
(function_specification) @function.around
(procedure_specification) @function.around
(package_declaration) @function.around
(generic_package_declaration) @function.around
(package_body) @function.around
