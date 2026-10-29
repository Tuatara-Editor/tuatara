;;;; lisp/lsp/semantic.lisp --- Code coloured as its language server sees it.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; A server that gives semantic tokens says what each name in the code is: a
;;;; parameter, a field, a macro, a type, a module. With *LSP-SEMANTIC-TOKENS*
;;;; on (M-x lsp-semantic-tokens-mode), each is coloured in the face
;;;; *LSP-SEMANTIC-FACES* gives its kind, over the colours the mode gives the
;;;; syntax; off, the mode's colours alone. Asked for each time the text is sent
;;;; to the server, and moved with the text typed meanwhile.

(in-package #:tuatara.defaults)

(defvar *lsp-semantic-tokens* nil
  "True for code to be coloured as its language server sees it: each name in
the face *LSP-SEMANTIC-FACES* gives its kind, over the mode's colours.")

(defvar *lsp-most-tokens* 200000
  "The most semantic tokens kept for one buffer.")

(defvar *lsp-semantic-faces*
  '(("namespace" . :namespace) ("type" . :type) ("class" . :type) ("enum" . :type) ("interface" . :type)
    ("struct" . :type) ("typeParameter" . :type) ("builtinType" . :type) ("typeAlias" . :type)
    ("parameter" . :variable.parameter) ("variable" . :variable) ("property" . :property)
    ("enumMember" . :constant) ("event" . :property) ("function" . :function) ("method" . :function.method)
    ("macro" . :function.macro) ("keyword" . :keyword) ("selfKeyword" . :keyword) ("modifier" . :keyword.storage.modifier)
    ("comment" . :comment) ("string" . :string) ("number" . :constant.numeric) ("regexp" . :string.regexp)
    ("operator" . :operator) ("decorator" . :attribute) ("attribute" . :attribute) ("lifetime" . :label)
    ("label" . :label) ("escapeSequence" . :constant.character.escape))
  "The face each kind of name a language server tells apart is coloured in,
as (KIND . FACE), KIND as the server names it; a kind not here keeps the
mode's colour.")

(defun lsp-token-kinds ()
  "The kinds of name a server is told the editor colours (*LSP-SEMANTIC-FACES*), as a vector."
  (map 'vector #'car *lsp-semantic-faces*))

(defun lsp-ask-tokens (doc)
  "Ask DOC's server that gives semantic tokens for those of all it shows, kept
to colour the code once they come while its buffer's text is still as asked of."
  (let ((server (and *lsp-semantic-tokens* (doc-server-offers doc "semanticTokensProvider")))
        (buffer (lsp-doc-buffer doc)))
    (when (and server (tuatara.text:json-get (lsp-server-capabilities server) "semanticTokensProvider" "full"))
      (let ((version (lsp-doc-version doc)))
        (lsp-request server "textDocument/semanticTokens/full"
                     (tuatara.text:json-object "textDocument" (doc-identifier doc))
                     (lambda (result error)
                       (when (and (null error) (doc-unchanged-p doc version))
                         (keep-placed buffer 'lsp-tokens doc (decode-tokens doc server (tuatara.text:json-items result "data"))))))))))

(defun decode-tokens (doc server data)
  "DATA, the numbers of SERVER's semantic tokens for DOC, five a token (its
line and start, each from the token before, its length, kind and modifiers),
as a vector of (START END FACE) in DOC's buffer, in order, the kinds of no
face in *LSP-SEMANTIC-FACES* left out; none when DATA holds anything but
whole numbers."
  (let* ((kinds (tuatara.text:json-items (lsp-server-capabilities server) "semanticTokensProvider" "legend" "tokenTypes"))
         (faces (map 'vector (lambda (kind) (let ((face (cdr (assoc kind *lsp-semantic-faces* :test #'equal))))
                                              (and face (tuatara.display:find-face face) face)))
                     kinds))
         (line 0) (character 0) (tokens '()))
    (loop for i from 0 below (if (every #'integerp data) (min (- (length data) 4) (* 5 *lsp-most-tokens*)) 0) by 5
          do (let ((delta-line (aref data i)) (delta-start (aref data (+ i 1))) (length (aref data (+ i 2)))
                   (kind (aref data (+ i 3))))
               (if (zerop delta-line)
                   (incf character delta-start)
                   (setf line (+ line delta-line) character delta-start))
               (let ((face (and (< -1 kind (length faces)) (aref faces kind))))
                 (when face
                   (push (list (doc-line-byte doc server line character :after) (doc-line-byte doc server line (+ character length)) face)
                         tokens)))))
    (coerce (nreverse tokens) 'simple-vector)))

(defun semantic-highlights (editor window rows)
  "The names in ROWS of WINDOW coloured as its language servers see them."
  (declare (ignore editor))
  (let ((buffer (tuatara.display:window-buffer window)))
    (when (and (tuatara.buffer:buffer-local buffer 'lsp-tokens) (plusp (length rows)))
      (loop for (from . to) in (tuatara.display:shown-ranges rows :buffer buffer)
            nconc (loop for (nil . placed) in (tuatara.buffer:buffer-local buffer 'lsp-tokens)
                        ;; from the first token ending past FROM: they are in order and apart
                        append (loop for i from (placed-first placed buffer (lambda (token) (> (second token) from)))
                                       below (length (placed-items placed))
                                     for (start end face) = (placed-item placed i buffer)
                                     while (< start to)
                                     when (< start end) collect (list start end face)))))))

(defun forget-tokens (buffer)
  "BUFFER no longer coloured as its servers see it."
  (setf (tuatara.buffer:buffer-local buffer 'lsp-tokens) nil))

(define-command lsp-semantic-tokens-mode ()
  "Colour code as its language server sees it, or no longer: each name in the
face of its kind (a parameter, a field, a macro), over the mode's colours."
  (toggle-server-showing '*lsp-semantic-tokens* #'lsp-ask-tokens #'forget-tokens "Colours from the language servers"))

(defun install-semantic-tokens ()
  "Colour code as its servers see it, over the mode's colours, while *LSP-SEMANTIC-TOKENS* is on."
  (setf *syntax-highlight-functions* (append (remove 'semantic-highlights *syntax-highlight-functions*)
                                             (list 'semantic-highlights))))
