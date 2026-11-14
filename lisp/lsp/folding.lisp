;;;; lisp/lsp/folding.lisp --- Code folded as its language server sees its blocks; the selection grown by its syntax.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; A server that knows a file's blocks (a function's body, a struct, a run
;;;; of imports or comments) says where they begin and end. M-x lsp-fold
;;;; hides the lines of the block the cursor is in under its first line, an
;;;; ellipsis in their place, and shows them again; M-x lsp-fold-all hides every
;;;; outermost block, or shows all again. Where a file has no syntax tree,
;;;; M-x expand-selection grows the selection as its server sees the code.

(in-package #:tuatara.lsp)

(defun lsp-ask-folding-ranges (buffer then)
  "Ask BUFFER's server for the blocks it knows, and call THEN with them once it
answers, when BUFFER is as it was asked of: each (START END) of the octets a
fold of it hides, from the end of a block's first line to the end of its last.
Says so when there is no such server."
  (with-lsp-server (doc server) (buffer "foldingRangeProvider" "folding"
                                        :none "No language server here knows the file's blocks")
    (lsp-ask-doc server doc "textDocument/foldingRange"
                 (tuatara.text:json-object "textDocument" (doc-identifier doc))
                 "find the file's blocks"
                 (lambda (ranges)
                   (let ((rope (tuatara.buffer:buffer-text buffer)))
                     (funcall then
                              (loop for r across (tuatara.text:json-items ranges)
                                    for first = (tuatara.text:json-integer r "startLine")
                                    for last = (tuatara.text:json-integer r "endLine")
                                    when (and first last (<= 0 first) (< first last)
                                              (< last (max 1 (tuatara.text:rope-lines rope))))
                                    collect (list (line-end-byte rope first) (line-end-byte rope last)))))))))

(define-command lsp-fold ()
  "Hide the lines of the block the cursor is in (a function's body, a struct,
a run of imports), as its language server knows them, under the block's first
line; on a line whose block is hidden, show it again."
  (let* ((buffer (current-buffer))
         (here (point))
         (line-end (line-end-byte (tuatara.buffer:buffer-text buffer)
                                  (tuatara.text:byte->line (tuatara.buffer:buffer-text buffer) here)))
         (hidden (find-if (lambda (fold) (= (car fold) line-end)) (tuatara.display:buffer-folds buffer))))
    (if hidden
        (tuatara.display:unfold-region buffer (car hidden) (cdr hidden))
        (lsp-ask-folding-ranges
         buffer
         (lambda (ranges)
           ;; the innermost block holding the cursor's line, or beginning on it
           (let ((block (first (sort (remove-if-not (lambda (r) (<= (first r) line-end (second r))) (copy-list ranges))
                                     #'< :key (lambda (r) (- (second r) (first r)))))))
             (if (null block)
                 (message "No block here")
                 (progn (when (and (< (first block) here) (eq buffer (current-buffer))) (goto (first block)))
                        (tuatara.display:fold-region buffer (first block) (second block))))))))))

(defun outermost-ranges (ranges)
  "RANGES, each (START END), but those inside another of them and those given
twice, in order of their start: each looked at once, after a sort."
  (let ((reach -1))
    (loop for range in (sort (remove-duplicates ranges :test #'equal)
                             (lambda (a b) (or (< (first a) (first b))
                                               (and (= (first a) (first b)) (> (second a) (second b))))))
          when (> (second range) reach)
          collect range
          do (setf reach (max reach (second range))))))

(define-command lsp-fold-all ()
  "Hide every outermost block of the file, as its language server knows them,
under its first line; with any hidden, show all again."
  (let ((buffer (current-buffer)))
    (if (tuatara.display:buffer-folds buffer)
        (progn (tuatara.display:unfold-all buffer) (message "All shown"))
        (lsp-ask-folding-ranges
         buffer
         (lambda (ranges)
           (let ((outermost (outermost-ranges ranges)))
             (dolist (r outermost)
               (tuatara.display:fold-region buffer (first r) (second r)))
             (when (eq buffer (current-buffer))
               (let ((fold (tuatara.display:fold-at buffer (point))))
                 (when fold (goto (car fold)))))
             (message "~d block~:p hidden" (length outermost))))))))

;;; The selection grown by the server's syntax

(defun lsp-selection-ranges (buffer position)
  "The ranges around POSITION of BUFFER its server sees as pieces of the code,
the smallest first, each (START . END), a LATER (LSP-LATER) giving NIL when
the server does not answer in time (said in the echo area); NIL when there is
no such server. One of *SELECTION-RANGES-FUNCTIONS*."
  (with-lsp-server (doc server) (buffer "selectionRangeProvider" "selection-range" :position position)
    (lsp-later doc server "textDocument/selectionRange"
               (tuatara.text:json-object "textDocument" (doc-identifier doc)
                                         "positions" (vector (doc-position doc server position)))
               "grow the selection"
               (lambda (result)
                 (loop for r = (tuatara.text:json-get result 0) then (tuatara.text:json-get r "parent")
                       while (hash-table-p r)
                       for (start end) = (multiple-value-list
                                          (doc-range-bytes doc server (tuatara.text:json-get r "range")))
                       when start
                       collect (cons start end))))))
