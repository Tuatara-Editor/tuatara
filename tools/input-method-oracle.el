;;; tools/input-method-oracle.el --- Emacs's input methods written as Lisp data for Tuatara.
;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;
;;; Usage: emacs -Q --batch -l tools/input-method-oracle.el OUTPUT
;;; Writes OUTPUT (lisp/selecting/input-method-table.lisp): for each input method
;;; named below, its name, the title Emacs shows in the mode line, the first
;;; line of its description, and every rule of its table, the keys typed and
;;; the text they give (the first choice, where a rule offers several). The
;;; tables are Emacs's (leim/quail, GPL-3.0-or-later). rfc1345 is given its
;;; name, title and description only: Tuatara makes its rules from the digraph
;;; table, each digraph after an ampersand, as Emacs's are.

(require 'quail)
(require 'cl-lib)

(defvar input-method-oracle-methods
  '("rfc1345" "swedish-postfix" "norwegian-postfix" "danish-postfix" "finnish-postfix" "polish-slash"
    "german-postfix" "french-postfix" "spanish-postfix" "italian-postfix" "latin-postfix" "latin-prefix")
  "The input methods written out.")

(defun input-method-oracle-rules ()
  "The rules of the current Quail package, as (KEYS . TEXT), in order of KEYS."
  (let ((rules '()))
    (cl-labels ((walk (map keys)
                  (let* ((tr (car map))
                         (text (cond ((characterp tr) (string tr))
                                     ((stringp tr) tr)
                                     ((and (vectorp tr) (> (length tr) 0))
                                      (let ((first (aref tr 0))) (if (characterp first) (string first) first)))
                                     ((and (consp tr) (vectorp (cdr tr)) (> (length (cdr tr)) 0))
                                      (let ((first (aref (cdr tr) 0))) (if (characterp first) (string first) first))))))
                    (when (and (stringp text) (> (length keys) 0)) (push (cons keys text) rules)))
                  (dolist (e (cdr map))
                    (when (and (consp e) (characterp (car e)))
                      (walk (cdr e) (concat keys (string (car e))))))))
      (walk (quail-map) ""))
    (sort rules (lambda (a b) (string< (car a) (car b))))))

(let ((out (car command-line-args-left))
      (print-escape-newlines t))
  (with-temp-file out
    (set-buffer-file-coding-system 'utf-8-unix)
    (insert ";;;; lisp/selecting/input-method-table.lisp --- Emacs's input methods as data: written by tools/input-method-oracle.el.\n"
            ";;;; The tables are GNU Emacs's (leim/quail), GPL-3.0-or-later; see LICENSE.\n\n"
            "(in-package #:tuatara.defaults)\n\n"
            "(defparameter *input-method-table*\n  '(")
    (dolist (name input-method-oracle-methods)
      (activate-input-method name)
      (let ((doc (replace-regexp-in-string "\\\\=" "" (car (split-string (or (quail-docstring) "") "\n")))))
        (insert (format "(%S %S %S\n     " name (quail-title) doc))
        (if (string= name "rfc1345")
            (insert ":digraphs")
          (insert "(")
          (dolist (rule (input-method-oracle-rules))
            (insert (format "(%S . %S) " (car rule) (cdr rule))))
          (delete-char -1)
          (insert ")")))
      (insert ")\n    ")
      (deactivate-input-method))
    (delete-char -5)
    (insert ")\n  \"The input methods Tuatara has, each (NAME TITLE DESCRIPTION RULES):
the title the mode line shows while it is on, RULES each (KEYS . TEXT), or
:DIGRAPHS for the digraphs after an ampersand (*DIGRAPH-TABLE*).\")\n")))

(setq command-line-args-left nil)
