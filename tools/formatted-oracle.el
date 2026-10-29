;;; tools/formatted-oracle.el --- Denote's own dynamic blocks of notes, written for Tuatara's tests.
;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;
;;; Usage: LC_ALL=C.UTF-8 emacs -Q --batch -l tools/formatted-oracle.el ELPA NOTES EXPECTED
;;; ELPA is the folder Emacs keeps its packages in (~/.config/emacs/elpa), with
;;; denote, denote-org and denote-sequence in it; NOTES the notes directory
;;; (test/org/formatted/notes/); EXPECTED the folder the results go in
;;; (test/org/formatted/expected/). Each note of NOTES whose name has --case-
;;; is visited there, its dynamic blocks made by denote-org (org-update-all-dblocks),
;;; and the text written to EXPECTED as the title of its name with .org; the note
;;; itself is left as it was. Tags are put one space after a headline, as
;;; Tuatara's *org-tags-column* 0 puts them. Tuatara's test (test/org-blocks.lisp) makes
;;; the blocks of the same notes and compares the two.

(require 'org)
(require 'cl-lib)

(let ((elpa (pop command-line-args-left)))
  (dolist (package '("denote" "denote-org" "denote-sequence"))
    (add-to-list 'load-path (car (last (seq-filter #'file-directory-p
                                                  (file-expand-wildcards (expand-file-name (concat package "-[0-9]*") elpa))))))))

(require 'denote)
(require 'denote-org)
(require 'denote-sequence)

(let* ((notes (file-name-as-directory (expand-file-name (pop command-line-args-left))))
       (expected (file-name-as-directory (expand-file-name (pop command-line-args-left))))
       (org-tags-column 0))
  (setq denote-directory notes)
  (dolist (case (directory-files notes t "--case-"))
    (let ((buffer (find-file-noselect case)))
      (with-current-buffer buffer
        (let ((inhibit-message t)) (org-update-all-dblocks))
        (let ((coding-system-for-write 'utf-8-unix)
              (name (progn (string-match "--case-\\([^_.]+\\)" case) (match-string 1 case))))
          (write-region (point-min) (point-max) (concat expected name ".org") nil 'quiet))
        (set-buffer-modified-p nil))
      (kill-buffer buffer))))

(setq command-line-args-left nil)
