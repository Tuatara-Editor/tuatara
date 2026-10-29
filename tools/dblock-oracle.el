;;; tools/dblock-oracle.el --- Org's own dynamic blocks of an Org file, written for Tuatara's tests.
;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;
;;; Usage: LC_ALL=C.UTF-8 emacs -Q --batch -l tools/dblock-oracle.el DIRECTORY
;;; (the locale decides how :sort orders letters; Tuatara orders them by their codes)
;;; For each FILE.org of DIRECTORY (test/org/dblock/), writes FILE.expected: the
;;; file as org-update-all-dblocks leaves it, the time taken as 2026-10-02 15:40
;;; (the clock tables' captions keep the time they are made at, which the test
;;; leaves out of the comparison). Tuatara's test (test/org-blocks.lisp) makes the
;;; blocks of the same file and compares the two. Each file is read in its own
;;; folder, as its buffer would be, and the agenda is the files of multi/ there,
;;; in order of name.

(require 'org)
(require 'org-clock)
(require 'cl-lib)

(defvar dblock-oracle-time (encode-time 0 40 15 2 10 2026)
  "The time the blocks are made at.")

(setq org-agenda-files
      (directory-files (expand-file-name "multi" (car command-line-args-left)) t "\\.org\\'"))

(dolist (in (directory-files (car command-line-args-left) t "\\.org\\'"))
  (with-temp-buffer
    (setq default-directory (file-name-directory in))
    (insert-file-contents-literally in)
    (decode-coding-region (point-min) (point-max) 'utf-8)
    (org-mode)
    (cl-letf (((symbol-function 'current-time) (lambda () dblock-oracle-time)))
      (let ((inhibit-message t)) (org-update-all-dblocks)))
    (let ((coding-system-for-write 'utf-8-unix))
      (write-region (point-min) (point-max) (concat (file-name-sans-extension in) ".expected") nil 'quiet))))

(setq command-line-args-left nil)
