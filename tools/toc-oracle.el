;;; tools/toc-oracle.el --- toc-org's own table of contents of an Org file, written for Tuatara's tests.
;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;
;;; Usage: emacs -Q --batch -l tools/toc-oracle.el DIRECTORY
;;; For each FILE.org and FILE.md of DIRECTORY (test/org/toc/), writes
;;; FILE.expected: the file as toc-org (Sergei Nosov's package, from MELPA,
;;; installed in your Emacs's elpa directory) leaves it on saving, its table of
;;; contents made again under the first headline tagged :TOC:. A Markdown file
;;; is read in a markdown-mode of its own, as toc-org only asks for the mode's
;;; name. Tuatara's test (test/org-blocks.lisp) makes the table of the same file and
;;; compares the two, character for character.

(add-to-list 'load-path (car (file-expand-wildcards (expand-file-name "~/.config/emacs/elpa/toc-org-*"))))
(require 'org)
(require 'toc-org)

(define-derived-mode markdown-mode text-mode "Markdown"
  "Markdown, as toc-org asks for it by name.")

(dolist (in (directory-files (car command-line-args-left) t "\\.\\(org\\|md\\)\\'"))
  (with-temp-buffer
    (insert-file-contents-literally in)
    (decode-coding-region (point-min) (point-max) 'utf-8)
    (if (string-suffix-p ".md" in) (markdown-mode) (org-mode))
    (setq toc-org-hrefify-hash (make-hash-table :test 'equal))
    (let ((inhibit-message t)) (toc-org-insert-toc))
    (let ((coding-system-for-write 'utf-8-unix))
      (write-region (point-min) (point-max) (concat (file-name-sans-extension in) ".expected") nil 'quiet))))

(setq command-line-args-left nil)
