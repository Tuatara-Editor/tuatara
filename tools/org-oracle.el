;;; tools/org-oracle.el --- Emacs's own parse of an Org file, written for Tuatara's tests.
;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;
;;; Usage: emacs -Q --batch -l tools/org-oracle.el FILE.org ... (tools/org-oracle.sh)
;;; For each FILE.org, writes FILE.expected: the tree org-element-parse-buffer
;;; makes of it, each node (TYPE PROPERTIES CHILDREN...) with the positions as
;;; offsets from 0 in characters and only the properties Tuatara's parser
;;; gives, strings plain. Tuatara's test (test/org.lisp) parses FILE.org and
;;; compares its tree, written the same way, with this one.

(require 'org)
(require 'org-element)

(defconst org-oracle-common '(:begin :end :contents-begin :contents-end :post-blank :post-affiliated)
  "Positions every node has.")

(defconst org-oracle-properties
  '((headline :level :todo-keyword :todo-type :priority :commentedp :raw-value :tags :archivedp)
    (plain-list :type)
    (item :bullet :checkbox :counter)
    (src-block :language :switches :parameters :value)
    (example-block :switches :value)
    (export-block :type :value)
    (comment-block :value)
    (special-block :type)
    (drawer :drawer-name)
    (node-property :key :value)
    (keyword :key :value)
    (babel-call :call :inside-header :arguments :end-header)
    (dynamic-block :block-name :arguments)
    (latex-environment :value)
    (fixed-width :value)
    (comment :value)
    (footnote-definition :label)
    (table :type :tblfm)
    (table-row :type)
    (planning :closed :deadline :scheduled)
    (clock :status :duration :value)
    (diary-sexp :value)
    (verbatim :value)
    (code :value)
    (link :type :path :format :raw-link)
    (timestamp :type :raw-value)
    (entity :name :use-brackets-p)
    (latex-fragment :value)
    (footnote-reference :label :type)
    (inline-src-block :language :parameters :value)
    (inline-babel-call :call :inside-header :arguments :end-header)
    (macro :key :args)
    (target :value)
    (statistics-cookie :value)
    (subscript :use-brackets-p)
    (superscript :use-brackets-p)
    (export-snippet :back-end :value)
    (citation :style :prefix :suffix)
    (citation-reference :key))
  "The properties written for each type of node, besides the positions.")

(defun org-oracle-value (value)
  "VALUE as written: a timestamp as its raw text, a string plain, a list of them each so."
  (cond ((and (consp value) (eq (car value) 'timestamp)) (org-element-property :raw-value value))
        ((stringp value) (substring-no-properties value))
        ((and (consp value) (not (symbolp (car value)))) (mapcar #'org-oracle-value value))
        ((and (consp value) (memq (org-element-type value) '(plain-text bold italic link)))
         (substring-no-properties (org-element-interpret-data value)))
        (t value)))

(defun org-oracle-node (node)
  "NODE, an element or object of org-element's tree, as written."
  (if (stringp node)
      nil                               ; plain text: its place is between its neighbours'
    (let* ((type (org-element-type node))
           (properties
            (append
             (cl-loop for key in org-oracle-common
                      for value = (org-element-property key node)
                      when value append (list key (if (and (integerp value) (not (eq key :post-blank))) (1- value) value)))
             (cl-loop for key in (cdr (assq type org-oracle-properties))
                      for value = (org-element-property key node)
                      when value append (list key (org-oracle-value value)))
             (let ((name (org-element-property :name node)))
               (and name (list :name name))))))
      (append (list type properties)
              (delq nil (mapcar #'org-oracle-node (org-element-contents node)))))))

(defun org-oracle-file (file)
  "Write FILE.expected beside FILE: Emacs's parse of it."
  (with-temp-buffer
    (insert-file-contents file)
    (let ((org-inhibit-startup t))
      (delay-mode-hooks (org-mode)))
    (let ((tree (org-oracle-node (org-element-parse-buffer)))
          (print-length nil) (print-level nil) (print-escape-newlines nil))
      (with-temp-file (concat (file-name-sans-extension file) ".expected")
        (insert ";; org-element-parse-buffer of " (file-name-nondirectory file)
                ", Org " (org-version) ", by tools/org-oracle.el\n")
        (prin1 tree (current-buffer))
        (insert "\n")))))

(dolist (file command-line-args-left)
  (org-oracle-file (expand-file-name file))
  (message "%s" file))
(setq command-line-args-left nil)
