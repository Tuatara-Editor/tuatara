;;;; lisp/modes/org.lisp --- Org files: the mode, ahead of Tuatara's own Org implementation.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.defaults)

(define-major-mode org-mode text-mode
  "Org documents: outlines, notes, tasks and literate programs, in
Org's syntax: headlines folded with TAB and S-TAB and moved with M-arrows, TODO states,
tags, timestamps, scheduling and deadlines for the agenda, checkboxes, tables
and their formulas, links, the clock, dynamic blocks and source blocks to
tangle; \\[describe-bindings] lists its keys."
  :lighter "Org" :comment "#" :extensions ("org")
  ;; past what Org's own readers take (the agenda, the clock table): a larger file opens in text mode
  :largest-file *org-file-read-limit*)

(defmethod mode-unfilled-line-p ((mode org-mode) text)
  "A headline, a keyword or comment line (#+, # ), a table's line, a drawer's
or a property's, a fixed-width line (: ), a planning or clock line."
  (or (and (plusp (length text)) (char= (char text 0) #\*))
      (cl-ppcre:scan "^\\s*(?:#\\+|#(?:\\s|$)|\\||:|(?:SCHEDULED|DEADLINE|CLOSED|CLOCK):)" text)))

(defparameter *org-laid-out-blocks* '("src" "example" "export" "comment")
  "The Org blocks whose contents are not prose; a quote, verse or centred
block is, and so is one of a name of your own (#+begin_note).")

(defun org-block-line-kind (text)
  "What TEXT, a line of Org, is to the blocks kept as written: :VERBATIM when it
begins one whose contents are not prose (*ORG-LAID-OUT-BLOCKS*), :OUTSIDE
when it ends a block or begins one of prose, else NIL."
  (let ((block (nth-value 1 (cl-ppcre:scan-to-strings "^(?i)\\s*#\\+(begin|end)_(\\S+)" text))))
    (cond ((null block) nil)
          ((string-equal (aref block 0) "end") :outside)
          ((member (aref block 1) *org-laid-out-blocks* :test #'string-equal) :verbatim)
          (t :outside))))

(defmethod mode-verbatim-line-p ((mode org-mode) buffer line)
  "Inside a block whose contents are not prose (*ORG-LAID-OUT-BLOCKS*): the
nearest #+begin_ or #+end_ line above is a #+begin_ of such a block."
  (eq :verbatim (lines-above buffer line #'org-block-line-kind)))

(defmethod mode-verbatim-after ((mode org-mode) text verbatim)
  "A #+begin_ or #+end_ line says; any other line leaves it as it was."
  (case (org-block-line-kind text)
    (:verbatim t)
    (:outside nil)
    (t verbatim)))
