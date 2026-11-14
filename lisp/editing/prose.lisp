;;;; lisp/editing/prose.lisp --- Telling prose from code, tables and links in any mode.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; prose-p says whether a position is in running text, in an Org note by
;;;; its parse (lisp/org/prose.lisp), in other modes of text by the syntax
;;;; tree, in a programming language inside a comment. Loaded after Org.

(in-package #:tuatara.defaults)

(defparameter *laid-out-node-names*
  (concatenate 'string "code|fence|table|link_destination|uri|url|math|formula|equation|verbatim|raw|literal|"
               "front_matter|metadata|html_block")
  "A pattern matching the names a language's syntax tree gives the parts of a
document that are not prose: code, tables, link targets, maths.")


(defun half-typed-p (line-text)
  "True when LINE-TEXT, a line up to a position, leaves something open that is
not prose: an Org link with no ]] yet, a Markdown link's target with no ) yet,
an odd number of backquotes (a code span), or an Org ~code~ or =verbatim=
begun after a blank or at the line's start and not closed yet."
  (or (cl-ppcre:scan "\\[\\[[^\\]]*$" line-text)
      (cl-ppcre:scan "\\]\\([^)]*$" line-text)
      (oddp (count #\` line-text))
      (cl-ppcre:scan "(?:^|[ \\t])(?:~[^ \\t~][^~]*|=[^ \\t=][^=]*)$" line-text)))


(defun line-before (buffer position)
  "The text of BUFFER's line up to POSITION."
  (tuatara.buffer:buffer-string buffer :start (tuatara.buffer:buffer-line-start buffer position) :end position))


(defun prose-buffer-p (buffer)
  "True when BUFFER's major mode is one for text, prose throughout but its
code, tables, links and maths: text-mode and those made from it (Org among
them), and fundamental-mode itself; false in a programming language, whose
prose is its comments."
  (let ((mode (buffer-major-mode buffer)))
    (or (typep mode 'text-mode) (eq (type-of mode) 'fundamental-mode))))


(defun laid-out-node-p (syntax position)
  "True when POSITION of SYNTAX's text is inside a part its tree names as not
prose (*LAID-OUT-NODE-NAMES*)."
  (some (lambda (node)
          (and (tuatara.syntax:tree-node-named node)
               (cl-ppcre:scan *laid-out-node-names* (tuatara.syntax:tree-node-name node))))
        (tuatara.syntax:node-ancestry syntax position (1+ position))))


(defun prose-ready-p (buffer)
  "True when PROSE-P can tell for BUFFER now: an Org buffer, one with no
syntax tree, or one whose syntax tree is up to date."
  (or (typep (buffer-major-mode buffer) 'org-mode)
      (null (tuatara.syntax:buffer-syntax buffer))
      (and (ready-syntax buffer) t)))


(defun prose-p (buffer position)
  "True when the octet POSITION of BUFFER is in prose: in an Org buffer as its
parse tells; in another mode of text (PROSE-BUFFER-P) outside the code,
tables, links and maths its syntax tree names, and anywhere in one with no
syntax tree; in a programming language only inside a comment. Never in a
line's link or code span not closed yet. NIL where a syntax tree is needed
and is not ready (PROSE-READY-P)."
  (cond ((typep (buffer-major-mode buffer) 'org-mode) (org-prose-p buffer position))
        ((prose-buffer-p buffer)
         (or (null (tuatara.syntax:buffer-syntax buffer))
             (let ((syntax (ready-syntax buffer)))
               (and syntax
                    (not (laid-out-node-p syntax position))
                    (not (half-typed-p (line-before buffer position)))))))
        (t (let ((syntax (ready-syntax buffer)))
             (and syntax (eq (tuatara.syntax:text-kind-around syntax position) :comment))))))


(defun prose-test (buffer)
  "A function telling, as PROSE-P does, whether an octet position of BUFFER is
in prose, for positions asked in order from the first: an Org buffer's parse
read once for all of them."
  (if (typep (buffer-major-mode buffer) 'org-mode)
      (let* ((parse (org-parse-now buffer))
             (text (and parse (org-parse-text parse)))
             (ranges (org-buffer-laid-out-ranges buffer (and parse (org-parse-tree parse)) text)))
        (lambda (position)
          (let ((at (org-parse-char parse position)))
            (loop while (and ranges (<= (cdar ranges) at)) do (pop ranges))
            (org-text-prose-p ranges text at))))
      (lambda (position) (prose-p buffer position))))

