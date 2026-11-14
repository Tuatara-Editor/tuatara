;;;; lisp/modes/markdown-links.lisp --- Following the links of a Markdown file.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; C-c C-o and RET in NORMAL state follow the link point is on: to a heading of
;;;; the file, an address in the desktop's program, or a file. A heading is found
;;;; with the table of contents' reading of the text (lisp/org/toc.lisp).

(in-package #:tuatara.defaults)

(defparameter *markdown-link-re*
  "(!)?\\[([^\\]^][^\\]]*|)\\]\\(([^)]*?)(?:\\s+(\"[^\"]*\"))?\\)"
  "A Markdown link, [label](target \"title\"), or a picture's, ![label](target):
the label the second group, the target the third.")

(defun markdown-link-at (text position)
  "The Markdown link of TEXT holding the character POSITION, as two values: its
label and its target; NIL when there is none there. Only the line is looked
at, at most *TOC-LONGEST-LINE* characters of it."
  (let* ((start (string-line-start text position))
         (end (min (string-line-end text position) (+ start *toc-longest-line*))))
    (cl-ppcre:do-scans (s e starts ends *markdown-link-re* text nil :start start :end end)
      (when (and (<= s position) (< position e))
        (return-from markdown-link-at
          (values (subseq text (aref starts 1) (aref ends 1)) (subseq text (aref starts 2) (aref ends 2))))))
    nil))

(defun markdown-heading-named (text label)
  "The character of the Markdown TEXT where the first heading whose title is
LABEL begins (#s, a space, then LABEL); NIL when none is."
  (let ((view (toc-markdown-view text)) (start 0) (n (length text)))
    (loop while (< start n)
          do (let ((end (or (position #\Newline view :start start) n)))
               (when (and (< start end) (char= (char view start) #\*))
                 (let ((title (string-left-trim "* " (subseq view start end))))
                   (when (uiop:string-prefix-p label title) (return start))))
               (setf start (1+ end))))))

(defun markdown-follow-link (buffer)
  "Follow the Markdown link point is on in BUFFER: to the heading of this file
#an-anchor (a table of contents's) leads to, else one titled as its label; to
a web or mail address in the desktop's program, after asking; to a file,
visited. True when point is on a link."
  (let* ((point (tuatara.buffer:buffer-point buffer))
         (start (tuatara.buffer:buffer-line-start buffer point))
         (line (buffer-line-head buffer point (* 4 *toc-longest-line*)))
         (column (and (<= (- point start) (tuatara.text:string-octet-length line))
                      (length (tuatara.buffer:buffer-string buffer :start start :end point)))))
    (multiple-value-bind (label target) (and column (markdown-link-at line column))
      (when target
        (cond ((uiop:string-prefix-p "#" target)
               (let* ((parse (org-text-now buffer))
                      (text (org-parse-text parse))
                      (at (or (toc-target-position text target :markdown t) (markdown-heading-named text label))))
                 (if at
                     (org-goto-char parse at)
                     (message "No heading here for ~a" target))))
              ((open-address-p target) (open-address target))
              ((string= target "") (message "The link leads nowhere"))
              (t (visit-path (tuatara.io:native (tuatara.io:expand-path (subseq target 0 (or (position #\# target)
                                                                                             (length target)))
                                                                        :directory (buffer-directory buffer))))))
        t))))

(define-command markdown-follow-link-at-point ()
  "Follow the Markdown link point is on (\\`C-c C-o', and \\`RET' in NORMAL state): to
the heading a table of contents's link (#an-anchor) leads to, a web or mail
address in the desktop's program after asking, or a file, visited."
  (unless (markdown-follow-link (current-buffer))
    (message "No link here")))

(defun markdown-follow-link-for-return (buffer)
  "For \\`RET' in NORMAL state: follow the Markdown link point is on, when
BUFFER is in Markdown mode; true when it did."
  (and (toc-markdown-p buffer) (markdown-follow-link buffer)))

(defun install-markdown-links ()
  "Bind Markdown's keys for links: \\`C-c C-o' follows the one point is on, and
\\`RET' in NORMAL state does too."
  (define-key (mode-keymap 'markdown-mode) "C-c C-o" 'markdown-follow-link-at-point)
  (pushnew 'markdown-follow-link-for-return *enter-dwim-functions*))
