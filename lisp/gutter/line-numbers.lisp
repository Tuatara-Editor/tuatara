;;;; lisp/gutter/line-numbers.lisp --- Line numbers in the gutter, absolute or relative.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.defaults)

(defun add-gutter-provider (name width cell &key prepare first)
  "Put the gutter provider NAME, with its functions WIDTH, CELL and PREPARE
as *GUTTER-PROVIDERS* describes them, in the gutter in place of one of that
name: after the others, or with FIRST before them."
  (let ((provider (list* :name name :width width :cell cell (and prepare (list :prepare prepare))))
        (others (remove name *gutter-providers* :key (lambda (p) (getf p :name)) :test #'equal)))
    (setf *gutter-providers* (if first (cons provider others) (append others (list provider))))))

(define-display-toggle line-numbers "Line numbers"
  "Show absolute line numbers in the gutter of the current buffer, or none:
each line its own number, from 1. In a buffer showing relative ones, absolute
ones in their place."
  :command display-line-numbers-mode :lookup line-numbers-mode
  :global *line-numbers-default*
  :global-doc "Line numbers for buffers that have not chosen, and whose major mode
*LINE-NUMBERS-MODES* says nothing of: NIL for none, :absolute, or :relative
\(the current line shows its own number, the others their distance). Set it in
the init file, (setf *line-numbers-default* :relative) for example;
\\[toggle-line-numbers] and the other commands still choose for one buffer,
and \\[global-display-line-numbers-mode] switches it while the editor runs."
  :what "show line numbers in the gutter"
  :example "'((prog-mode . :relative) (org-mode . nil))"
  :values ((:absolute "absolute") (:relative "relative")))

(defun past-text-line-p (buffer line)
  "True when LINE of BUFFER is the empty one after its last newline, or the
only line of an empty buffer: no part of the text, only a place for point."
  (and (= line (1- (tuatara.buffer:buffer-line-count buffer)))
       (= (tuatara.buffer:buffer-goto-line buffer line) (tuatara.buffer:buffer-size buffer))))

(defun line-numbers-width (editor window)
  "Gutter columns for line numbers in WINDOW: digits of the last line number, or 0 when off.
The line after the text's last newline has no number, so it does not count.
While a large file's lines are counted, the digits the count so far foresees,
a tenth more to spare; 0 until it has counted any."
  (declare (ignore editor))
  (let ((buffer (tuatara.display:window-buffer window)))
    (cond ((null (line-numbers-mode buffer)) 0)
          ((tuatara.buffer:buffer-indexed-p buffer)
           (let ((count (tuatara.buffer:buffer-line-count buffer)))
             (length (princ-to-string (max 1 (if (past-text-line-p buffer (1- count)) (1- count) count))))))
          (t (multiple-value-bind (lines reached) (tuatara.io:counted-lines buffer)
               (if (and lines (plusp reached))
                   (length (princ-to-string (max 1 (ceiling (* 11/10 (1+ lines) (tuatara.buffer:buffer-size buffer)) reached))))
                   0))))))

(defun line-numbers-cell (editor window row line)
  "The number shown beside ROW: for a row that goes on from the one above, none,
or where long lines wrap at a word's edge the wrap indicator
(*SOFT-WRAP-INDICATOR*); in relative mode the distance from the window's
current line, which shows its own number. The line after the text's last
newline, where point can rest though it holds no text, shows ~ instead."
  (declare (ignore editor row))
  (let ((buffer (tuatara.display:window-buffer window)))
    (if (null line)
        (if (and *soft-wrap-indicator* (soft-wrap-p buffer))
            (values *soft-wrap-indicator* :wrap-indicator)
            (values "" :line-number))
        ;; while the lines are counted, point's line where the count has passed it
        (let ((current (tuatara.buffer:buffer-known-line-number buffer (tuatara.display:window-point window)))
              (counted (tuatara.buffer:buffer-indexed-p buffer)))
          (cond ((and counted (past-text-line-p buffer line)) (values "~" :line-number))
                ((eql line current) (values (princ-to-string (1+ line)) :current-line-number))
                ((and current (eq (line-numbers-mode buffer) :relative))
                 (values (princ-to-string (abs (- line current))) :line-number))
                (t (values (princ-to-string (1+ line)) :line-number)))))))

(define-command display-relative-line-numbers-mode ()
  "Show relative line numbers in the gutter of the current buffer, or none:
the current line shows its number, the others how many lines away they are,
which is what counted motion like \\[universal-argument] 7 \\<normal>\\[next-line]
expects; with a count, on when it is positive, off when it is zero or less."
  (switch-display 'line-numbers :value :relative))

(define-command toggle-line-numbers ()
  "Cycle the gutter of the current buffer through no line numbers, absolute, relative."
  (set-display 'line-numbers (case (line-numbers-mode) ((nil) :absolute) (:absolute :relative) (t nil))))

(defun install-line-numbers ()
  "Register the line-number gutter provider."
  (add-gutter-provider :line-numbers #'line-numbers-width #'line-numbers-cell))
