;;;; lisp/editing/tabs.lisp --- Tabs: the columns a tab takes, and what TAB puts in.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; A tab character reaches the next tab stop, the stops being every
;;;; *tab-width* columns. TAB indents the line where the major mode indents
;;;; (Lisp, Rust, C, shell scripts), and completes the name at point when the
;;;; line was indented already (*tab-always-indent*); elsewhere, and M-i
;;;; anywhere, it goes on to the next stop, putting spaces in, or a tab
;;;; character where *indent-tabs-mode* says so. Both are Emacs's settings
;;;; by Emacs's names, with 4 and spaces as the standard. Each can be set for a
;;;; major mode and the modes derived from it (set-tab-width,
;;;; set-indent-tabs-mode), and M-x indent-tabs-mode switches one buffer: the
;;;; buffer's own setting counts first, then its mode's, then that of the mode
;;;; it derives from, and so on, then the one for all.

(in-package #:tuatara.defaults)

(defvar *tab-width* 4
  "The columns from one tab stop to the next: a tab character shows its text
on at the next stop, and \\[indent-for-tab-command] goes on to it. For every
buffer whose major mode has no width of its own (set-tab-width). A whole
number from 1 to 64; 4 unless set.")

(defvar *indent-tabs-mode* nil
  "True to put tab characters in where \\[indent-for-tab-command] and
indentation go on to a tab stop, NIL to put spaces in; for every buffer whose
major mode says nothing of its own (set-indent-tabs-mode). NIL unless set, so
\\[indent-for-tab-command] puts spaces in; \\[quoted-insert] \\`TAB' puts a tab
character in whatever this says.")

(defvar *mode-tab-widths* (make-hash-table :test 'eq)
  "The tab widths set for major modes (set-tab-width), by the mode's name.")

(defvar *mode-indent-tabs* (make-hash-table :test 'eq)
  "Whether tab characters or spaces go in, set for major modes
(set-indent-tabs-mode), by the mode's name.")

;; tabs are what separates the fields of tab-separated data
(setf (gethash 'tsv-mode *mode-indent-tabs*) t)

(defun buffer-tab-width (buffer)
  "The columns from one tab stop to the next in BUFFER (see *TAB-WIDTH*): a
whole number from 1 to 64, 4 when the setting is none."
  (tuatara.display:sensible-tab-width (buffer-setting buffer :tab-width *mode-tab-widths* *tab-width*) 4))

(defun buffer-indent-tabs-p (buffer)
  "True when \\[indent-for-tab-command] and indentation put tab characters in
BUFFER, NIL when they put spaces in (see *INDENT-TABS-MODE*)."
  (and (buffer-setting buffer :indent-tabs-mode *mode-indent-tabs* *indent-tabs-mode*) t))

(setf tuatara.display:*tab-width-function* 'buffer-tab-width)

(defvar *tab-columns* nil
  "The columns from one tab stop to the next in the text being measured, when
no buffer is at hand: bound to the buffer's by the commands that indent and
fill; NIL means *TAB-WIDTH*.")

(defun tab-columns ()
  "The columns from one tab stop to the next in the text being measured (see *TAB-COLUMNS*)."
  (tuatara.display:sensible-tab-width (or *tab-columns* *tab-width*) 4))

(defun line-blanks (octets &optional (width (tab-columns)))
  "The blanks OCTETS, a line without its newline, begins with (spaces, tabs
and carriage returns): how many octets, whether the line holds nothing else,
and the column its text starts at, a tab going to the next of the stops WIDTH
columns apart, as three values."
  (let ((p 0) (column 0))
    (loop while (< p (length octets))
          do (case (aref octets p)
               (9 (setf column (next-tab-stop column width)))
               ((32 13) (incf column))
               (t (return)))
             (incf p))
    (values p (= p (length octets)) column)))

(defun rope-line-blanks (rope line &optional (width (tab-columns)))
  "What LINE-BLANKS says of the line of ROPE starting at octet LINE, reading
no further than its blanks: how many octets of blanks it begins with, whether
it holds nothing else, and the column its text starts at, as three values."
  (let* ((end (tuatara.text:rope-line-end rope line))
         (p (tuatara.text:rope-skip-octets rope line tuatara.text:*blank-line-octets* :bound end)))
    (multiple-value-bind (count blank column) (line-blanks (tuatara.text:rope-octets rope :start line :end p) width)
      (declare (ignore blank))
      (values count (= p end) column))))

(defun indentation-text (buffer column)
  "What indents a line of BUFFER to COLUMN: spaces, or where BUFFER wants tabs
(BUFFER-INDENT-TABS-P) as many tab characters as fit, and spaces after them."
  (if (buffer-indent-tabs-p buffer)
      (let ((width (buffer-tab-width buffer)))
        (concatenate 'string
                     (make-string (floor column width) :initial-element #\Tab)
                     (make-string (mod column width) :initial-element #\Space)))
      (make-string column :initial-element #\Space)))

;;; Setting them for a major mode

(defun mode-named (name)
  "The major mode NAME stands for, matched by name from any package: the mode
of that name, or the one of that name with -mode after it, as org stands for
org-mode. Signals an error when there is none."
  (let ((modes (mode-names)))
    (or (find (symbol-name name) modes :key #'symbol-name :test #'string=)
        (find (concatenate 'string (symbol-name name) "-MODE") modes :key #'symbol-name :test #'string=)
        (error "No major mode called ~(~a~); there are ~{~(~a~)~^, ~}." name modes))))

(defun tuatara-user:set-tab-width (mode columns)
  "Put the tab stops COLUMNS columns apart (1 to 64) in buffers of major mode
MODE and the modes derived from it, where a tab character shows and where
\\[indent-for-tab-command] goes; other buffers keep *tab-width*. MODE is a
mode's name, with or without -mode. For example, in the init file:
(set-tab-width 'c-mode 8), or (set-tab-width 'prog 2) for every programming
language."
  (let ((found (mode-named mode)))
    (unless (tuatara.display:sensible-tab-width columns nil)
      (error "The columns must be a whole number from 1 to 64, not ~s." columns))
    (setf (gethash found *mode-tab-widths*) columns)))

(defun tuatara-user:set-indent-tabs-mode (mode tabs)
  "Put tab characters in (TABS true) or spaces (TABS NIL) where
\\[indent-for-tab-command] and indentation go on to a tab stop, in buffers of
major mode MODE and the modes derived from it; other buffers keep
*indent-tabs-mode*. MODE is a mode's name, with or without -mode. For example,
in the init file: (set-indent-tabs-mode 'bash t). Tab-separated data
(tsv-mode) has tabs unless set otherwise."
  (setf (gethash (mode-named mode) *mode-indent-tabs*) (and tabs t)))

;;; Commands

(define-command tab-to-tab-stop ()
  "Go on to the next tab stop, putting spaces in up to it, or a tab character
where the buffer wants tabs (*indent-tabs-mode*, set-indent-tabs-mode,
\\[indent-tabs-mode]); with a count, that many stops. The stops are every
*tab-width* columns (4), or as set for the buffer's major mode
(set-tab-width). \\[tab-to-tab-stop] runs it, and \\[indent-for-tab-command]
where the major mode does not indent; \\[quoted-insert] \\`TAB' puts a tab
character in whatever the settings say."
  (let ((b (current-buffer)) (count (max 1 (insert-count))))
    (if (hex-view-here-p)
        ;; the hex view: from the hex digits to the text, or back
        (hex-switch-column)
        (let* ((p (point))
               (width (buffer-tab-width b))
               (column (tuatara.display:buffer-screen-column b p))
               (goal (+ (next-tab-stop column width) (* width (1- count)))))
          (goto (tuatara.buffer:buffer-insert b p (if (buffer-indent-tabs-p b)
                                                      (make-string count :initial-element #\Tab)
                                                      (make-string (- goal column) :initial-element #\Space))))))))

(defun key-character (key)
  "The character KEY stands for when typed after \\[quoted-insert]: a key's own
character, a control character for \\`C-a' to \\`C-z' and \\`C-@' to \\`C-_'
(\\`C-?' delete), a tab for \\`TAB', a carriage return for \\`RET', an escape
for \\`ESC'; NIL for any other key, a Meta one among them."
  (let ((base (tuatara.frontend:key-base key)))
    (cond ((tuatara.frontend:key-meta key) nil)
          ((characterp base)
           (if (tuatara.frontend:key-ctrl key)
               (let ((code (char-code (char-upcase base))))
                 (cond ((<= 64 code 95) (code-char (- code 64)))
                       ((= code 63) (code-char 127))
                       ((= code 32) (code-char 0))
                       (t nil)))
               base))
          (t (case base
               (:tab #\Tab)
               (:enter (code-char 13))
               (:escape (code-char 27))
               (:backspace (code-char 127))
               (t nil))))))

(defvar *tab-always-indent* :complete
  "What \\[indent-for-tab-command] does where the major mode indents lines
(Lisp, Rust, C, shell scripts): :COMPLETE (the standard) indents the current
line, and when that changes nothing completes the name at point, as
\\<lisp-mode>\\[completion-at-point] does in Lisp; T only indents it, wherever
point is; NIL indents it when point is in its leading blanks, else goes on to
the next tab stop. Where the mode does not indent (text, Org, CSV),
\\[indent-for-tab-command] goes on to the next tab stop.")

(define-command indent-region ()
  "Indent each line the selection touches as its major mode indents it; the
lines inside a comment or a string begun on a line before keep their blanks,
which are text. Without a selection it says so and does nothing."
  (let ((b (current-buffer)))
    (if (tuatara.buffer:buffer-mark-active-p b)
        (multiple-value-bind (start end) (tuatara.buffer:buffer-region b)
          (let ((changed (indent-lines b start end)))
            (setf (tuatara.buffer:buffer-mark-active-p b) nil)
            (case changed
              (:none (message "No indentation rules for ~a" (tuatara.buffer:buffer-local b :mode-name "this buffer")))
              (:too-large)
              (t (message "Indented ~d line~:p" (or changed 0))))))
        (message "No selection: indent-region was not run"))))

(define-command indent-for-tab-command ()
  "Indent the current line as its major mode indents it: point in the blanks
the line begins with goes to where its text begins, point in its text stays
with the text. When that changes nothing and *tab-always-indent* is :complete
(as standard), complete the name at point instead, as
\\<lisp-mode>\\[completion-at-point] does in Lisp. With the selection active,
indent the lines it touches. Where the mode does not indent (text, Org, CSV),
go on to the next tab stop, as \\[tab-to-tab-stop] (tab-to-tab-stop) does
anywhere. After a snippet's key, the snippet (\\[insert-snippet])."
  (when (hex-view-here-p)
    (return-from indent-for-tab-command (hex-switch-column)))
  ;; after a snippet's key: the snippet
  (when (expand-snippet-at-point)
    (return-from indent-for-tab-command))
  (let* ((b (current-buffer))
         (p (point))
         (line (tuatara.buffer:buffer-line-start b p)))
    (cond ((tuatara.buffer:buffer-mark-active-p b)
           (multiple-value-bind (start end) (tuatara.buffer:buffer-region b)
             (when (eq (indent-lines b start end) :none) (tab-to-tab-stop))))
          (t
           (let ((in-blanks (<= p (+ line (line-indentation b line)))))
             (if (and (null *tab-always-indent*) (not in-blanks))
                 (tab-to-tab-stop)
                 (let ((changed (indent-lines b line line :blank-lines t)))
                   (cond ((eq changed :none) (tab-to-tab-stop))
                         (t
                          ;; point in the blanks goes to the text; in the text it moved with it
                          (when in-blanks
                            (let ((line (tuatara.buffer:buffer-line-start b (point))))
                              (goto (+ line (line-indentation b line)))))
                          (when (and (eq *tab-always-indent* :complete) (eql changed 0) (= (point) p))
                            (complete-at-point-in b :quiet t)))))))))))

(define-command quoted-insert ()
  "Read the next key and put in the character it stands for, whatever that key
does otherwise: \\[quoted-insert] \\`TAB' a tab character where
\\[indent-for-tab-command] puts spaces in, \\[quoted-insert] \\`C-l' a form
feed, \\[quoted-insert] \\`C-m' a carriage return. With a count, that many. A
digit after \\[quoted-insert] goes in as it is, not as the start of a
character's code."
  (let* ((key (let ((*keys-as-typed* t)) (read-key (substitute-command-keys "\\[quoted-insert]-" :marks t))))
         (char (key-character key)))
    (if (null char)
        (message "~a is no character to put in" (tuatara.frontend:key-description (tuatara.frontend:key-spec key)))
        (let ((text (make-string (max 1 (insert-count)) :initial-element char)))
          (if (hex-view-here-p)
              (hex-overwrite (tuatara.text:string-octets text))
              (goto (tuatara.buffer:buffer-insert (current-buffer) (point) text)))))))

(define-command indent-tabs-mode ()
  "Switch the current buffer between tab characters and spaces where
\\[indent-for-tab-command] and indentation go on to a tab stop; with a count,
tabs when it is positive, spaces when it is zero or less. Other buffers keep
what their major modes and *indent-tabs-mode* say."
  (let* ((b (current-buffer))
         (tabs (switched-by-count (buffer-indent-tabs-p b))))
    (setf (tuatara.buffer:buffer-local b :indent-tabs-mode) tabs)
    (message "Indent-Tabs mode ~:[disabled~;enabled~] in current buffer" tabs)))
