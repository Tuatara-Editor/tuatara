;;;; lisp/scratch.lisp --- The scratch buffers the editor starts with.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; *lisp-scratch* and *org-scratch* are made at startup, unless the init
;;;; file says otherwise (*auto-scratch-buffers*). M-x lisp-scratch and
;;;; M-x org-scratch show them, and make them anew, as they were at startup,
;;;; when they have been killed or were never made.

(in-package #:tuatara.defaults)

(defparameter *scratch-buffers*
  '(("*lisp-scratch*" lisp-interaction-mode
     ";; This buffer holds unsaved text and serves as a space for Lisp evaluation.
;; To create a file, visit it with ‘\\[find-file]’ and enter text in its buffer.

")
    ("*org-scratch*" org-mode
     "# This buffer is for scribbling in org-mode.
# Start your scribble here and save to file with ‘\\[save-buffer]’ for persistence.

"))
  "The scratch buffers made at startup: name, major mode and first text, a
header comment saying what the buffer is for, the keys it names written in
when the buffer is made (SUBSTITUTE-COMMAND-KEYS).")

(defvar tuatara-user:*auto-scratch-buffers* '("*lisp-scratch*" "*org-scratch*")
  "The scratch buffers made without being asked for: at startup, and when the
last buffer is killed. NIL for none: starting without files shows an empty
*scratch* then. \\[lisp-scratch] and \\[org-scratch] make theirs whenever they
are asked. Set it in the init file.")

(defvar tuatara-user:*startup-buffer* "*lisp-scratch*"
  "The buffer shown when the editor starts without files: \"*lisp-scratch*\" or
\"*org-scratch*\" (or any buffer the init file makes). Set it in the init file.")

(defun make-scratch-buffer (name mode text)
  "Make the scratch buffer NAME in MODE holding TEXT, with point after it and
nothing to undo; return it."
  (let ((b (tuatara.buffer:make-buffer name :text (substitute-command-keys text))))
    (set-major-mode b mode)
    (setf (tuatara.buffer:buffer-point b) (tuatara.buffer:buffer-size b))
    ;; the first text is not a change you made
    (setf (tuatara.buffer:buffer-undo-stack b) '())
    b))

(defun ensure-scratch-buffers ()
  "Make the scratch buffers *AUTO-SCRATCH-BUFFERS* names that do not exist
yet, each in its mode, with point after its first text."
  (loop for (name mode text) in *scratch-buffers*
        when (and (member name tuatara-user:*auto-scratch-buffers* :test #'string=)
                  (not (tuatara.buffer:find-buffer name)))
          do (make-scratch-buffer name mode text)))

(defun auto-scratch-name ()
  "The scratch buffer made when there is nothing else to show: *STARTUP-BUFFER*
when *AUTO-SCRATCH-BUFFERS* has it, else the first scratch buffer it has; NIL
when it has none."
  (let ((allowed tuatara-user:*auto-scratch-buffers*))
    (if (member tuatara-user:*startup-buffer* allowed :test #'string=)
        tuatara-user:*startup-buffer*
        (find-if (lambda (name) (assoc name *scratch-buffers* :test #'string=)) allowed))))

(defun show-scratch (name)
  "Show the scratch buffer NAME, one of *SCRATCH-BUFFERS*: the one there is,
as it is, or, when it was killed, a new one as at startup, which is said."
  (destructuring-bind (mode text) (rest (assoc name *scratch-buffers* :test #'string=))
    (let ((existing (tuatara.buffer:find-buffer name)))
      (show-buffer (or existing (make-scratch-buffer name mode text)))
      (unless existing (message "Made a new ~a" name)))))

(define-command lisp-scratch ()
  "Show *lisp-scratch*, the Lisp buffer that is not saved, where
\\<lisp-interaction-mode>\\[eval-last-expression] evaluates in the running editor
and \\[eval-print-last-expression] puts what the expression before point printed
and its value on the lines after it; when it was killed, a new one."
  (show-scratch "*lisp-scratch*"))

(define-command org-scratch ()
  "Show *org-scratch*, the Org buffer for notes that are not saved; when it was
killed, a new one."
  (show-scratch "*org-scratch*"))

(defun remade-scratch-buffer (&optional dying)
  "The buffer shown when no other is left: the scratch buffer AUTO-SCRATCH-NAME
names, made as at startup, in its mode with its first text, or the one there
is when it is not DYING, the buffer being killed (DYING, when it has that
name, gives it up to the new one); an empty *scratch* when
*AUTO-SCRATCH-BUFFERS* has none."
  (let* ((name (auto-scratch-name))
         (existing (and name (tuatara.buffer:find-buffer name))))
    (cond
      ((null name) (let ((*new-buffer-function* nil)) (new-buffer dying)))
      ((and existing (not (eq existing dying))) existing)
      (t
       (when existing
         (setf (tuatara.buffer:buffer-name existing) (format nil " ~a (killed)" name)))
       (destructuring-bind (mode text) (rest (assoc name *scratch-buffers* :test #'string=))
         (make-scratch-buffer name mode text))))))

(defun startup-buffer ()
  "The buffer to show when the editor starts without files: *STARTUP-BUFFER*,
made if it is one of the scratch buffers *AUTO-SCRATCH-BUFFERS* allows; else
the first scratch buffer made; else an empty *scratch*."
  (ensure-scratch-buffers)
  (or (tuatara.buffer:find-buffer tuatara-user:*startup-buffer*)
      (let ((name (auto-scratch-name))) (and name (tuatara.buffer:find-buffer name)))
      (let ((*new-buffer-function* nil)) (new-buffer))))
