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

(define-setting tuatara-user:*auto-scratch-buffers* '("*lisp-scratch*" "*org-scratch*")
  "The scratch buffers made without being asked for, at startup and when the
last buffer is killed. \\[lisp-scratch] and \\[org-scratch] make theirs
whenever they are asked. Set it in the init file.
- a list of buffer names: those buffers are made, \"*lisp-scratch*\" and
  \"*org-scratch*\" as standard.
- nil: none; starting without files shows an empty *scratch* then."
  :values (list-of string))

(define-setting tuatara-user:*startup-buffer* "*lisp-scratch*"
  "The buffer shown when the editor starts without files. Set it in the init
file.
- \"*lisp-scratch*\": the Lisp scratch buffer, as standard.
- \"*org-scratch*\": the Org scratch buffer.
- any other text: the buffer of that name, which the init file makes."
  :values string)

(defun make-scratch-buffer (name mode text)
  "Make the scratch buffer NAME in MODE holding TEXT, with point after it and
nothing to undo; return it."
  (let ((b (tuatara.buffer:make-buffer name :text (substitute-command-keys text))))
    (set-major-mode b mode)
    (setf (tuatara.buffer:buffer-point b) (tuatara.buffer:buffer-size b))
    ;; the first text is not a change you made
    (tuatara.buffer:forget-history b)
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

(defun client-scratch (client &optional kind except)
  "CLIENT's scratch buffer of KIND, the name of one of *SCRATCH-BUFFERS*,
of any kind when KIND is NIL, other than EXCEPT: the one CLIENT has, else a
new one, as at startup, its name numbered when that name is taken
\(*lisp-scratch*<2>), of KIND or the kind AUTO-SCRATCH-NAME names (an empty
*scratch* when that is none). A new one stands in CLIENT's directory and is
CLIENT's. The second value is true when it is new."
  (let ((own (find-if (lambda (b)
                        (let ((made-as (tuatara.buffer:buffer-local b :client-scratch)))
                          (and made-as (not (eq b except)) (member client (buffer-clients b))
                               (or (null kind) (equal made-as kind)))))
                      tuatara.buffer:*buffers*)))
    (if own
        (values own nil)
        (let* ((kind (or kind (auto-scratch-name) "*scratch*"))
               (entry (assoc kind *scratch-buffers* :test #'string=))
               (name (tuatara.buffer:unique-buffer-name kind))
               (buffer (if entry
                           (make-scratch-buffer name (second entry) (third entry))
                           (tuatara.buffer:make-buffer name))))
          (when (client-directory client)
            (setf (buffer-given-directory buffer) (client-directory client)))
          (setf (tuatara.buffer:buffer-local buffer :client-scratch) kind)
          (join-client buffer client)
          (values buffer t)))))

(defun working-client ()
  "The client working in the frame when it is one of an editor running in
the background that has not ended, else NIL."
  (let ((client (current-client)))
    (and client (client-remote client) (not (client-ended client)) client)))

(defun show-scratch (name)
  "Show the scratch buffer NAME, one of *SCRATCH-BUFFERS*: the one there is,
as it is, or, when it was killed, a new one as at startup, which is said. In
a client of an editor running in the background, the client's own of that
kind (CLIENT-SCRATCH), the others' left where they are."
  (destructuring-bind (mode text) (rest (assoc name *scratch-buffers* :test #'string=))
    (multiple-value-bind (buffer made)
        (let ((client (working-client)))
          (if client
              (client-scratch client name)
              (let ((existing (tuatara.buffer:find-buffer name)))
                (values (or existing (make-scratch-buffer name mode text)) (not existing)))))
      (show-buffer buffer)
      (when made (message "Made a new ~a" (tuatara.buffer:buffer-name buffer))))))

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

(defmethod host-new-buffer ((host defaults-host) dying)
  "The buffer shown when no other is left: the scratch buffer AUTO-SCRATCH-NAME
names, made as at startup, in its mode with its first text, or the one there
is when it is not DYING, the buffer being killed (DYING, when it has that
name, gives it up to the new one); NIL, for an empty *scratch*, when
*AUTO-SCRATCH-BUFFERS* has none. In a client of an editor running in the
background, a scratch buffer of the client's own other than DYING
\(CLIENT-SCRATCH)."
  (let* ((client (working-client))
         (name (auto-scratch-name))
         (existing (and name (not client) (tuatara.buffer:find-buffer name))))
    (cond
      (client (client-scratch client nil dying))
      ((null name) nil)
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
      (new-buffer)))
