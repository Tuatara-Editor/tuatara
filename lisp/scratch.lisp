;;;; lisp/scratch.lisp --- The scratch buffers the editor starts with.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; *lisp-scratch* and *org-scratch* are made at startup, unless the init
;;;; file says otherwise (*auto-scratch-buffers*). M-x lisp-scratch and
;;;; M-x org-scratch show them, and make them anew, as they were at startup,
;;;; when they have been killed or were never made. Their text goes when the
;;;; editor stops, unless *scratch-text-kept* keeps it in the state directory
;;;; (the list "scratch"), written with the auto-saves and as the editor stops.

(in-package #:tuatara.defaults)

(defparameter *scratch-buffers*
  '(("*lisp-scratch*" lisp-interaction-mode
     ";; Lisp to try out: \\<lisp-interaction-mode>\\[eval-last-expression] after an expression evaluates it.
;; This text is not kept: it goes when Tuatara quits, unless
;; *scratch-text-kept* is on. \\<global>\\[save-buffer] saves it to a file;
;; \\[find-file] opens one.

")
    ("*org-scratch*" org-mode
     "# Notes in Org that are not kept: they go when Tuatara quits, unless
# *scratch-text-kept* is on. \\[save-buffer] saves them to a file;
# \\[find-file] opens one.

"))
  "The scratch buffers made at startup: name, major mode and first text, a
header comment saying what the buffer is for and that its text is not kept,
the keys it names written in when the buffer is made
\(SUBSTITUTE-COMMAND-KEYS).")

(define-setting tuatara-user:*scratch-text-kept* nil
  "Whether the text of the scratch buffers (*lisp-scratch*, *org-scratch*) is
kept between sessions, in the state directory. It is written with the
auto-saves (*AUTO-SAVE*, *AUTO-SAVE-IDLE*) and as the editor stops, and put
back when they are made at the next start.
- t: the text is kept.
- nil: the text goes when the editor stops (the standard)."
  :values boolean)

(defun parse-scratch-text-line (line)
  "The kept text of a scratch buffer LINE writes, (NAME . TEXT), or NIL for a
line that makes no sense. A tab ends each field: the buffer's name and its
text, escaped (ESCAPE-PATH)."
  (let ((fields (state-line-fields line)))
    (when (and (<= 2 (length fields)) (plusp (length (first fields))))
      (cons (unescape-path (first fields)) (unescape-path (second fields))))))

(defun scratch-text-line (kept)
  "The line of the file of scratch texts for KEPT, (NAME . TEXT)."
  (format nil "~a~c~a" (escape-path (car kept)) #\Tab (escape-path (cdr kept))))

(define-state-list *scratch-texts* "scratch" 'parse-scratch-text-line 'scratch-text-line nil
  :header (format nil "# The text of the scratch buffers, kept while *scratch-text-kept* is on. A tab~%~
                       # ends each field: the buffer's name and its text (\\\\, \\n, \\r and \\t stand~%~
                       # for \\, a newline, a return and a tab).~%")
  :path-of #'car :class :kept :fields 2)

(defun kept-scratch-text (name)
  "The text kept for the scratch buffer NAME (*SCRATCH-TEXT-KEPT*), or NIL when
none is or the setting is off."
  (and tuatara-user:*scratch-text-kept*
       (cdr (assoc name (state-list-entries *scratch-texts*) :test #'string=))))

(defun kept-scratch-buffers ()
  "The scratch buffers whose text *SCRATCH-TEXT-KEPT* keeps: those of
*SCRATCH-BUFFERS* under their own names, not the numbered ones of clients."
  (loop for (name) in *scratch-buffers*
        for buffer = (tuatara.buffer:find-buffer name)
        when buffer collect buffer))

(defun keep-scratch-text (buffer)
  "Keep BUFFER's text, a scratch buffer's, in the state directory, written in
the background; noted as kept, so that it is not written again unchanged. A
text longer than *REWRITE-THRESHOLD* octets is not kept, and that is said once."
  (if (<= (tuatara.buffer:buffer-size buffer) *rewrite-threshold*)
      (state-list-put *scratch-texts* (list (cons (tuatara.buffer:buffer-name buffer)
                                                  (tuatara.buffer:buffer-string buffer))))
      (unless (tuatara.buffer:buffer-local buffer 'scratch-too-long)
        (setf (tuatara.buffer:buffer-local buffer 'scratch-too-long) t)
        (message "~a is too long to keep between sessions" (tuatara.buffer:buffer-name buffer))))
  (setf (tuatara.buffer:buffer-local buffer 'scratch-kept) (tuatara.buffer:buffer-content-number buffer)))

(defun keep-scratch-texts (&optional editor)
  "Keep the text of each scratch buffer changed since it was last kept, while
*SCRATCH-TEXT-KEPT* is on: as the editor stops (*EXIT-FUNCTIONS*). EDITOR is
not used."
  (declare (ignore editor))
  (when tuatara-user:*scratch-text-kept*
    (dolist (buffer (kept-scratch-buffers))
      (unless (eql (tuatara.buffer:buffer-local buffer 'scratch-kept) (tuatara.buffer:buffer-content-number buffer))
        (ignoring-errors-reported ("keeping the text of ~a" (tuatara.buffer:buffer-name buffer))
          (keep-scratch-text buffer))))))

(defun scratch-text-tick (editor)
  "Tick function: while *SCRATCH-TEXT-KEPT* and *AUTO-SAVE* are on, keep the
text of each scratch buffer once it has been left alone *AUTO-SAVE-IDLE*
seconds since it changed; the seconds until the next is due, or NIL."
  (declare (ignore editor))
  (when (and tuatara-user:*scratch-text-kept* *auto-save*)
    (let ((now (get-internal-real-time))
          (idle (round (* *auto-save-idle* internal-time-units-per-second)))
          (soonest nil))
      (dolist (buffer (kept-scratch-buffers))
        (let ((content (tuatara.buffer:buffer-content-number buffer))
              (seen (tuatara.buffer:buffer-local buffer 'scratch-seen)))
          (unless (eql content (tuatara.buffer:buffer-local buffer 'scratch-kept))
            ;; SEEN: the text's number and when it last changed, in internal time units
            (unless (eql content (car seen))
              (setf seen (cons content now)
                    (tuatara.buffer:buffer-local buffer 'scratch-seen) seen))
            (let ((due (+ (cdr seen) idle)))
              (if (>= now due)
                  (keep-scratch-text buffer)
                  (setf soonest (if soonest (min soonest (- due now)) (- due now))))))))
      (and soonest (ceiling soonest internal-time-units-per-second)))))

(defun install-scratch-texts ()
  "Keep the scratch buffers' text as *SCRATCH-TEXT-KEPT* says: with the
auto-saves, and as the editor stops, before the lists kept between sessions
are written."
  (pushnew 'scratch-text-tick *tick-functions*)
  (pushnew 'keep-scratch-texts *exit-functions*))

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

(defun make-scratch-buffer (name &optional (kind name))
  "Make the scratch buffer NAME as KIND, the name of one of *SCRATCH-BUFFERS*,
says: in its mode, holding its first text, or the text kept from the session
before (*SCRATCH-TEXT-KEPT*), with point after it and nothing to undo; a plain
empty buffer when KIND is none of them. Return it."
  (let ((entry (assoc kind *scratch-buffers* :test #'string=)))
    (if (null entry)
        (tuatara.buffer:make-buffer name)
        (destructuring-bind (mode text) (rest entry)
          (let* ((kept (and (string= name kind) (kept-scratch-text name)))
                 (b (tuatara.buffer:make-buffer name :text (or kept (substitute-command-keys text)))))
            (set-major-mode b mode)
            (setf (tuatara.buffer:buffer-point b) (tuatara.buffer:buffer-size b))
            ;; the first text is not a change you made
            (tuatara.buffer:forget-history b)
            ;; the first text is not kept, so a later release's header reaches the setting's users
            (setf (tuatara.buffer:buffer-local b 'scratch-kept) (tuatara.buffer:buffer-content-number b))
            b)))))

(defun ensure-scratch-buffers ()
  "Make the scratch buffers *AUTO-SCRATCH-BUFFERS* names that do not exist
yet, each in its mode, with point after its first text."
  (loop for (name) in *scratch-buffers*
        when (and (member name tuatara-user:*auto-scratch-buffers* :test #'string=)
                  (not (tuatara.buffer:find-buffer name)))
          do (make-scratch-buffer name)))

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
               (buffer (make-scratch-buffer (tuatara.buffer:unique-buffer-name kind) kind)))
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
  (multiple-value-bind (buffer made)
      (let ((client (working-client)))
        (if client
            (client-scratch client name)
            (let ((existing (tuatara.buffer:find-buffer name)))
              (values (or existing (make-scratch-buffer name)) (not existing)))))
    (show-buffer buffer)
    (when made (message "Made a new ~a" (tuatara.buffer:buffer-name buffer)))))

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
names, made as at startup (MAKE-SCRATCH-BUFFER), or the one there
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
       (make-scratch-buffer name)))))

(defun startup-buffer ()
  "The buffer to show when the editor starts without files: *STARTUP-BUFFER*,
made if it is one of the scratch buffers *AUTO-SCRATCH-BUFFERS* allows; else
the first scratch buffer made; else an empty *scratch*."
  (ensure-scratch-buffers)
  (or (tuatara.buffer:find-buffer tuatara-user:*startup-buffer*)
      (let ((name (auto-scratch-name))) (and name (tuatara.buffer:find-buffer name)))
      (new-buffer)))
