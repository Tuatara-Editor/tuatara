;;;; src/command/clients.lisp --- Those who work with the editor, each on screens of their own, and their buffers.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; A client (editor.lisp) is someone working with the editor: in an editor
;;;; of its own, the one who started it; in an editor running in the
;;;; background, each program that attached to show it in a terminal. Each
;;;; frame is a client's. A buffer shown in a client's frame is that
;;;; client's, and the questions that offer buffers (switching, the next
;;;; one, saving them all) offer each client its own and those of no one's
;;;; (*Messages*); a buffer may be several clients'. A client that ends
;;;; leaves its buffers, which another may take up.

(in-package #:tuatara.command)

(defun current-client (&optional (editor *editor*))
  "The client who works in EDITOR's current frame, or NIL without an editor."
  (and editor (frame-client (editor-frame editor))))

(defparameter *buffers-for-every-client* '("*Messages*" "*Debug*" "*Jobs*" "*Notifications*")
  "The buffers every client is offered, whoever shows them: what the editor
says and what it does in the background, the same for all.")

(defparameter *client-variables* '("TERM" "COLORTERM" "NO_COLOR" "LANG" "LC_*" "DISPLAY" "WAYLAND_DISPLAY"
                                    "SSH_AUTH_SOCK" "PATH" "GIT_AUTHOR_NAME" "GIT_AUTHOR_EMAIL"
                                    "GIT_COMMITTER_NAME" "GIT_COMMITTER_EMAIL")
  "The environment variables of a client the editor keeps for it, a * at the
end standing for any ending: they say where its terminal and desktop are,
its language, how its keys reach other machines, where its programs are, and
whose name its commits bear. A program started from its frames has them in
place of the editor's own, and none of these the client has not. No variable
that names where git works, runs a program or writes is kept; GPG_TTY is the
client's terminal, as the editor checked it.")

(defun client-variable-p (name)
  "True when the environment variable NAME is one the editor keeps for a
client (*CLIENT-VARIABLES*)."
  (some (lambda (kept)
          (if (char= #\* (char kept (1- (length kept))))
              (uiop:string-prefix-p (subseq kept 0 (1- (length kept))) name)
              (string= kept name)))
        *client-variables*))

(defun buffer-clients (buffer)
  "The clients BUFFER is offered to (CLIENT-SEES-BUFFER-P), or NIL when it is
everyone's."
  (tuatara.buffer:buffer-local buffer :clients))

(defun join-client (buffer client)
  "Make BUFFER, shown in a frame of CLIENT, one of CLIENT's buffers too. The
one who started an editor of its own has every buffer already, as do the
buffers every client is offered (*BUFFERS-FOR-EVERY-CLIENT*). Nothing is made
for a buffer that is CLIENT's already."
  (when (and client (client-remote client) (not (client-ended client)) (not (member client (buffer-clients buffer)))
             (not (member (tuatara.buffer:buffer-name buffer) *buffers-for-every-client* :test #'string=)))
    (setf (tuatara.buffer:buffer-local buffer :clients) (cons client (buffer-clients buffer)))))

(defun client-sees-buffer-p (client buffer)
  "True when BUFFER is offered to CLIENT where buffers are chosen from (the
buffers to switch to, the one used before, the files to save): it is
CLIENT's, or no one's, or CLIENT is the one who started an editor of its
own, who is offered every buffer."
  (let ((clients (buffer-clients buffer)))
    (and (or (null client) (not (client-remote client)) (null clients) (member client clients)) t)))

(defun client-buffers (&optional (buffers tuatara.buffer:*buffers*) (client (current-client)))
  "Those of BUFFERS offered to CLIENT (CLIENT-SEES-BUFFER-P), in their order."
  (if (or (null client) (not (client-remote client)))
      buffers
      (remove-if-not (lambda (buffer) (client-sees-buffer-p client buffer)) buffers)))

(defun client-frames (editor client)
  "The frames of EDITOR CLIENT works in, in the order they were opened."
  (remove client (editor-frames editor) :key #'frame-client :test-not #'eq))

(defun add-client (editor frontend &key directory environment (buffer (constantly nil)) start)
  "Make a client of EDITOR on FRONTEND, a remote frontend, started in
DIRECTORY with ENVIRONMENT (CLIENT), numbered from 1 after the others, with a frame
on FRONTEND showing what BUFFER, a function of the client, gives (by default
the buffer used last); the frame EDITOR keeps with no screen gives way to it
\(KEEP-IDLE-FRAME), and what waited to be done there is done in it. START, a
function of none, is done in that frame as soon as the editor is free.
Returns the client."
  (let* ((client (make-client :frontend frontend :directory directory :environment environment
                              :number (1+ (loop for other in (editor-clients editor)
                                                when (client-remote other) maximize (client-number other)))))
         (shown (progn (setf (editor-clients editor) (append (editor-clients editor) (list client)))
                       (funcall buffer client)))
         (frame (if shown (add-frame editor frontend shown) (add-frame editor frontend)))
         (idle (find-if (lambda (frame) (idle-frame-p editor frame)) (editor-frames editor))))
    (setf (frame-client frame) client)
    (when idle (remove-frame editor idle))
    (when start
      (setf (frame-deferred frame) (append (frame-deferred frame) (list start))))
    client))

(defvar *client-end-functions* '()
  "Functions of a client called as it ends, after its own (CLIENT-ENDINGS):
what goes with it, as its encrypted buffers do.")

(defun keep-idle-frame (editor)
  "Give EDITOR, an editor running in the background (EDITOR-IDLE-FRONTEND), a
frame on its frontend with no screen, as it has while no client shows it, its
client the editor's first; returns it."
  (let ((frame (add-frame editor (editor-idle-frontend editor) (other-buffer nil t))))
    (setf (frame-client frame) (first (editor-clients editor)))
    frame))

(defun idle-frame-p (editor frame)
  "True when FRAME is the frame EDITOR keeps while no client shows it, on its
frontend with no screen (KEEP-IDLE-FRAME)."
  (let ((idle (editor-idle-frontend editor)))
    (and idle (eq (frame-frontend frame) idle))))

(defun end-client (editor client)
  "End CLIENT of EDITOR, once: its own endings are called (CLIENT-ENDINGS,
oldest first), then *CLIENT-END-FUNCTIONS*; a client on a frontend of its own
is told the code it ends with and what it says, and its frames are closed, the
idle frame kept in their place when they were EDITOR's last (KEEP-IDLE-FRAME).
Its buffers stay, still its, for another client to take up; of the client
itself only what names its folder is kept, its frontend, environment and
files let go."
  (unless (client-ended client)
    (setf (client-ended client) (get-universal-time))
    (dolist (ending (reverse (client-endings client)))
      (call-guarded ending ending client))
    (tuatara.runtime:run-hook *client-end-functions* client)
    (let ((frontend (client-frontend client)))
      (when (typep frontend 'tuatara.frontend:remote-frontend)
        (setf (tuatara.frontend:remote-exit-code frontend) (client-exit-code client)
              (tuatara.frontend:remote-exit-words frontend) (client-exit-words client)))
      (when frontend
        (let ((frames (client-frames editor client)))
          (when (and frames (= (length frames) (length (editor-frames editor))) (editor-idle-frontend editor))
            (keep-idle-frame editor))
          (dolist (frame frames)
            (if (rest (editor-frames editor))
                (remove-frame editor frame)
                (tuatara.runtime:ignoring-errors-reported ("closing a client's screen")
                  (tuatara.frontend:frontend-stop frontend))))))
      (when (client-remote client)
        (setf (client-frontend client) nil (client-environment client) '()
              (client-startup-buffers client) '() (client-endings client) '())))))

(defun client-folder-words (client)
  "What the lists of buffers call CLIENT's: when it ended, or began, and
where it was started (\"ended 14:20, client 2, ~/cases/x\", \"client 3
since 14:02, ~/notes\")."
  (format nil "~:[client ~d since ~a~;ended ~:*~a, client ~d~*~]~@[, ~a~]"
          (and (client-ended client) (tuatara.dates:format-time "%H:%M" (client-ended client)))
          (client-number client)
          (tuatara.dates:format-time "%H:%M" (client-began client))
          (and (client-directory client) (abbreviated-path (client-directory client)))))

(defmethod tuatara.io:host-program-environment ((host editor-host))
  "The editor's variables, those it keeps for a client (*CLIENT-VARIABLES*)
taken from the client the work is for (CLIENT-ENVIRONMENT) where it is one
on a screen of its own, and GPG_TTY naming its terminal: the client of the frame a job was begun in, on its
thread, else of the frame worked in."
  (let* ((editor (or *editor* (running-editor)))
         (frame (or (thread-frame) (and editor (editor-frame editor))))
         (client (and frame (frame-client frame)))
         (environment (sb-ext:posix-environ)))
    (if (not (and client (client-remote client) (client-frontend client)))
        environment
        (let ((terminal (tuatara.frontend:frontend-terminal-device (client-frontend client))))
          (append (and terminal (list (format nil "GPG_TTY=~a" terminal)))
                  (loop for (name . value) in (client-environment client) collect (format nil "~a=~a" name value))
                  (remove-if (lambda (setting)
                               (let ((equals (position #\= setting)))
                                 (and equals (let ((name (subseq setting 0 equals)))
                                               (or (client-variable-p name) (string= name "GPG_TTY"))))))
                             environment))))))
