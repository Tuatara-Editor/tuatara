;;;; lisp/help/notifications.lisp --- Notifications: told on the desktop and kept in *Notifications*.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; Whatever has something to tell as it happens, the agenda as an entry's
;;;; time comes or a package as mail arrives, calls NOTIFY, from any thread.
;;;; The notification is shown on the desktop by notify-send and kept for the
;;;; session in *Notifications*, a line each with the date and time it was
;;;; told, as *Messages* keeps the messages.

(in-package #:tuatara.defaults)

(defvar *notify-program* "notify-send"
  "The program that shows a notification on the desktop: a name looked for
on the PATH, or a path. It is given the title and the words, as notify-send
takes them. NIL shows none, notifications kept in *Notifications* alone.")

(sb-ext:defglobal **notify-missing-said** nil
  "True once it has been said that *NOTIFY-PROGRAM* is not installed.")

(defparameter *notification-columns* 300
  "How many columns of a notification's title, and of its words, are kept:
what a note or a package tells is cut there, on one line.")

(defun notification-text (text)
  "TEXT as a notification keeps it: on one line, cut at *NOTIFICATION-COLUMNS*."
  (tuatara.display:shorten-string (or text "") *notification-columns* :one-line t))

(defun notify-arguments (title words)
  "The arguments *NOTIFY-PROGRAM* is given to show TITLE and WORDS: its name
as Tuatara's, and -- before them, so that a title beginning with - is shown
as it is; the words escaped, the program reading markup in them, so that
nothing in them becomes a link or a picture."
  (list "--app-name=Tuatara" "--" title (xml-escape words)))

(defun notifications-buffer ()
  "The *Notifications* buffer, made when there is none: read-only, shown where
*Messages* is."
  (find-or-make-buffer "*Notifications*" :mode 'special-mode
                                         :made (lambda (b)
                                                 (setf (tuatara.buffer:buffer-local b :read-only) t
                                                       (tuatara.buffer:buffer-local b :kind) :messages))))

(defun notification-line (source title words time)
  "The line *Notifications* keeps of a notification from SOURCE told at the
universal TIME: 2026-10-23 Fri 13:50  Agenda  Dentist · 14:00, in 10 min."
  (format nil "~a  ~a  ~a~@[ · ~a~]" (format-time "%F %a %H:%M" time) source title
          (and words (plusp (length words)) words)))

(defun log-notification (line)
  "Add LINE at the end of *Notifications*, which keeps the last
*MESSAGE-LOG-MAX* lines told this session, as *Messages* keeps its own."
  (add-to-log (notifications-buffer) *message-log-max*
              (lambda (buffer)
                (tuatara.buffer:buffer-insert buffer (tuatara.buffer:buffer-size buffer)
                                              (format nil "~a~%" line)))))

(defun notify (title words &key (source "Tuatara") (desktop t))
  "Tell TITLE, with WORDS under it, as something that has just happened: kept
in *Notifications* (\\[view-notifications]) as a line with the date and time
and SOURCE, who tells it (\"Agenda\", \"Mail\"), and shown on the desktop by
*NOTIFY-PROGRAM* unless DESKTOP is NIL. It may be called from any thread;
there the line is added on the editor's. When the program is not installed
this is said once in the session and the notifications are kept all the
same. Title and words are kept on one line, cut where they are long
\(NOTIFICATION-TEXT). True when it was shown on the desktop."
  (let* ((title (notification-text title))
         (words (notification-text words))
         (line (notification-line (notification-text source) title words (get-universal-time)))
         (program (and desktop *notify-program* (tuatara.io:find-program *notify-program*))))
    (if (away-from-editor-p)
        (defer-action (lambda () (log-notification line)))
        (log-notification line))
    (cond (program
           (handler-case (progn (start-detached program (notify-arguments title words) :label "notification") t)
             (error () nil)))
          ((and desktop *notify-program* (not **notify-missing-said**))
           (setf **notify-missing-said** t)
           (message "~a is not installed: notifications are kept in *Notifications* only (\\[view-notifications]); ~a"
                    *notify-program* (outside-program-install-words :notify))
           nil))))

(define-command view-notifications ()
  "Show *Notifications*: what was told so far this session, the agenda's
entries tagged :notify: as their time came and what packages told, a line
each with the date and time it was told and who told it, the latest at the
end. It goes where *Messages* goes (\\[view-echo-area-messages]);
\\<special-mode>\\[quit-window] closes it."
  (display-buffer (notifications-buffer) :at :end))

(defun notify-health-words ()
  "How notifications are told here, as M-x check-health says it."
  (outside-program-health-words :notify "notifications told on the desktop"
                                (and *notify-program* (tuatara.io:find-program *notify-program*))
                                :how "the agenda's and packages', kept in *Notifications* too"))

(defun install-notifications ()
  "Bind \\[view-notifications], which shows what was told, to \\`C-h N'."
  (define-key *global-keymap* "C-h N" 'view-notifications))
