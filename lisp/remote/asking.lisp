;;;; lisp/remote/asking.lisp --- What ssh asks, put to you in the minibuffer.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; A connection to a server's files runs ssh with nobody at its terminal, so
;;;; what it would ask there comes here: ssh runs a small script (SSH_ASKPASS)
;;;; in the connection's private folder, which passes the question through one
;;;; named pipe of that folder and prints the answer it gets through the other.
;;;; A script needs no program of its own, in a package or an AppImage, and
;;;; starts at once. The words of a question may be a server's, so they are
;;;; cleaned before they are shown; a password or passphrase is read hidden,
;;;; goes through the pipe only, and is wiped here once sent.

(in-package #:tuatara.defaults)

(defconstant +ssh-most-question+ 4096
  "The most octets of one question ssh asks that are read. A named pipe passes that many in one piece.")

(defconstant +ssh-most-answer+ 4096 "The most octets of an answer given to ssh; a longer one counts as none.")

(defconstant +ssh-most-shown+ 60
  "The most columns of what a server asks shown in the prompt: the question whole is in its details.")

(defconstant +ssh-most-questions+ 32
  "The most questions one connection may ask, past which it is ended: a server can go on asking for ever.")

(defparameter *ssh-asking-script*
  (format nil "#!/bin/sh~@
               [ \"$SSH_ASKPASS_PROMPT\" = none ] && exit 0~@
               cd \"${0%/*}\" || exit 1~@
               exec 3<answer || exit 1~@
               printf '%.4000s\\0' \"$1\" >question || exit 1~@
               IFS= read -r given <&3 || exit 1~@
               [ \"$given\" = y ] || exit 1~@
               exec head -n 1 <&3~%")
  "What ssh runs for each question (SSH_ASKPASS), in the connection's
folder: it does nothing for a mere notice of ssh's, else passes the question
on and reads a first line, y when an answer follows; the answer goes from the
pipe to ssh by a program reading one line of it, through no argument and no
variable of the shell. Any other first line ends it with a failure.")

(defparameter *ssh-connecting-seconds* 1
  "How long a connection to a server has taken before the echo area says so.")

(defstruct (ssh-asking (:constructor make-ssh-asking (host jump folder question answer)) (:copier nil))
  "How ssh's questions for one connection reach you. HOST is the machine asked
for. JUMP is the machines ssh goes through to it (ProxyJump), or NIL. FOLDER
is the connection's private folder. QUESTION and ANSWER are this end of the
named pipes the script uses, as file descriptors. ASKED is how many questions
it has asked. SAID is how many octets of what ssh wrote on its error output
have been shown."
  (host "" :type string :read-only t)
  (jump nil :read-only t)
  (folder "" :type string :read-only t)
  (question 0 :type fixnum :read-only t)
  (answer 0 :type fixnum :read-only t)
  (asked 0 :type fixnum)
  (said 0 :type fixnum))

(setf (documentation 'make-ssh-asking 'function) "An SSH-ASKING for HOST with those pipes."
      (documentation 'ssh-asking-p 'function) "True when OBJECT is an SSH-ASKING."
      (documentation 'ssh-asking-host 'function) "The machine ASKING's connection is to, as ssh takes it."
      (documentation 'ssh-asking-jump 'function) "The machines ASKING's ssh goes through, or NIL."
      (documentation 'ssh-asking-folder 'function) "The private folder of ASKING's connection."
      (documentation 'ssh-asking-said 'function) "How many octets of what ASKING's ssh wrote have been shown."
      (documentation 'ssh-asking-question 'function) "This end of ASKING's pipe of questions."
      (documentation 'ssh-asking-answer 'function) "This end of ASKING's pipe of answers."
      (documentation 'ssh-asking-asked 'function) "How many questions ASKING's connection has asked.")

(defvar *ssh-may-ask* t
  "True when a connection made now may put ssh's questions to you: not for
work nobody is waiting on, such as completing a file name as it is typed.")

(defun someone-to-ask-p (editor)
  "True when EDITOR, the running one or NIL, shows a frame in which a question
can be answered: not one running in the background with no client."
  (and editor (not (tuatara.command::idle-frame-p editor (tuatara.command::editor-frame editor)))))

(defun ssh-can-ask-p (program)
  "True when questions of ssh, the program PROGRAM, can be put in the
minibuffer. That needs them allowed (*SSH-MAY-ASK*), someone to answer them
\(SOMEONE-TO-ASK-P), and OpenSSH 8.5 or later. That version reads
SSH_ASKPASS_REQUIRE, so never asks on the editor's terminal. It also writes
who a server's own question is for before it, so that a server cannot ask in
the words of a question of ssh's."
  (and *ssh-may-ask*
       (someone-to-ask-p (running-editor))
       (let ((version (openssh-version program)))
         (and version (tuatara.io:version-at-least-p version '(8 5))))))

(defun ssh-jump (program host)
  "The machines ssh, PROGRAM, goes through to reach HOST (ProxyJump), cleaned to be shown, or NIL."
  (let ((jump (cdr (assoc "proxyjump" (ssh-settings program host) :test #'string=))))
    (and jump (string/= jump "none") (tuatara.frontend:shown-words jump))))

(defun open-ssh-asking (program folder host)
  "An SSH-ASKING for a connection to HOST through ssh, PROGRAM, its pipes and
script made in FOLDER, private to you; each pipe held open both ways here, so
that the script never waits to open one and sees it end should the editor. A
TUATARA.IO:FILE-SYSTEM-ERROR when they cannot be made, nothing left open."
  (flet ((pipe (name) (tuatara.io:open-named-pipe (tuatara.io:path-join folder name))))
    (tuatara.io:write-new-file (ssh-asking-script-path folder) *ssh-asking-script* :mode #o700)
    (let ((question (pipe "question")) (made nil))
      (unwind-protect
           (prog1 (make-ssh-asking host (ssh-jump program host) folder question (pipe "answer"))
             (setf made t))
        (unless made (sb-posix:close question))))))

(defun ssh-asking-script-path (folder)
  "Where the script ssh runs for a question is in FOLDER, the connection's."
  (tuatara.io:path-join folder "ask"))

(defun close-ssh-asking (asking)
  "Take away ASKING's pipes and close them: a question asked later finds none
and ends with no answer, one waiting for its answer ends with none too."
  (flet ((path (name) (tuatara.io:path-join (ssh-asking-folder asking) name)))
    (tuatara.io:close-named-pipe (ssh-asking-question asking) (path "question"))
    (tuatara.io:close-named-pipe (ssh-asking-answer asking) (path "answer"))))

(defun ssh-asking-options (asking)
  "The ssh options of a connection with ASKING, or NIL for one with nobody to
ask: first, as ssh takes the first of an option given twice. With ASKING a key
not seen before may be accepted, a changed one is refused all the same, and
ssh asks nothing beyond that and what logging in needs (no updated keys, no
keys added to an agent); with none, nothing is asked at all."
  (if asking
      (list "-o" "StrictHostKeyChecking=ask" "-o" "UpdateHostKeys=no" "-o" "AddKeysToAgent=no")
      (list "-o" "BatchMode=yes")))

(defun ssh-asking-environment (asking)
  "The settings of ssh's environment for a connection with ASKING; none without ASKING."
  (and asking
       (list (format nil "SSH_ASKPASS=~a" (ssh-asking-script-path (ssh-asking-folder asking)))
             "SSH_ASKPASS_REQUIRE=force")))

(defstruct (awaited-answer (:constructor make-awaited-answer ()) (:copier nil))
  "An answer waited for from the editor's thread: STATE is :WAITING, then
:ANSWERED or, the job waiting having stopped, :ABANDONED; ANSWER, what was
answered; LOCK held while either is looked at or changed."
  (state :waiting)
  (answer nil)
  (lock (sb-thread:make-mutex :name "ssh answer") :read-only t))

(setf (documentation 'make-awaited-answer 'function) "An AWAITED-ANSWER, still waited for."
      (documentation 'awaited-answer-p 'function) "True when OBJECT is an AWAITED-ANSWER."
      (documentation 'awaited-answer-state 'function) "Whether AWAITED is :WAITING, :ANSWERED or :ABANDONED."
      (documentation 'awaited-answer-answer 'function) "What AWAITED was answered."
      (documentation 'awaited-answer-lock 'function) "The lock held while AWAITED is looked at or changed.")

(defun ask-for-awaited (awaited ask)
  "On the editor's thread, call ASK and keep its answer in AWAITED; ASK not
called when AWAITED is no longer waited for, and an answer that comes after
that wiped."
  (when (eq :waiting (sb-thread:with-mutex ((awaited-answer-lock awaited)) (awaited-answer-state awaited)))
    (let ((got nil))
      (unwind-protect (setf got (funcall ask))
        (sb-thread:with-mutex ((awaited-answer-lock awaited))
          (if (eq :abandoned (awaited-answer-state awaited))
              (wipe-string got)
              (setf (awaited-answer-answer awaited) got
                    (awaited-answer-state awaited) :answered)))))))

(defun abandon-awaited (awaited)
  "Stop waiting for AWAITED, unless it has been answered."
  (sb-thread:with-mutex ((awaited-answer-lock awaited))
    (unless (eq :answered (awaited-answer-state awaited))
      (setf (awaited-answer-state awaited) :abandoned))))

(defun ask-on-editor (ask)
  "What ASK, a function of no arguments that puts a question in the
minibuffer, gives on the editor's thread: run at once on that thread, else as
its next command, waited for as long as it takes, the job asking stopping the
wait. NIL when the question was given up."
  (if (away-from-editor-p)
      (let ((awaited (make-awaited-answer)))
        (defer-action (lambda () (ask-for-awaited awaited ask)))
        (unwind-protect
             (progn (tuatara.runtime:wait-until (lambda ()
                                                  (tuatara.runtime:job-check-stop)
                                                  (eq :answered (awaited-answer-state awaited)))
                                                nil)
                    (awaited-answer-answer awaited))
          (abandon-awaited awaited)))
      (funcall ask)))

(defun ssh-question-words (text)
  "TEXT, a question of ssh's, its lines cleaned of what a server could have put
in them (TUATARA.FRONTEND:SHOWN-WORDS) and joined with a space."
  (format nil "~{~a~^ ~}" (remove "" (mapcar (lambda (line) (string-trim " " (tuatara.frontend:shown-words line)))
                                             (uiop:split-string text :separator '(#\Newline #\Return)))
                                  :test #'string=)))

(defun ssh-question-kind (text)
  "What TEXT, a question ssh asks as it wrote it, is: :KEY, ssh's own question
about a host key, :LOCAL, ssh asking for what unlocks a key of yours, or
:SERVER, anything else, as what a server asks. ssh writes who a server's
question is for before it, so that a server cannot begin one in these forms."
  (cond ((or (cl-ppcre:scan "(?s)\\AThe authenticity of host '.*\\(yes/no/\\[fingerprint\\]\\)\\? *\\z"
                            text)
             (cl-ppcre:scan "(?s)\\AWarning: the \\S+ host key for '.*\\(yes/no\\)\\? *\\z" text))
         :key)
        ((cl-ppcre:scan "\\A(Enter passphrase for key '[^\\n]*'|Enter PIN for [^\\n]* key [^\\n]*|Enter PIN for '[^\\n]*'): *\\z"
                        text)
         :local)
        (t :server)))

(defun ssh-question-prompt (host jump text said &key warnings)
  "What is shown to ask TEXT, a question of ssh's about HOST, as four values:
the short prompt it is answered at, its kind (SSH-QUESTION-KIND), for :KEY the
fingerprint that may be typed for yes, and its details: who asks, WARNINGS of
ssh's, SAID, what ssh passed on from the server since the last question, and
the question whole. JUMP, the machines ssh goes through, says the question may
be for one of them."
  (let* ((kind (ssh-question-kind text))
         (words (ssh-question-words text))
         (to (format nil "ssh to ~a~@[ (or the jump host ~a)~]" host jump))
         (question (string-right-trim ": " words)))
    (flet ((details (line)
             (format nil "~a~%~%~{~a~%~}~@[~%~a~%~]~%~a~%" to warnings (and (plusp (length said)) said) line)))
      (ecase kind
        (:key
         (let ((end (or (cl-ppcre:scan "\\s*\\(yes/no(/\\[fingerprint\\])?\\)\\?\\s*$" words) (length words)))
               (typed (search "[fingerprint]" text)))
           (values (format nil "Accept this key (yes/no~:[~;/fingerprint~]): " typed)
                   kind
                   (let ((found (nth-value 1 (cl-ppcre:scan-to-strings "key fingerprint is:? (\\S+?)\\.?(\\s|$)" words))))
                     (and found (aref found 0)))
                   (details (format nil "~a?~%~%Type yes~:[~; or the fingerprint~] to accept, anything else refuses."
                                    (subseq words 0 end) typed)))))
        (:local
         (values "Your key's passphrase, stays here: " kind nil
                 (details (format nil "Your key's passphrase, which stays here: ~a" question))))
        (:server
         (values (format nil "The server asks: ~a: " (tuatara.display:shorten-string question +ssh-most-shown+))
                 kind nil
                 (details (format nil "The server asks, and what you type goes to the server: ~a" question))))))))

(defun ssh-key-answer (typed fingerprint)
  "What is answered to ssh for TYPED, typed at its question about a host key:
TYPED when it is yes or FINGERPRINT, the key's, else NIL, a refusal."
  (let ((typed (string-trim " " typed)))
    (and (or (string= typed "yes") (and fingerprint (string= typed fingerprint))) typed)))

(defun ask-ssh-person (asking text)
  "Put TEXT, what ASKING's ssh asks, in the minibuffer: whether to accept a
key not seen before, its question in full, a yes typed out or its fingerprint
the only way to accept it; else a password or passphrase, hidden and kept
nowhere, said to be for your own key or for the server. What it says in full
is in a buffer shown beside the short prompt (CALL-WITH-DETAILS-SHOWN), a
warning of ssh's going to *Messages* too. The answer as a string, which the
caller wipes, or NIL for none and for a question given up."
  (multiple-value-bind (warnings said) (ssh-new-said asking)
    (multiple-value-bind (prompt kind fingerprint details)
        (ssh-question-prompt (ssh-asking-host asking) (ssh-asking-jump asking) text said :warnings warnings)
      (ask-on-editor
       (lambda ()
         (dolist (warning warnings) (message "ssh to ~a: ~a" (ssh-asking-host asking) warning))
         (call-with-details-shown
          details
          (lambda ()
            (if (eq kind :key)
                (tuatara.command::call-asked-unasked
                 prompt (lambda () (ssh-key-answer (read-string prompt) fingerprint)))
                (read-password prompt :fresh t)))))))))

(defun ssh-said-parts (text)
  "TEXT, what ssh wrote on its error output, in two parts as two values: its
warnings to you (lines that begin **, as about a connection not safe against
quantum computers), a list of lines cleaned to be shown without the stars, and
the rest (a server's banner and instructions), cleaned, on one line."
  (let ((warnings '()) (rest '()))
    (dolist (line (uiop:split-string text :separator '(#\Newline #\Return)))
      (if (uiop:string-prefix-p "** " line)
          (push (ssh-question-words (subseq line 3)) warnings)
          (push line rest)))
    (values (nreverse warnings) (ssh-question-words (format nil "~{~a~%~}" (nreverse rest))))))

(defun ssh-new-said (asking)
  "What ASKING's ssh has written on its error output since this was last
asked, as two values: its warnings to you and the rest (SSH-SAID-PARTS)."
  (let ((octets (or (ignoring-errors-reported ("reading what ssh said")
                      (tuatara.io:file-octets (tuatara.io:path-join (ssh-asking-folder asking) "e")
                                              :limit tuatara.io:*program-said-limit*))
                    (tuatara.text:make-octets 0))))
    (multiple-value-prog1 (ssh-said-parts (tuatara.text:lenient-string (subseq octets (min (ssh-asking-said asking)
                                                                                           (length octets)))))
      (setf (ssh-asking-said asking) (length octets)))))

(defun answer-ssh-question (asking)
  "Read the question waiting in ASKING's pipe, put it to you, and send back
the answer, or that there is none. True when one was given; NIL when none
was or too many have been asked, and the connection should end."
  (let* ((tuatara.runtime:*secret-at-hand* t)
         (octets (tuatara.text:make-octets +ssh-most-question+))
         (got (tuatara.io:with-octets-sap (sap octets)
                (sb-unix:unix-read (ssh-asking-question asking) sap +ssh-most-question+))))
    (when (null got) (return-from answer-ssh-question t))
    (when (<= (incf (ssh-asking-asked asking)) +ssh-most-questions+)
      (let* ((end (or (position 0 octets :end got) got))
             (answer (ask-ssh-person asking (tuatara.text:lenient-string (subseq octets 0 end))))
             (line (if answer (tuatara.text:string-octets answer) (tuatara.text:make-octets 0)))
             (given (and answer (<= (length line) +ssh-most-answer+)))
             (end-at (and given (or (position 10 line) (length line))))
             (reply (tuatara.text:make-octets (if given (+ 3 end-at) 2))))
        (unwind-protect
             (progn
               (setf (aref reply 0) (char-code (if given #\y #\n))
                     (aref reply 1) 10)
               (when given
                 (replace reply line :start1 2 :end2 end-at)
                 (setf (aref reply (1- (length reply))) 10))
               (tuatara.io:write-octets (ssh-asking-answer asking) reply)
               (and given t))
          (wipe-string answer)
          (fill line 0)
          (fill reply 0))))))

(defun say-connecting (host)
  "Say in the echo area, not in the record of messages, that a connection to
HOST is being made; the words said, for UNSAY-CONNECTING."
  (let ((words (format nil "Connecting to ~a..." host)))
    (when (away-from-editor-p)
      (defer-action (lambda () (echo "~a" words)) :without-screen t))
    words))

(defun unsay-connecting (words)
  "Clear WORDS, as SAY-CONNECTING said them, from the echo area, unless something else is shown there now."
  (when (away-from-editor-p)
    (defer-action (lambda ()
                    (when (equal words (editor-message *editor*))
                      (setf (editor-message *editor*) nil)))
                  :without-screen t)))
