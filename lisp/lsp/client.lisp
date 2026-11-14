;;;; lisp/lsp/client.lisp --- Talking to language servers: their programs, the messages, the answers.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; A language server is a program run for a project and a language, in the
;;;; project's directory. Tuatara writes it messages on its standard input and
;;;; reads its own from its standard output, each JSON with a Content-Length
;;;; header (the Language Server Protocol). Each server has a thread that writes
;;;; what is sent to it, so that a server slow to read never holds the editor up
;;;; (one that leaves *LSP-MOST-UNSENT* octets unread is ended), and one that
;;;; reads what it sends, never trusting it (a message past
;;;; *LSP-LARGEST-MESSAGE* is let go unread), handing it to the editor's thread,
;;;; woken for it, which answers the server's own questions, takes in what it
;;;; says, and calls what waited for an answer. Nothing waits on a server but
;;;; where a command must have its answer to go on (a completion, *LSP-WAIT*
;;;; seconds at most), the editor drawing meanwhile and C-g stopping it.

(in-package #:tuatara.lsp)

(defvar *lsp-largest-message* (* 32 1024 1024)
  "The largest message read from a language server, in octets: a larger one
is let go unread, said so.")

(defvar *lsp-most-unsent* (* 64 1024 1024)
  "The most octets of messages waiting for a language server to read them: a
server that leaves more unread has stopped reading, and is ended.")

(define-setting *lsp-wait* 2
  "How long a language server's answer is waited for before the question is
given up (a completion, a definition, a rename); work goes on meanwhile.
Raise it for a slow server, one still indexing a large project; lower it to
give up sooner on one that is slow.
- a number: that many seconds, 2 as standard."
  :values (number 0 *))

(defvar *lsp-servers* '()
  "The language servers running, the most recently started first.")

(defvar *lsp-inbox* (sb-concurrency:make-queue :name "language servers")
  "What the servers' threads read, for the editor's thread: each (SERVER KIND
. WHAT), KIND :MESSAGE for a message (WHAT, read JSON), :ENDED for the
server's output ended (WHAT, why in words), :TOO-LARGE for a message let go
unread (WHAT, its length).")

(define-record (lsp-server (:constructor make-lsp-server (name command root language options wake)) (:copier nil))
  "A language server. NAME is its name as known (rust-analyzer). COMMAND is
its program and arguments. ROOT is the project's directory it serves. LANGUAGE is
the language it is started for. OPTIONS is what it is told at its start (read
JSON), or NIL. WAKE is a function that wakes the editor, or NIL. PROCESS is
the program running. OUTBOX is the messages waiting to be written to it, as
octets, UNSENT how many octets those are, and STALLED true once they were too
many. NEXT-ID is the next request's number. WAITING is the requests not yet
answered, by number, each the function called with the answer's result and its
error. PROMISED is those whose answer a thread waits for (LSP-PROMISE), by
number, given it by the thread that reads the server. CAPABILITIES is what it
said it can do. ENCODING is :UTF-8 or :UTF-16, how its positions count
characters. STATUS is :STARTING, :READY or :STOPPED. DOCUMENTS are the
buffers it has open. SAID is its last lines on its error output, SAID-COUNT
how many. PROGRESS is what it said it is busy with, by token, each (TITLE
WORDS PERCENT). EVENTS are its last messages either way, newest first, each
\(TIME TO-IT TEXT), TEXT their start (*LSP-EVENT-LENGTH*), EVENTS-COUNT how
many. ACTING is how many of its commands chosen to be run are under way. LOCK
is held while UNSENT, PROMISED, SAID or EVENTS change."
  (name nil :documentation "The name of LSP-SERVER as known, as in rust-analyzer.")
  (command nil :documentation "The program and arguments of LSP-SERVER.")
  (root nil :documentation "The project's directory LSP-SERVER serves.")
  (language nil :documentation "The language LSP-SERVER is started for.")
  (options nil :documentation "What LSP-SERVER is told at its start (read JSON), or NIL.")
  (wake nil :documentation "A function that wakes the editor, or NIL.")
  (process nil :documentation "The program running as LSP-SERVER.")
  (outbox (sb-concurrency:make-mailbox :name "language server writes")
          :documentation "The messages waiting to be written to LSP-SERVER, as octets.")
  (unsent 0 :documentation "How many octets wait in the outbox of LSP-SERVER.")
  (stalled nil :documentation "True once too many octets waited in the outbox of LSP-SERVER.")
  (lock (sb-thread:make-mutex :name "language server")
        :documentation "The mutex held while UNSENT, PROMISED, SAID or EVENTS of LSP-SERVER change.")
  (next-id 1 :documentation "The number of the next request to LSP-SERVER.")
  (waiting (make-hash-table)
           :documentation "The requests not yet answered, by number, each the function called with the answer's result
and its error.")
  (promised (make-hash-table)
            :documentation "The requests whose answer a thread waits for (LSP-PROMISE), by number, given it by the
thread that reads the server.")
  (capabilities nil :documentation "What LSP-SERVER said it can do.")
  (encoding :utf-16 :documentation "How the positions of LSP-SERVER count characters, :UTF-8 or :UTF-16.")
  (status :starting :documentation "The status of LSP-SERVER: :STARTING, :READY or :STOPPED.")
  (documents '() :documentation "The buffers LSP-SERVER has open.")
  (said '() :documentation "The last lines LSP-SERVER wrote on its error output.")
  (said-count 0 :documentation "How many lines LSP-SERVER has written on its error output.")
  (progress (make-hash-table :test 'equal)
            :documentation "What LSP-SERVER said it is busy with, by token, each (TITLE WORDS PERCENT).")
  (events '()
          :documentation "The last messages either way, newest first, each (TIME TO-IT TEXT), TEXT their start
\(*LSP-EVENT-LENGTH*).")
  (events-count 0 :documentation "How many messages LSP-SERVER has had either way.")
  (acting 0 :documentation "How many commands of LSP-SERVER chosen to be run are under way."))

(setf (documentation 'lsp-server-p 'function) "True when OBJECT is an LSP-SERVER.")

(defvar *lsp-events-kept* 500
  "How many of a language server's last messages, either way, are kept for
\\[lsp-events]; 0 for none.")

(defvar *lsp-event-length* 2000
  "The most characters of a message kept for \\[lsp-events].")

(defvar *lsp-said-kept* 20
  "How many of a language server's last lines on its error output are kept,
for \\[lsp-events] and to say why it stopped.")

(defvar *lsp-said-length* 300
  "The most characters of a line a language server writes on its error output that are kept.")

(defun push-capped (item list count most)
  "LIST, of COUNT items, with ITEM before it, kept to about MOST items: cut
back to MOST once it holds twice as many. Two values: the list and how many
it holds."
  (if (< count (* 2 most))
      (values (cons item list) (1+ count))
      (values (cons item (subseq list 0 (1- most))) most)))

(defun lsp-note-event (server to-it text)
  "Keep the start of TEXT, a message to SERVER (TO-IT true) or from it, among
its events for \\[lsp-events]."
  (when (plusp *lsp-events-kept*)
    (let ((kept (subseq text 0 (min (length text) *lsp-event-length*))))
      (sb-thread:with-mutex ((lsp-server-lock server))
        (setf (values (lsp-server-events server) (lsp-server-events-count server))
              (push-capped (list (get-universal-time) to-it kept) (lsp-server-events server)
                           (lsp-server-events-count server) *lsp-events-kept*))))))

;;; What a server sends, taken in with care

(defparameter *command-line-scanner*
  (cl-ppcre:create-scanner
   "(?:(?<=spawn )|cd \"[^\"]*\" && )(?:[A-Za-z_][A-Za-z0-9_]*=\"[^\"]*\" )*\"([^\"]+)\"(?: \"[^\"]*\")*")
  "The pattern of a command line a server could not run, as it writes one in
its words (rust-analyzer's Failed to spawn cd \"directory\" &&
RUSTUP_AUTO_INSTALL=\"0\" \"rustfmt\"): after spawn, or beginning cd
\"directory\" &&; then settings NAME=\"value\", the program and its
arguments, each in quotes. Words it does not match are said whole.")

(defun lsp-error (code words)
  "A JSON-RPC error of CODE, a number the protocol gives, saying WORDS."
  (tuatara.text:json-object "code" code "message" words))

;;; Writing

(defparameter *lsp-message-size* 512
  "What a message made on the writer thread counts for in *LSP-MOST-UNSENT*
before it is written: its length is not known until it is made.")

(defun lsp-message-octets (server message)
  "The octets of MESSAGE, a JSON object, as written to SERVER, its text noted
among the events for \\[lsp-events]."
  (let ((text (tuatara.text:json-write message)))
    (lsp-note-event server t text)
    (tuatara.text:string-octets text)))

(defun lsp-send (server message &key (size *lsp-message-size*))
  "Have MESSAGE, a JSON object, written to SERVER by its writer thread, framed
with its length; NIL when SERVER is no longer there to read it. MESSAGE may be
a function making the object, for one holding a whole file: it is called, and
its text made, on the writer thread and not on the editor's, and counts SIZE
octets as unsent until it is written. A server that leaves more than
*LSP-MOST-UNSENT* octets unread is ended."
  (unless (or (eq (lsp-server-status server) :stopped) (lsp-server-stalled server))
    (let* ((octets (if (functionp message) message (lsp-message-octets server message)))
           (size (if (functionp message) size (length octets)))
           (unsent (sb-thread:with-mutex ((lsp-server-lock server))
                     (incf (lsp-server-unsent server) size))))
      (if (> unsent *lsp-most-unsent*)
          (progn (setf (lsp-server-stalled server) t)
                 (lsp-server-ended server "it stopped reading what it is sent")
                 nil)
          (progn (sb-concurrency:send-message (lsp-server-outbox server) (cons size octets))
                 t)))))

(defun lsp-start-writing (server)
  "Write what is sent to SERVER (its OUTBOX) on a thread of its own, until it
is told to end, then end its standard input; once a write fails, the rest is
let go."
  (let ((stream (sb-ext:process-input (lsp-server-process server)))
        (broken nil))
    (tuatara.runtime:make-guarded-thread
     (lambda ()
       (loop for sent = (sb-concurrency:receive-message (lsp-server-outbox server))
             until (eq sent :end)
             do (unless broken
                  (handler-case
                      (reporting-errors ("writing to ~a" (lsp-server-name server))
                        (let ((octets (if (functionp (cdr sent))
                                          (lsp-message-octets server (funcall (cdr sent)))
                                          (cdr sent))))
                          (write-sequence (tuatara.text:string-octets
                                           (format nil "Content-Length: ~d~c~c~c~c"
                                                   (length octets) #\Return #\Newline #\Return #\Newline))
                                          stream)
                          (write-sequence octets stream)
                          (finish-output stream)))
                    (error () (setf broken t))))
             (sb-thread:with-mutex ((lsp-server-lock server))
               (decf (lsp-server-unsent server) (car sent))))
       (ignoring-errors-reported ("closing the input of ~a" (lsp-server-name server))
         (close stream :abort broken)))
     :name (format nil "tuatara ~a writes" (lsp-server-name server)))))

(defun lsp-notify (server method &optional params &key (size *lsp-message-size*))
  "Tell SERVER of METHOD with PARAMS, wanting no answer. PARAMS may be a function
making them, called on the writer thread (LSP-SEND), for a whole file's text,
which is SIZE octets."
  (flet ((notification (params)
           (tuatara.text:json-object "jsonrpc" "2.0" "method" method "params" (or params :omit))))
    (lsp-send server
              (if (functionp params) (lambda () (notification (funcall params))) (notification params))
              :size size)))

(defvar *lsp-deadlines* '()
  "The requests to servers that are given up when not answered in time
\(LSP-REQUEST's SECONDS), each (DEADLINE SERVER ID), DEADLINE as
TUATARA.RUNTIME:DEADLINE-IN gives it.")

(defun lsp-request (server method params then &key seconds)
  "Ask SERVER METHOD with PARAMS; THEN is called on the editor's thread with
the answer's result and its error (a JSON object, or NIL) once it comes.
With SECONDS, a question not answered in that time is no longer wanted
\(LSP-CANCEL), THEN called with no result and the error :TIMEOUT
\(LSP-GIVE-UP-LATE). Returns the request's number."
  (let ((id (shiftf (lsp-server-next-id server) (1+ (lsp-server-next-id server)))))
    (setf (gethash id (lsp-server-waiting server)) then)
    (if (lsp-send server (tuatara.text:json-object "jsonrpc" "2.0" "id" id "method" method "params" (or params :omit)))
        (when seconds
          (push (list (tuatara.runtime:deadline-in seconds) server id) *lsp-deadlines*))
        (progn (remhash id (lsp-server-waiting server))
               (funcall then nil (tuatara.text:json-object "message" "the server is not running"))))
    id))

(defun lsp-cancel (server id)
  "Tell SERVER the request ID is no longer wanted; its answer, if it comes, is let go."
  (when (remhash id (lsp-server-waiting server))
    (lsp-notify server "$/cancelRequest" (tuatara.text:json-object "id" id))))

(defun lsp-give-up-late ()
  "Give up each request of *LSP-DEADLINES* past its time, telling what waited
for it :TIMEOUT, and let go of those answered; the seconds until the next is
due, or NIL when none waits."
  (let ((soonest nil))
    (dolist (entry *lsp-deadlines*)
      (destructuring-bind (deadline server id) entry
        (let ((then (gethash id (lsp-server-waiting server)))
              (left (tuatara.runtime:seconds-left deadline)))
          (cond ((null then) (setf *lsp-deadlines* (remove entry *lsp-deadlines*)))
                ((minusp left)
                 (setf *lsp-deadlines* (remove entry *lsp-deadlines*))
                 (lsp-cancel server id)
                 (handler-case (reporting-errors ("giving up waiting for ~a" (lsp-server-name server))
                                 (funcall then nil :timeout))
                   (error (e)
                     (log-message (format nil "~a: what waited for an answer failed: ~a" (lsp-server-name server) e)))))
                (t (setf soonest (if soonest (min soonest left) left)))))))
    (and soonest (max 0.01 soonest))))

(defun short-failure-words (words)
  "WORDS, why a program or a server failed, short enough for the echo area,
their first line that says anything, a program it could not run named
alone, not by the command line it was run by (*COMMAND-LINE-SCANNER*):
Failed to spawn rustfmt; \"it failed\" when they say nothing."
  (let ((line (find-if (lambda (l) (plusp (length l)))
                       (mapcar (lambda (l) (string-trim '(#\Space #\Tab #\Return) l))
                               (uiop:split-string (or words "") :separator '(#\Newline))))))
    (if (null line)
        "it failed"
        (cl-ppcre:regex-replace-all *command-line-scanner* line
                                    (lambda (match program)
                                      (declare (ignore match))
                                      (tuatara.io:native-file-name program))
                                    :simple-calls t))))

(defun seconds-words (seconds)
  "SECONDS as a person reads them: 1 second, 2 seconds, 0.5 seconds."
  (let ((n (if (and (rationalp seconds) (not (integerp seconds))) (float seconds) seconds)))
    (when (and (floatp n) (= n (round n))) (setf n (round n)))
    (format nil "~a second~:[s~;~]" n (eql n 1))))

(defun lsp-error-words (error &key (seconds *lsp-wait*) (setting '*lsp-wait*))
  "Why a question to a server came to nothing, in words short enough for the
echo area: for ERROR :TIMEOUT, no answer within SECONDS (as SETTING says);
for :STOPPED, that a key stopped the wait; for a server's answer saying it
failed, its words made short (SHORT-FAILURE-WORDS)."
  (case error
    (:timeout (format nil "no answer within ~a (~(~a~))" (seconds-words seconds) setting))
    (:stopped "a key stopped the wait")
    (t (short-failure-words (and (hash-table-p error) (tuatara.text:json-text error "message"))))))

;;; Answers a thread waits for

(define-record (lsp-promise (:constructor make-lsp-promise (server id)) (:copier nil))
  "The answer to SERVER's request ID, which a thread waits for (LSP-PROMISE-WAIT):
RESULT and ERROR once DONE, ARRIVED signalled then. The thread reading the
server gives it, so that it comes while the editor's thread waits too."
  (server nil :documentation "The server of the request LSP-PROMISE answers.")
  (id nil :documentation "The number of the request LSP-PROMISE answers.")
  (result nil :documentation "The result of the answer, once LSP-PROMISE is done.")
  (error nil :documentation "The error of the answer, once LSP-PROMISE is done.")
  (done nil :documentation "True once the answer of LSP-PROMISE has come.")
  (arrived (sb-thread:make-semaphore :name "language server answer")
           :documentation "The semaphore signalled once LSP-PROMISE is done."))

(setf (documentation 'lsp-promise-p 'function) "True when OBJECT is an LSP-PROMISE.")

(defun keep-promise (promise result error)
  "Give PROMISE its RESULT and ERROR, waking what waits for it."
  (setf (lsp-promise-result promise) result
        (lsp-promise-error promise) error
        (lsp-promise-done promise) t)
  (sb-thread:signal-semaphore (lsp-promise-arrived promise)))

(defun lsp-promise (server method params)
  "Ask SERVER METHOD with PARAMS, the answer kept in the LSP-PROMISE returned,
for any thread to wait for (LSP-PROMISE-WAIT). Asked on the editor's thread."
  (let* ((id (shiftf (lsp-server-next-id server) (1+ (lsp-server-next-id server))))
         (promise (make-lsp-promise server id)))
    (sb-thread:with-mutex ((lsp-server-lock server))
      (setf (gethash id (lsp-server-promised server)) promise))
    (unless (lsp-send server (tuatara.text:json-object "jsonrpc" "2.0" "id" id "method" method
                                                       "params" (or params :omit)))
      (sb-thread:with-mutex ((lsp-server-lock server))
        (remhash id (lsp-server-promised server)))
      (keep-promise promise nil (tuatara.text:json-object "message" "the server is not running")))
    promise))

(defun promise-answered (server id)
  "The promise of SERVER's request ID, let go of as its answer has come; NIL
when no thread waits for that answer."
  (sb-thread:with-mutex ((lsp-server-lock server))
    (let ((promise (gethash id (lsp-server-promised server))))
      (when promise
        (remhash id (lsp-server-promised server))
        promise))))

(defun break-promises (server words)
  "Give every answer a thread waits for from SERVER the error WORDS: the
server ended."
  (let ((promised (sb-thread:with-mutex ((lsp-server-lock server))
                    (prog1 (loop for promise being the hash-values of (lsp-server-promised server)
                                 collect promise)
                      (clrhash (lsp-server-promised server))))))
    (dolist (promise promised)
      (keep-promise promise nil (tuatara.text:json-object "message" words)))))

(defun lsp-promise-wait (promise &key (seconds *lsp-wait*) halt)
  "Wait for PROMISE's answer, SECONDS at most, on any thread: its result and
error as two values, the error :TIMEOUT when it did not come in time and
:STOPPED when HALT, a function asked as it waits, turned true. A request given
up so is no longer wanted: its answer, if it comes, is let go."
  (let ((deadline (tuatara.runtime:deadline-in seconds)))
    (loop until (lsp-promise-done promise)
          do (let ((left (tuatara.runtime:seconds-left deadline)))
               (when (or (minusp left) (and halt (funcall halt)))
                 (promise-answered (lsp-promise-server promise) (lsp-promise-id promise))
                 (return-from lsp-promise-wait (values nil (if (minusp left) :timeout :stopped))))
               (sb-thread:wait-on-semaphore (lsp-promise-arrived promise) :timeout (min 1/20 (max left 1/1000)))))
    (values (lsp-promise-result promise) (lsp-promise-error promise))))

;;; Reading

(defun lsp-read-message (stream)
  "The next message STREAM (a server's output) holds, read JSON, and its text,
as two values; NIL at its end. One past *LSP-LARGEST-MESSAGE* is read and let
go: :TOO-LARGE and its length. A header line past 1024 characters, or
anything not framed as the protocol frames it, is an error."
  (let ((length nil))
    (loop (multiple-value-bind (line longer) (tuatara.io:read-line-bounded stream 1024)
            (cond ((null line) (return-from lsp-read-message nil))
                  (longer (refuse "A header line of more than 1024 characters"))
                  ((string= (string-right-trim '(#\Return) line) "") (return))
                  ((string-equal "Content-Length:" line :end2 (min (length line) 15))
                   (setf length (parse-integer line :start 15 :junk-allowed t))))))
    (unless (and length (<= 0 length))
      (refuse "A message of ~:[no length~;~:*~:d octets~]" length))
    (if (> length *lsp-largest-message*)
        (let ((scrap (make-array (min length 65536) :element-type '(unsigned-byte 8))))
          (loop with left = length
                while (plusp left)
                do (let ((read (read-sequence scrap stream :end (min left (length scrap)))))
                     (when (zerop read) (return-from lsp-read-message nil))
                     (decf left read)))
          (values :too-large length))
        (let ((octets (make-array length :element-type '(unsigned-byte 8))))
          (unless (= length (read-sequence octets stream))
            (return-from lsp-read-message nil))
          ;; an octet that is not UTF-8 as its raw byte's character, as in a file's text
          (let ((text (tuatara.text:octets-string octets)))
            (values (tuatara.text:json-read text) text))))))

(defun lsp-arrive (server kind what)
  "Hand WHAT from SERVER, of KIND (as *LSP-INBOX* has them), to the editor's
thread, waking it."
  (sb-concurrency:enqueue (list* server kind what) *lsp-inbox*)
  (let ((wake (lsp-server-wake server)))
    (when wake (funcall wake))))

(defun lsp-receive (server message)
  "Take MESSAGE, read from SERVER, on the thread reading it: an answer a
thread waits for given to it (LSP-PROMISE), anything else handed to the
editor's thread."
  (let* ((id (and (hash-table-p message) (null (tuatara.text:json-get message "method"))
                  (tuatara.text:json-get message "id")))
         (promise (and id (promise-answered server id))))
    (if promise
        (keep-promise promise (tuatara.text:json-get message "result") (tuatara.text:json-get message "error"))
        (lsp-arrive server :message message))))

(defun lsp-start-reading (server)
  "Read what SERVER sends on threads of its own: its messages taken in
\(LSP-RECEIVE); its error output's last lines into its SAID."
  (let ((process (lsp-server-process server)))
    (tuatara.runtime:make-guarded-thread
     (lambda ()
       (let ((why "it stopped"))
         (handler-case
             (reporting-errors ("reading what ~a sends" (lsp-server-name server))
               (loop (multiple-value-bind (message text) (lsp-read-message (sb-ext:process-output process))
                       (cond ((null message) (return))
                             ((eq message :too-large) (lsp-arrive server :too-large text))
                             (t (lsp-note-event server nil text)
                                (lsp-receive server message))))))
           (error (e) (setf why (princ-to-string e))))
         (break-promises server (format nil "the server stopped (~a)" why))
         (lsp-arrive server :ended why)))
     :name (format nil "tuatara ~a" (lsp-server-name server)))
    (tuatara.runtime:make-guarded-thread
     (lambda ()
       (ignoring-errors-reported ("reading what ~a says" (lsp-server-name server))
         (handler-case
             (loop for line = (tuatara.io:read-line-bounded (sb-ext:process-error process) *lsp-said-length*)
                   while line
                   do (sb-thread:with-mutex ((lsp-server-lock server))
                        (setf (values (lsp-server-said server) (lsp-server-said-count server))
                              (push-capped line (lsp-server-said server) (lsp-server-said-count server)
                                           *lsp-said-kept*))))
           ;; its error output closed as it ends
           (stream-error () nil))))
     :name (format nil "tuatara ~a errors" (lsp-server-name server)))))

(defun lsp-said (server)
  "SERVER's last lines on its error output, the newest first."
  (sb-thread:with-mutex ((lsp-server-lock server))
    (copy-list (lsp-server-said server))))

;;; Taking in, on the editor's thread

(defvar *lsp-notification-functions* (make-hash-table :test 'equal)
  "Functions of a server and the parameters of what it tells by a method's
name (textDocument/publishDiagnostics), called on the editor's thread.")

(defvar *lsp-request-functions* (make-hash-table :test 'equal)
  "Functions of a server and the parameters of what it asks by a method's
name (workspace/configuration), whose value is the answer, called on the
editor's thread; one returning :LATER answers by calling *LSP-ANSWER* once it
knows. A question no function answers is answered with an error.")

(defvar *lsp-answer* nil
  "While a server's question is answered, a function of the answer and an
error (a JSON object, or NIL) that sends them to the server: for a function
of *LSP-REQUEST-FUNCTIONS* that answers later.")

(defmacro define-lsp-notification (method (server params) &body body)
  "Define what is done when a server tells METHOD, SERVER and PARAMS bound."
  `(setf (gethash ,method *lsp-notification-functions*)
         (lambda (,server ,params) (declare (ignorable ,server ,params)) ,@body)))

(defmacro define-lsp-request (method (server params) &body body)
  "Define the answer to a server asking METHOD, SERVER and PARAMS bound."
  `(setf (gethash ,method *lsp-request-functions*)
         (lambda (,server ,params) (declare (ignorable ,server ,params)) ,@body)))

(defun lsp-take-in ()
  "Take in what the servers sent: answers to the functions waiting for them,
questions answered, what they tell done; true when there was anything."
  (loop for (server kind . what) = (sb-concurrency:dequeue *lsp-inbox*)
        while server
        count t
        do (handler-case (reporting-errors ("taking in what ~a sent" (lsp-server-name server))
                           (ecase kind
                             (:message (lsp-take-in-message server what))
                             (:ended (lsp-server-ended server what))
                             (:too-large
                              (message "~a sent a message of ~:d octets, more than *lsp-largest-message*: let go"
                                       (lsp-server-name server) what))))
             (error (e)
               (log-message (format nil "~a: what it sent could not be taken in: ~a" (lsp-server-name server) e))))))

(defun lsp-take-in-message (server message)
  "Take in MESSAGE from SERVER: an answer, a question or something it tells."
  (let ((id (tuatara.text:json-get message "id"))
        (method (tuatara.text:json-text message "method")))
    (cond ((and id method) (lsp-answer-request server id method (tuatara.text:json-get message "params")))
          (method
           (let ((function (gethash method *lsp-notification-functions*)))
             (when function (funcall function server (tuatara.text:json-get message "params")))))
          (id
           (let ((then (gethash id (lsp-server-waiting server))))
             (when then
               (remhash id (lsp-server-waiting server))
               (funcall then (tuatara.text:json-get message "result") (tuatara.text:json-get message "error"))))))))

(defun lsp-answer-request (server id method params)
  "Answer SERVER's question ID, METHOD with PARAMS, by its function of
*LSP-REQUEST-FUNCTIONS*: at once, or once it calls *LSP-ANSWER*; with an
error when no function answers METHOD, or the function failed."
  (let* ((answered nil)
         (answer (lambda (result &optional error)
                   (unless answered
                     (setf answered t)
                     (lsp-send server (if error
                                          (tuatara.text:json-object "jsonrpc" "2.0" "id" id "error" error)
                                          (tuatara.text:json-object "jsonrpc" "2.0" "id" id "result" result))))))
         (function (gethash method *lsp-request-functions*)))
    (if (null function)
        (funcall answer nil (lsp-error -32601 (format nil "~a is not answered here" method)))
        (let ((result (handler-case (reporting-errors ("answering ~a's ~a" (lsp-server-name server) method)
                                      (let ((*lsp-answer* answer)) (funcall function server params)))
                        (error (e)
                          (log-message (format nil "~a asked ~a, which failed: ~a" (lsp-server-name server) method e))
                          (funcall answer nil (lsp-error -32603 (princ-to-string e)))))))
          (unless (eq result :later)
            (funcall answer result))))))

(defvar *lsp-background* '()
  "Work on what servers sent that is done in the background (LSP-IN-BACKGROUND),
each (CELL . THEN), CELL as TUATARA.RUNTIME:START-BACKGROUND-RESULT gives it.")

(defun lsp-in-background (work then)
  "Call WORK, a function of no arguments that reads nothing the editor
changes, on a thread of its own, and THEN with what it returns on the
editor's thread once it is done (LSP-TAKE-IN-BACKGROUND), or with NIL when it
failed."
  (push (cons (tuatara.runtime:start-background-result (lambda () (list (funcall work)))
                                                       "tuatara language server work" (editor-job-waker))
              then)
        *lsp-background*))

(defun lsp-take-in-background ()
  "Hand what the work of *LSP-BACKGROUND* that is done made to what waits for
it; a failure is noted in *Messages*, and NIL handed on."
  (dolist (entry *lsp-background*)
    (destructuring-bind (cell . then) entry
      (when (car cell)
        (setf *lsp-background* (remove entry *lsp-background*))
        (funcall then (if (eq (first (car cell)) :error)
                          (progn (log-message (format nil "Work on a language server's answer failed: ~a"
                                                      (second (car cell))))
                                 nil)
                          (first (car cell))))))))

(defun lsp-tick (editor)
  "Take in what servers sent (LSP-TAKE-IN) and the work done on it in the
background (LSP-TAKE-IN-BACKGROUND), send the changes waiting
\(LSP-SEND-CHANGES), and give up the questions not answered in their time
\(LSP-GIVE-UP-LATE); look again when the soonest change still waiting or
question's time is due, else not until a server sends something or a key is
typed."
  (declare (ignore editor))
  (lsp-take-in)
  (lsp-take-in-background)
  (lsp-send-changes)
  (let ((changes (lsp-changes-due-in))
        (late (lsp-give-up-late)))
    (if (and changes late) (min changes late) (or changes late))))

;;; Starting and stopping

(defun lsp-server-for (name root)
  "The server NAME running for the project at ROOT, or NIL."
  (find-if (lambda (s) (and (string= (lsp-server-name s) name) (string= (lsp-server-root s) root)
                            (not (eq (lsp-server-status s) :stopped))))
           *lsp-servers*))

(defvar *lsp-client-commands* (make-hash-table :test 'equal)
  "Commands a server names that the editor runs itself, by name: functions of
the server and the command's arguments (a vector).")

(defmacro define-lsp-client-command (name (server arguments) &body body)
  "Define how the editor runs NAME, a command a server leaves to its editor,
SERVER and ARGUMENTS bound."
  `(setf (gethash ,name *lsp-client-commands*)
         (lambda (,server ,arguments) (declare (ignorable ,server ,arguments)) ,@body)))

(defun lsp-capabilities ()
  "What Tuatara can do, as the protocol's initialize tells a server."
  (let ((o #'tuatara.text:json-object))
    (funcall o
             "general" (funcall o "positionEncodings" #("utf-8" "utf-16"))
             "workspace" (funcall o "configuration" t "workspaceDirectories" t "applyEdit" t
                                  "workspaceEdit" (funcall o "documentChanges" t)
                                  "inlayHint"
                                  (funcall o "refreshSupport" t)
                                  "semanticTokens"
                                  (funcall o "refreshSupport" t)
                                  "symbol" (funcall o) "diagnostics" (funcall o "refreshSupport" t)
                                  "codeLens" (funcall o "refreshSupport" t)
                                  "didChangeWatchedFiles"
                                  (funcall o "dynamicRegistration" t "relativePatternSupport" t))
             "window" (funcall o "workDoneProgress" t "showMessage" (funcall o))
             ;; the commands of a server's own that Tuatara runs itself (DEFINE-LSP-CLIENT-COMMAND)
             "experimental" (funcall o "commands"
                                     (funcall o "commands"
                                              (coerce (loop for name being the hash-keys of *lsp-client-commands*
                                                            collect name)
                                                      'vector)))
             "textDocument"
             (funcall o "synchronization" (funcall o "didSave" t "dynamicRegistration" :false)
                      "publishDiagnostics" (funcall o "relatedInformation" :false "versionSupport" t)
                      "completion" (funcall o "completionItem"
                                            (funcall o "snippetSupport" t
                                                     "documentationFormat" #("plaintext" "markdown")
                                                     "resolveSupport"
                                                     (funcall o "properties" #("documentation" "detail"))))
                      "hover" (funcall o "contentFormat" #("plaintext" "markdown"))
                      "signatureHelp"
                      (funcall o "signatureInformation" (funcall o "documentationFormat" #("plaintext" "markdown")))
                      "definition" (funcall o) "references" (funcall o) "implementation" (funcall o)
                      "typeDefinition" (funcall o)
                      "rename" (funcall o)
                      "formatting" (funcall o) "rangeFormatting" (funcall o)
                      "codeAction" (funcall o "codeActionLiteralSupport"
                                            (funcall o
                                                     "codeActionKind"
                                                     (funcall o "valueSet" #("" "quickfix" "refactor" "refactor.extract"
                                                                             "refactor.inline" "refactor.rewrite"
                                                                             "source" "source.organizeImports"))))
                      "inlayHint" (funcall o) "callHierarchy" (funcall o) "documentHighlight" (funcall o)
                      "diagnostic" (funcall o "relatedDocumentSupport" :false)
                      "codeLens" (funcall o) "foldingRange" (funcall o "lineFoldingOnly" t) "selectionRange" (funcall o)
                      "documentSymbol" (funcall o "hierarchicalDocumentSymbolSupport" t)
                      "semanticTokens" (funcall o "requests" (funcall o "full" t) "formats" #("relative")
                                                "tokenTypes" (lsp-token-kinds)
                                                "tokenModifiers" #()
                                                "overlappingTokenSupport" :false "multilineTokenSupport" :false)))))

(defun lsp-start (spec root language)
  "Start the server SPEC (a plist as LANGUAGE-SERVER-COMMANDS gives) for the
project at ROOT and LANGUAGE, and ask it to begin; the server, or NIL when its
program cannot be started (said so)."
  (let* ((command (getf spec :command))
         (program (installed-program command))
         (options (let ((text (getf spec :options)))
                    (and text (handler-case (tuatara.text:json-read text)
                                (tuatara.text:json-error (e)
                                  (message "~a's options are not JSON (~a): none given" (getf spec :name) e)
                                  nil)))))
         (server (make-lsp-server (getf spec :name) command root language options (editor-job-waker))))
    (unless program
      (message "~a is not installed: no language server for ~a" (first command) (abbreviated-path root))
      (return-from lsp-start nil))
    (setf (lsp-server-process server)
          (handler-case
              (reporting-errors ("starting ~a" (getf spec :name))
                (tuatara.io:start-program (cons program (rest command)) :directory root
                                          :input :stream :output :stream :error :stream :external-format :latin-1
                                          :environment (append (getf spec :environment)
                                                               (tuatara.io:program-environment))))
            (error (e) (message "~a could not be started: ~a" (getf spec :name) e) (return-from lsp-start nil))))
    (push server *lsp-servers*)
    (lsp-start-reading server)
    (lsp-start-writing server)
    (lsp-request server "initialize"
                 (tuatara.text:json-object
                  "processId" (sb-posix:getpid)
                  "clientInfo" (tuatara.text:json-object "name" "Tuatara")
                  "rootUri" (path-uri root) "rootPath" (tuatara.io:native root)
                  "workspaceDirectories"
                  (vector (tuatara.text:json-object "uri" (path-uri root) "name" (tuatara.io:native-file-name root)))
                  "capabilities" (lsp-capabilities)
                  "initializationOptions" (or options :omit))
                 (lambda (result error)
                   (if error
                       (progn (message "~a did not start: ~a" (lsp-server-name server) (lsp-error-words error))
                              (lsp-stop server))
                       (lsp-server-ready server result))))
    (message "Starting ~a for ~a" (lsp-server-name server) (abbreviated-path root))
    server))

(defun lsp-server-ready (server result)
  "SERVER answered its start with RESULT: keep what it can do, tell it all is
ready, and open the documents that waited for it."
  (let ((capabilities (tuatara.text:json-get result "capabilities")))
    (setf (lsp-server-capabilities server) capabilities
          (lsp-server-encoding server)
          (if (equal (tuatara.text:json-get capabilities "positionEncoding") "utf-8") :utf-8 :utf-16)
          (lsp-server-status server) :ready)
    (lsp-notify server "initialized" (tuatara.text:json-object))
    (dolist (doc (lsp-server-documents server))
      (lsp-open-document server doc))
    (message "~a is ready for ~a" (lsp-server-name server) (abbreviated-path (lsp-server-root server)))))

(defun lsp-stop (server)
  "Close SERVER's documents, ask it to end, and end it if it has not a second
later; what waited for its answers is told it stopped, and its project's
directories are no longer watched for it."
  (unless (eq (lsp-server-status server) :stopped)
    (dolist (doc (copy-list (lsp-server-documents server)))
      (lsp-forget-document server doc))
    (let ((waiting (loop for then being the hash-values of (lsp-server-waiting server) collect then)))
      (clrhash (lsp-server-waiting server))
      (when (eq (lsp-server-status server) :ready)
        (lsp-request server "shutdown" nil (lambda (result error) (declare (ignore result error))))
        (lsp-notify server "exit"))
      (setf (lsp-server-status server) :stopped
            *lsp-servers* (remove server *lsp-servers*))
      (directories-followed-changed)
      (sb-concurrency:send-message (lsp-server-outbox server) :end)
      (let ((process (lsp-server-process server)))
        (tuatara.runtime:make-guarded-thread
         (lambda ()
           (sleep 1)
           (when (sb-ext:process-alive-p process)
             (ignoring-errors-reported ("ending ~a" (lsp-server-name server)) (sb-ext:process-kill process 15)))
           (sleep 1)
           (when (sb-ext:process-alive-p process)
             (ignoring-errors-reported ("ending ~a" (lsp-server-name server)) (sb-ext:process-kill process 9)))
           (ignoring-errors-reported ("closing ~a" (lsp-server-name server)) (sb-ext:process-close process)))
         :name "tuatara server ending"))
      (break-promises server "the server stopped")
      (forget-file-diagnostics server)
      (dolist (then waiting)
        (funcall then nil (tuatara.text:json-object "message" "the server stopped"))))))

(defun lsp-server-ended (server why)
  "SERVER's program ended, or must (WHY, words): say so, with its last words, and stop it."
  (unless (eq (lsp-server-status server) :stopped)
    (message "~a stopped (~a)~@[: ~a~]" (lsp-server-name server) why (first (lsp-said server)))
    (lsp-stop server)))

(defun lsp-stop-all (&optional editor)
  "End every language server (as the editor stops)."
  (declare (ignore editor))
  (mapc #'lsp-stop *lsp-servers*))

;;; The server's own questions, and what it tells

(define-lsp-request "workspace/configuration" (server params)
  ;; each item asked: what the server was given at its start under that section, else null
  (let ((options (lsp-server-options server)))
    (map 'vector (lambda (item)
                   (let ((section (tuatara.text:json-text item "section")))
                     (or (and options section
                              (apply #'tuatara.text:json-get options (uiop:split-string section :separator ".")))
                         :null)))
         (tuatara.text:json-items params "items"))))

(define-lsp-request "workspace/workspaceDirectories" (server params)
  (let ((root (lsp-server-root server)))
    (vector (tuatara.text:json-object "uri" (path-uri root) "name" (tuatara.io:native-file-name root)))))

(define-lsp-request "window/workDoneProgress/create" (server params) :null)

(define-lsp-request "window/showMessageRequest" (server params)
  (message "~a: ~a" (lsp-server-name server) (tuatara.text:json-text params "message"))
  :null)

(define-lsp-notification "window/showMessage" (server params)
  ;; errors and warnings said; the rest kept in *Messages*
  (let ((text (format nil "~a: ~a" (lsp-server-name server) (tuatara.text:json-text params "message"))))
    (if (member (tuatara.text:json-integer params "type") '(1 2))
        (message "~a" text)
        (log-message text))))

(define-lsp-notification "window/logMessage" (server params)
  (tuatara.runtime:debug-report :file "~a: ~a" (lsp-server-name server) (tuatara.text:json-text params "message")))

(define-lsp-notification "$/progress" (server params)
  (let ((token (princ-to-string (tuatara.text:json-get params "token")))
        (value (tuatara.text:json-get params "value")))
    ;; each piece of work the server does (indexing, checking), as (TITLE WORDS PERCENT)
    (if (equal (tuatara.text:json-text value "kind") "end")
        (remhash token (lsp-server-progress server))
        (let* ((before (gethash token (lsp-server-progress server)))
               (title (or (tuatara.text:json-text value "title") (first before)))
               (percent (tuatara.text:json-get value "percentage")))
          (setf (gethash token (lsp-server-progress server))
                (list title
                      (format nil "~@[~a~]~@[: ~a~]" title (tuatara.text:json-text value "message"))
                      ;; the figure only rises
                      (if (realp percent) (max (round percent) (or (third before) 0)) (third before))))))))

(defun server-work (servers &key projects)
  "The work SERVERS are doing, each (WORDS PERCENT NAME), PERCENT NIL when the
server does not say, NAME the server's, with PROJECTS its project's after it."
  (loop for server in servers
        append (loop for (nil words percent) being the hash-values of (lsp-server-progress server)
                     collect (list words percent
                                   (if projects
                                       (format nil
                                               "~a ~a"
                                               (lsp-server-name server)
                                               (abbreviated-path (lsp-server-root server)))
                                       (lsp-server-name server))))))

(defun lsp-outside-work (&optional (buffer nil buffer-p))
  "The language servers' work, as *OUTSIDE-WORK-FUNCTIONS* has it: that of
the servers BUFFER is open in, or with none given that of every server, each
with its project."
  (if buffer-p
      (server-work (buffer-servers buffer))
      (server-work *lsp-servers* :projects t)))

;;; What went to and from the servers

(define-major-mode lsp-events-mode special-mode
  "The last messages to and from the language servers (\\[lsp-events]), the
newest last: when, which way (→ to the server, ← from it), and the message's
start; then the server's last lines on its error output. \\<lsp-events-mode>\\[revert-buffer] lists them
again; \\[quit-window] closes."
  :lighter "Events" :interactive nil)

(defun lsp-events-pieces (servers)
  "The text of the list of SERVERS' events, as RENDER-PIECES takes it."
  (if (null servers)
      (list (format nil "~%   No language server is running~%"))
      (loop for server in servers
            append (list* (cons (format nil "~a for ~a (~(~a~))~%" (lsp-server-name server)
                                        (abbreviated-path (lsp-server-root server)) (lsp-server-status server))
                                :header-line)
                          (append
                           (loop for (time to-it text) in (reverse (sb-thread:with-mutex ((lsp-server-lock server))
                                                                     (copy-list (lsp-server-events server))))
                                 collect (format nil "~a ~:[←~;→~] ~a~%"
                                                 (format-time "%T" time) to-it (tuatara.text:one-line text)))
                           (when (lsp-said server)
                             (cons (format nil "~%  Its error output, the last lines:~%")
                                   (mapcar (lambda (line) (format nil "  ~a~%" line)) (reverse (lsp-said server)))))
                           (list (string #\Newline)))))))

(define-command lsp-events ()
  "List the last messages to and from the language servers the current buffer
is open in (all those running when it is in none), the newest last, and what
each said on its error output: to see what a server was asked and answered
when it does not do what it should; the last 500 of each are kept."
  (let* ((servers (or (buffer-servers (current-buffer)) *lsp-servers*))
         (list (buffer-in-mode "*Language server events*" 'lsp-events-mode)))
    (render-pieces list (lambda () (lsp-events-pieces servers)) :point :start)
    (show-buffer list)
    (goto (tuatara.buffer:buffer-size list))))
