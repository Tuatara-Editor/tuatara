;;;; src/frontend/remote.lisp --- A frontend whose screen is in another process, and the client that paints it.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; A REMOTE-FRONTEND paints on the screen of a client at the other end of a
;;;; local socket: each call of the painter interface becomes a message
;;;; (src/frontend/messages.lisp), and each event the client reads comes back
;;;; as one. Painting never waits on the client: messages are put in a buffer
;;;; that a thread of their own sends, and a frame not yet sent when the next
;;;; is ready is replaced by it, so a client that does not read costs one
;;;; buffer and never a wait. The client (RUN-REMOTE-CLIENT) reads the
;;;; messages and calls its own frontend with them, on the thread that reads
;;;; that frontend's events, as the editor would.

(in-package #:tuatara.frontend)

(defparameter *hello-seconds* 5
  "The seconds each end of a new connection waits for the other's hello and,
in the editor, for what the client's screen is.")

(defparameter *starting-seconds* 60
  "The seconds a daemon may take to start, reading its init file and loading
its packages: how long tuatara --daemon waits for it to say it is ready, and a
client that connected meanwhile for its hello.")

(defparameter *missed-answer-seconds* 5
  "Seconds a client that gave no answer in time is not asked again
\(ASK-CLIENT), its answer taken to be none.")

(defparameter *answer-seconds* 1/5
  "The seconds the editor waits for a client to answer a question (the text
of its clipboard) before taking it as having none.")

(defparameter *window-seconds* 10
  "The seconds a client asked for another window has to open it and attach it,
before it is no longer waited for.")

(defconstant +most-client-windows+ 32
  "The most windows one client of an editor running elsewhere shows at once:
past it, another is neither asked for by the editor nor opened by the
client.")

(defconstant +most-windows-asked+ 4
  "The most windows asked of one client that may be still to come: past it,
another is not asked for.")

(defparameter *stall-seconds* 600
  "The seconds a client may take to take in what it was sent before it is let
go. What waits for a client that takes nothing (one stopped, as by C-z) is
bounded, its frames replaced by newer ones, so it is kept long: given its
terminal again, it is given the frame of now.")

(defconstant +most-unsent+ 2097152
  "The most octets of messages kept for a client beside its newest frame:
past it, a title or clipboard text for a client that does not read is left out.")

(define-condition remote-refused (error)
  ((words :initarg :words :reader remote-refused-words
          :documentation "Why the connection was refused, in words.")
   (other :initarg :other :initform nil :reader remote-refused-other
          :documentation "The version line of the other end when it is of another build, else NIL."))
  (:report (lambda (condition stream) (write-string (remote-refused-words condition) stream)))
  (:documentation "Signalled when a new connection between the editor and a client is refused: the other end
is of another build, said nothing it should, or refused it, saying why."))

(setf (documentation 'remote-refused-words 'function) "Why the connection of CONDITION was refused, in words."
      (documentation 'remote-refused-other 'function)
      "The version line of the other end of CONDITION's connection when it is of another build, else NIL.")

(defun send-outbox (fd box stall-seconds)
  "Send the messages of BOX over the socket FD, waiting while it is full, and
empty BOX; its descriptors are closed once passed or given up on. True when
all were sent; :STALLED when the other end had not taken them all within
STALL-SECONDS, however little at a time it took; NIL when the socket failed."
  (let ((octets (outbox-octets box)) (fill (outbox-fill box))
        (passes (and (outbox-descriptors box) (sort (copy-list (outbox-descriptors box)) #'< :key #'car)))
        (at 0) (began (get-internal-real-time)))
    (declare (type fixnum fill at))
    (unwind-protect
         (tuatara.io:with-octets-sap (sap octets)
           (loop while (< at fill)
                 do (multiple-value-bind (sent errno)
                        (let ((end (loop for (pass) in passes when (> pass at) return pass finally (return fill))))
                          (tuatara.io:send-with-descriptors fd (sb-sys:sap+ sap at) (- end at)
                                                            (and passes (= (car (first passes)) at)
                                                                 (list (cdr (first passes))))))
                      (cond ((plusp sent)
                             (when (and passes (= (car (first passes)) at)) (pop passes))
                             (incf at sent))
                            ((= errno sb-posix:eintr))
                            ((/= errno sb-posix:eagain) (return-from send-outbox nil))
                            ((< (tuatara.runtime:seconds-since began) stall-seconds) (tuatara.io:poll-fd fd 4 1000))
                            (t (return-from send-outbox :stalled)))))
           t)
      (close-descriptors (mapcar #'cdr (outbox-descriptors box)))
      (empty-outbox box)
      (setf (outbox-descriptors box) '()))))

(defun shown-words (text)
  "TEXT, from the other end of a connection, without its control characters
and the invisible characters that change the direction text is drawn in or
draw nothing (TUATARA.DISPLAY:HIDING-CODE-P), to be shown or put in a
log; TUATARA.DISPLAY:ESCAPE-CONTROLS writes them out instead."
  (remove-if (lambda (c)
               (let ((code (char-code c)))
                 (or (tuatara.text:control-code-p code) (tuatara.display:hiding-code-p code))))
             text))

(defun read-hello (inbox)
  "Read the other end's hello from INBOX, which is the first thing it says;
signals REMOTE-REFUSED when it says something else, nothing in time, is of
another build, or refuses the connection, saying why."
  (multiple-value-bind (line other)
      (handler-case
          (case (read-message inbox)
            (:hello (take-hello inbox))
            (:refused (let ((words (take-text inbox 4000)))
                        (message-read inbox)
                        (error 'remote-refused :words (shown-words words))))
            (t (error 'remote-refused :words "The other end said no hello.")))
        (malformed-message (condition)
          (error 'remote-refused :words (format nil "The other end's hello: ~a" (malformed-message-reason condition)))))
    (unless line
      (error 'remote-refused :words (format nil "The other end runs ~a; this is ~a, with messages of format ~d."
                                            (shown-words other) (tuatara:version-line) +protocol+)
                             :other (shown-words other)))
    line))

(defun put-exit (box code words)
  "Add to BOX the message that tells a client to exit with CODE, from 0 to 255,
saying WORDS."
  (let ((at (begin-message box :exit)))
    (put-number box code 1)
    (put-text box words 1024)
    (end-message box at)))

(defun refuse-connection (fd words)
  "Tell the client at the other end of the new connection FD, which has said
nothing yet, that it is refused, and why, WORDS; then close FD."
  (let ((box (make-outbox)))
    (let ((at (begin-message box :refused)))
      (put-text box words 4000)
      (end-message box at))
    (tuatara.runtime:ignoring-errors-reported ("refusing a client")
      (send-outbox fd box 1))
    (shut-and-close fd)))

(defun shut-and-close (fd)
  "Shut the socket FD both ways and close it."
  (tuatara.runtime:ignoring-errors-reported ("closing a client's socket")
    (tuatara.io:shut-socket fd)
    (sb-posix:close fd)))

(defun text-pieces (text)
  "TEXT cut into pieces of at most +TEXT-PIECE+ octets as UTF-8, at
characters, in order; one piece, empty, for no text."
  (let ((pieces '()) (start 0) (octets 0))
    (dotimes (i (length text))
      (let ((n (tuatara.text:char-octet-length (char text i))))
        (when (> (+ octets n) +text-piece+)
          (push (subseq text start i) pieces)
          (setf start i octets 0))
        (incf octets n)))
    (push (subseq text start) pieces)
    (nreverse pieces)))

(defun gather-text (gathered piece more)
  "Add PIECE, the next piece of a text sent in pieces (TEXT-PIECES), to
GATHERED, (OCTETS . PIECES) of those come so far, newest first, or NIL; at
most +MOST-TEXT+ octets are kept in all. Two values: while MORE, the new
GATHERED and NIL; once all came, NIL and the whole text, or NIL and how many
octets it had when that was more than +MOST-TEXT+, nothing of it kept."
  (let* ((octets (+ (if gathered (car gathered) 0) (tuatara.text:string-octet-length piece)))
         (pieces (and (<= octets +most-text+) (cons piece (cdr gathered)))))
    (cond (more (values (cons octets pieces) nil))
          ((<= octets +most-text+) (values nil (apply #'concatenate 'string (reverse pieces))))
          (t (values nil octets)))))

(defconstant +most-waiting-events+ 1024
  "The most events of one client waiting for the editor to take them: past
it, what the client sends is not read until the editor has taken some.")

;;; The editor's side: a frontend whose screen is a client's

(defclass remote-frontend ()
  ((fd :initarg :fd :reader remote-fd
       :documentation "The socket to the client, a file descriptor.")
   (facts :initform (make-facts) :accessor remote-facts
          :documentation "What the client said its screen is, replaced whole when it says it again.")
   (queue :initarg :queue :initform (make-event-queue) :reader remote-queue
          :documentation "Where the client's events go for the editor, the queue the editor's frames share.")
   (frame :initform nil :accessor remote-frame
          :documentation "The editor's frame this frontend paints, named in its events.")
   (lock :initform (sb-thread:make-mutex :name "remote frontend") :reader remote-lock
         :documentation "Held while the messages not yet sent, the answer awaited or the connection's end change.")
   (moved :initform (sb-thread:make-waitqueue :name "remote frontend") :reader remote-moved
          :documentation "Signalled when there are messages to send, an answer came, or the connection ends.")
   (unsent :initform (make-outbox 65536) :accessor remote-unsent
           :documentation "The messages for the client not yet taken to be sent.")
   (sending :initform (make-outbox 65536) :accessor remote-sending
            :documentation "The messages being sent, by the sending thread alone.")
   (frame-start :initform -1 :accessor remote-frame-start
                :documentation "Where the frame in UNSENT begins, -1 when it holds none.")
   (frame-end :initform -1 :accessor remote-frame-end
              :documentation "Where the frame in UNSENT ends.")
   (painting :initform (make-outbox) :reader remote-painting
             :documentation "The calls of the frame being painted, in order.")
   (kept-calls :initform (make-array 64 :element-type 'fixnum :adjustable t :fill-pointer 0) :reader remote-kept-calls
               :documentation "Where PAINTING has each row painted as the frame before painted it.")
   (kept :initform (make-array +most-rows+ :initial-element nil) :reader remote-kept
         :documentation "Each row's painting as last sent, as octets, by row.")
   (kept-lengths :initform (make-array +most-rows+ :element-type 'fixnum :initial-element 0)
                 :reader remote-kept-lengths
                 :documentation "How many octets of each row's KEPT are its painting.")
   (names :initform (make-face-names) :reader remote-names
          :documentation "The numbers face names are sent by.")
   (faces-generation :initform -1 :accessor remote-faces-generation
                     :documentation "The faces' generation (TUATARA.DISPLAY:*FACE-GENERATION*) last sent.")
   (faces-sent :initform (make-hash-table :test 'eq) :reader remote-faces-sent
               :documentation "Each face as last sent, as octets, by name.")
   (scratch :initform (make-outbox) :reader remote-scratch
            :documentation "Where a face is written to be compared with how it was sent.")
   (pictures :initform '() :accessor remote-pictures
             :documentation "The numbers of the pictures the client keeps, newest first.")
   (frame-pictures :initform '() :accessor remote-frame-pictures
                   :documentation "The numbers of the pictures put in the frame being painted.")
   (unsent-pixels :initform 0 :accessor remote-unsent-pixels
                  :documentation "The octets of the pictures among the messages not yet taken to be sent.")
   (far-pictures :initform '() :accessor remote-far-pictures
                 :documentation "The pictures of a client on another machine: each a FAR-PICTURE, newest first.")
   (frame-number :initform 0 :accessor remote-frame-number
                 :documentation "How many frames have been begun, the frame being painted the last.")
   (far-sender :initform nil :accessor remote-far-sender
               :documentation "The thread sending pictures to a client on another machine, once there is one.")
   (requests :initform 0 :accessor remote-requests
             :documentation "How many questions have been put to the client.")
   (asked :initform nil :accessor remote-asked
          :documentation "The number of the question awaiting its answer, or NIL.")
   (asked-question :initform nil :accessor remote-asked-question
                   :documentation "What the question awaiting its answer asks (*QUESTIONS*).")
   (answer :initform :none :accessor remote-answer
           :documentation "The answer that came, or :NONE.")
   (missed :initform nil :accessor remote-missed
           :documentation "When the client last gave no answer in time, an internal real time, or NIL.")
   (pieces :initform '() :accessor remote-pieces
           :documentation "The pieces of a text answer come so far, as GATHER-TEXT keeps them.")
   (pasted :initform '() :accessor remote-pasted
           :documentation "The pieces of a paste come so far, as GATHER-TEXT keeps them, by the reading thread alone.")
   (stopping :accessor remote-stopping
             :documentation "A function of no arguments, true once the frontend is being stopped.")
   (cut-said :initform nil :accessor remote-cut-said
             :documentation "True once the client has been told a frame was too large to send whole.")
   (received :initform 0 :accessor remote-received
             :documentation "How many events the client has sent.")
   (ended :initform nil :accessor remote-ended
          :documentation "True once the connection has ended.")
   (closing :initform nil :accessor remote-closing
            :documentation "True once the frontend is being stopped: what is unsent is sent, then nothing more.")
   (closed :initform nil :accessor remote-closed
           :documentation "True once the socket is closed.")
   (exit-code :initform 0 :accessor remote-exit-code
              :documentation "The code the client exits with when this frontend stops.")
   (exit-words :initform nil :accessor remote-exit-words
               :documentation "Words the client says as it exits when this frontend stops, or NIL.")
   (request :initform (make-request) :accessor remote-request
            :documentation "What the client asked for as it attached (REQUEST).")
   (pid :initform nil :accessor remote-pid
        :documentation "The process the client connected from, as the system said, or NIL.")
   (settings-sent :initform nil :accessor remote-settings-sent
                  :documentation "The values of *DISPLAY-SETTINGS* as last told the client, in their order.")
   (inbox :initform nil :accessor remote-inbox
          :documentation "What the client sends, as it is read.")
   (reader :initform nil :accessor remote-reader
           :documentation "The thread reading what the client sends.")
   (writer :initform nil :accessor remote-writer
           :documentation "The thread sending the client its messages."))
  (:documentation "A frontend painting on the screen of a client at the other end of a local socket, and
reading the client's events from it (MAKE-REMOTE-FRONTEND)."))

(setf (documentation 'remote-exit-code 'function) "The code the client of the remote frontend F
exits with when F stops."
      (documentation 'remote-exit-words 'function) "What the client of the remote frontend F says as it exits, or NIL."
      (documentation 'remote-request 'function) "What the client of the remote frontend F asked for
as it attached (REQUEST)."
      (documentation 'remote-pid 'function) "The process the client of the remote frontend F connected from, or NIL.")

(defmethod initialize-instance :after ((f remote-frontend) &key)
  "Give the remote frontend F the test its threads stop by: once it is closing or
closed."
  (setf (remote-stopping f) (lambda () (or (remote-closing f) (remote-closed f)))))

(defun settings-now ()
  "The values of *DISPLAY-SETTINGS* here, in their order, :UNSET for one
unset."
  (loop for (name) in *display-settings* collect (if (boundp name) (symbol-value name) :unset)))

(defun make-remote-frontend (fd &key queue admit)
  "A frontend whose screen is the client's at the other end of the local
socket FD, once both have said hello, the client been told how its screen is
to be shown (*DISPLAY-SETTINGS*), and it has said what its screen is and
what it asks for (REMOTE-REQUEST). Its events go into QUEUE, the queue of
the editor's other frames, or a queue of its own. ADMIT, a function of the
request, may give words why this client is not taken. It is told them as it
ends, before anything it sends is read. Signals REMOTE-REFUSED when the
client is refused so, is of another build or says nothing it should. FD is
then the caller's to close.
A client may take up to *HELLO-SECONDS* to say hello, so this is called on the
thread taking connections, never the editor's. That thread bounds how many are
being greeted at once. The one descriptor a client may pass, its standard
input with its attach, is the frontend's request's to close once taken."
  (let ((remote (make-instance 'remote-frontend :fd fd :queue (or queue (make-event-queue))))
        (inbox (make-inbox fd :editor :deadline (tuatara.runtime:deadline-in *hello-seconds*)))
        (box (make-outbox))
        (greeted nil))
    (setf (inbox-most-descriptors inbox) 1)
    (unwind-protect
         (progn
           (put-hello box)
           (let ((at (begin-message box :settings)))
             (put-settings box)
             (end-message box at))
           (setf (remote-settings-sent remote) (settings-now))
           (unless (eq t (send-outbox fd box *hello-seconds*))
             (error 'remote-refused :words "The client took no hello."))
           (read-hello inbox)
           (handler-case
               (progn (unless (eq (read-message inbox) :attach)
                        (error 'remote-refused :words "The client did not say what its screen is."))
                      (setf (remote-facts remote) (take-facts inbox :whole t))
                      (let ((request (take-request inbox)))
                        (message-read inbox)
                        ;; its standard input passed with it when it asks for it to be read, and else nothing
                        (unless (= (length (inbox-descriptors inbox)) (if (request-standard-input request) 1 0))
                          (malformed "~d descriptors passed with an attach" (length (inbox-descriptors inbox))))
                        (setf (request-standard-input request) (pop (inbox-descriptors inbox))
                              (remote-request remote) request)
                        (let ((why (and admit (funcall admit request))))
                          (when why
                            (let ((box (make-outbox)))
                              (put-exit box 1 why)
                              (send-outbox fd box 1))
                            (error 'remote-refused :words why)))))
             (malformed-message (condition)
               (error 'remote-refused :words (format nil
                                                     "The client's attach: ~a"
                                                     (malformed-message-reason condition)))))
           (setf greeted t))
      (setf (inbox-most-descriptors inbox) nil)
      (close-descriptors (shiftf (inbox-descriptors inbox) '()))
      (unless greeted
        (let ((input (request-standard-input (remote-request remote))))
          (when (integerp input) (close-descriptors (list input))))))
    (setf (inbox-deadline inbox) nil
          (remote-inbox remote) inbox
          (remote-reader remote) (tuatara.runtime:make-guarded-thread (lambda () (read-from-client remote))
                                                                      :name "tuatara remote reading" :niceness 0)
          (remote-writer remote) (tuatara.runtime:make-guarded-thread (lambda () (send-to-client remote))
                                                                      :name "tuatara remote sending" :niceness 0))
    remote))

(defun end-connection (remote reason)
  "End REMOTE's connection, for REASON (words, or NIL when the client went or
was let go): nothing more is sent or read, and the editor is told that the
frame's screen went away (:DETACHED), which closes that frame alone."
  (let ((ended (sb-thread:with-mutex ((remote-lock remote))
                 (shiftf (remote-ended remote) t))))
    (unless ended
      (when reason
        (tuatara.runtime:debug-report :remote "a client's connection ended: ~a" reason))
      (sb-thread:with-mutex ((remote-lock remote))
        (unless (remote-closed remote) (tuatara.io:shut-socket (remote-fd remote)))
        (sb-thread:condition-broadcast (remote-moved remote)))
      (unless (remote-closing remote)
        (queue-push (remote-queue remote) (make-event :detached :frame (remote-frame remote)))))))

(defun send-to-client (remote)
  "Send REMOTE's messages as they come, until it is stopped and has sent them
all, or its connection ends; a client that takes nothing for
*STALL-SECONDS* is let go, and so is the connection when sending fails."
  (let ((reason "sending to the client failed"))
    (unwind-protect
         (loop
           (let ((box (sb-thread:with-mutex ((remote-lock remote))
                        (loop until (or (plusp (outbox-fill (remote-unsent remote)))
                                        (remote-closing remote) (remote-ended remote))
                              do (sb-thread:condition-wait (remote-moved remote) (remote-lock remote)))
                        (when (and (plusp (outbox-fill (remote-unsent remote))) (not (remote-ended remote)))
                          (rotatef (remote-unsent remote) (remote-sending remote))
                          (setf (remote-frame-start remote) -1 (remote-frame-end remote) -1
                                (remote-unsent-pixels remote) 0)
                          (remote-sending remote)))))
             (unless box
               (setf reason nil)
               (return))
             (case (send-outbox (remote-fd remote) box *stall-seconds*)
               ((nil) (return))
               (:stalled
                (setf reason (format nil "the client took in nothing for ~d minutes" (round *stall-seconds* 60)))
                (queue-push (remote-queue remote)
                            (make-event :said :frame (remote-frame remote)
                                              :text (format nil
                                                            "A client that took in nothing for ~d minutes was let go"
                                                            (round *stall-seconds* 60))))
                (return)))))
      (when reason (end-connection remote reason)))))

(defun read-from-client (remote)
  "Read what REMOTE's client sends and act on it, until the client goes or
sends what it should not, which ends the connection."
  (let ((inbox (remote-inbox remote)) (reason "reading what the client sent failed"))
    (unwind-protect
         (handler-case
             (progn (loop for kind = (read-message inbox)
                          while kind
                          do (unless (take-from-client remote kind inbox) (return)))
                    (setf reason nil))
           (malformed-message (condition) (setf reason (malformed-message-reason condition))))
      (close-descriptors (shiftf (inbox-descriptors inbox) '()))
      (end-connection remote reason))))

(defun take-from-client (remote kind inbox)
  "Act on the message of KIND REMOTE's client sent, being read from INBOX:
an event goes to the editor, an answer to the question awaiting it, what
the screen is now to REMOTE-FACTS. NIL when the client is going."
  (ecase kind
    (:event
     (multiple-value-bind (event more) (take-event inbox (remote-frame remote))
       (when (stringp event)
         ;; a paste comes in pieces, an event once whole; one past +MOST-TEXT+ is left out, and said
         (multiple-value-bind (gathered whole) (gather-text (remote-pasted remote) event more)
           (setf (remote-pasted remote) gathered
                 event (cond (more nil)
                             ((stringp whole) (make-event :paste :text whole :frame (remote-frame remote)))
                             (t (make-event :said :frame (remote-frame remote)
                                                  :text (format nil "Paste of ~,1f MB left out: more than ~d MB"
                                                                (/ whole 1048576) (floor +most-text+ 1048576))))))))
       (when event
         (case (event-kind event)
           (:resize (let ((facts (copy-facts (remote-facts remote))))
                      (setf (facts-width facts) (event-width event) (facts-height facts) (event-height event)
                            (remote-facts remote) facts)))
           ;; the client's window closed: that client's frame goes, never the editor
           (:close (setf event (make-event :detached :frame (remote-frame remote)))))
         (incf (remote-received remote))
         (push-event remote event))
       t))
    (:facts
     (let ((facts (take-facts inbox)) (before (remote-facts remote)))
       (message-read inbox)
       (setf (facts-health facts) (facts-health before) (facts-fonts facts) (facts-fonts before)
             (facts-font-faces facts) (facts-font-faces before)
             (remote-facts remote) facts)
       ;; the desktop's style, a cell's size: the editor looks again
       (push-event remote (make-event :tick :frame (remote-frame remote)))
       t))
    (:answer
     (multiple-value-bind (request answer more) (take-answer inbox)
       (sb-thread:with-mutex ((remote-lock remote))
         (when (eql request (remote-asked remote))
           ;; the answer of the kind the question asks for, or none; a text's pieces all text
           (unless (or (and (null answer) (not more) (null (remote-pieces remote)))
                       (if (member (remote-asked-question remote) '(:font-steps :lend))
                           (integerp answer)
                           (stringp answer)))
             (malformed "an answer of the wrong kind to a question of ~(~a~)" (remote-asked-question remote)))
           (if (stringp answer)
               (multiple-value-bind (gathered whole) (gather-text (remote-pieces remote) answer more)
                 (setf (remote-pieces remote) gathered)
                 (unless more (setf (remote-answer remote) (and (stringp whole) whole))))
               (setf (remote-answer remote) answer))
           (sb-thread:condition-broadcast (remote-moved remote)))))
     t)
    (:no-window (let ((word (take-text inbox 64)) (words (take-text inbox 1024)))
                  (message-read inbox)
                  (window-declined remote word words))
     t)
    (:picture-taken
     (let ((id (take-number inbox 4)) (taken (take-number inbox 4)))
       (message-read inbox)
       (picture-taken remote id taken))
     t)
    (:detach (message-read inbox) nil)
    ((:hello :attach) (malformed "a ~(~a~) message after the first" kind))))

(defun push-event (remote event)
  "Hand EVENT from REMOTE's client to the editor, first waiting while
+MOST-WAITING-EVENTS+ of the client's wait to be taken, as long as the
frontend is not being stopped."
  (queue-push (remote-queue remote) event :most +most-waiting-events+ :stop (remote-stopping remote)))

(defun queue-message (remote kind write &key (needed t))
  "Put a message of KIND for REMOTE's client among those to send, its fields
written by WRITE, a function of the box; a message not NEEDED is left out
for a client with more than +MOST-UNSENT+ octets not yet sent. True when it
was put."
  (sb-thread:with-mutex ((remote-lock remote))
    (let ((box (remote-unsent remote)))
      (unless (or (remote-ended remote) (remote-closing remote)
                  (and (not needed) (> (outbox-fill box) +most-unsent+)))
        (let ((at (begin-message box kind)))
          (funcall write box)
          (end-message box at))
        (sb-thread:condition-broadcast (remote-moved remote))
        t))))

(defun ask-client (remote question &optional argument (seconds *answer-seconds*))
  "Ask REMOTE's client QUESTION (*QUESTIONS*), with ARGUMENT, a font's name,
font steps or words to show, and wait at most SECONDS (*ANSWER-SECONDS*) for
its answer; NIL when it gives none. An answer read only after the client's
events waiting to be handled (the most, +MOST-WAITING-EVENTS+, while the
editor is busy) comes too late, and is NIL too. A client that gave no answer
in time is not asked again for *MISSED-ANSWER-SECONDS*, the answer NIL at
once, so that one that answers nothing holds the editor up once."
  (sb-thread:with-mutex ((remote-lock remote))
    (unless (or (remote-ended remote) (remote-closing remote)
                (let ((missed (remote-missed remote)))
                  (and missed (< (tuatara.runtime:seconds-since missed) *missed-answer-seconds*))))
      (let ((box (remote-unsent remote)) (request (incf (remote-requests remote))))
        (setf (remote-asked remote) request (remote-asked-question remote) question
              (remote-answer remote) :none (remote-pieces remote) '())
        (let ((at (begin-message box :ask)))
          (put-number box request 4)
          (put-number box (position question *questions*) 1)
          (case question
            ((:font :lend) (put-text box argument 1024))
            (:font-steps (put-number box (max -64 (min 64 argument)) 2)))
          (end-message box at))
        (sb-thread:condition-broadcast (remote-moved remote))
        (loop with deadline = (tuatara.runtime:deadline-in seconds)
              until (or (not (eq (remote-answer remote) :none)) (remote-ended remote)
                        (tuatara.runtime:deadline-passed-p deadline))
              do (tuatara.runtime:wait-on (remote-moved remote) (remote-lock remote)
                                          (max 0.001 (tuatara.runtime:seconds-left deadline))))
        (setf (remote-asked remote) nil)
        (let ((answer (shiftf (remote-answer remote) :none)))
          (setf (remote-missed remote) (and (eq answer :none) (get-internal-real-time)))
          (if (eq answer :none) nil answer))))))

(defun send-faces (remote)
  "Put among REMOTE's messages the faces changed since they were last sent,
after the names they use; a face whose name has no number (past the
+MOST-NAMES+ first) is not sent, as no row can name it."
  (let ((names (remote-names remote)) (scratch (remote-scratch remote)) (changed '()))
    (setf (remote-faces-generation remote) tuatara.display:*face-generation*)
    (maphash (lambda (name face)
               (when (plusp (face-name-number names name))
                 (setf (outbox-fill scratch) 0)
                 (put-face scratch names face)
                 (let ((sent (gethash name (remote-faces-sent remote))))
                   (unless (and sent (= (length sent) (outbox-fill scratch))
                                (not (mismatch sent (outbox-octets scratch) :end2 (outbox-fill scratch))))
                     (setf (gethash name (remote-faces-sent remote))
                           (subseq (outbox-octets scratch) 0 (outbox-fill scratch)))
                     (push name changed)))))
             tuatara.display:*faces*)
    (when changed
      (sb-thread:with-mutex ((remote-lock remote))
        (let ((box (remote-unsent remote)))
          (put-names box names)
          (let ((at (begin-message box :faces)))
            (put-number box (length changed) 2)
            (dolist (name changed)
              (put-octets box (gethash name (remote-faces-sent remote))))
            (end-message box at)))))))

(defmethod frontend-start ((f remote-frontend))
  "Nothing: the remote frontend F is going once its client has said hello."
  nil)

(defmethod frontend-stop ((f remote-frontend))
  "Stop the remote frontend F, unless it is stopped already (FRONTEND-STOP-ONCE)."
  (unless (remote-closed f)
    (frontend-stop-once f)))

(defun frontend-stop-once (f)
  "Stop the remote frontend F, not stopped before: what is unsent is sent
while its client takes it, the client told to exit, and its socket closed."
  (sb-thread:with-mutex ((remote-lock f))
    (unless (remote-ended f)
      (put-exit (remote-unsent f) (max 0 (min 255 (remote-exit-code f))) (or (remote-exit-words f) "")))
    (setf (remote-closing f) t)
    (sb-thread:condition-broadcast (remote-moved f)))
  ;; what is left is sent while the client takes it; then both threads end with the socket
  (tuatara.runtime:join-within (remote-writer f) 2)
  (tuatara.io:shut-socket (remote-fd f))
  (tuatara.runtime:join-within (remote-writer f) 2)
  (tuatara.runtime:join-within (remote-reader f) 2)
  (when (remote-far-sender f) (tuatara.runtime:join-within (remote-far-sender f) 2))
  (sb-thread:with-mutex ((remote-lock f))
    (unless (shiftf (remote-closed f) t)
      (tuatara.runtime:ignoring-errors-reported ("closing a client's socket") (sb-posix:close (remote-fd f))))
    (setf (remote-ended f) t)
    (dolist (box (list (remote-unsent f) (remote-sending f)))
      (close-descriptors (mapcar #'cdr (shiftf (outbox-descriptors box) '())))))
  nil)

(defmethod frontend-frame ((f remote-frontend))
  "The editor's frame the remote frontend F paints."
  (remote-frame f))

(defmethod (setf frontend-frame) (frame (f remote-frontend))
  "Have the remote frontend F paint FRAME, named in its events."
  (setf (remote-frame f) frame))

(defmethod frontend-size ((f remote-frontend))
  "The columns and rows of the client's screen, as two values."
  (let ((facts (remote-facts f)))
    (values (facts-width facts) (facts-height facts))))

(defmethod frontend-read-event ((f remote-frontend))
  "The next event of F's queue, waiting for one."
  (queue-pop (remote-queue f) nil))

(defmethod frontend-read-event-timeout ((f remote-frontend) seconds)
  "The next event of F's queue, or a :TICK when none comes in SECONDS."
  (or (queue-pop (remote-queue f) seconds) (make-event :tick)))

(defmethod frontend-wake ((f remote-frontend))
  "Wake the editor reading F's queue, a :TICK put in it."
  (queue-push (remote-queue f) (make-event :tick)))

(defmethod frontend-input-pending-p ((f remote-frontend))
  "True when F's queue holds an event of input."
  (queue-input-p (remote-queue f)))

(defmethod frontend-colour-count ((f remote-frontend))
  "How many colours the client's screen shows."
  (facts-colours (remote-facts f)))

(defmethod frontend-description ((f remote-frontend))
  "The words the client gave for its screen."
  (facts-description (remote-facts f)))

(defmethod frontend-health ((f remote-frontend))
  "What the client said of how its screen fares."
  (facts-health (remote-facts f)))

(defmethod frontend-images-p ((f remote-frontend))
  "True when the client's screen shows pictures."
  (facts-images (remote-facts f)))

(defmethod frontend-reorders-text-p ((f remote-frontend))
  "True when the client's screen puts right-to-left text in order itself."
  (facts-reorders (remote-facts f)))

(defmethod frontend-cell-pixels ((f remote-frontend))
  "The width and height in pixels of a cell of the client's screen, as two
values, or NIL."
  (let ((facts (remote-facts f)))
    (and (facts-cell-width facts) (values (facts-cell-width facts) (facts-cell-height facts)))))

(defmethod frontend-desktop-style ((f remote-frontend))
  "The desktop's light or dark style, as the client last said it."
  (facts-style (remote-facts f)))

(defmethod frontend-font-steps ((f remote-frontend))
  "How many steps larger or smaller than its own the client's window draws its
font."
  (facts-font-steps (remote-facts f)))

(defmethod (setf frontend-font-steps) (steps (f remote-frontend))
  "Ask the client's window to draw its font STEPS larger or smaller; the steps it
took."
  (if (facts-window (remote-facts f))
      (or (ask-client f :font-steps steps) (facts-font-steps (remote-facts f)))
      steps))

(defmethod frontend-fonts ((f remote-frontend))
  "The fonts the client's window can draw."
  (facts-fonts (remote-facts f)))

(defmethod frontend-font-faces ((f remote-frontend))
  "The faces of the fonts the client's window can draw."
  (facts-font-faces (remote-facts f)))

(defmethod frontend-set-font ((f remote-frontend) name)
  "Ask the client's window to draw in the font NAME; true when it did."
  (and (facts-window (remote-facts f)) (stringp name) (<= (tuatara.text:string-octet-length name) 1024)
       (ask-client f :font name)))

(defmethod frontend-clipboard ((f remote-frontend))
  "The text of the client's clipboard (ASK-CLIENT), or NIL."
  (ask-client f :clipboard))

(defmethod frontend-primary-selection ((f remote-frontend))
  "The text of the client's primary selection (ASK-CLIENT), or NIL."
  (ask-client f :primary))

(defun send-selection (remote which text)
  "Have REMOTE's client make TEXT its clipboard (WHICH 0) or its primary
selection (1); NIL when TEXT, with what goes with it, is longer than the
client takes (+MOST-TEXT+), or left out."
  (and (<= (tuatara.text:string-octet-length text) (- +most-text+ 16))
       (queue-message remote :selection (lambda (box) (put-number box which 1) (put-text box text)) :needed nil)))

(defmethod frontend-set-clipboard ((f remote-frontend) text)
  "Have the client make TEXT its clipboard (SEND-SELECTION)."
  (send-selection f 0 text))

(defmethod frontend-set-primary-selection ((f remote-frontend) text)
  "Have the client make TEXT its primary selection (SEND-SELECTION)."
  (send-selection f 1 text))

(defmethod frontend-set-title ((f remote-frontend) title)
  "Have the client show TITLE as its window's title, unless it is too long to
send."
  (when (<= (tuatara.text:string-octet-length title) 60000)
    (queue-message f :title (lambda (box) (put-text box title)) :needed nil)))

(defmethod frontend-toggle-state ((f remote-frontend) state)
  "Have the client's window go full screen or maximised (STATE), or back."
  (and (facts-window (remote-facts f)) (member state '(:fullscreen :maximised))
       (queue-message f :toggle (lambda (box) (put-number box (if (eq state :fullscreen) 0 1) 1)) :needed nil)))

(defmethod frontend-raise ((f remote-frontend))
  "Have the client bring its window to the front."
  (queue-message f :raise (lambda (box) (declare (ignore box))) :needed nil))

(defmethod frontend-scales-rows-p ((f remote-frontend))
  "True when the client draws rows large, as a window of its own does."
  (facts-window (remote-facts f)))

(defmethod frontend-settings-changed ((f remote-frontend))
  "Tell the client the display settings, when one has another value than it was
told."
  (unless (loop for (name) in *display-settings*
                for sent in (remote-settings-sent f)
                always (equal sent (if (boundp name) (symbol-value name) :unset)))
    (setf (remote-settings-sent f) (settings-now))
    (queue-message f :settings #'put-settings)))

;;; Another window for a client, which it attaches as a connection of its own

(defvar *windows-asked-for* '()
  "The windows asked of clients and not yet attached, each (WORD ASKER WHAT
DEADLINE): the word the client was given for it, the remote frontend asked,
what the asking kept with it (FRONTEND-ASK-ANOTHER), and the internal real
time past which it is no longer waited for.")

(defvar *windows-asked-lock* (sb-thread:make-mutex :name "windows asked for")
  "Held while *WINDOWS-ASKED-FOR* changes.")

(defmethod frontend-ask-another ((f remote-frontend) what windows)
  "Ask the client, shown in a window, to open another, a connection of its own
attached with the word it is given, WHAT kept for it; WINDOWS is how many it
shows now. Held to its most windows and to a few asked for at once: the words
why not, or NIL when it is asked."
  (when (and (facts-window (remote-facts f)) (not (remote-ended f)) (not (remote-closing f)))
    (let ((word (tuatara.io:random-name)))
      (or (sb-thread:with-mutex (*windows-asked-lock*)
            (let ((asked (count f *windows-asked-for* :key #'second)))
              (cond ((>= (+ windows asked) +most-client-windows+)
                     (format nil "the client shows ~d windows, the most it opens" +most-client-windows+))
                    ((>= asked +most-windows-asked+)
                     (format nil "~d windows asked for are still to come" asked))
                    (t (push (list word f what (tuatara.runtime:deadline-in *window-seconds*)) *windows-asked-for*)
                       nil))))
          (queue-message f :another (lambda (box) (put-text box word 64)))))))

(defun take-window-asked-for (word pid)
  "The remote frontend asked for the window WORD names (FRONTEND-ASK-ANOTHER)
and what was kept with it, two values, the request taken away, when the
connection of the process PID brings it: the process the asking connection
is of (REMOTE-PID), within *WINDOW-SECONDS*. NIL for any other word, process
or time."
  (sb-thread:with-mutex (*windows-asked-lock*)
    (let ((entry (find word *windows-asked-for* :key #'first :test #'equal)))
      (when (and entry (eql pid (remote-pid (second entry)))
                 (not (tuatara.runtime:deadline-passed-p (fourth entry))))
        (setf *windows-asked-for* (remove entry *windows-asked-for*))
        (values (second entry) (third entry))))))

(defun windows-not-come ()
  "Take away the windows asked for that did not come within *WINDOW-SECONDS*,
and return what was kept with each, as a list; as a second value, the seconds
until the next of those waited for is due, or NIL when none is."
  (sb-thread:with-mutex (*windows-asked-lock*)
    (let ((late (remove-if-not (lambda (entry) (tuatara.runtime:deadline-passed-p (fourth entry)))
                               *windows-asked-for*)))
      (setf *windows-asked-for* (set-difference *windows-asked-for* late))
      (values (mapcar #'third late)
              (and *windows-asked-for*
                   (reduce #'min *windows-asked-for*
                           :key (lambda (entry) (max 0.01 (tuatara.runtime:seconds-left (fourth entry))))))))))

(defun window-declined (remote word words)
  "Take away the window asked of REMOTE's client by WORD, which the client
said it does not open, why being WORDS; said in its frame. Nothing for a
word it was not asked."
  (when (sb-thread:with-mutex (*windows-asked-lock*)
          (let ((entry (find-if (lambda (entry) (and (equal word (first entry)) (eq remote (second entry))))
                                *windows-asked-for*)))
            (when entry
              (setf *windows-asked-for* (remove entry *windows-asked-for*))
              t)))
    (queue-push (remote-queue remote)
                (make-event :said :frame (remote-frame remote)
                                  :text (format nil "No other window was opened: ~a" (shown-words words))))))

(defmethod frontend-terminal-device ((f remote-frontend))
  "The terminal the client runs in, or NIL for one in a window of its own."
  (and (not (facts-window (remote-facts f))) (request-terminal (remote-request f))))

(defparameter *lend-seconds* 2
  "The seconds the editor waits for a client to give its terminal back to be
lent to a program before the program runs without it.")

(defmethod frontend-lend-terminal ((f remote-frontend) function &key words)
  "Call FUNCTION with the client's terminal given back to be lent to a program,
WORDS shown there; the client takes it again afterwards whatever it answered."
  (if (frontend-terminal-device f)
      (let ((lent (eql 1 (ask-client f :lend (shown-words (or words "")) *lend-seconds*))))
        (unwind-protect (funcall function)
          (when lent (tuatara.runtime:debug-report :remote "a client's terminal lent and taken again"))
          (queue-message f :take-back (lambda (box) (declare (ignore box))))))
      (funcall function)))

(defmethod frontend-begin-frame ((f remote-frontend))
  "Begin a frame for the client: the faces changed sent first, the painting of
the last frame let go."
  (unless (= (remote-faces-generation f) tuatara.display:*face-generation*)
    (send-faces f))
  (empty-outbox (remote-painting f))
  (setf (fill-pointer (remote-kept-calls f)) 0
        (remote-frame-pictures f) '())
  (when (remote-far-pictures f)
    (sb-thread:with-mutex ((remote-lock f))
      (incf (remote-frame-number f)))))

(defparameter *row-left-out* "[This row was left out: the screen is too large to send whole]"
  "What a client is shown in place of a row that does not fit in a frame it can take.")

(defconstant +most-frame+ (- 16777216 (* +most-rows+ 128) 65536)
  "The most octets of a frame's painting before rows are sent plainer, or
left out, with room kept for every row to say so.")

(defun row-left-out (remote)
  "Say once, for REMOTE's connection, that rows of a frame were too large to send whole."
  (unless (shiftf (remote-cut-said remote) t)
    (tuatara.runtime:debug-report :remote "a frame too large for the client: rows sent without colours, or left out")
    (queue-push (remote-queue remote)
                (make-event :said :frame (remote-frame remote)
                                  :text (concatenate 'string "This screen is too large to send whole: some rows "
                                                     "show without colours or are left out")))))

(defmethod frontend-put-row ((f remote-frontend) row string &key face spans scales over parts)
  "Paint ROW of the frame with STRING in FACE and SPANS, as FRONTEND-PUT-ROW
says, kept as the client is to be sent it."
  (when (< -1 row +most-rows+)
    (let* ((box (remote-painting f)) (start (outbox-fill box)))
      (put-row-call box (remote-names f) row string face spans scales over parts)
      ;; a frame larger than a client takes: the row's text alone, and past that a word in its place
      (when (> (outbox-fill box) +most-frame+)
        (setf (outbox-fill box) start)
        (put-row-call box (remote-names f) row string nil nil nil nil nil)
        (when (> (outbox-fill box) +most-frame+)
          (setf (outbox-fill box) start)
          (put-row-call box (remote-names f) row *row-left-out* nil nil nil nil nil))
        (row-left-out f))
      (let* ((end (outbox-fill box)) (length (- end start))
             (kept (svref (remote-kept f) row)) (kept-length (aref (remote-kept-lengths f) row)))
        (if (and kept (= length kept-length)
                 (not (mismatch (outbox-octets box) kept :start1 start :end1 end :end2 kept-length)))
            ;; painted as the frame before painted it: said so in three octets
            (progn (setf (outbox-fill box) start)
                   (vector-push-extend start (remote-kept-calls f))
                   (put-kept-row-call box row))
            (progn (unless (and kept (<= length (length kept)))
                     (setf kept (setf (svref (remote-kept f) row) (tuatara.text:make-octets (max 64 length)))))
                   (replace kept (outbox-octets box) :start2 start :end2 end)
                   (setf (aref (remote-kept-lengths f) row) length)))))))

(defmethod frontend-set-cursor ((f remote-frontend) row column shape &optional colour)
  "Put the frame's cursor at ROW and COLUMN in SHAPE and COLOUR."
  (put-cursor-call (remote-painting f) row column shape colour))

(defun send-picture (remote image)
  "Put IMAGE among the messages for REMOTE's client, which then keeps it,
letting go of the oldest picture it keeps that the frame being painted does
not show when it keeps +MOST-PICTURES+; true when it was put. It is left out
when the frame shows +MOST-PICTURES+ already, or while the client has more
than +MOST-UNSENT+ octets not yet sent, the pictures' pixels counted."
  (sb-thread:with-mutex ((remote-lock remote))
    (let* ((pictures (remote-pictures remote))
           (let-go (and (>= (length pictures) +most-pictures+)
                        (find-if-not (lambda (id) (member id (remote-frame-pictures remote))) pictures :from-end t)))
           (unsent (remote-unsent remote)))
      (unless (or (remote-ended remote) (remote-closing remote)
                  (and (>= (length pictures) +most-pictures+) (not let-go))
                  (> (+ (outbox-fill unsent) (remote-unsent-pixels remote)) +most-unsent+)
                  (>= (length (outbox-descriptors unsent)) +most-pictures+))
        (tuatara.runtime:ignoring-errors-reported ("sending a picture to a client")
          (put-picture unsent image let-go)
          (incf (remote-unsent-pixels remote) (length (image-pixels image)))
          (setf (remote-pictures remote) (cons (image-id image) (remove let-go pictures)))
          (sb-thread:condition-broadcast (remote-moved remote))
          t)))))

(defmethod frontend-put-image ((f remote-frontend) image row column columns rows &optional offset)
  "Show IMAGE in the frame over COLUMNS by ROWS cells from ROW and COLUMN, from
OFFSET, as the client takes pictures."
  (when (and (facts-images (remote-facts f))
             (<= 1 (image-width image) 65535) (<= 1 (image-height image) 65535) (<= 3 (image-channels image) 4)
             (<= (* (image-width image) (image-height image) (image-channels image)) +most-picture-octets+)
             (= (length (image-pixels image)) (* (image-width image) (image-height image) (image-channels image))))
    (if (request-far (remote-request f))
        (let ((far (far-picture-to-show f image columns rows offset)))
          (when far
            (put-picture-call (remote-painting f) (far-picture-id far) row column columns rows offset)))
        (when (or (member (image-id image) (remote-pictures f)) (send-picture f image))
          (push (image-id image) (remote-frame-pictures f))
          (put-picture-call (remote-painting f) (image-id image) row column columns rows offset)))))

(defun put-frame (remote box whole)
  "Put the frame REMOTE has painted in BOX as one message; WHOLE, each row
painted as before written out in full."
  (let ((painting (remote-painting remote)) (at (begin-message box :frame)) (from 0))
    (when whole
      (loop for call across (remote-kept-calls remote)
            do (let ((row (logior (aref (outbox-octets painting) (+ call 1))
                                  (ash (aref (outbox-octets painting) (+ call 2)) 8))))
                 (put-octets box (outbox-octets painting) from call)
                 (put-octets box (svref (remote-kept remote) row) 0 (aref (remote-kept-lengths remote) row))
                 (setf from (+ call 3)))))
    (put-octets box (outbox-octets painting) from (outbox-fill painting))
    (end-message box at)))

(defmethod frontend-end-frame ((f remote-frontend))
  "Send the client the frame painted."
  (sb-thread:with-mutex ((remote-lock f))
    (unless (or (remote-ended f) (remote-closing f))
      (let ((box (remote-unsent f)) (whole nil))
        ;; a frame not yet sent is left out for this one, which then gives every row in full
        (when (>= (remote-frame-start f) 0)
          (outbox-cut box (remote-frame-start f) (remote-frame-end f))
          (setf whole t))
        (put-names box (remote-names f))
        (setf (remote-frame-start f) (outbox-fill box))
        (put-frame f box whole)
        (setf (remote-frame-end f) (outbox-fill box))
        (when (remote-far-pictures f) (settle-far-pictures f))
        (sb-thread:condition-broadcast (remote-moved f))))))

;;; Pictures for a client on another machine, sent as bytes
;;; (a descriptor does not cross ssh) by a thread of their own

(define-record (far-picture (:constructor make-far-picture (source width height channels image shown-at))
                           (:copier nil))
  "A picture of an editor's for its client on another machine. It is a copy of
the picture numbered SOURCE, the size it is shown at or its own (WIDTH by
HEIGHT pixels of CHANNELS octets, sent as number ID, given when the copy is
made). IMAGE is the picture it is made of, until it is sent. STATE is :WANTED,
:SENDING, :READY once the client has it all, :REFUSED when it cannot be sent,
or :CANCELLED. SHOWN-AT is the frame that last asked for it. TAKEN is the
octets of TOTAL the client has said it has."
  (source 0 :type fixnum :read-only t :documentation "The number of the picture this is a copy of.")
  (id 0 :type (unsigned-byte 32) :documentation "The number the client knows this picture by.")
  (width 0 :type fixnum :read-only t :documentation "The width in pixels of the copy sent.")
  (height 0 :type fixnum :read-only t :documentation "The height in pixels of the copy sent.")
  (channels 0 :type fixnum :read-only t :documentation "The octets of each pixel of the copy sent.")
  (image nil :documentation "The picture this is a copy of, until it has been sent.")
  (state :wanted :type (member :wanted :sending :ready :refused :cancelled)
         :documentation "How far this picture has got (FAR-PICTURE).")
  (shown-at 0 :type fixnum :documentation "The frame that last asked for this picture.")
  (taken 0 :type fixnum :documentation "How many octets the client has said it has of this picture.")
  (total 0 :type fixnum :documentation "How many packed octets this picture goes in."))

(setf (documentation 'make-far-picture 'function) "A FAR-PICTURE wanted of IMAGE, the copy to send, by frame SHOWN-AT.")

(defun far-picture-octets (far)
  "How many octets of pixels FAR's copy has."
  (* (far-picture-width far) (far-picture-height far) (far-picture-channels far)))

(defun picture-size-for-client (remote image columns rows offset)
  "The WIDTH and HEIGHT, two values, of the copy of IMAGE to send REMOTE's
client to show in COLUMNS by ROWS cells; with OFFSET, IMAGE's own, as it is
shown at its own size (FRONTEND-PUT-IMAGE). The size it is drawn at, as large
as fits but never larger than it is, held to what a client on another
machine takes; IMAGE's own when the client's cell is not known."
  (multiple-value-bind (cell-width cell-height) (frontend-cell-pixels remote)
    (if (or offset (null cell-width))
        (values (image-width image) (image-height image))
        (let* ((box-width (max 1 (* columns cell-width))) (box-height (max 1 (* rows cell-height)))
               (scale (image-fit-scale image box-width box-height))
               (side-scale (min 1 (/ +most-far-picture-side+ (max (image-width image) (image-height image)))))
               (count (* (image-width image) (image-height image) (image-channels image)))
               (octets-scale (if (> count +most-far-picture-octets+)
                                 (/ (isqrt (floor (* +most-far-picture-octets+ 1000000) count)) 1000)
                                 1))
               (scale (min scale side-scale octets-scale)))
          (values (max 1 (min (image-width image) (ceiling (* scale (image-width image)))))
                  (max 1 (min (image-height image) (ceiling (* scale (image-height image))))))))))

(defun far-picture-to-show (remote image columns rows offset)
  "What of IMAGE REMOTE's client on another machine has, to show in COLUMNS
by ROWS cells in the frame being painted, or NIL while it has none: the one it
has of the largest size, when the size wanted comes to more than a quarter
more, a sharper one sent for meanwhile, which comes in a later frame. With
OFFSET the client places it in pixels of its own size, so only one of
IMAGE's own size will do."
  (multiple-value-bind (width height) (picture-size-for-client remote image columns rows offset)
    (sb-thread:with-mutex ((remote-lock remote))
      (unless (or (remote-ended remote) (remote-closing remote))
        (let ((number (remote-frame-number remote))
              ;; of the copies of IMAGE the size wanted takes, the largest the client has and one coming
              (ready nil) (coming nil))
          (dolist (far (remote-far-pictures remote))
            (when (and (= (far-picture-source far) (image-id image))
                       (or (not offset) (= (far-picture-width far) width)))
              (case (far-picture-state far)
                (:ready (when (or (null ready) (> (far-picture-width far) (far-picture-width ready)))
                          (setf ready far)))
                ((:wanted :sending :refused) (unless coming (setf coming far))))))
          (when ready (setf (far-picture-shown-at ready) number))
          (unless (and ready
                       (or (>= (* 5 (far-picture-width ready)) (* 4 width))
                           (= (far-picture-width ready) (image-width image))))
            (if coming
                (setf (far-picture-shown-at coming) number)
                (want-far-picture remote image width height number))
            (start-far-sender remote))
          ready)))))

(defun want-far-picture (remote image width height number)
  "Have a copy of IMAGE WIDTH by HEIGHT pixels sent to REMOTE's client, as
frame NUMBER asks, or marked :REFUSED, for good, when it is larger than a
client on another machine takes; REMOTE-LOCK is held."
  (let ((far (make-far-picture (image-id image) width height (image-channels image) image number)))
    (push far (remote-far-pictures remote))
    (if (or (> (max width height) +most-far-picture-side+) (> (far-picture-octets far) +most-far-picture-octets+))
        (progn (tuatara.runtime:debug-report :remote "a picture of ~dx~d too large for a client on another machine"
                                             width height)
               (setf (far-picture-state far) :refused (far-picture-image far) nil))
        (sb-thread:condition-broadcast (remote-moved remote)))))

(defun start-far-sender (remote)
  "Start the thread sending REMOTE's client on another machine its pictures
when something is wanted and none runs; REMOTE-LOCK is held."
  (when (and (not (remote-far-sender remote))
             (find :wanted (remote-far-pictures remote) :key #'far-picture-state))
    (setf (remote-far-sender remote)
          (tuatara.runtime:make-guarded-thread (lambda () (send-far-pictures remote))
                                               :name "tuatara remote pictures"))))

(defun settle-far-pictures (remote)
  "After a frame has been put, with REMOTE-LOCK held: the pictures of
REMOTE's client on another machine that frame did not ask for and that are
not there yet are cancelled, and those that could not be sent forgotten."
  (let ((number (remote-frame-number remote)))
    (setf (remote-far-pictures remote)
          (delete-if (lambda (far)
                       (and (/= (far-picture-shown-at far) number)
                            (case (far-picture-state far)
                              ((:wanted :sending) (setf (far-picture-state far) :cancelled) nil)
                              (:refused t))))
                     (remote-far-pictures remote)))))

(defun picture-taken (remote id taken)
  "REMOTE's client says it has TAKEN octets of the picture numbered ID, which
is sending to it or just cancelled; more octets than the picture holds is
malformed."
  (sb-thread:with-mutex ((remote-lock remote))
    (let ((far (find id (remote-far-pictures remote) :key #'far-picture-id)))
      (when (and far (member (far-picture-state far) '(:sending :cancelled)))
        (unless (<= (far-picture-taken far) taken (far-picture-total far))
          (malformed "~d octets said taken of a picture of ~d" taken (far-picture-total far)))
        (setf (far-picture-taken far) taken)
        (sb-thread:condition-broadcast (remote-moved remote))))))

(defun make-room-for-far-picture (remote far)
  "Have REMOTE's client let go of as many of the pictures it keeps, oldest
first, as lets it keep FAR too, within the most it keeps; none the frame begun
last has asked for goes, which a frame named before it is said to go still
shows, as the client paints in order. NIL when there is no such room.
REMOTE-LOCK is held."
  (let ((number (remote-frame-number remote)))
    (loop
      (let* ((kept (remove :ready (remote-far-pictures remote) :key #'far-picture-state :test-not #'eq))
             (octets (reduce #'+ kept :key #'far-picture-octets :initial-value (far-picture-octets far))))
        (when (and (< (length kept) +most-pictures+) (<= octets +most-far-pictures-kept+))
          (return t))
        (let ((victim (find-if (lambda (candidate) (/= (far-picture-shown-at candidate) number)) kept :from-end t)))
          (unless victim (return nil))
          (setf (remote-far-pictures remote) (remove victim (remote-far-pictures remote)))
          (let* ((box (remote-unsent remote)) (at (begin-message box :picture-forget)))
            (put-number box (far-picture-id victim) 4)
            (end-message box at)))))))

(defun far-picture-copy (far)
  "The picture to send for FAR, under a number of its own that FAR is given,
so that what a client says of an earlier send is never taken for this one:
made the size FAR goes at when that is smaller, else the same pixels."
  (let* ((image (far-picture-image far))
         (copy (if (and (= (far-picture-width far) (image-width image))
                        (= (far-picture-height far) (image-height image)))
                   (make-image (image-width image) (image-height image) (image-channels image) (image-pixels image))
                   (scaled-image image (far-picture-width far) (far-picture-height far)))))
    (setf (far-picture-id far) (image-id copy))
    copy))

(defun scaled-image (image width height)
  "A new picture of IMAGE made WIDTH by HEIGHT pixels, smaller, each pixel
the mean of those it covers."
  (let* ((channels (image-channels image))
         (from-width (image-width image)) (from-height (image-height image))
         (from (coerce (image-pixels image) 'tuatara.text:octets))
         (pixels (tuatara.text:make-octets (* width height channels)))
         (at 0))
    (declare (type tuatara.text:octets from pixels) (type fixnum channels from-width from-height at width height))
    (dotimes (y height)
      (let ((y0 (floor (* y from-height) height)) (y1 (ceiling (* (1+ y) from-height) height)))
        (dotimes (x width)
          (let ((x0 (floor (* x from-width) width)) (x1 (ceiling (* (1+ x) from-width) width)))
            (dotimes (c channels)
              (let ((sum 0) (count 0))
                (declare (type fixnum sum count))
                (loop for sy from y0 below y1
                      do (loop for sx from x0 below x1
                               do (incf sum (aref from (+ (* sy from-width channels) (* sx channels) c)))
                                  (incf count)))
                (setf (aref pixels at) (round sum count))
                (incf at)))))))
    (make-image width height channels pixels)))

(defun next-far-picture (remote)
  "The picture to send next to REMOTE's client on another machine, the oldest
wanted, waiting for one; NIL when the connection ends. Those cancelled before
they began are forgotten here."
  (sb-thread:with-mutex ((remote-lock remote))
    (loop
      (when (or (remote-closing remote) (remote-ended remote)) (return nil))
      (setf (remote-far-pictures remote)
            (remove :cancelled (remote-far-pictures remote) :key #'far-picture-state))
      (let ((far (find :wanted (remote-far-pictures remote) :key #'far-picture-state :from-end t)))
        (when far (return far)))
      (sb-thread:condition-wait (remote-moved remote) (remote-lock remote)))))

(defun send-far-pictures (remote)
  "Send the pictures REMOTE's client on another machine is waiting for, one
at a time as they are wanted, until the connection ends. Each is made the size
it shows at and packed here, off the editor's thread. It is then sent a little
at a time among the frames, no more than +FAR-PICTURES-IN-FLIGHT+ octets ahead
of what the client has said it took, so that no key waits long behind one. One
that could not be sent is :REFUSED, not tried again while it stays shown."
  (unwind-protect
       (loop for far = (next-far-picture remote)
             while far
             do (handler-case
                    (tuatara.runtime:ignoring-errors-reported ("sending a picture to a client")
                      (send-far-picture remote far))
                  (storage-condition ()
                    (tuatara.runtime:debug-report :remote "not memory enough to send a picture to a client")))
                ;; a send that failed once begun leaves the client waiting for the rest: it is told to forget it
                (when (sb-thread:with-mutex ((remote-lock remote))
                        (let ((begun (eq (far-picture-state far) :sending)))
                          (when (member (far-picture-state far) '(:wanted :sending))
                            (setf (far-picture-state far) :refused (far-picture-image far) nil))
                          begun))
                  (queue-message remote :picture-forget (lambda (box) (put-number box (far-picture-id far) 4)))))
    (sb-thread:with-mutex ((remote-lock remote))
      (setf (remote-far-sender remote) nil))))

(defun send-far-picture (remote far)
  "Make, pack and send REMOTE's client the picture FAR (SEND-FAR-PICTURES);
the editor is asked for a frame that shows it when the client has it all.
One cancelled, or a connection ended, on the way: the client told to forget
what came of it, and FAR forgotten here."
  (let* ((copy (far-picture-copy far))
         (packed (packed-picture copy))
         (total (length packed))
         (sent 0))
    (flet ((stop-p ()
             (or (remote-closing remote) (remote-ended remote) (eq (far-picture-state far) :cancelled)))
           (forget-far ()
             (setf (remote-far-pictures remote) (remove far (remote-far-pictures remote)))))
      (flet ((wait-until (done)
               ;; true once DONE is, NIL when stopped first
               (sb-thread:with-mutex ((remote-lock remote))
                 (loop until (or (stop-p) (funcall done))
                       do (tuatara.runtime:wait-on (remote-moved remote) (remote-lock remote) 1))
                 (not (stop-p))))
             (give-up ()
               (queue-message remote :picture-forget (lambda (box) (put-number box (far-picture-id far) 4)))
               (sb-thread:with-mutex ((remote-lock remote)) (forget-far))))
        (unless (sb-thread:with-mutex ((remote-lock remote))
                  (cond ((stop-p) (forget-far) nil)
                        ((not (make-room-for-far-picture remote far))
                         (setf (far-picture-state far) :refused (far-picture-image far) nil)
                         nil)
                        (t (setf (far-picture-state far) :sending (far-picture-total far) total
                                 (far-picture-image far) nil)
                           (put-picture-begin (remote-unsent remote) (far-picture-id far) (far-picture-width far)
                                              (far-picture-height far) (far-picture-channels far) total)
                           (sb-thread:condition-broadcast (remote-moved remote))
                           t)))
          (return-from send-far-picture nil))
        (loop while (< sent total)
              do (unless (wait-until (lambda () (< (- sent (far-picture-taken far)) +far-pictures-in-flight+)))
                   (give-up)
                   (return-from send-far-picture nil))
                 (let ((from sent) (end (min total (+ sent +far-picture-piece+))))
                   (queue-message remote :picture-piece
                                  (lambda (box) (put-picture-piece-of box (far-picture-id far) packed from end)))
                   (setf sent end)))
        (if (wait-until (lambda () (= (far-picture-taken far) total)))
            (progn
              (sb-thread:with-mutex ((remote-lock remote))
                (setf (far-picture-state far) :ready))
              (queue-push (remote-queue remote) (make-event :tick :frame (remote-frame remote))))
            (give-up))))))

;;; The client's side: a frontend painted as the editor says

(define-record (remote-client (:constructor %make-remote-client (fd frontend inbox)) (:copier nil))
  "A client of an editor at the other end of the socket FD, painting what the
editor sends on FRONTEND and sending it FRONTEND's events.
INBOX is what the editor sends. OUT is the messages being sent. ARRIVED is
what was read and is not yet painted, newest first, under LOCK. NAMES is the
face names by their numbers. KEPT is each row as last painted. PICTURES is the
pictures kept, newest first. ARRIVING is the picture coming in pieces from an
editor on another machine (ARRIVING-PICTURE), or NIL. UNPACKING is those whole
and being unpacked. PAINTING is what is being painted, until it has been.
CLOSING is true once the connection is being closed here. ENDED is true once
the editor is gone, EXIT-CODE the code it gave, and EXIT-WORDS what it said
with it. FACTS is the facts last sent as octets. READER is the thread reading.
MOVED is signalled when something has arrived or the editor is gone. SETTINGS
is the settings the editor last said (TAKE-SETTINGS). ANOTHER is a function of
the client and a word, which opens another window attached with that word when
the editor asks for one, or says it does not (DECLINE-WINDOW), or NIL. PAINTED
is true once the editor has painted a frame on it.
FAR is true when the editor is on another machine. Then nothing of this
machine's clipboard is given it or taken from it, no terminal is lent and no
dropped file is named to it, but what ALLOW lets through. ALLOW is a plist of
:CLIPBOARD, true for the clipboard shared, as text with no control characters
taken (FAR-TEXT), and :DROP, a function sending the files dropped to that
machine (see MAKE-REMOTE-CLIENT)."
  (fd -1 :type fixnum :read-only t :documentation "The socket to the editor, a file descriptor.")
  (frontend nil :read-only t :documentation "The frontend CLIENT paints on and reads events from.")
  (inbox nil :read-only t :documentation "What the editor sends CLIENT, as it is read.")
  (out (make-outbox) :read-only t :documentation "Where CLIENT's messages are written to be sent.")
  (lock (sb-thread:make-mutex :name "remote client") :read-only t
        :documentation "Held while what CLIENT has read and not
painted, or its end, change.")
  (moved (sb-thread:make-waitqueue :name "remote client") :read-only t
         :documentation "Signalled when something has arrived for
CLIENT or its editor is gone.")
  (exit-words nil :documentation "What the editor said as it told CLIENT to exit, or NIL.")
  (arrived '() :documentation "What CLIENT has read and not yet painted, newest first.")
  (names (make-array (1+ +most-names+) :initial-element nil) :read-only t
         :documentation "The face names the editor gave CLIENT, by their numbers.")
  (kept (make-array +most-rows+ :initial-element nil) :read-only t
        :documentation "Each row as CLIENT last painted it, by row.")
  (pictures '() :documentation "The pictures CLIENT keeps, newest first.")
  (arriving nil :documentation "The picture coming to CLIENT in pieces, or NIL.")
  (unpacking '() :documentation "The pictures CLIENT has whole and is unpacking.")
  (painting '() :documentation "What CLIENT is painting now, in the order it came.")
  (closing nil :documentation "True once CLIENT's connection is being closed here.")
  (ended nil :documentation "True once the editor CLIENT paints for is gone.")
  (exit-code nil :documentation "The code the editor told CLIENT to exit with, or NIL.")
  (facts (make-outbox) :read-only t :documentation "The facts CLIENT last sent of its screen, as octets.")
  (reader nil :documentation "The thread reading what the editor sends CLIENT.")
  (settings '() :documentation "The settings CLIENT's editor last said, each (NAME . VALUE).")
  (another nil
           :documentation "The function of CLIENT and a word that opens another window for CLIENT's editor, or NIL.")
  (painted nil
           :documentation "True once CLIENT's editor has painted a frame on it: a
window asked for and let go before is not.")
  (far nil :documentation "True when CLIENT's editor is on another machine, reached over ssh.")
  (allow '()
         :documentation "What CLIENT's editor on another machine is given all
the same: a plist of :CLIPBOARD and :DROP."))

(setf (documentation '%make-remote-client 'function) "A REMOTE-CLIENT of the editor at FD painting on FRONTEND.")

(defun send-to-editor (client write)
  "Send CLIENT's editor the messages WRITE, a function of the box, writes;
NIL when they could not be sent."
  (let ((box (remote-client-out client)))
    (setf (outbox-fill box) 0)
    (funcall write box)
    (eq t (send-outbox (remote-client-fd client) box *stall-seconds*))))

(defun put-facts-message (box frontend kind &optional (request (make-request)))
  "Put a message of KIND (:ATTACH, whole, with REQUEST, or :FACTS) in BOX
saying what FRONTEND is."
  (let ((at (begin-message box kind)))
    (put-facts box frontend :whole (eq kind :attach))
    (when (eq kind :attach)
      (put-request box request))
    (end-message box at)))

(defun use-settings (settings &key before at-start)
  "Give each of SETTINGS, (NAME . VALUE) of *DISPLAY-SETTINGS* as the editor
said them, its value here, but those said the same in BEFORE, settings said
before, and with AT-START false those taken only as the screen opens; true
when one was given."
  (let ((given nil))
    (loop for (name . value) in settings
          for entry = (assoc name *display-settings*)
          unless (or (and (not at-start) (member :at-start entry))
                     (let ((was (assoc name before))) (and was (equal (cdr was) value))))
            do (setf (symbol-value name) value given t))
    given))

(defun wait-for-editor (fd starting)
  "Wait until the editor at the other end of the new local socket FD says
something, *STARTING-SECONDS* at most, as a daemon still reading its init file
says nothing yet; once *HELLO-SECONDS* have gone by in silence, call STARTING,
a function of none, if given, to say so."
  (let ((early (tuatara.runtime:deadline-in *hello-seconds*))
        (deadline (tuatara.runtime:deadline-in *starting-seconds*)))
    (loop until (or (plusp (tuatara.io:poll-fd fd 1 1000)) (tuatara.runtime:deadline-passed-p deadline))
          when (and starting (tuatara.runtime:deadline-passed-p early))
            do (funcall (shiftf starting nil)))))

(defun greet-editor (fd &key (take t) starting)
  "Read the hello of the editor at the other end of the new local socket FD
and the settings it shows its clients by (*DISPLAY-SETTINGS*), and with TAKE
take them here, before a client's screen opens (not for another window of a
client that took them already): what was read, for MAKE-REMOTE-CLIENT. A
daemon still starting is waited for (WAIT-FOR-EDITOR), STARTING called to say
so. Signals REMOTE-REFUSED when the editor is of another build, refuses the
client or says nothing it should; FD is then the caller's to close."
  (wait-for-editor fd starting)
  (let ((inbox (make-inbox fd :client :deadline (tuatara.runtime:deadline-in *hello-seconds*))))
    (handler-bind ((remote-refused (lambda (condition)
                                     (declare (ignore condition))
                                     (close-descriptors (shiftf (inbox-descriptors inbox) '())))))
      (read-hello inbox)
      (let ((settings (handler-case (if (eq (read-message inbox) :settings)
                                        (take-settings inbox)
                                        (error 'remote-refused :words "The editor said no settings."))
                        (malformed-message (condition)
                          (error 'remote-refused :words (format nil "The editor's settings: ~a"
                                                                (malformed-message-reason condition)))))))
        ;; nothing else is sent before the client attaches: a descriptor passed with these is no picture's
        (when (inbox-descriptors inbox)
          (error 'remote-refused :words "The editor passed a descriptor with its hello."))
        (when take (use-settings settings :at-start t))
        (list inbox settings)))))

(defun make-remote-client (fd frontend &key (request (make-request)) standard-input greeted another allow)
  "A client painting on FRONTEND, started, what the editor at the other end
of the local socket FD sends, once both have said hello and the client what
its screen is and what it asks for, REQUEST; STANDARD-INPUT, a descriptor
passed with it to be read into a buffer (REQUEST-STANDARD-INPUT then true),
is closed here once passed. GREETED is what GREET-EDITOR read of FD, when it
was called before FRONTEND was made, else it is called here. ANOTHER, a
function of the client and a word, opens another window when the editor
asks for one (REMOTE-CLIENT-ANOTHER). ALLOW, for an editor on another
machine, says what it is given all the same: :CLIPBOARD true shares the
clipboard; :DROP, a function of a list of the paths of files dropped,
called on a thread of its own, sends them there and gives their paths
there, or NIL when they could not be sent. Signals REMOTE-REFUSED when the
editor is of another build, refuses the client or says nothing it should;
FD is then the caller's to close."
  (destructuring-bind (inbox settings)
      (or greeted
          (handler-bind ((remote-refused (lambda (condition)
                                           (declare (ignore condition))
                                           (when standard-input (close-descriptors (list standard-input))))))
            (greet-editor fd)))
    (let ((client (%make-remote-client fd frontend inbox)))
      (setf (remote-client-settings client) settings
            (remote-client-another client) another
            (remote-client-far client) (request-far request)
            (remote-client-allow client) allow)
      (unless (send-to-editor client (lambda (box)
                                       (put-hello box)
                                       (let ((at (outbox-fill box)))
                                         (put-facts-message box frontend :attach request)
                                         (when standard-input
                                           (push (cons at standard-input) (outbox-descriptors box))))))
        (error 'remote-refused :words "The editor took no hello."))
      (setf (inbox-deadline inbox) nil)
      (put-facts (remote-client-facts client) frontend)
      (setf (remote-client-reader client)
            (tuatara.runtime:make-guarded-thread (lambda () (read-from-editor client))
                                                 :name "tuatara client reading" :niceness 0))
      client)))

(defun read-from-editor (client)
  "Read what CLIENT's editor sends, keep it to be painted and wake CLIENT's
frontend for it, until the editor says the client is done, goes, or sends
what it should not."
  (let ((inbox (remote-client-inbox client)) (frontend (remote-client-frontend client)))
    (unwind-protect
         (handler-case
             (loop for kind = (read-message inbox)
                   while kind
                   do (let ((item (take-from-editor client kind inbox)))
                        (when (and (inbox-descriptors inbox) (= (inbox-start inbox) (inbox-end inbox)))
                          (malformed "a descriptor passed, where none is taken"))
                        (when item
                          (sb-thread:with-mutex ((remote-client-lock client))
                            (push item (remote-client-arrived client))
                            (sb-thread:condition-broadcast (remote-client-moved client)))
                          (frontend-wake frontend))
                        (when (remote-client-far client) (wait-for-painting client))
                        (when (eq kind :exit) (return))))
           (malformed-message (condition)
             (tuatara.runtime:debug-report :remote "the editor's connection ended: ~a"
                                           (malformed-message-reason condition))))
      (close-descriptors (shiftf (inbox-descriptors inbox) '()))
      (sb-thread:with-mutex ((remote-client-lock client))
        (setf (remote-client-ended client) t)
        (sb-thread:condition-broadcast (remote-client-moved client)))
      (frontend-wake frontend))))

(defun take-from-editor (client kind inbox)
  "Read the message of KIND CLIENT's editor sent from INBOX; what is to be
done with it on the frontend's thread, as a list whose first is what it is
\(:FACES :FRAME :PICTURE-TAKEN :TITLE :SELECTION :TOGGLE :RAISE :ASK
:TAKE-BACK :SETTINGS :ANOTHER), or NIL when reading it was all."
  (let ((names (remote-client-names client)))
    (ecase kind
      (:names (take-names inbox names) nil)
      (:faces (prog1 (cons :faces (loop repeat (take-count inbox 2 +most-names+ 19) collect (take-face inbox names)))
                (message-read inbox)))
      (:frame (cons :frame (take-frame inbox names (remote-client-kept client) (remote-client-pictures client))))
      ;; the picture kept, and the one the editor let go of to keep it let go of here too
      (:picture (multiple-value-bind (image let-go) (take-picture inbox)
                  (sb-thread:with-mutex ((remote-client-lock client))
                    (when let-go (let-go-of-picture client let-go))
                    (keep-picture client image))
                  nil))
      ;; a picture from another machine comes in pieces, each but the last answered as it is painted
      (:picture-begin
       (when (remote-client-arriving client) (malformed "a picture begun while another comes"))
       (setf (remote-client-arriving client) (take-picture-begin inbox))
       nil)
      (:picture-piece
       (let ((arriving (or (remote-client-arriving client) (malformed "a piece of a picture not begun"))))
         (cond ((take-picture-piece inbox arriving)
                (setf (remote-client-arriving client) nil)
                (start-unpacking client arriving)
                nil)
               (t (list :picture-taken (arriving-picture-id arriving) (arriving-picture-filled arriving))))))
      (:picture-forget
       (let* ((id (take-number inbox 4)) (arriving (remote-client-arriving client)))
         (message-read inbox)
         (if (and arriving (= (arriving-picture-id arriving) id))
             (setf (remote-client-arriving client) nil)
             (sb-thread:with-mutex ((remote-client-lock client))
               (let ((unpacking (find id (remote-client-unpacking client) :key #'arriving-picture-id)))
                 (if unpacking
                     (setf (arriving-picture-cancelled unpacking) t)
                     (let-go-of-picture client id)))))
         nil))
      (:title (prog1 (list :title (take-text inbox 60000)) (message-read inbox)))
      (:selection (prog1 (list :selection (take-bounded inbox 1 0 1 "a selection") (take-text inbox +most-text+))
                    (message-read inbox)))
      (:toggle (prog1 (list :toggle (if (zerop (take-bounded inbox 1 0 1 "a state")) :fullscreen :maximised))
                 (message-read inbox)))
      (:raise (message-read inbox) (list :raise))
      (:ask (let* ((request (take-number inbox 4))
                   (question (svref *questions* (take-bounded inbox 1 0 (1- (length *questions*)) "a question")))
                   (argument (case question
                               ((:font :lend) (take-text inbox 1024))
                               (:font-steps (take-signed inbox 2)))))
              (message-read inbox)
              (list :ask request question argument)))
      (:take-back (message-read inbox) (list :take-back))
      (:settings (list :settings (take-settings inbox)))
      (:another (prog1 (let ((word (take-text inbox 64)))
                         (unless (window-word-p word)
                           (malformed "a window asked for by a word of other characters"))
                         (list :another word))
                  (message-read inbox)))
      (:exit (setf (remote-client-exit-code client) (take-number inbox 1))
       (let ((words (shown-words (take-text inbox 1024))))
         (setf (remote-client-exit-words client) (and (plusp (length words)) words)))
       (message-read inbox)
       nil)
      ((:hello :refused) (malformed "a ~(~a~) after the first hello" kind)))))

(defun start-unpacking (client arriving)
  "Unpack the picture ARRIVING, whole, for CLIENT on a thread of its own, so
that the frames and everything else behind it are not kept waiting; the
editor names it in a frame only once it is told it was taken, which is said
when it is unpacked. With +MOST-PICTURES-UNPACKING+ unpacking already, waits
for one to be done; left out when CLIENT is being closed meanwhile."
  (sb-thread:with-mutex ((remote-client-lock client))
    (loop while (and (not (remote-client-closing client))
                     (>= (length (remote-client-unpacking client)) +most-pictures-unpacking+))
          do (tuatara.runtime:wait-on (remote-client-moved client) (remote-client-lock client) 1))
    (unless (remote-client-closing client)
      (push arriving (remote-client-unpacking client))
      (setf (arriving-picture-thread arriving)
            (tuatara.runtime:make-guarded-thread (lambda () (unpack-picture client arriving))
                                                 :name "tuatara picture unpacking")))))

(defun unpack-picture (client arriving)
  "Unpack the picture ARRIVING and have CLIENT keep it, and tell its editor it
has it, unless it was told to forget it meanwhile; when it is not what it said
it was, or there is not memory enough, the connection ends, why being logged,
unless it is being closed here already. However it ends, the reader waiting
for a place among those unpacking is told."
  (flet ((give-up (words)
           (tuatara.runtime:debug-report :remote "the editor's connection ended: ~a" words)
           ;; under the lock: once CLOSING is set the descriptor may be closed, and its number used again
           (sb-thread:with-mutex ((remote-client-lock client))
             (unless (remote-client-closing client)
               (tuatara.runtime:ignoring-errors-reported ("ending the editor's connection")
                 (tuatara.io:shut-socket (remote-client-fd client)))))))
    (unwind-protect
         (handler-case
             (let ((image (arrived-picture arriving)))
               (when (sb-thread:with-mutex ((remote-client-lock client))
                       (setf (remote-client-unpacking client) (remove arriving (remote-client-unpacking client)))
                       (unless (arriving-picture-cancelled arriving)
                         (keep-picture client image :most-octets +most-far-pictures-kept+)
                         (push (list :picture-taken (arriving-picture-id arriving) (arriving-picture-filled arriving))
                               (remote-client-arrived client))
                         t))
                 (frontend-wake (remote-client-frontend client))))
           (malformed-message (condition) (give-up (malformed-message-reason condition)))
           (storage-condition () (give-up "not memory enough to unpack a picture")))
      (sb-thread:with-mutex ((remote-client-lock client))
        (setf (remote-client-unpacking client) (remove arriving (remote-client-unpacking client)))
        (sb-thread:condition-broadcast (remote-client-moved client))))))

(defconstant +most-unpainted+ 256
  "The most things read from an editor on another machine that wait to be
painted before reading more waits for them.")

(defun too-much-unpainted-p (client)
  "True when more waits to be painted on CLIENT than may: +MOST-UNPAINTED+
things, or the pictures CLIENT keeps and those only frames not yet painted
still name, past +MOST-FAR-PICTURES-KEPT+ octets of pixels. CLIENT's lock is held."
  (let ((kept (remote-client-pictures client)))
    (or (> (+ (length (remote-client-arrived client)) (length (remote-client-painting client))) +most-unpainted+)
        (let ((octets (loop for image in kept sum (length (image-pixels image))))
              (others '()))
          ;; the pictures only frames not yet painted name, each counted once
          (flet ((count-named (items)
                   (dolist (item items)
                     (when (eq (first item) :frame)
                       (dolist (call (rest item))
                         (let ((image (and (eq (first call) :picture) (second call))))
                           (when (and image (not (member image kept :test #'eq)) (not (member image others :test #'eq)))
                             (push image others)
                             (incf octets (length (image-pixels image))))))))))
            (count-named (remote-client-arrived client))
            (count-named (remote-client-painting client)))
          (> octets +most-far-pictures-kept+)))))

(defun wait-for-painting (client)
  "Wait, on the thread reading CLIENT's editor, while more waits to be painted
than may (TOO-MUCH-UNPAINTED-P), so that an editor on another machine can
keep no more memory here than that; until CLIENT is closed."
  (sb-thread:with-mutex ((remote-client-lock client))
    (loop while (and (not (remote-client-closing client)) (too-much-unpainted-p client))
          do (tuatara.runtime:wait-on (remote-client-moved client) (remote-client-lock client) 1))))

(defun let-go-of-picture (client id)
  "Have CLIENT let go of the picture numbered ID, as its editor says to, for
a frame that names it no more; CLIENT's lock is held. Malformed when CLIENT
does not keep it."
  (unless (find id (remote-client-pictures client) :key #'image-id)
    (malformed "picture ~d let go of, never kept" id))
  (setf (remote-client-pictures client) (remove id (remote-client-pictures client) :key #'image-id)))

(defun keep-picture (client image &key most-octets)
  "Have CLIENT keep IMAGE, among the +MOST-PICTURES+ it may, and within
MOST-OCTETS of pixels in all when given; CLIENT's lock is held. Malformed
past those."
  (let ((pictures (remote-client-pictures client)))
    (when (>= (length pictures) +most-pictures+)
      (malformed "a picture past the ~d kept" +most-pictures+))
    (when (and most-octets
               (> (reduce #'+ pictures :key (lambda (p) (length (image-pixels p)))
                                       :initial-value (length (image-pixels image)))
                  most-octets))
      (malformed "pictures of more than ~d octets kept" most-octets))
    (setf (remote-client-pictures client) (cons image pictures))))

(defun face-now (face)
  "The attributes of FACE as a message carries them (TAKE-FACE), as SET-FACE-ATTRIBUTES takes them."
  (flet ((colour (value)
           (and (or (and (keywordp value) (find value tuatara.display:*basic-colour-names*))
                    (and (integerp value) (<= 0 value 255))
                    (and (stringp value) (<= (length value) 64)))
                value)))
    (let ((font (tuatara.display:face-font face)) (height (tuatara.display:face-height face))
          (underline (tuatara.display:face-underline face)))
      (list :fg (colour (tuatara.display:face-fg face)) :bg (colour (tuatara.display:face-bg face))
            :bold (and (tuatara.display:face-bold face) t) :italic (and (tuatara.display:face-italic face) t)
            :reverse (and (tuatara.display:face-reverse face) t) :dim (and (tuatara.display:face-dim face) t)
            :extend (and (tuatara.display:face-extend face) t)
            :underline (if (find underline *underlines*) underline (and underline t))
            :underline-colour (colour (tuatara.display:face-underline-colour face))
            :font (and (stringp font) (plusp (length font)) (<= (length font) 256) font)
            :height (and (rationalp height) (> height 1) (min height 3))
            :inherit (tuatara.display:face-inherit face)))))

(defun take-faces (faces)
  "Make the faces here as FACES, each (NAME . ATTRIBUTES), say, those already
so left as they are."
  (loop for (name . attributes) in faces
        for face = (and (symbolp name) (tuatara.display:find-face name))
        unless (or (not (symbolp name)) (and face (equal attributes (face-now face))))
          do (apply #'tuatara.display:set-face-attributes name attributes)))

(defun lend-terminal (client request words later)
  "Give CLIENT's terminal back, as its editor asked in the question numbered
REQUEST, to be lent to a program, WORDS shown there, and answer that it was;
take it again once the editor says so (:TAKE-BACK) or is gone. When LATER,
what arrived after the question, holds that already, the editor has given
up waiting, and nothing is lent."
  (if (find :take-back later :key #'first)
      (send-to-editor client (lambda (box) (put-answer box request nil)))
      (frontend-lend-terminal
       (remote-client-frontend client)
       (lambda ()
         (send-to-editor client (lambda (box) (put-answer box request 1)))
         (sb-thread:with-mutex ((remote-client-lock client))
           (loop until (or (remote-client-ended client)
                           (find :take-back (remote-client-arrived client) :key #'first))
                 do (sb-thread:condition-wait (remote-client-moved client) (remote-client-lock client)))))
       :words words)))

(defun far-text (text)
  "TEXT, copied by an editor on another machine, as this machine's clipboard
takes it: without its control characters but tabs and line ends, and
without the invisible characters that change the direction text is drawn in
or draw nothing (TUATARA.DISPLAY:*DIRECTION-CONTROLS*, *ESCAPED-INVISIBLES*),
so that nothing pasted from it runs, moves or hides what it should not."
  (remove-if (lambda (c)
               (or (and (eq (sb-unicode:general-category c) :cc) (not (member c '(#\Tab #\Newline))))
                   (tuatara.display:hiding-code-p (char-code c))))
             text))

(defun clipboard-closed-p (client)
  "True when CLIENT's editor is on another machine and may not have the
clipboard now: it is not shared with it, or CLIENT's window does not have
the keyboard (FRONTEND-FOCUSED-P), so that one left in the background can
neither read what is copied elsewhere nor put something in its place."
  (and (remote-client-far client)
       (not (and (getf (remote-client-allow client) :clipboard)
                 (frontend-focused-p (remote-client-frontend client))))))

(defun answer-editor (client request question argument)
  "Answer CLIENT's editor's question QUESTION, with ARGUMENT, numbered
REQUEST, from CLIENT's frontend; an editor on another machine is given
nothing of the clipboard unless it is shared with it and CLIENT's window
has the keyboard (CLIPBOARD-CLOSED-P)."
  (let* ((frontend (remote-client-frontend client))
         (far (clipboard-closed-p client))
         (answer (ecase question
                   (:clipboard (and (not far) (frontend-clipboard frontend)))
                   (:primary (and (not far) (frontend-primary-selection frontend)))
                   (:font (frontend-set-font frontend argument))
                   (:font-steps (setf (frontend-font-steps frontend) argument))
                   (:lend nil))))
    (send-to-editor client
                    (lambda (box)
                      (if (and (stringp answer) (<= (tuatara.text:string-octet-length answer) +most-text+))
                          (loop for (piece . more) on (text-pieces answer)
                                do (put-answer box request piece :more more))
                          (put-answer box request (and (integerp answer) answer)))))))

(defun paint-arrived (client)
  "Paint on CLIENT's frontend what its editor has sent since this was last
done, and answer its questions, in the order they came. An editor on
another machine puts nothing in the clipboard, unless it is shared with it
and CLIENT's window has the keyboard, and then text with no control
characters (FAR-TEXT), and is lent no
terminal; the files sent it are named to it once there."
  (let ((frontend (remote-client-frontend client))
        (far (remote-client-far client))
        (items (sb-thread:with-mutex ((remote-client-lock client))
                 (setf (remote-client-painting client) (reverse (shiftf (remote-client-arrived client) '()))))))
    (loop for (item . later) on items
          do (ecase (first item)
               (:faces (take-faces (rest item)))
               (:frame (setf (remote-client-painted client) t)
                (frontend-begin-frame frontend)
                (dolist (call (rest item))
                  (ecase (first call)
                    (:row (destructuring-bind (row string face spans scales over parts) (rest call)
                            (frontend-put-row frontend row string :face face :spans spans :scales scales
                                              :over over :parts parts)))
                    (:cursor (apply #'frontend-set-cursor frontend (rest call)))
                    (:picture (when (frontend-images-p frontend)
                                (apply #'frontend-put-image frontend (rest call))))))
                (frontend-end-frame frontend))
               (:picture-taken (send-to-editor client (lambda (box)
                                                        (let ((at (begin-message box :picture-taken)))
                                                          (put-number box (second item) 4)
                                                          (put-number box (third item) 4)
                                                          (end-message box at)))))
               (:title (frontend-set-title frontend (second item)))
               (:selection (let ((text (if far (far-text (third item)) (third item))))
                             (cond ((clipboard-closed-p client))
                                   ((zerop (second item)) (frontend-set-clipboard frontend text))
                                   (t (frontend-set-primary-selection frontend text)))))
               (:sent (send-to-editor client (lambda (box) (put-event box (second item)))))
               (:toggle (frontend-toggle-state frontend (second item)))
               (:raise (frontend-raise frontend))
               (:settings (when (use-settings (second item) :before (remote-client-settings client))
                            (frontend-settings-changed frontend))
                (setf (remote-client-settings client) (second item)))
               (:another (let ((open (remote-client-another client)))
                           (if open
                               (funcall open client (second item))
                               (send-to-editor client (lambda (box)
                                                        (put-no-window box (second item) "this client opens none"))))))
               (:decline (send-to-editor client (lambda (box) (put-no-window box (second item) (third item)))))
               ;; taken again where it was lent (LEND-TERMINAL), else nothing to take
               (:take-back)
               (:ask (if (and (eq (third item) :lend) (not far))
                         (lend-terminal client (second item) (fourth item) later)
                         (apply #'answer-editor client (rest item))))))
    (sb-thread:with-mutex ((remote-client-lock client))
      (setf (remote-client-painting client) '())
      (sb-thread:condition-broadcast (remote-client-moved client)))))

(defun decline-window (client word words)
  "Have CLIENT tell its editor the window it asked for by WORD is not opened,
why being WORDS: from any thread, said on the thread painting CLIENT."
  (sb-thread:with-mutex ((remote-client-lock client))
    (push (list :decline word words) (remote-client-arrived client))
    (sb-thread:condition-broadcast (remote-client-moved client)))
  (frontend-wake (remote-client-frontend client)))

(defun send-dropped (client event)
  "Have the files of EVENT, dropped on CLIENT's frontend, sent to the machine
of CLIENT's editor, on a thread of its own, and the drop named to the editor
with their paths there once they are (PAINT-ARRIVED); when they could not
be, nothing is named."
  (let ((send (getf (remote-client-allow client) :drop)))
    (when send
      (tuatara.runtime:make-guarded-thread
       (lambda ()
         (let ((there (funcall send (event-files event))))
           (when there
             (sb-thread:with-mutex ((remote-client-lock client))
               (push (list :sent (make-event :drop :files there :column (event-column event) :row (event-row event)
                                                   :frame (event-frame event)))
                     (remote-client-arrived client))
               (sb-thread:condition-broadcast (remote-client-moved client)))
             (frontend-wake (remote-client-frontend client)))))
       :name "tuatara files dropped sent"))
    t))

(defun send-event (client event)
  "Send CLIENT's editor EVENT, read from CLIENT's frontend, when it is of a
kind the editor takes; a paste in pieces. Files dropped are not named to an
editor on another machine: they are sent there when it is let have them
\(SEND-DROPPED). NIL when it could not be sent."
  (or (not (sendable-event-p event))
      (and (remote-client-far client) (eq (event-kind event) :drop) (send-dropped client event))
      (send-to-editor client
                      (lambda (box)
                        (if (eq (event-kind event) :paste)
                            (loop for (piece . more) on (text-pieces (event-text event))
                                  do (put-event box event :text piece :more more))
                            (put-event box event))))))

(defun send-facts (client)
  "Send CLIENT's editor what CLIENT's screen is when it has changed since it
was last sent (a cell's size, the desktop's style); NIL when it could not be sent."
  (let ((now (remote-client-out client)) (sent (remote-client-facts client)))
    (setf (outbox-fill now) 0)
    (put-facts now (remote-client-frontend client))
    (or (and (= (outbox-fill now) (outbox-fill sent))
             (not (mismatch (outbox-octets now) (outbox-octets sent) :end1 (outbox-fill now) :end2 (outbox-fill sent))))
        (progn (setf (outbox-fill sent) 0)
               (put-facts sent (remote-client-frontend client))
               (send-to-editor client (lambda (box) (put-facts-message box (remote-client-frontend client) :facts)))))))

(defun close-remote-client (client)
  "End CLIENT's connection: its reading thread stopped, the pictures it was
unpacking let finish, and its socket closed."
  (sb-thread:with-mutex ((remote-client-lock client))
    (setf (remote-client-closing client) t)
    (sb-thread:condition-broadcast (remote-client-moved client)))
  (tuatara.io:shut-socket (remote-client-fd client))
  (tuatara.runtime:join-within (remote-client-reader client) 2)
  (let ((deadline (tuatara.runtime:deadline-in 2)))
    (dolist (arriving (sb-thread:with-mutex ((remote-client-lock client)) (copy-list (remote-client-unpacking client))))
      (when (arriving-picture-thread arriving)
        (tuatara.runtime:join-within (arriving-picture-thread arriving)
                                     (max 0.001 (tuatara.runtime:seconds-left deadline))))))
  (tuatara.runtime:ignoring-errors-reported ("closing the editor's socket") (sb-posix:close (remote-client-fd client)))
  nil)

(defun run-remote-client (client)
  "Paint on CLIENT's frontend what its editor sends and send the editor the
events read from the frontend, until the editor says the client is done or
goes, or the frontend delivers nothing more; then close the connection.
Returns the code the editor said to exit with, or NIL when it said none, and
the words it said with it, or NIL. Runs on the thread that reads the
frontend's events, which paints it too."
  (let ((frontend (remote-client-frontend client)))
    (unwind-protect
         (handler-case
             (loop
               (paint-arrived client)
               (when (remote-client-ended client)
                 (paint-arrived client)
                 (return))
               (let ((event (frontend-read-event frontend)))
                 (unless (case (event-kind event)
                           (:tick (send-facts client))
                           (:quit (send-to-editor client (lambda (box) (end-message box (begin-message box :detach))))
                            nil)
                           (:resize (and (send-event client event) (send-facts client)))
                           (t (send-event client event)))
                   ;; the editor gone mid-send: what it sent before going is still shown
                   (paint-arrived client)
                   (return))))
           (error (condition)
             (tuatara.runtime:debug-report :remote "painting what the editor sent: ~a"
                                           (tuatara.runtime:failure-words condition))))
      (close-remote-client client))
    (values (remote-client-exit-code client) (remote-client-exit-words client))))

;;; A frontend with no screen

(defclass null-frontend ()
  ((queue :initarg :queue :initform (make-event-queue) :reader null-queue
          :documentation "Where the events of the frames sharing this frontend's source come."))
  (:documentation "A frontend with no screen: it paints nothing, is 80 by 24 cells of 256 colours, asks
nothing, and reads the events of the queue it shares with other frames (MAKE-NULL-FRONTEND). The frame of
an editor that runs in the background shows on it while no screen shows the editor, and a client asked
only to stop the editor, with no terminal, has one."))

(defun make-null-frontend (&optional (queue (make-event-queue)))
  "A frontend with no screen whose events come from QUEUE (NULL-FRONTEND)."
  (make-instance 'null-frontend :queue queue))

(defmethod frontend-start ((f null-frontend))
  "Nothing: a frontend with no screen has nothing to start."
  nil)
(defmethod frontend-stop ((f null-frontend))
  "Nothing: a frontend with no screen has nothing to stop."
  nil)

(defmethod frontend-size ((f null-frontend))
  "Eighty columns and twenty-four rows, as two values."
  (values 80 24))

(defmethod frontend-colour-count ((f null-frontend))
  "256 colours."
  256)

(defmethod frontend-read-event ((f null-frontend))
  "The next event of F's queue, waiting for one."
  (queue-pop (null-queue f) nil))

(defmethod frontend-read-event-timeout ((f null-frontend) seconds)
  "The next event of F's queue, or a :TICK when none comes in SECONDS."
  (or (queue-pop (null-queue f) seconds) (make-event :tick)))

(defmethod frontend-wake ((f null-frontend))
  "Wake the editor reading F's queue, a :TICK put in it."
  (queue-push (null-queue f) (make-event :tick)))

(defmethod frontend-input-pending-p ((f null-frontend))
  "True when F's queue holds an event of input."
  (queue-input-p (null-queue f)))

(defmethod frontend-begin-frame ((f null-frontend))
  "Nothing: no screen is painted."
  nil)

(defmethod frontend-put-row ((f null-frontend) row string &key face spans scales over parts)
  "Nothing: no screen is painted."
  (declare (ignore row string face spans scales over parts))
  nil)

(defmethod frontend-set-cursor ((f null-frontend) row column shape &optional colour)
  "Nothing: no screen is painted."
  (declare (ignore row column shape colour))
  nil)

(defmethod frontend-end-frame ((f null-frontend))
  "Nothing: no screen is painted."
  nil)
