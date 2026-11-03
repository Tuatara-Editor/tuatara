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
      (setf (outbox-fill box) 0 (outbox-descriptors box) '()))))

(defun shown-words (text)
  "TEXT, from the other end of a connection, without its control characters,
to be shown or put in a log."
  (remove-if (lambda (c) (tuatara.text:control-code-p (char-code c))) text))

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
   (requests :initform 0 :accessor remote-requests
             :documentation "How many questions have been put to the client.")
   (asked :initform nil :accessor remote-asked
          :documentation "The number of the question awaiting its answer, or NIL.")
   (asked-question :initform nil :accessor remote-asked-question
                   :documentation "What the question awaiting its answer asks (*QUESTIONS*).")
   (answer :initform :none :accessor remote-answer
           :documentation "The answer that came, or :NONE.")
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

(setf (documentation 'remote-exit-code 'function) "The code the client of the remote frontend F exits with when F stops."
      (documentation 'remote-exit-words 'function) "What the client of the remote frontend F says as it exits, or NIL."
      (documentation 'remote-request 'function) "What the client of the remote frontend F asked for as it attached (REQUEST)."
      (documentation 'remote-pid 'function) "The process the client of the remote frontend F connected from, or NIL.")

(defmethod initialize-instance :after ((f remote-frontend) &key)
  (setf (remote-stopping f) (lambda () (or (remote-closing f) (remote-closed f)))))

(defun settings-now ()
  "The values of *DISPLAY-SETTINGS* here, in their order, :UNSET for one
unset."
  (loop for (name) in *display-settings* collect (if (boundp name) (symbol-value name) :unset)))

(defun make-remote-frontend (fd &key queue)
  "A frontend whose screen is the client's at the other end of the local
socket FD, once both have said hello, the client been told how its screen is
to be shown (*DISPLAY-SETTINGS*), and it has said what its screen is and
what it asks for (REMOTE-REQUEST); its events go into QUEUE, the queue of
the editor's other frames, or a queue of its own. Signals REMOTE-REFUSED
when the client is of another build or says nothing it should; FD is then
the caller's to close. A client may take up to *HELLO-SECONDS* to say hello,
so this is called on the thread taking connections, never the editor's, and
that thread bounds how many are being greeted at once. The one descriptor a
client may pass, its standard input with its attach, is the frontend's
request's to close once taken."
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
                              (remote-request remote) request)))
             (malformed-message (condition)
               (error 'remote-refused :words (format nil "The client's attach: ~a" (malformed-message-reason condition)))))
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
                                              :text (format nil "A client that took in nothing for ~d minutes was let go"
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
its answer; NIL when it gives none. An answer read only after the client's events waiting to be
handled (the most, +MOST-WAITING-EVENTS+, while the editor is busy) comes
too late, and is NIL too."
  (sb-thread:with-mutex ((remote-lock remote))
    (unless (or (remote-ended remote) (remote-closing remote))
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

(defmethod frontend-start ((f remote-frontend)) nil)

(defmethod frontend-stop ((f remote-frontend))
  (unless (remote-closed f)
    (frontend-stop-once f)))

(defun frontend-stop-once (f)
  "Stop the remote frontend F, not stopped before: what is unsent is sent
while its client takes it, the client told to exit, and its socket closed."
  (sb-thread:with-mutex ((remote-lock f))
    (unless (remote-ended f)
      (let* ((box (remote-unsent f)) (at (begin-message box :exit)))
        (put-number box (max 0 (min 255 (remote-exit-code f))) 1)
        (put-text box (or (remote-exit-words f) "") 1024)
        (end-message box at)))
    (setf (remote-closing f) t)
    (sb-thread:condition-broadcast (remote-moved f)))
  ;; what is left is sent while the client takes it; then both threads end with the socket
  (tuatara.runtime:join-within (remote-writer f) 2)
  (tuatara.io:shut-socket (remote-fd f))
  (tuatara.runtime:join-within (remote-writer f) 2)
  (tuatara.runtime:join-within (remote-reader f) 2)
  (sb-thread:with-mutex ((remote-lock f))
    (unless (shiftf (remote-closed f) t)
      (tuatara.runtime:ignoring-errors-reported ("closing a client's socket") (sb-posix:close (remote-fd f))))
    (setf (remote-ended f) t)
    (dolist (box (list (remote-unsent f) (remote-sending f)))
      (close-descriptors (mapcar #'cdr (shiftf (outbox-descriptors box) '())))))
  nil)

(defmethod frontend-frame ((f remote-frontend))
  (remote-frame f))

(defmethod (setf frontend-frame) (frame (f remote-frontend))
  (setf (remote-frame f) frame))

(defmethod frontend-size ((f remote-frontend))
  (let ((facts (remote-facts f)))
    (values (facts-width facts) (facts-height facts))))

(defmethod frontend-read-event ((f remote-frontend))
  (queue-pop (remote-queue f) nil))

(defmethod frontend-read-event-timeout ((f remote-frontend) seconds)
  (or (queue-pop (remote-queue f) seconds) (make-event :tick)))

(defmethod frontend-wake ((f remote-frontend))
  (queue-push (remote-queue f) (make-event :tick)))

(defmethod frontend-input-pending-p ((f remote-frontend))
  (queue-input-p (remote-queue f)))

(defmethod frontend-colour-count ((f remote-frontend))
  (facts-colours (remote-facts f)))

(defmethod frontend-description ((f remote-frontend))
  (facts-description (remote-facts f)))

(defmethod frontend-health ((f remote-frontend))
  (facts-health (remote-facts f)))

(defmethod frontend-images-p ((f remote-frontend))
  (facts-images (remote-facts f)))

(defmethod frontend-reorders-text-p ((f remote-frontend))
  (facts-reorders (remote-facts f)))

(defmethod frontend-cell-pixels ((f remote-frontend))
  (let ((facts (remote-facts f)))
    (and (facts-cell-width facts) (values (facts-cell-width facts) (facts-cell-height facts)))))

(defmethod frontend-desktop-style ((f remote-frontend))
  (facts-style (remote-facts f)))

(defmethod frontend-font-steps ((f remote-frontend))
  (facts-font-steps (remote-facts f)))

(defmethod (setf frontend-font-steps) (steps (f remote-frontend))
  (if (facts-window (remote-facts f))
      (or (ask-client f :font-steps steps) (facts-font-steps (remote-facts f)))
      steps))

(defmethod frontend-fonts ((f remote-frontend))
  (facts-fonts (remote-facts f)))

(defmethod frontend-font-faces ((f remote-frontend))
  (facts-font-faces (remote-facts f)))

(defmethod frontend-set-font ((f remote-frontend) name)
  (and (facts-window (remote-facts f)) (stringp name) (<= (tuatara.text:string-octet-length name) 1024)
       (ask-client f :font name)))

(defmethod frontend-clipboard ((f remote-frontend))
  (ask-client f :clipboard))

(defmethod frontend-primary-selection ((f remote-frontend))
  (ask-client f :primary))

(defun send-selection (remote which text)
  "Have REMOTE's client make TEXT its clipboard (WHICH 0) or its primary
selection (1); NIL when TEXT, with what goes with it, is longer than the
client takes (+MOST-TEXT+), or left out."
  (and (<= (tuatara.text:string-octet-length text) (- +most-text+ 16))
       (queue-message remote :selection (lambda (box) (put-number box which 1) (put-text box text)) :needed nil)))

(defmethod frontend-set-clipboard ((f remote-frontend) text)
  (send-selection f 0 text))

(defmethod frontend-set-primary-selection ((f remote-frontend) text)
  (send-selection f 1 text))

(defmethod frontend-set-title ((f remote-frontend) title)
  (when (<= (tuatara.text:string-octet-length title) 60000)
    (queue-message f :title (lambda (box) (put-text box title)) :needed nil)))

(defmethod frontend-toggle-state ((f remote-frontend) state)
  (and (facts-window (remote-facts f)) (member state '(:fullscreen :maximised))
       (queue-message f :toggle (lambda (box) (put-number box (if (eq state :fullscreen) 0 1) 1)) :needed nil)))

(defmethod frontend-raise ((f remote-frontend))
  (queue-message f :raise (lambda (box) (declare (ignore box))) :needed nil))

(defmethod frontend-scales-rows-p ((f remote-frontend))
  (facts-window (remote-facts f)))

(defmethod frontend-settings-changed ((f remote-frontend))
  ;; told again only when one of them has another value than the client was told
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
  ;; a client in a window opens another, a connection of its own attached with the word it is given; held to
  ;; its most windows, and to a few asked for at once
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
  (and (not (facts-window (remote-facts f))) (request-terminal (remote-request f))))

(defparameter *lend-seconds* 2
  "The seconds the editor waits for a client to give its terminal back to be
lent to a program before the program runs without it.")

(defmethod frontend-lend-terminal ((f remote-frontend) function &key words)
  ;; the client gives its terminal back and shows WORDS there; told to take it again whatever it answered
  (if (frontend-terminal-device f)
      (let ((lent (eql 1 (ask-client f :lend (shown-words (or words "")) *lend-seconds*))))
        (unwind-protect (funcall function)
          (when lent (tuatara.runtime:debug-report :remote "a client's terminal lent and taken again"))
          (queue-message f :take-back (lambda (box) (declare (ignore box))))))
      (funcall function)))

(defmethod frontend-begin-frame ((f remote-frontend))
  (unless (= (remote-faces-generation f) tuatara.display:*face-generation*)
    (send-faces f))
  (setf (outbox-fill (remote-painting f)) 0
        (fill-pointer (remote-kept-calls f)) 0
        (remote-frame-pictures f) '()))

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
                                  :text "This screen is too large to send whole: some rows show without colours or are left out"))))

(defmethod frontend-put-row ((f remote-frontend) row string &key face spans scales over parts)
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
  (when (and (facts-images (remote-facts f))
             (<= 1 (image-width image) 65535) (<= 1 (image-height image) 65535) (<= 3 (image-channels image) 4)
             (<= (* (image-width image) (image-height image) (image-channels image)) +most-picture-octets+)
             (= (length (image-pixels image)) (* (image-width image) (image-height image) (image-channels image)))
             (or (member (image-id image) (remote-pictures f))
                 (send-picture f image)))
    (push (image-id image) (remote-frame-pictures f))
    (put-picture-call (remote-painting f) image row column columns rows offset)))

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
        (sb-thread:condition-broadcast (remote-moved f))))))

;;; The client's side: a frontend painted as the editor says

(defstruct (remote-client (:constructor %make-remote-client (fd frontend inbox)) (:copier nil))
  "A client of an editor at the other end of the socket FD, painting what the
editor sends on FRONTEND and sending it FRONTEND's events: INBOX, what the
editor sends; OUT, the messages being sent; ARRIVED, what was read and is
not yet painted, newest first, under LOCK; NAMES, the face names by their
numbers; KEPT, each row as last painted; PICTURES, the pictures kept,
newest first; ENDED, true once the editor is gone, and EXIT-CODE, the code
it gave, EXIT-WORDS, what it said with it; FACTS, the facts last sent as
octets; READER, the thread reading; MOVED, signalled when something has
arrived or the editor is gone; SETTINGS, the settings the editor last said
\(TAKE-SETTINGS); ANOTHER, a function of the client and a word, which opens
another window attached with that word when the editor asks for one, or
says it does not (DECLINE-WINDOW), or NIL; PAINTED, true once the editor
has painted a frame on it."
  (fd -1 :type fixnum :read-only t)
  (frontend nil :read-only t)
  (inbox nil :read-only t)
  (out (make-outbox) :read-only t)
  (lock (sb-thread:make-mutex :name "remote client") :read-only t)
  (moved (sb-thread:make-waitqueue :name "remote client") :read-only t)
  (exit-words nil)
  (arrived '())
  (names (make-array (1+ +most-names+) :initial-element nil) :read-only t)
  (kept (make-array +most-rows+ :initial-element nil) :read-only t)
  (pictures '())
  (ended nil)
  (exit-code nil)
  (facts (make-outbox) :read-only t)
  (reader nil)
  (settings '())
  (another nil)
  (painted nil))

(setf (documentation '%make-remote-client 'function) "A REMOTE-CLIENT of the editor at FD painting on FRONTEND."
      (documentation 'remote-client-p 'function) "True when OBJECT is a REMOTE-CLIENT."
      (documentation 'remote-client-fd 'function) "The socket to the editor, a file descriptor."
      (documentation 'remote-client-frontend 'function) "The frontend CLIENT paints on and reads events from."
      (documentation 'remote-client-inbox 'function) "What the editor sends CLIENT, as it is read."
      (documentation 'remote-client-out 'function) "Where CLIENT's messages are written to be sent."
      (documentation 'remote-client-lock 'function) "Held while what CLIENT has read and not painted, or its end, change."
      (documentation 'remote-client-arrived 'function) "What CLIENT has read and not yet painted, newest first."
      (documentation 'remote-client-names 'function) "The face names the editor gave CLIENT, by their numbers."
      (documentation 'remote-client-kept 'function) "Each row as CLIENT last painted it, by row."
      (documentation 'remote-client-pictures 'function) "The pictures CLIENT keeps, newest first."
      (documentation 'remote-client-ended 'function) "True once the editor CLIENT paints for is gone."
      (documentation 'remote-client-exit-code 'function) "The code the editor told CLIENT to exit with, or NIL."
      (documentation 'remote-client-facts 'function) "The facts CLIENT last sent of its screen, as octets."
      (documentation 'remote-client-reader 'function) "The thread reading what the editor sends CLIENT."
      (documentation 'remote-client-moved 'function) "Signalled when something has arrived for CLIENT or its editor is gone."
      (documentation 'remote-client-exit-words 'function) "What the editor said as it told CLIENT to exit, or NIL."
      (documentation 'remote-client-settings 'function) "The settings CLIENT's editor last said, each (NAME . VALUE)."
      (documentation 'remote-client-another 'function)
      "The function of CLIENT and a word that opens another window for CLIENT's editor, or NIL."
      (documentation 'remote-client-painted 'function)
      "True once CLIENT's editor has painted a frame on it: a window asked for and let go before is not.")

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

(defun greet-editor (fd &key (take t))
  "Read the hello of the editor at the other end of the new local socket FD
and the settings it shows its clients by (*DISPLAY-SETTINGS*), and with TAKE
take them here, before a client's screen opens (not for another window of a
client that took them already): what was read, for MAKE-REMOTE-CLIENT.
Signals REMOTE-REFUSED when the editor is of another build, refuses the
client or says nothing it should; FD is then the caller's to close."
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

(defun make-remote-client (fd frontend &key (request (make-request)) standard-input greeted another)
  "A client painting on FRONTEND, started, what the editor at the other end
of the local socket FD sends, once both have said hello and the client what
its screen is and what it asks for, REQUEST; STANDARD-INPUT, a descriptor
passed with it to be read into a buffer (REQUEST-STANDARD-INPUT then true),
is closed here once passed. GREETED is what GREET-EDITOR read of FD, when it
was called before FRONTEND was made, else it is called here. ANOTHER, a
function of the client and a word, opens another window when the editor
asks for one (REMOTE-CLIENT-ANOTHER). Signals REMOTE-REFUSED when the editor is of
another build, refuses the client or says nothing it should; FD is then the
caller's to close."
  (destructuring-bind (inbox settings)
      (or greeted
          (handler-bind ((remote-refused (lambda (condition)
                                           (declare (ignore condition))
                                           (when standard-input (close-descriptors (list standard-input))))))
            (greet-editor fd)))
    (let ((client (%make-remote-client fd frontend inbox)))
      (setf (remote-client-settings client) settings
            (remote-client-another client) another)
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
\(:FACES :FRAME :TITLE :SELECTION :TOGGLE :RAISE :ASK :TAKE-BACK :SETTINGS
:ANOTHER), or NIL when reading it was all."
  (let ((names (remote-client-names client)))
    (ecase kind
      (:names (take-names inbox names) nil)
      (:faces (prog1 (cons :faces (loop repeat (take-count inbox 2 +most-names+ 19) collect (take-face inbox names)))
                (message-read inbox)))
      (:frame (cons :frame (take-frame inbox names (remote-client-kept client) (remote-client-pictures client))))
      ;; the picture kept, and the one the editor let go of to keep it let go of here too
      (:picture (multiple-value-bind (image let-go) (take-picture inbox)
                  (let ((pictures (remote-client-pictures client)))
                    (when let-go
                      (unless (find let-go pictures :key #'image-id)
                        (malformed "picture ~d let go of, never kept" let-go))
                      (setf pictures (remove let-go pictures :key #'image-id)))
                    (when (>= (length pictures) +most-pictures+)
                      (malformed "a picture past the ~d kept" +most-pictures+))
                    (setf (remote-client-pictures client) (cons image pictures)))
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
        for face = (tuatara.display:find-face name)
        unless (and face (equal attributes (face-now face)))
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

(defun answer-editor (client request question argument)
  "Answer CLIENT's editor's question QUESTION, with ARGUMENT, numbered
REQUEST, from CLIENT's frontend."
  (let* ((frontend (remote-client-frontend client))
         (answer (ecase question
                   (:clipboard (frontend-clipboard frontend))
                   (:primary (frontend-primary-selection frontend))
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
done, and answer its questions, in the order they came."
  (let ((frontend (remote-client-frontend client))
        (items (sb-thread:with-mutex ((remote-client-lock client))
                 (reverse (shiftf (remote-client-arrived client) '())))))
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
               (:title (frontend-set-title frontend (second item)))
               (:selection (if (zerop (second item))
                               (frontend-set-clipboard frontend (third item))
                               (frontend-set-primary-selection frontend (third item))))
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
               (:ask (if (eq (third item) :lend)
                         (lend-terminal client (second item) (fourth item) later)
                         (apply #'answer-editor client (rest item))))))))

(defun decline-window (client word words)
  "Have CLIENT tell its editor the window it asked for by WORD is not opened,
why being WORDS: from any thread, said on the thread painting CLIENT."
  (sb-thread:with-mutex ((remote-client-lock client))
    (push (list :decline word words) (remote-client-arrived client))
    (sb-thread:condition-broadcast (remote-client-moved client)))
  (frontend-wake (remote-client-frontend client)))

(defun send-event (client event)
  "Send CLIENT's editor EVENT, read from CLIENT's frontend, when it is of a
kind the editor takes; a paste in pieces. NIL when it could not be sent."
  (or (not (sendable-event-p event))
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
  "End CLIENT's connection: its reading thread stopped and its socket closed."
  (tuatara.io:shut-socket (remote-client-fd client))
  (tuatara.runtime:join-within (remote-client-reader client) 2)
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

(defmethod frontend-start ((f null-frontend)) nil)
(defmethod frontend-stop ((f null-frontend)) nil)

(defmethod frontend-size ((f null-frontend))
  (values 80 24))

(defmethod frontend-colour-count ((f null-frontend))
  256)

(defmethod frontend-read-event ((f null-frontend))
  (queue-pop (null-queue f) nil))

(defmethod frontend-read-event-timeout ((f null-frontend) seconds)
  (or (queue-pop (null-queue f) seconds) (make-event :tick)))

(defmethod frontend-wake ((f null-frontend))
  (queue-push (null-queue f) (make-event :tick)))

(defmethod frontend-input-pending-p ((f null-frontend))
  (queue-input-p (null-queue f)))

(defmethod frontend-begin-frame ((f null-frontend)) nil)

(defmethod frontend-put-row ((f null-frontend) row string &key face spans scales over parts)
  (declare (ignore row string face spans scales over parts))
  nil)

(defmethod frontend-set-cursor ((f null-frontend) row column shape &optional colour)
  (declare (ignore row column shape colour))
  nil)

(defmethod frontend-end-frame ((f null-frontend)) nil)
