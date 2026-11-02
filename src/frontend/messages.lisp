;;;; src/frontend/messages.lisp --- What a remote frontend and its client say to each other.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; The painter interface carried over a local socket. Each message is a
;;;; 4-octet length, a 1-octet kind and its fields in a fixed order: numbers
;;;; least significant octet first, text as UTF-8 after its length in octets.
;;;; This file is the one place the format is written and read. Nothing read
;;;; is trusted: every length, count and number is held to its bound before
;;;; it is used, and a message that breaks one is a MALFORMED-MESSAGE, which
;;;; ends that connection and nothing else.

(in-package #:tuatara.frontend)

;;; Kinds and bounds

(defconstant +protocol+ 1
  "The number of the messages' format, said first both ways: a client and an
editor of different numbers, or of different builds, do not talk.")

(defparameter *message-kinds*
  '((:hello 1 1024 1024)
    ;; from a client to the editor
    (:attach 2 1048576 nil) (:facts 3 65536 nil) (:event 4 65536 nil) (:answer 5 65536 nil) (:detach 6 64 nil)
    ;; from the editor to a client
    (:frame 16 nil 16777216) (:names 17 nil 1048576) (:faces 18 nil 1048576) (:picture 19 nil 64)
    (:title 20 nil 65536) (:selection 21 nil 16777216) (:toggle 22 nil 64) (:raise 23 nil 64)
    (:ask 24 nil 65536) (:exit 25 nil 64))
  "Each kind of message as (NAME CODE FROM-CLIENT FROM-EDITOR): its name, its
number on the wire, and the most octets it may take as the editor reads it
from a client and as a client reads it from the editor, NIL where it is never
sent that way.")

(defconstant +most-columns+ 4096 "The most columns a remote screen may have.")
(defconstant +most-rows+ 2048 "The most rows a remote screen may have.")
(defconstant +most-names+ 4096 "The most face names one connection numbers.")
(defconstant +most-characters+ 1048576
  "The most octets of a row's text, and so the most characters its spans reach into.")
(defconstant +most-place+ 65535 "The most a cell of a picture, or the cursor, is from the screen's or picture's first.")
(defconstant +most-text+ 16777216 "The most octets of text pasted, or of a clipboard, carried at once.")
(defconstant +text-piece+ 60000 "The most octets of text one message from a client carries; more goes in pieces.")
(defconstant +most-pictures+ 16 "The most pictures a client keeps; both ends let go of the oldest first.")
(defconstant +most-picture-octets+ 268435456 "The most octets of pixels of one picture.")

(define-condition malformed-message (error)
  ((reason :initarg :reason :reader malformed-message-reason
           :documentation "What was wrong with the message, in words."))
  (:report (lambda (condition stream)
             (format stream "A malformed message: ~a" (malformed-message-reason condition))))
  (:documentation "Signalled when what the other end of a connection sent breaks the format or one of its
bounds; the connection is ended."))

(setf (documentation 'malformed-message-reason 'function) "What was wrong with the message of CONDITION, in words.")

(defun malformed (control &rest arguments)
  "Signal a MALFORMED-MESSAGE whose reason FORMAT makes of CONTROL and ARGUMENTS."
  (error 'malformed-message :reason (apply #'format nil control arguments)))

(defun kind-code (kind)
  "The number on the wire of the message kind KIND (*MESSAGE-KINDS*)."
  (second (assoc kind *message-kinds*)))

;;; Writing

(defstruct (outbox (:constructor make-outbox (&optional (size 4096) &aux (octets (tuatara.text:make-octets size))))
                   (:copier nil))
  "Messages written and not yet sent: the first FILL octets of OCTETS, and
DESCRIPTORS, each (AT . FD), the open file descriptor (FD) passed with the
octet at AT."
  (octets nil :type tuatara.text:octets)
  (fill 0 :type fixnum)
  (descriptors '() :type list))

(setf (documentation 'make-outbox 'function) "An empty OUTBOX with room for SIZE octets before it grows."
      (documentation 'outbox-p 'function) "True when OBJECT is an OUTBOX."
      (documentation 'outbox-octets 'function) "The octets of BOX's messages, the first OUTBOX-FILL of them written."
      (documentation 'outbox-fill 'function) "How many octets of messages BOX holds."
      (documentation 'outbox-descriptors 'function) "The descriptors BOX passes, each (AT . FD), newest first.")

(declaim (inline outbox-room))
(defun outbox-room (box count)
  "Where the next COUNT octets go in BOX, room made for them."
  (declare (type outbox box) (type fixnum count))
  (let ((at (outbox-fill box)) (octets (outbox-octets box)))
    (when (> (+ at count) (length octets))
      (let ((larger (tuatara.text:make-octets (max (+ at count) (* 2 (length octets))))))
        (replace larger octets :end2 at)
        (setf (outbox-octets box) larger)))
    (setf (outbox-fill box) (+ at count))
    at))

(defun put-number (box value count)
  "Put the integer VALUE in COUNT octets of BOX, least significant first, a
negative one as two's complement."
  (declare (type outbox box) (type integer value) (type (integer 1 4) count))
  (let* ((at (outbox-room box count)) (octets (outbox-octets box)))
    (dotimes (k count)
      (setf (aref octets (+ at k)) (ldb (byte 8 (* 8 k)) value)))))

(defun put-octets (box octets &optional (start 0) (end (length octets)))
  "Put OCTETS from START below END in BOX as they are."
  (let ((at (outbox-room box (- end start))))
    (replace (outbox-octets box) octets :start1 at :start2 start :end2 end)))

(defun put-bounded (box value most)
  "Put VALUE, an integer, in 4 octets of BOX, as 0 when less and as MOST when
more: a place on the screen or in a row, which the reading end holds to MOST."
  (put-number box (max 0 (min most value)) 4))

(defun patch-length (box at)
  "Write over the 4 octets of BOX at AT how many octets BOX holds after them."
  (let ((length (- (outbox-fill box) at 4)) (octets (outbox-octets box)))
    (dotimes (k 4)
      (setf (aref octets (+ at k)) (ldb (byte 8 (* 8 k)) length)))))

(defun put-text (box string &optional most)
  "Put STRING in BOX as UTF-8 after its length in octets, in 4 octets; with
MOST, only as much of it as takes at most MOST octets, cut at a character, as
the reading end takes no more."
  (let ((at (outbox-room box 4)))
    (multiple-value-bind (octets end) (tuatara.text:put-string-octets string (outbox-octets box) (+ at 4))
      (setf (outbox-octets box) octets
            (outbox-fill box) (if (and most (> (- end at 4) most))
                                  (loop for cut downfrom (+ at 4 most) above (+ at 4)
                                        unless (tuatara.text:continuation-byte-p (aref octets cut)) return cut
                                        finally (return (+ at 4)))
                                  end))
      (patch-length box at))))

(defun begin-message (box kind)
  "Begin a message of KIND (*MESSAGE-KINDS*) at the end of BOX; where it
begins, for END-MESSAGE."
  (let ((at (outbox-fill box)))
    (put-number box 0 4)
    (put-number box (kind-code kind) 1)
    at))

(defun end-message (box at)
  "End the message of BOX begun at AT (BEGIN-MESSAGE), its length written."
  (patch-length box at))

(defun outbox-cut (box start end)
  "Take the octets of BOX from START below END out of it, those after moved
down, and the descriptors to be passed with them, closed."
  (let ((octets (outbox-octets box)) (gone (- end start)))
    (replace octets octets :start1 start :start2 end :end2 (outbox-fill box))
    (decf (outbox-fill box) gone)
    (when (outbox-descriptors box)
      (close-descriptors (loop for (at . fd) in (outbox-descriptors box) when (<= start at (1- end)) collect fd))
      (setf (outbox-descriptors box)
            (loop for (at . fd) in (outbox-descriptors box)
                  unless (<= start at (1- end)) collect (cons (if (>= at end) (- at gone) at) fd))))))

;;; Reading

(defstruct (inbox (:constructor make-inbox (fd side &key (octets (tuatara.text:make-octets 65536))
                                                      (end 0) deadline))
                  (:copier nil))
  "What the other end of a connection has sent, read from FD (NIL when
OCTETS hold all there is) by SIDE, :EDITOR for the editor reading a client
and :CLIENT for a client reading the editor: OCTETS from START below END not
yet read as messages; the message being read has its next field at AT and
ends at STOP. DESCRIPTORS are those passed and not yet taken; DEADLINE, an
internal real time past which waiting for more is given up (NIL: none)."
  (fd nil)
  (side :editor :type (member :editor :client))
  (octets (tuatara.text:make-octets 65536) :type tuatara.text:octets)
  (start 0 :type fixnum) (end 0 :type fixnum)
  (at 0 :type fixnum) (stop 0 :type fixnum)
  (descriptors '() :type list)
  (deadline nil))

(setf (documentation 'make-inbox 'function)
      "An INBOX reading what FD sends, for SIDE (:EDITOR or :CLIENT); with FD NIL, OCTETS below END are all there is."
      (documentation 'inbox-p 'function) "True when OBJECT is an INBOX."
      (documentation 'inbox-fd 'function) "The socket INBOX reads, or NIL when its octets are all there is."
      (documentation 'inbox-side 'function) "Who reads INBOX: :EDITOR, from a client, or :CLIENT, from the editor."
      (documentation 'inbox-octets 'function) "The octets INBOX has read."
      (documentation 'inbox-start 'function) "Where INBOX's octets not yet read as messages begin."
      (documentation 'inbox-end 'function) "Where INBOX's octets read so far end."
      (documentation 'inbox-at 'function) "Where the next field of the message being read is in INBOX's octets."
      (documentation 'inbox-stop 'function) "Where the message being read ends in INBOX's octets."
      (documentation 'inbox-descriptors 'function) "The descriptors passed to INBOX and not yet taken, oldest first."
      (documentation 'inbox-deadline 'function) "When INBOX gives up waiting for more, an internal real time, or NIL.")

(defun close-descriptors (descriptors)
  "Close each of DESCRIPTORS, open file descriptors."
  (dolist (fd descriptors)
    (tuatara.runtime:ignoring-errors-reported ("closing descriptor ~d" fd) (sb-posix:close fd))))

(defun fill-inbox (inbox wanted)
  "Read into INBOX until at least WANTED octets are there not yet read as
messages, waiting for them; NIL when the other end has gone first. More
descriptors passed than its side takes are malformed."
  (loop while (< (- (inbox-end inbox) (inbox-start inbox)) wanted)
        do (let ((fd (inbox-fd inbox)) (octets (inbox-octets inbox)))
             (unless fd (return-from fill-inbox nil))
             ;; what is not yet read moved to the front, and room made for the rest
             (when (plusp (inbox-start inbox))
               (replace octets octets :start2 (inbox-start inbox) :end2 (inbox-end inbox))
               (setf (inbox-end inbox) (- (inbox-end inbox) (inbox-start inbox)) (inbox-start inbox) 0))
             (when (< (length octets) wanted)
               (let ((larger (tuatara.text:make-octets wanted)))
                 (replace larger octets :end2 (inbox-end inbox))
                 (setf (inbox-octets inbox) (setf octets larger))))
             (multiple-value-bind (got descriptors cut errno)
                 (tuatara.io:with-octets-sap (sap octets)
                   (tuatara.io:receive-with-descriptors fd (sb-sys:sap+ sap (inbox-end inbox))
                                                        (- (length octets) (inbox-end inbox))))
               (when descriptors
                 (setf (inbox-descriptors inbox) (append (inbox-descriptors inbox) descriptors)))
               ;; the editor takes no descriptor from a client; a client one for each picture, a few at a
               ;; time: past that, every one passed is closed at once
               (when (or cut (> (length (inbox-descriptors inbox))
                                (if (eq (inbox-side inbox) :editor) 0 +most-pictures+)))
                 (close-descriptors (shiftf (inbox-descriptors inbox) '()))
                 (malformed "descriptors passed, more than are taken"))
               (cond ((plusp got) (incf (inbox-end inbox) got))
                     ((zerop got) (return-from fill-inbox nil))
                     ((= errno sb-posix:eintr))
                     ((= errno sb-posix:eagain)
                      (let ((deadline (inbox-deadline inbox)))
                        (when (and deadline (tuatara.runtime:deadline-passed-p deadline))
                          (malformed "nothing said in time"))
                        (tuatara.io:poll-fd fd 1 (if deadline
                                                     (max 1 (ceiling (* 1000 (tuatara.runtime:seconds-left deadline))))
                                                     1000))))
                     (t (return-from fill-inbox nil))))))
  t)

(defun read-message (inbox)
  "The kind of the next message INBOX has, read whole, its fields to be taken
in order with the TAKE- functions; NIL when the other end has gone. A message
of an unknown kind, of a kind never sent this way, or longer than its kind
may be, is malformed."
  (unless (fill-inbox inbox 5) (return-from read-message nil))
  (let* ((octets (inbox-octets inbox)) (start (inbox-start inbox))
         (length (loop for k below 4 sum (ash (aref octets (+ start k)) (* 8 k))))
         (code (aref octets (+ start 4)))
         (entry (find code *message-kinds* :key #'second))
         (most (and entry (if (eq (inbox-side inbox) :editor) (third entry) (fourth entry)))))
    (unless most (malformed "a message of kind ~d, which is not taken here" code))
    (unless (<= 1 length most) (malformed "a ~(~a~) message of ~d octets, past its ~d" (first entry) length most))
    (unless (fill-inbox inbox (+ 4 length)) (return-from read-message nil))
    (let ((start (inbox-start inbox)))
      (setf (inbox-at inbox) (+ start 5) (inbox-stop inbox) (+ start 4 length) (inbox-start inbox) (+ start 4 length)))
    (first entry)))

(defun take-place (inbox count)
  "Where the next COUNT octets of the message being read begin in INBOX's
octets, taken; malformed when the message ends before them."
  (let ((at (inbox-at inbox)))
    (unless (<= (+ at count) (inbox-stop inbox)) (malformed "a message cut short"))
    (setf (inbox-at inbox) (+ at count))
    at))

(defun take-number (inbox count)
  "The unsigned integer in the next COUNT octets of the message being read
from INBOX, least significant first."
  (let ((at (take-place inbox count)) (octets (inbox-octets inbox)))
    (loop for k below count sum (ash (aref octets (+ at k)) (* 8 k)))))

(defun take-signed (inbox count)
  "The signed integer in the next COUNT octets of the message being read from
INBOX, as two's complement."
  (let ((value (take-number inbox count)))
    (if (logbitp (1- (* 8 count)) value) (- value (ash 1 (* 8 count))) value)))

(defun take-bounded (inbox count least most what)
  "The unsigned integer in the next COUNT octets from INBOX, malformed unless
from LEAST to MOST; WHAT names it in the reason."
  (let ((value (take-number inbox count)))
    (unless (<= least value most) (malformed "~a ~d, past its bounds ~d to ~d" what value least most))
    value))

(defun take-text (inbox most)
  "The text next in the message being read from INBOX, at most MOST octets;
what is not UTF-8 in it read as raw bytes."
  (let* ((length (take-bounded inbox 4 0 most "a text of"))
         (at (take-place inbox length)))
    (tuatara.text:octets-string (inbox-octets inbox) :start at :end (+ at length))))

(defun take-count (inbox count most each)
  "A count in the next COUNT octets from INBOX, at most MOST, of things each
taking at least EACH octets of what is left of the message."
  (let ((value (take-bounded inbox count 0 most "a count of")))
    (unless (<= (* value each) (- (inbox-stop inbox) (inbox-at inbox)))
      (malformed "a count of ~d, more than the message holds" value))
    value))

(defun message-read (inbox)
  "Malformed unless the message being read from INBOX has no octets left."
  (unless (= (inbox-at inbox) (inbox-stop inbox))
    (malformed "~d octets left over at a message's end" (- (inbox-stop inbox) (inbox-at inbox)))))

;;; Saying hello

(defun put-hello (box)
  "Put a hello in BOX: the format's number and this build's version line."
  (let ((at (begin-message box :hello)))
    (put-number box +protocol+ 4)
    (put-text box (tuatara:version-line) 512)
    (end-message box at)))

(defun take-hello (inbox)
  "Read a hello from INBOX, whose message has begun; the other end's version
line, or NIL when its format or build differs from this one's, with what it
runs in words as a second value."
  (let ((protocol (take-number inbox 4))
        (line (take-text inbox 512)))
    (message-read inbox)
    (cond ((/= protocol +protocol+) (values nil (format nil "~a, with messages of format ~d" line protocol)))
          ((string/= line (tuatara:version-line)) (values nil line))
          (t line))))

;;; Facts: what a client's screen is, for the editor's questions

(defstruct (facts (:constructor make-facts ()) (:copier copy-facts))
  "What a client said of its screen, from which the editor's questions to a
remote frontend are answered: WINDOW, true for a window of its own, else a
terminal; WIDTH and HEIGHT in cells; COLOURS it can show; CELL-WIDTH and
CELL-HEIGHT in pixels, or NIL; IMAGES, true when it shows pictures; REORDERS,
true when it turns right-to-left text round itself; STYLE, the desktop's,
:dark, :light or NIL; FONT-STEPS, or NIL; DESCRIPTION, words for where it
shows the editor; HEALTH, (LABEL . TEXT) pairs; FONTS and FONT-FACES, as
FRONTEND-FONTS and FRONTEND-FONT-FACES give them."
  (window nil) (width 80 :type fixnum) (height 24 :type fixnum) (colours 256)
  (cell-width nil) (cell-height nil) (images nil) (reorders nil) (style nil) (font-steps nil)
  (description nil) (health '()) (fonts '()) (font-faces '()))

(setf (documentation 'make-facts 'function) "FACTS of an 80 by 24 terminal of 256 colours, as standard."
      (documentation 'copy-facts 'function) "A copy of FACTS, to be changed where the original is not."
      (documentation 'facts-p 'function) "True when OBJECT is FACTS."
      (documentation 'facts-window 'function) "True when the client's screen is a window of its own."
      (documentation 'facts-width 'function) "The client's screen's width in cells."
      (documentation 'facts-height 'function) "The client's screen's height in cells."
      (documentation 'facts-colours 'function) "How many colours the client can show."
      (documentation 'facts-cell-width 'function) "A cell's width in pixels, or NIL."
      (documentation 'facts-cell-height 'function) "A cell's height in pixels, or NIL."
      (documentation 'facts-images 'function) "True when the client shows pictures."
      (documentation 'facts-reorders 'function) "True when the client turns right-to-left text round itself."
      (documentation 'facts-style 'function) "The desktop's style the client knows, :dark, :light or NIL."
      (documentation 'facts-font-steps 'function) "The steps the client's font is scaled by, or NIL."
      (documentation 'facts-description 'function) "Where the client shows the editor, in words, or NIL."
      (documentation 'facts-health 'function) "What the client says of itself for the health check, (LABEL . TEXT) pairs."
      (documentation 'facts-fonts 'function) "The fixed-width font families the client can show."
      (documentation 'facts-font-faces 'function) "The font families and their variants the client can show.")

(defparameter *styles* #(nil :dark :light)
  "The desktop's styles, by their numbers in a message.")

(defun put-facts (box frontend &key whole)
  "Put what FRONTEND is (FACTS) in BOX, as fields of a message; with WHOLE,
its health and its fonts too."
  (multiple-value-bind (width height) (frontend-size frontend)
    (multiple-value-bind (cell-width cell-height) (frontend-cell-pixels frontend)
      (let ((steps (frontend-font-steps frontend)))
        (put-number box (if steps 1 0) 1)
        (put-number box (min +most-columns+ (max 1 width)) 2)
        (put-number box (min +most-rows+ (max 1 height)) 2)
        (put-number box (max 0 (min (frontend-colour-count frontend) 16777216)) 4)
        (put-number box (if cell-width (min 65535 (max 1 (round cell-width))) 0) 2)
        (put-number box (if cell-height (min 65535 (max 1 (round cell-height))) 0) 2)
        (put-number box (logior (if (frontend-images-p frontend) 1 0) (if (frontend-reorders-text-p frontend) 2 0)) 1)
        (put-number box (or (position (frontend-desktop-style frontend) *styles*) 0) 1)
        (put-number box (if steps (max -64 (min 64 steps)) 0) 2)
        (put-text box (or (frontend-description frontend) "") 1024)
        (when whole
          (let ((health (frontend-health frontend)))
            (put-number box (min 64 (length health)) 2)
            (loop for (label . text) in health repeat 64
                  do (put-text box (princ-to-string label) 1024) (put-text box (princ-to-string text) 4096)))
          (let ((fonts (frontend-fonts frontend)))
            (put-number box (min 8192 (length fonts)) 4)
            (loop for font in fonts repeat 8192 do (put-text box font 256)))
          (let ((families (frontend-font-faces frontend)))
            (put-number box (min 8192 (length families)) 4)
            (loop for (family fixed variants) in families repeat 8192
                  do (put-text box family 256)
                     (put-number box (if fixed 1 0) 1)
                     (put-number box (min 256 (length variants)) 2)
                     (loop for (name font) in variants repeat 256
                           do (put-text box name 256) (put-text box font 512)))))))))

(defun take-facts (inbox &key whole)
  "Read FACTS from INBOX, as PUT-FACTS put them, WHOLE as given there."
  (let ((facts (make-facts)))
    (setf (facts-window facts) (= 1 (take-bounded inbox 1 0 1 "a kind of screen"))
          (facts-width facts) (take-bounded inbox 2 1 +most-columns+ "a width")
          (facts-height facts) (take-bounded inbox 2 1 +most-rows+ "a height")
          (facts-colours facts) (take-bounded inbox 4 0 16777216 "a count of colours"))
    (let ((cell-width (take-number inbox 2)) (cell-height (take-number inbox 2)))
      (when (and (plusp cell-width) (plusp cell-height))
        (setf (facts-cell-width facts) cell-width (facts-cell-height facts) cell-height)))
    (let ((flags (take-bounded inbox 1 0 3 "flags")))
      (setf (facts-images facts) (logtest 1 flags) (facts-reorders facts) (logtest 2 flags)))
    (setf (facts-style facts) (svref *styles* (take-bounded inbox 1 0 (1- (length *styles*)) "a style")))
    (let ((steps (take-signed inbox 2)))
      (unless (<= -64 steps 64) (malformed "font steps ~d" steps))
      (when (facts-window facts) (setf (facts-font-steps facts) steps)))
    (let ((description (take-text inbox 1024)))
      (setf (facts-description facts) (and (plusp (length description)) description)))
    (when whole
      (setf (facts-health facts)
            (loop repeat (take-count inbox 2 64 8)
                  collect (cons (take-text inbox 1024) (take-text inbox 4096)))
            (facts-fonts facts)
            (loop repeat (take-count inbox 4 8192 4) collect (take-text inbox 256))
            (facts-font-faces facts)
            (loop repeat (take-count inbox 4 8192 7)
                  collect (list (take-text inbox 256) (= 1 (take-bounded inbox 1 0 1 "a fixed width"))
                                (loop repeat (take-count inbox 2 256 8)
                                      collect (list (take-text inbox 256) (take-text inbox 512)))))))
    facts))

;;; Events, from a client

(defparameter *special-keys*
  (coerce (remove-duplicates (remove-if-not #'keywordp (mapcar #'cdr *key-names*)) :from-end t) 'simple-vector)
  "The keys that are no character (KEY-BASE), by their numbers in a message.")

(defparameter *event-kinds* #(:key :resize :paste :drop :close :focus)
  "The kinds of event a client sends, by their numbers in a message.")

(defun sendable-event-p (event)
  "True when EVENT is of a kind a client sends (*EVENT-KINDS*), a key's base
a character or one of *SPECIAL-KEYS*."
  (and (find (event-kind event) *event-kinds*)
       (or (not (eq (event-kind event) :key))
           (characterp (key-base (event-key event)))
           (find (key-base (event-key event)) *special-keys*))
       t))

(defun put-event (box event &key text more)
  "Put an event message of EVENT in BOX, which SENDABLE-EVENT-P says can be
sent; a paste's TEXT, when given, in its place, MORE true when more of it
follows in the next."
  (let ((at (begin-message box :event)))
    (put-number box (position (event-kind event) *event-kinds*) 1)
    (ecase (event-kind event)
      (:key (let* ((key (event-key event)) (base (key-base key))
                   (column (key-column key)) (row (key-row key)) (place (and column row t)))
              (put-number box (logior (if (key-ctrl key) 1 0) (if (key-meta key) 2 0) (if (key-shift key) 4 0)
                                      (if (characterp base) 0 8) (if place 16 0))
                          1)
              (if (characterp base)
                  (put-number box (char-code base) 4)
                  (put-number box (position base *special-keys*) 1))
              (when place
                (put-number box (max 0 (min 65535 column)) 2)
                (put-number box (max 0 (min 65535 row)) 2))))
      (:resize (put-number box (min +most-columns+ (max 1 (event-width event))) 2)
               (put-number box (min +most-rows+ (max 1 (event-height event))) 2))
      (:paste (put-number box (if more 1 0) 1)
              (put-text box (or text (event-text event)) +text-piece+))
      (:drop (put-number box (max 0 (min 65535 (or (event-column event) 0))) 2)
             (put-number box (max 0 (min 65535 (or (event-row event) 0))) 2)
             (let ((files (event-files event)))
               (put-number box (min 1024 (length files)) 2)
               (loop for file in files repeat 1024 do (put-text box (if (pathnamep file) (namestring file) file) 4096))))
      ((:close :focus)))
    (end-message box at)))

(defun take-event (inbox frame)
  "Read an event from INBOX, as PUT-EVENT put it, naming FRAME; a paste as
two values, its text's piece and true when more of it follows, the event
then made by the caller once it has all."
  (let ((kind (svref *event-kinds* (take-bounded inbox 1 0 (1- (length *event-kinds*)) "an event kind"))))
    (multiple-value-prog1
        (ecase kind
          (:key (let* ((flags (take-bounded inbox 1 0 31 "a key's flags"))
                       (base (if (logtest 8 flags)
                                 (svref *special-keys* (take-bounded inbox 1 0 (1- (length *special-keys*)) "a key"))
                                 (let ((code (take-bounded inbox 4 0 (1- char-code-limit) "a character")))
                                   (or (code-char code) (malformed "no character ~d" code)))))
                       (column (and (logtest 16 flags) (take-number inbox 2)))
                       (row (and (logtest 16 flags) (take-number inbox 2))))
                  (make-event :key :key (make-key base :ctrl (logtest 1 flags) :meta (logtest 2 flags)
                                                       :shift (logtest 4 flags) :column column :row row)
                                   :frame frame)))
          (:resize (make-event :resize :width (take-bounded inbox 2 1 +most-columns+ "a width")
                                       :height (take-bounded inbox 2 1 +most-rows+ "a height")
                                       :frame frame))
          (:paste (let ((more (= 1 (take-bounded inbox 1 0 1 "a paste's flag"))))
                    (values (take-text inbox +text-piece+) more)))
          (:drop (let* ((column (take-number inbox 2)) (row (take-number inbox 2))
                        (files (loop repeat (take-count inbox 2 1024 4) collect (take-text inbox 4096))))
                   (make-event :drop :files files :column column :row row :frame frame)))
          ((:close :focus) (make-event kind :frame frame)))
      (message-read inbox))))

;;; Answers, from a client

(defparameter *questions* #(:clipboard :primary :font :font-steps)
  "What the editor asks a client, by their numbers in a message: the text of
its clipboard or primary selection, the font it chose given a name, the font
steps it took given a number.")

(defun put-answer (box request answer &key more)
  "Put the answer to request number REQUEST in BOX: ANSWER, NIL, a text (a
piece of it, MORE true when more follows) or an integer."
  (let ((at (begin-message box :answer)))
    (put-number box request 4)
    (etypecase answer
      (null (put-number box 0 1))
      (string (put-number box 1 1) (put-number box (if more 1 0) 1) (put-text box answer +text-piece+))
      (integer (put-number box 2 1) (put-number box (max -64 (min 64 answer)) 2)))
    (end-message box at)))

(defun take-answer (inbox)
  "Read an answer from INBOX, as PUT-ANSWER put it: its request's number, the
answer, and true when it is a piece of text with more to follow."
  (let ((request (take-number inbox 4)))
    (multiple-value-prog1
        (ecase (take-bounded inbox 1 0 2 "an answer's kind")
          (0 (values request nil nil))
          (1 (let ((more (= 1 (take-bounded inbox 1 0 1 "an answer's flag"))))
               (values request (take-text inbox +text-piece+) more)))
          (2 (values request (take-signed inbox 2) nil)))
      (message-read inbox))))

;;; Face names, numbered for each connection

(defstruct (face-names (:constructor make-face-names ()) (:copier nil))
  "The numbers the messages of one connection give face names by: TABLE from
each name to its number, from 1 in the order the names were first given one
\(0 is no face); FRESH, the names numbered since they were last sent, newest
first."
  (table (make-hash-table :test 'eq) :read-only t)
  (count 0 :type fixnum)
  (fresh '() :type list))

(setf (documentation 'make-face-names 'function) "FACE-NAMES with no name numbered yet."
      (documentation 'face-names-p 'function) "True when OBJECT is FACE-NAMES."
      (documentation 'face-names-table 'function) "The number of each face name of NAMES, by name."
      (documentation 'face-names-count 'function) "How many face names NAMES has numbered."
      (documentation 'face-names-fresh 'function) "The face names NAMES numbered since last sent, newest first.")

(defun face-name-number (names name)
  "The number NAME, a face name, has in NAMES, given one when it has none; 0
for NIL, and for any name past the +MOST-NAMES+ first."
  (cond ((not (and name (symbolp name) (<= (tuatara.text:string-octet-length (symbol-name name)) 128))) 0)
        ((gethash name (face-names-table names)))
        ((< (face-names-count names) +most-names+)
         (push name (face-names-fresh names))
         (setf (gethash name (face-names-table names)) (incf (face-names-count names))))
        (t 0)))

(defun put-names (box names)
  "Put a message in BOX naming the face names NAMES numbered since it last
did, if any."
  (when (face-names-fresh names)
    (let ((at (begin-message box :names)) (fresh (reverse (face-names-fresh names))))
      (put-number box (length fresh) 2)
      (dolist (name fresh)
        (put-number box (gethash name (face-names-table names)) 2)
        (put-text box (symbol-name name) 128))
      (end-message box at)
      (setf (face-names-fresh names) '()))))

(defun take-names (inbox names)
  "Read the face names of a message from INBOX into NAMES, a vector of them
by number; each number is given a name once."
  (loop repeat (take-count inbox 2 +most-names+ 6)
        do (let ((number (take-bounded inbox 2 1 +most-names+ "a face name's number"))
                 (name (take-text inbox 128)))
             (when (svref names number) (malformed "the face name ~d given twice" number))
             (setf (svref names number) (intern name :keyword))))
  (message-read inbox))

(defun put-face-spec (box names spec)
  "Put the face spec SPEC, a face name, a list of them or NIL, in BOX, its
names as their NAMES numbers."
  (cond ((null spec) (put-number box 0 1))
        ((atom spec) (put-number box 1 1) (put-number box (face-name-number names spec) 2))
        (t (let ((count (min 255 (length spec))))
             (put-number box 2 1)
             (put-number box count 1)
             (loop for name in spec repeat count do (put-number box (face-name-number names name) 2))))))

(defun take-face-name (inbox names)
  "A face name read from INBOX by its number in NAMES, a vector of names."
  (let ((number (take-bounded inbox 2 0 +most-names+ "a face name's number")))
    (if (zerop number)
        nil
        (or (svref names number) (malformed "the face name ~d, never given" number)))))

(defun take-face-spec (inbox names)
  "A face spec read from INBOX, as PUT-FACE-SPEC put it."
  (ecase (take-bounded inbox 1 0 2 "a face spec")
    (0 nil)
    (1 (take-face-name inbox names))
    (2 (loop repeat (take-count inbox 1 255 2) collect (take-face-name inbox names)))))

;;; Colours, proportions and spans

(defun put-colour (box colour)
  "Put COLOUR, as faces take colours, in BOX; anything else as none."
  (let ((basic (and (keywordp colour) (position colour tuatara.display:*basic-colour-names*))))
    (cond (basic (put-number box 1 1) (put-number box basic 1))
          ((and (integerp colour) (<= 0 colour 255)) (put-number box 2 1) (put-number box colour 1))
          ((and (stringp colour) (<= (length colour) 64)) (put-number box 3 1) (put-text box colour))
          (t (put-number box 0 1)))))

(defun take-colour (inbox)
  "A colour read from INBOX, as PUT-COLOUR put it."
  (ecase (take-bounded inbox 1 0 3 "a colour")
    (0 nil)
    (1 (svref tuatara.display:*basic-colour-names*
              (take-bounded inbox 1 0 (1- (length tuatara.display:*basic-colour-names*)) "a colour")))
    (2 (take-number inbox 1))
    (3 (take-text inbox 256))))

(defun put-proportion (box value most)
  "Put VALUE, a rational from 0 to MOST whose denominator is at most 65535 (a
scale, or a shift in rows), in BOX; anything else as 0."
  (let ((value (if (and (rationalp value) (<= 0 value most) (<= (denominator value) 65535)) value 0)))
    (put-number box (numerator value) 4)
    (put-number box (denominator value) 2)))

(defun take-proportion (inbox most)
  "A rational read from INBOX, as PUT-PROPORTION put it, at most MOST."
  (let* ((numerator (take-number inbox 4))
         (denominator (take-bounded inbox 2 1 65535 "a denominator"))
         (value (/ numerator denominator)))
    (unless (<= value most) (malformed "a proportion of ~a, past ~d" value most))
    value))

(defun put-spans (box names spans)
  "Put SPANS, each (START END FACE), in BOX."
  (put-number box (min +most-characters+ (length spans)) 4)
  (loop for (start end face) in spans repeat +most-characters+
        do (put-bounded box start +most-characters+) (put-bounded box end +most-characters+)
           (put-face-spec box names face)))

(defun take-spans (inbox names)
  "Spans read from INBOX, as PUT-SPANS put them."
  (loop repeat (take-count inbox 4 +most-characters+ 9)
        collect (list (take-bounded inbox 4 0 +most-characters+ "a span's start")
                      (take-bounded inbox 4 0 +most-characters+ "a span's end")
                      (take-face-spec inbox names))))

;;; Faces, as the editor has them

(defparameter *underlines* #(nil t :wave :dotted :box)
  "The ways a face is underlined, by their numbers in a message.")

(defun put-face (box names face)
  "Put FACE, a face of TUATARA.DISPLAY, in BOX: its name, colours, flags,
underline, font, height and the face it inherits from, names as their NAMES
numbers."
  (put-number box (face-name-number names (tuatara.display:face-name face)) 2)
  (put-colour box (tuatara.display:face-fg face))
  (put-colour box (tuatara.display:face-bg face))
  (put-number box (logior (if (tuatara.display:face-bold face) 1 0) (if (tuatara.display:face-italic face) 2 0)
                          (if (tuatara.display:face-reverse face) 4 0) (if (tuatara.display:face-dim face) 8 0)
                          (if (tuatara.display:face-extend face) 16 0))
              1)
  (put-number box (or (position (tuatara.display:face-underline face) *underlines*) 1) 1)
  (put-colour box (tuatara.display:face-underline-colour face))
  (let ((font (tuatara.display:face-font face)))
    (put-text box (if (and (stringp font) (<= (tuatara.text:string-octet-length font) 256)) font "")))
  (put-proportion box (or (tuatara.display:face-height face) 0) 3)
  (put-number box (face-name-number names (tuatara.display:face-inherit face)) 2))

(defun take-face (inbox names)
  "A face read from INBOX, as PUT-FACE put it, as a list: its name, then its
attributes as SET-FACE-ATTRIBUTES takes them."
  (let ((name (or (take-face-name inbox names) (malformed "a face with no name")))
        (fg (take-colour inbox)) (bg (take-colour inbox))
        (flags (take-bounded inbox 1 0 31 "a face's flags"))
        (underline (svref *underlines* (take-bounded inbox 1 0 (1- (length *underlines*)) "an underline")))
        (underline-colour (take-colour inbox))
        (font (take-text inbox 256))
        (height (take-proportion inbox 3))
        (inherit (take-face-name inbox names)))
    (list name :fg fg :bg bg :bold (logtest 1 flags) :italic (logtest 2 flags) :reverse (logtest 4 flags)
               :dim (logtest 8 flags) :extend (logtest 16 flags) :underline underline :underline-colour underline-colour
               :font (and (plusp (length font)) font) :height (and (> height 1) (min height 3)) :inherit inherit)))

;;; A frame, painted: what the painter interface was called with between its beginning and its end

(defparameter *cursor-shapes* #(:block :bar :underline :hidden)
  "The cursor's shapes, by their numbers in a message.")

(defun put-row-call (box names row string face spans scales over parts)
  "Put in BOX the painting of ROW (FRONTEND-PUT-ROW with STRING, FACE, SPANS,
SCALES, OVER and PARTS)."
  (put-number box 1 1)
  (put-number box row 2)
  (put-text box string +most-characters+)
  (put-face-spec box names face)
  (put-spans box names spans)
  (put-number box (min 4096 (length scales)) 2)
  (loop for (start end scale shift height) in scales repeat 4096
        do (put-bounded box start +most-columns+) (put-bounded box end +most-columns+)
           (put-proportion box scale 3) (put-proportion box shift +most-rows+) (put-proportion box height 3))
  (put-number box (min 4096 (length over)) 2)
  (loop for (left text over-spans) in over repeat 4096
        do (put-bounded box left +most-columns+) (put-text box text +most-characters+) (put-spans box names over-spans))
  (put-number box (min 4096 (length parts)) 2)
  (loop for part in parts repeat 4096 do (put-bounded box part +most-columns+)))

(defun put-kept-row-call (box row)
  "Put in BOX the painting of ROW as the frame before painted it."
  (put-number box 2 1)
  (put-number box row 2))

(defun put-cursor-call (box row column shape colour)
  "Put in BOX the cursor placed (FRONTEND-SET-CURSOR with ROW, COLUMN, SHAPE and COLOUR)."
  (put-number box 3 1)
  (put-number box (max (- +most-place+) (min +most-place+ row)) 4)
  (put-number box (max (- +most-place+) (min +most-place+ column)) 4)
  (put-number box (or (position shape *cursor-shapes*) 0) 1)
  (put-colour box colour))

(defun put-picture-call (box image row column columns rows offset)
  "Put in BOX the picture IMAGE shown (FRONTEND-PUT-IMAGE with ROW, COLUMN,
COLUMNS, ROWS and OFFSET), by its number; its pixels go in a message of
their own (PUT-PICTURE)."
  (put-number box 4 1)
  (put-number box (image-id image) 4)
  (put-bounded box row +most-rows+)
  (put-bounded box column +most-columns+)
  (put-bounded box columns +most-columns+)
  (put-bounded box rows +most-rows+)
  (put-number box (if offset 1 0) 1)
  (when offset
    (put-bounded box (first offset) +most-place+)
    (put-bounded box (second offset) +most-place+)))

(defun take-row-call (inbox names)
  "A row's painting read from INBOX, as PUT-ROW-CALL put it: (ROW STRING
FACE SPANS SCALES OVER PARTS)."
  (list (take-bounded inbox 2 0 (1- +most-rows+) "a row")
        (take-text inbox +most-characters+)
        (take-face-spec inbox names)
        (take-spans inbox names)
        (loop repeat (take-count inbox 2 4096 26)
              collect (list (take-bounded inbox 4 0 +most-columns+ "a column")
                            (take-bounded inbox 4 0 +most-columns+ "a column")
                            (take-proportion inbox 3) (take-proportion inbox +most-rows+) (take-proportion inbox 3)))
        (loop repeat (take-count inbox 2 4096 12)
              collect (list (take-bounded inbox 4 0 +most-columns+ "a column")
                            (take-text inbox +most-characters+) (take-spans inbox names)))
        (loop repeat (take-count inbox 2 4096 4) collect (take-bounded inbox 4 0 +most-columns+ "a column"))))

(defun take-place-signed (inbox)
  "A signed place read from INBOX, as the cursor's row or column, held to
+MOST-PLACE+ either way."
  (let ((value (take-signed inbox 4)))
    (unless (<= (- +most-place+) value +most-place+) (malformed "a place of ~d" value))
    value))

(defun take-frame (inbox names kept pictures)
  "The calls of a frame read from INBOX, in order, each (:ROW ROW STRING FACE
SPANS SCALES OVER PARTS), (:CURSOR ROW COLUMN SHAPE COLOUR) or (:PICTURE
IMAGE ROW COLUMN COLUMNS ROWS OFFSET). KEPT, a vector of the rows painted
last by row, is brought up to date and gives each row painted as before;
PICTURES, a list of the pictures sent, gives each picture by its number."
  (loop until (= (inbox-at inbox) (inbox-stop inbox))
        collect (ecase (take-bounded inbox 1 1 4 "a call")
                  (1 (let ((call (take-row-call inbox names)))
                       (setf (svref kept (first call)) call)
                       (cons :row call)))
                  (2 (let ((row (take-bounded inbox 2 0 (1- +most-rows+) "a row")))
                       (cons :row (or (svref kept row) (malformed "row ~d painted as before, never painted" row)))))
                  (3 (list :cursor (take-place-signed inbox) (take-place-signed inbox)
                           (svref *cursor-shapes* (take-bounded inbox 1 0 (1- (length *cursor-shapes*)) "a shape"))
                           (take-colour inbox)))
                  (4 (let* ((id (take-number inbox 4))
                            (image (or (find id pictures :key #'image-id) (malformed "picture ~d, never sent" id)))
                            (row (take-bounded inbox 4 0 +most-rows+ "a picture's row"))
                            (column (take-bounded inbox 4 0 +most-columns+ "a picture's column"))
                            (columns (take-bounded inbox 4 0 +most-columns+ "a picture's columns"))
                            (rows (take-bounded inbox 4 0 +most-rows+ "a picture's rows"))
                            (offset (and (= 1 (take-bounded inbox 1 0 1 "an offset's flag"))
                                         (list (take-bounded inbox 4 0 +most-place+ "an offset")
                                               (take-bounded inbox 4 0 +most-place+ "an offset")))))
                       (list :picture image row column columns rows offset))))))

;;; Pictures

(defun put-picture (box image let-go)
  "Put a message in BOX sending IMAGE: its number, size and channels, its
pixels passed as a sealed file in memory with it, and the number of the
picture the client lets go of to keep it, LET-GO, or NIL for none."
  (let* ((pixels (image-pixels image))
         (octets (if (typep pixels 'tuatara.text:octets) pixels (coerce pixels 'tuatara.text:octets)))
         (fd (tuatara.io:memory-file "tuatara picture" octets :kept nil))
         (at (begin-message box :picture)))
    (push (cons at fd) (outbox-descriptors box))
    (put-number box (image-id image) 4)
    (put-number box (image-width image) 4)
    (put-number box (image-height image) 4)
    (put-number box (image-channels image) 1)
    (put-number box (if let-go 1 0) 1)
    (put-number box (or let-go 0) 4)
    (end-message box at)))

(defun take-picture (inbox)
  "A picture read from INBOX, as PUT-PICTURE put it, its pixels from the
first descriptor passed, and as a second value the number of the picture to
let go of, or NIL; malformed unless that file is sealed and holds the pixels
its size says, no more and no less."
  (let* ((id (take-number inbox 4))
         (width (take-bounded inbox 4 1 65535 "a picture's width"))
         (height (take-bounded inbox 4 1 65535 "a picture's height"))
         (channels (take-bounded inbox 1 3 4 "a picture's channels"))
         (letting-go (= 1 (take-bounded inbox 1 0 1 "a flag")))
         (let-go (let ((number (take-number inbox 4))) (and letting-go number)))
         (count (* width height channels)))
    (message-read inbox)
    (unless (<= count +most-picture-octets+) (malformed "a picture of ~d octets" count))
    (let ((fd (or (pop (inbox-descriptors inbox)) (malformed "a picture with no pixels passed"))))
      (unwind-protect
           (let ((size (tuatara.io:sealed-file-size fd)))
             (unless (eql size count) (malformed "a picture's pixels, ~a octets for ~d" size count))
             (let ((pixels (tuatara.text:make-octets count)))
               (unless (= count (tuatara.io:read-fd-at fd pixels 0)) (malformed "a picture's pixels cut short"))
               (values (%make-image width height channels pixels id) let-go)))
        (close-descriptors (list fd))))))
