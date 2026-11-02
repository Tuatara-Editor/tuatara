;;;; tools/fuzz-remote.lisp --- Fuzz the messages of a remote frontend and its client, both ways.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; Loaded by tools/fuzz-remote.sh, which sets the limits. Records what an
;;;; editor sends a client over a few frames (names, every face, rows, kept
;;;; rows, a cursor, a title, a question) and what a client sends an editor
;;;; (a hello, its screen, every kind of event, pieces of a paste, answers),
;;;; spoils copies ROUNDS times from SEED (octets changed, cut, put in,
;;;; lengths and counts made huge, messages repeated, mixed and made up) and
;;;; reads each as the other end would: the editor's reader acting on what a
;;;; client sent (a question awaiting its answer), the client's reader and its
;;;; painting on a fake frontend. One round in fifty goes over a real socket
;;;; to a remote frontend or a client with their threads, now and then with a
;;;; descriptor passed, with the hello or after it, and must end its
;;;; connection within seconds. Every 5,000 rounds one of these hostile but
;;;; well-made rounds runs too: a paste past the most taken, more pictures in
;;;; a frame than are kept, a flood of events the editor does not take, an
;;;; editor busy for seconds with a client typing, a session of the editor
;;;; given sizes, mouse places and closings at their bounds, and attaches
;;;; asking for what may not be asked (variables past their count or
;;;; misnamed, paths not absolute or holding a NUL, a terminal that is no
;;;; device, descriptors passed with no standard input asked for, or too
;;;; many). A failure is an error other than a malformed message, a read
;;;; running past the deadline, a connection not ended, a hostile round not
;;;; answered as it should be, descriptors left open, or the heap growing
;;;; past the cap; the octets that made it are written to build/fuzz/ and the
;;;; run ends with exit 1.

(defpackage #:tuatara.fuzz-remote
  (:use #:cl)
  (:documentation "The remote frontend's fuzzer, run by tools/fuzz-remote.sh."))

(in-package #:tuatara.fuzz-remote)

(defvar *root* (pathname (concatenate 'string (string-right-trim "/" (sb-ext:posix-getenv "FUZZ_ROOT")) "/"))
  "The Tuatara source tree fuzzed, as tools/fuzz-remote.sh gives it.")

(load (merge-pathnames "tools/quicklisp.lisp" *root*))
(push *root* asdf:*central-registry*)
(ql:quickload '(:tuatara/defaults :tuatara/frontend-fake) :silent t)

(defun environment-integer (name default)
  "The whole number the environment variable NAME holds, or DEFAULT."
  (let ((text (sb-ext:posix-getenv name)))
    (or (and text (parse-integer text :junk-allowed t)) default)))

(defparameter *rounds* (environment-integer "FUZZ_ROUNDS" 200000)
  "How many spoilt streams of messages are read.")

(defparameter *seed* (environment-integer "FUZZ_SEED" 1)
  "The seed of the run's random choices: the same seed spoils alike.")

(defparameter *deadline* (environment-integer "FUZZ_DEADLINE" 10)
  "The most seconds one stream may take to be read before it counts as a hang.")

(defparameter *heap-cap* (* (environment-integer "FUZZ_HEAP_MB" 1024) 1024 1024)
  "The most octets of heap in use after a stream is read, once collected.")

(defparameter *random* (sb-ext:seed-random-state *seed*)
  "The run's random state.")

;; no init file is read, and so no package fetched or asked about
(setf tuatara.defaults::*config-file* #p"/nonexistent/init.lisp")

(tuatara.defaults:install-defaults (tuatara.command::make-editor (tuatara.frontend.fake:make-fake-frontend)))

;;; What each end says, recorded

(defun outbox-octets (box)
  "The octets of the messages BOX holds."
  (subseq (tuatara.frontend::outbox-octets box) 0 (tuatara.frontend::outbox-fill box)))

(defun editor-messages ()
  "What an editor sends a client over a few frames, as octets, from the
first message after its hello."
  (let ((remote (make-instance 'tuatara.frontend:remote-frontend :fd -1)))
    (dotimes (n 4)
      (tuatara.frontend:frontend-begin-frame remote)
      (tuatara.frontend:frontend-put-row remote 0 (format nil "frame ~d: kiwi tūī ~c" n (code-char #xDCE9))
                                         :face :mode-line :spans '((0 5 (:keyword :bold)) (6 10 :string)))
      (tuatara.frontend:frontend-put-row remote 1 "kept" :face '(:region nil :mode-line))
      (tuatara.frontend:frontend-put-row remote 2 "large" :scales '((0 2 1 0 3/2) (2 5 3/2 1/2 3/2) (5 6 0 0 0))
                                                           :over '((1 "list" ((0 4 :region)))))
      (tuatara.frontend:frontend-put-row remote 3 (format nil "parts ~d" (mod n 2)) :parts '(2 4 6))
      (tuatara.frontend:frontend-set-cursor remote (mod n 3) n (nth (mod n 4) '(:block :bar :underline :hidden))
                                            (nth (mod n 4) '(nil "#102030" :bright-red 200)))
      (tuatara.frontend:frontend-end-frame remote))
    (let ((box (tuatara.frontend::remote-unsent remote)))
      (flet ((message (kind write)
               (let ((at (tuatara.frontend::begin-message box kind)))
                 (funcall write box)
                 (tuatara.frontend::end-message box at))))
        (message :title (lambda (box) (tuatara.frontend::put-text box "Tuatara: kiwi")))
        (message :selection (lambda (box)
                              (tuatara.frontend::put-number box 1 1)
                              (tuatara.frontend::put-text box "chosen")))
        (message :toggle (lambda (box) (tuatara.frontend::put-number box 0 1)))
        (message :raise (lambda (box) (declare (ignore box))))
        (message :ask (lambda (box)
                        (tuatara.frontend::put-number box 7 4) (tuatara.frontend::put-number box 2 1)
                        (tuatara.frontend::put-text box "Mono 12")))
        (message :exit (lambda (box) (tuatara.frontend::put-number box 1 1))))
      (outbox-octets box))))

(defun client-messages ()
  "What a client sends an editor, as octets: its hello, its screen, events of
every kind, a paste in pieces and answers."
  (let ((box (tuatara.frontend::make-outbox)))
    (tuatara.frontend::put-hello box)
    (tuatara.frontend::put-facts-message
     box (tuatara.frontend.fake:make-fake-frontend
          :font-steps 1 :fonts '("Mono A" "Mono B") :cell-pixels '(8 16)
          :font-faces '(("Mono A" t (("Regular" "Mono A 11") ("Bold" "Mono A Bold 11")))))
     :attach
     (tuatara.frontend:make-request :directory "/home/kiwi/cases/" :terminal "/dev/pts/3"
                                    :environment '(("PATH" . "/usr/bin:/bin") ("LANG" . "mi_NZ.UTF-8") ("GIT_DIR" . "/x"))
                                    :files '(("/tmp/a b" 12 3) ("/tmp/tūī" nil nil)) :safe t :hex t :frames 3
                                    :diff '("/tmp/old" "/tmp/new") :merge '("/l" "/b" "/r" "/m") :wait t :stop t))
    (tuatara.frontend::put-facts-message box (tuatara.frontend.fake:make-fake-frontend :desktop-style :dark) :facts)
    (dolist (event (list (tuatara.frontend:make-event :key :key (tuatara.frontend:make-key #\a :ctrl t :meta t))
                         (tuatara.frontend:make-event :key :key (tuatara.frontend:make-key (code-char #x1F95D)))
                         (tuatara.frontend:make-event :key :key (tuatara.frontend:make-key :drag-mouse-1 :column 9 :row 3))
                         (tuatara.frontend:make-event :key :key (tuatara.frontend:make-key :f12 :shift t))
                         (tuatara.frontend:make-event :resize :width 90 :height 30)
                         (tuatara.frontend:make-event :drop :files '("/tmp/a b" "/tmp/c") :column 2 :row 2)
                         (tuatara.frontend:make-event :focus)
                         (tuatara.frontend:make-event :close)))
      (tuatara.frontend::put-event box event))
    (let ((paste (tuatara.frontend:make-event :paste :text "")))
      (tuatara.frontend::put-event box paste :text "first half, " :more t)
      (tuatara.frontend::put-event box paste :text "second half"))
    (tuatara.frontend::put-answer box 1 "clipboard text" :more t)
    (tuatara.frontend::put-answer box 1 " and more")
    (tuatara.frontend::put-answer box 2 3)
    (tuatara.frontend::put-answer box 3 nil)
    (let ((at (tuatara.frontend::begin-message box :detach)))
      (tuatara.frontend::end-message box at))
    (outbox-octets box)))

(defparameter *seeds* (list (cons :client (editor-messages)) (cons :editor (client-messages)))
  "Each side's seed: what it reads, as recorded.")

;;; Spoiling

(defun message-starts (octets)
  "Where each message of OCTETS begins, as their lengths say, in order."
  (loop with at = 0
        while (<= (+ at 5) (length octets))
        collect at
        do (incf at (+ 4 (tuatara.io:octets-integer octets at 4)))))

(defun spoil (octets)
  "A copy of OCTETS spoilt in one to four ways at random."
  (let ((out (copy-seq octets)))
    (dotimes (k (1+ (random 4 *random*)) out)
      (let ((starts (message-starts out)) (n (length out)))
        (when (zerop n) (return out))
        (setf out
              (case (random 9 *random*)
                ;; octets changed
                (0 (dotimes (i (1+ (random 8 *random*)) out) (setf (aref out (random n *random*)) (random 256 *random*))))
                ;; cut short
                (1 (subseq out 0 (random n *random*)))
                ;; octets put in
                (2 (let ((at (random n *random*)))
                     (concatenate 'tuatara.text:octets (subseq out 0 at)
                                  (map 'tuatara.text:octets (lambda (x) (declare (ignore x)) (random 256 *random*))
                                       (make-list (1+ (random 16 *random*))))
                                  (subseq out at))))
                ;; a length or a count made huge, or zero
                (3 (let ((at (random n *random*)))
                     (dotimes (i (min 4 (- n at)) out)
                       (setf (aref out (+ at i)) (if (zerop (random 3 *random*)) 0 255)))))
                ;; a message's length made wrong
                (4 (if starts
                       (let ((at (nth (random (length starts) *random*) starts)))
                         (tuatara.io:octets-put-integer out (random (ash 1 (+ 3 (random 29 *random*))) *random*) 4 :at at))
                       out))
                ;; a message's kind changed
                (5 (if starts
                       (let ((at (nth (random (length starts) *random*) starts)))
                         (setf (aref out (+ at 4)) (random 32 *random*))
                         out)
                       out))
                ;; a message repeated, or taken out
                (6 (if (cdr starts)
                       (let* ((i (random (1- (length starts)) *random*)) (start (nth i starts)) (end (nth (1+ i) starts)))
                         (if (zerop (random 2 *random*))
                             (concatenate 'tuatara.text:octets (subseq out 0 end) (subseq out start end) (subseq out end))
                             (concatenate 'tuatara.text:octets (subseq out 0 start) (subseq out end))))
                       out))
                ;; the other side's messages mixed in
                (7 (let* ((other (cdr (nth (random 2 *random*) *seeds*))) (at (random n *random*)))
                     (concatenate 'tuatara.text:octets (subseq out 0 at) other (subseq out at))))
                ;; made up whole
                (t (map 'tuatara.text:octets (lambda (x) (declare (ignore x)) (random 256 *random*))
                        (make-list (random 200 *random*))))))))))

;;; Reading

(defun read-as (octets side)
  "Read OCTETS as SIDE (:CLIENT or :EDITOR) reads what the other end sends,
acting on each message as it does, a client painting on a fake frontend."
  (let ((inbox (tuatara.frontend::make-inbox nil side :octets (copy-seq octets) :end (length octets))))
    (handler-case
        (if (eq side :client)
            (let* ((local (tuatara.frontend.fake:make-fake-frontend :cell-pixels '(8 16) :font-steps 0 :fonts '("Mono")))
                   (client (tuatara.frontend::%make-remote-client -1 local inbox)))
              (tuatara.frontend:frontend-start local)
              (loop for kind = (tuatara.frontend::read-message inbox)
                    while kind
                    do (let ((item (tuatara.frontend::take-from-editor client kind inbox)))
                         ;; the questions are answered over the socket, of which there is none
                         (when (and item (not (eq (first item) :ask)))
                           (push item (tuatara.frontend::remote-client-arrived client)))))
              (tuatara.frontend::paint-arrived client))
            (let ((remote (make-instance 'tuatara.frontend:remote-frontend :fd -1)))
              ;; no one takes the events here, so none is waited for; a question awaits its answer
              (setf (tuatara.frontend::remote-closing remote) t
                    (tuatara.frontend::remote-asked remote) (random 4 *random*)
                    (tuatara.frontend::remote-asked-question remote)
                    (svref tuatara.frontend::*questions* (random 4 *random*)))
              (loop for kind = (tuatara.frontend::read-message inbox)
                    while kind
                    do (case kind
                         (:hello (tuatara.frontend::take-hello inbox))
                         (:attach (tuatara.frontend::take-facts inbox :whole t)
                          (tuatara.frontend::take-request inbox)
                          (tuatara.frontend::message-read inbox))
                         (t (unless (tuatara.frontend::take-from-client remote kind inbox) (return)))))))
      (tuatara.frontend:malformed-message () nil))))

(defun over-a-socket (octets side descriptor with-hello)
  "Send OCTETS, spoilt, after a good hello, to a remote frontend (SIDE
:EDITOR) or a client (:CLIENT) with its threads, DESCRIPTOR (an open pipe's
end, or NIL) passed with them, or WITH-HELLO with the hello; true when its
connection ended, or the hello was refused, within five seconds."
  (multiple-value-bind (mine theirs) (tuatara.io:socket-pair)
    (let ((box (tuatara.frontend::make-outbox)) (vector (coerce octets 'tuatara.text:octets)))
      (when (and descriptor with-hello)
        (push (cons 0 (sb-posix:dup descriptor)) (tuatara.frontend::outbox-descriptors box))
        (setf descriptor nil))
      (flet ((send ()
               (tuatara.io:with-octets-sap (sap vector)
                 (tuatara.io:send-with-descriptors theirs sap (length vector) (and descriptor (list descriptor))))
               (tuatara.io:shut-socket theirs)))
        (unwind-protect
             (if (eq side :editor)
                 (progn
                   (tuatara.frontend::put-hello box)
                   (tuatara.frontend::put-facts-message box (tuatara.frontend.fake:make-fake-frontend) :attach)
                   (tuatara.frontend::send-outbox theirs box 5)
                   (let ((remote (handler-case (tuatara.frontend:make-remote-frontend mine)
                                   (tuatara.frontend:remote-refused () (sb-posix:close mine) nil))))
                     (or (null remote)
                         (progn
                           (send)
                           (prog1 (loop for event = (tuatara.frontend:queue-pop (tuatara.frontend::remote-queue remote) 5)
                                        while event
                                        thereis (eq (tuatara.frontend:event-kind event) :detached))
                             (tuatara.frontend:frontend-stop remote))))))
                 (let* ((local (tuatara.frontend.fake:make-fake-frontend))
                        (maker (sb-thread:make-thread (lambda () (tuatara.frontend:make-remote-client mine local)))))
                   (tuatara.frontend:frontend-start local)
                   (tuatara.frontend::put-hello box)
                   (tuatara.frontend::send-outbox theirs box 5)
                   (let ((client (sb-thread:join-thread maker)))
                     (send)
                     ;; the fake frontend reads no keys: the client's reading thread ends by itself
                     (let ((reader (tuatara.frontend::remote-client-reader client)))
                       (prog1 (not (eq :late (sb-thread:join-thread reader :timeout 5 :default :late)))
                         (tuatara.frontend::paint-arrived client)
                         (tuatara.frontend::close-remote-client client))))))
          (ignore-errors (sb-posix:close theirs)))))))

;;; Hostile rounds, well made

(defun open-descriptors ()
  "How many descriptors this process has open."
  (length (directory "/proc/self/fd/*" :resolve-symlinks nil)))

(defun peer-editor-frontend (&key (width 80) (height 24))
  "A remote frontend and the client's end of its socket, the client's hello
and screen said by hand: two values."
  (multiple-value-bind (mine theirs) (tuatara.io:socket-pair)
    (let ((box (tuatara.frontend::make-outbox)))
      (tuatara.frontend::put-hello box)
      (tuatara.frontend::put-facts-message
       box (tuatara.frontend.fake:make-fake-frontend :width width :height height :cell-pixels '(8 16)) :attach)
      (tuatara.frontend::send-outbox theirs box 5)
      (values (tuatara.frontend:make-remote-frontend mine) theirs))))

(defun send-events (fd events)
  "Send EVENTS over FD as a client does, a paste in pieces; true when all were sent."
  (let ((box (tuatara.frontend::make-outbox)))
    (dolist (event events)
      (if (eq (tuatara.frontend:event-kind event) :paste)
          (loop for (piece . more) on (tuatara.frontend::text-pieces (tuatara.frontend:event-text event))
                do (tuatara.frontend::put-event box event :text piece :more more))
          (tuatara.frontend::put-event box event)))
    (tuatara.frontend::send-outbox fd box 30)))

(defun paste-past-the-most ()
  "A paste a few octets past +MOST-TEXT+: NIL when it is left out and said, else what went wrong."
  (multiple-value-bind (remote theirs) (peer-editor-frontend)
    (unwind-protect
         (let* ((text (make-string (+ tuatara.frontend::+most-text+ 1 (random 100 *random*)) :initial-element #\p))
                (sender (sb-thread:make-thread
                         (lambda () (send-events theirs (list (tuatara.frontend:make-event :paste :text text))))))
                (event (tuatara.frontend:queue-pop (tuatara.frontend::remote-queue remote) 30)))
           (sb-thread:join-thread sender :timeout 30 :default nil)
           (unless (and event (eq (tuatara.frontend:event-kind event) :said))
             (format nil "a paste past the most came as ~s" (and event (tuatara.frontend:event-kind event)))))
      (tuatara.frontend:frontend-stop remote)
      (sb-posix:close theirs))))

(defun more-pictures-than-kept ()
  "Frames of 17 to 24 pictures each to a real client: NIL when it stays
connected and keeps at most +MOST-PICTURES+, else what went wrong."
  (let ((local (tuatara.frontend.fake:make-fake-frontend :cell-pixels '(8 16))))
    (tuatara.frontend:frontend-start local)
    (multiple-value-bind (mine theirs) (tuatara.io:socket-pair)
      (let* ((maker (sb-thread:make-thread (lambda () (tuatara.frontend:make-remote-client theirs local))))
             (remote (tuatara.frontend:make-remote-frontend mine))
             (client (sb-thread:join-thread maker))
             (images (loop repeat 40 collect (tuatara.frontend:make-image 2 2 3 (tuatara.text:make-octets 12)))))
        (unwind-protect
             (progn
               (dotimes (frame 3)
                 (tuatara.frontend:frontend-begin-frame remote)
                 (loop repeat (+ 17 (random 8 *random*)) for row from 0
                       do (tuatara.frontend:frontend-put-image remote (nth (random 40 *random*) images) row 0 1 1))
                 (tuatara.frontend:frontend-end-frame remote))
               (sleep 0.3)
               (cond ((tuatara.frontend::remote-client-ended client) "the client's connection ended")
                     ((> (length (tuatara.frontend::remote-client-pictures client)) tuatara.frontend::+most-pictures+)
                      "the client keeps more pictures than it may")))
          (tuatara.frontend:frontend-stop remote)
          (tuatara.frontend::close-remote-client client))))))

(defun flood ()
  "Events by the ten thousand the editor does not take: NIL when no more than
+MOST-WAITING-EVENTS+ are read ahead and the frontend stops at once, else what went wrong."
  (multiple-value-bind (remote theirs) (peer-editor-frontend)
    (let* ((key (tuatara.frontend:make-event :key :key (tuatara.frontend:make-key #\f)))
           (sender (sb-thread:make-thread (lambda () (send-events theirs (make-list 50000 :initial-element key))))))
      (sleep 0.5)
      (let ((read (tuatara.frontend::remote-received remote)) (began (get-internal-real-time)))
        (tuatara.frontend:frontend-stop remote)
        (sb-thread:join-thread sender :timeout 30 :default nil)
        (sb-posix:close theirs)
        (cond ((> read (1+ tuatara.frontend::+most-waiting-events+)) (format nil "~d events read ahead" read))
              ((> (tuatara.runtime:seconds-since began) 5) "the frontend took more than 5 s to stop"))))))

(defun editor-busy ()
  "An editor that takes no event for over two seconds while a client types:
NIL when the connection survives and every event comes once it takes them,
else what went wrong."
  (multiple-value-bind (remote theirs) (peer-editor-frontend)
    (let* ((count (+ 2000 (random 2000 *random*)))
           (key (tuatara.frontend:make-event :key :key (tuatara.frontend:make-key #\b)))
           (sender (sb-thread:make-thread (lambda () (send-events theirs (make-list count :initial-element key))))))
      (sleep (+ 2 (random 1.0 *random*)))
      (prog1 (cond ((tuatara.frontend::remote-ended remote) "the connection ended while the editor was busy")
                   ((/= count (loop repeat count
                                    count (tuatara.frontend:queue-pop (tuatara.frontend::remote-queue remote) 2)))
                    "events were lost")
                   ((tuatara.frontend::remote-ended remote) "the connection ended once events were taken"))
        (tuatara.frontend:frontend-stop remote)
        (sb-thread:join-thread sender :timeout 30 :default nil)
        (sb-posix:close theirs)))))

(defun editor-at-the-bounds ()
  "An editor session whose one screen is a client's, given sizes, mouse
places and closings at their bounds, then left: NIL when it ends so, else
what went wrong."
  (multiple-value-bind (remote theirs) (peer-editor-frontend)
    (let ((draining (sb-thread:make-thread
                     (lambda ()
                       (let ((octets (tuatara.text:make-octets 65536)))
                         (tuatara.io:with-octets-sap (sap octets)
                           (loop (let ((got (sb-unix:unix-read theirs sap 65536)))
                                   (unless (and got (plusp got)) (return))))))))))
      (flet ((key (base &rest keys) (tuatara.frontend:make-event :key :key (apply #'tuatara.frontend:make-key base keys))))
        (send-events theirs
                     (append (list (tuatara.frontend:make-event :resize :width 1 :height 1)
                                   (key #\a)
                                   (key :down-mouse-1 :column 65535 :row 65535)
                                   (tuatara.frontend:make-event :resize :width tuatara.frontend::+most-columns+
                                                                        :height tuatara.frontend::+most-rows+)
                                   (key :wheel-down :column 4095 :row 2047)
                                   (key :drag-mouse-1 :column 0 :row 65535)
                                   ;; pasted into *scratch*, before the file dropped is visited: nothing to save
                                   (tuatara.frontend:make-event :paste :text "kiwi")
                                   (tuatara.frontend:make-event :drop :files '("/nonexistent/fuzz") :column 65535 :row 0)
                                   (tuatara.frontend:make-event :focus)
                                   (tuatara.frontend:make-event :close)
                                   (tuatara.frontend:make-event :resize :width 80 :height 24))
                             (mapcar (lambda (spec) (tuatara.frontend:make-event :key :key (tuatara.frontend:spec-key spec)))
                                     (tuatara.frontend:parse-key-sequence "C-g C-x C-c"))))
        (let ((scratch (tuatara.buffer:find-buffer "*scratch*")))
          (when scratch (tuatara.buffer:kill-buffer scratch)))
        (tuatara.buffer:make-buffer "*scratch*" :text "")
        (prog1 (handler-case
                   (sb-ext:with-timeout 120
                     ;; no grammar is offered to be fetched, which would ask
                     (let ((tuatara.defaults::*grammars-not-now*
                             (mapcar (lambda (language)
                                       (string-downcase (symbol-name (tuatara.defaults::language-name language))))
                                     tuatara.defaults::*languages*)))
                       (tuatara.command:run-editor remote :setup #'tuatara.defaults:install-defaults
                                                          :buffer (tuatara.buffer:find-buffer "*scratch*")))
                     nil)
                 (sb-ext:timeout () "the session did not end in 120 s")
                 (error (condition) (format nil "the session failed: ~a" condition)))
          (sb-thread:join-thread draining :timeout 10 :default nil)
          (sb-posix:close theirs))))))

(defun put-raw-request (box &key (flags 1) (frames 1) (directory "/tmp") (terminal "") environment (files '())
                                  (diff '()) (merge '()) environment-count files-count)
  "Put in BOX the fields of a client's request as given, none held to its
bounds: ENVIRONMENT, (NAME . VALUE) pairs, and FILES, paths, after the counts
ENVIRONMENT-COUNT and FILES-COUNT say (their lengths unless given)."
  (tuatara.frontend::put-number box flags 2)
  (tuatara.frontend::put-number box frames 1)
  (tuatara.frontend::put-text box directory)
  (tuatara.frontend::put-text box terminal)
  (tuatara.frontend::put-number box (or environment-count (length environment)) 2)
  (loop for (name . value) in environment
        do (tuatara.frontend::put-text box name) (tuatara.frontend::put-text box value))
  (tuatara.frontend::put-number box (or files-count (length files)) 2)
  (dolist (path files)
    (tuatara.frontend::put-text box path) (tuatara.frontend::put-number box 0 4) (tuatara.frontend::put-number box 0 4))
  (dolist (paths (list diff merge))
    (tuatara.frontend::put-number box (length paths) 1)
    (dolist (path paths) (tuatara.frontend::put-text box path))))

(defparameter *hostile-attaches*
  `((:taken)
    (:taken :environment ,(loop for n below 128 collect (cons (format nil "GIT_V~d" n) "x")))
    (:refused :environment ,(loop for n below 129 collect (cons (format nil "GIT_V~d" n) "x")))
    (:refused :environment-count 65535 :environment (("PATH" . "/bin")))
    (:refused :environment (("path" . "/bin")))
    (:refused :environment (("9PATH" . "/bin")))
    (:refused :environment (("PATH" . ,(format nil "/bin~c/x" (code-char 0)))))
    (:refused :files ("relative.txt"))
    (:refused :files (""))
    (:refused :files (,(format nil "/tmp/a~cb" (code-char 0))))
    (:refused :files (,(concatenate 'string "/" (make-string 4096 :initial-element #\a))))
    (:refused :files-count 65535 :files ("/tmp/a"))
    (:refused :directory "relative")
    (:refused :directory "")
    (:refused :terminal "/tmp/pts")
    (:refused :terminal ,(format nil "/dev/pts/1~c" (code-char 0)))
    (:refused :frames 0)
    (:refused :frames 17)
    (:refused :flags 64)
    (:refused :diff ("/a"))
    (:refused :merge ("/a" "/b" "/c"))
    (:refused :descriptors 1)
    (:refused :flags 17)
    (:refused :flags 17 :descriptors 2)
    (:taken :flags 17 :descriptors 1))
  "Requests a client may send as it attaches, each (OUTCOME . KEYS): KEYS as
PUT-RAW-REQUEST takes them, and :DESCRIPTORS, how many pipe ends are passed
with the attach; OUTCOME, whether the editor takes the client or refuses it.")

(defun pipe-closed-everywhere-p (in)
  "True when the pipe read at IN has no writing end open anywhere (IN is closed after)."
  (unwind-protect
       (and (plusp (tuatara.io:poll-fd in 1 1000))
            (let ((octet (tuatara.text:make-octets 1)))
              (zerop (tuatara.io:with-octets-sap (sap octet) (sb-posix:read in sap 1)))))
    (sb-posix:close in)))

(defun hostile-attach (outcome keys)
  "Attach a client whose request is KEYS (*HOSTILE-ATTACHES*) to a remote
frontend: NIL when it is taken or refused as OUTCOME says, and every
descriptor passed is closed once the frontend is gone; else what went wrong."
  (let ((count (or (getf keys :descriptors) 0))
        (request-keys (loop for (key value) on keys by #'cddr unless (eq key :descriptors) append (list key value))))
    (multiple-value-bind (mine theirs) (tuatara.io:socket-pair)
      (let ((pipes (loop repeat count collect (multiple-value-list (sb-posix:pipe))))
            (box (tuatara.frontend::make-outbox)))
        (tuatara.frontend::put-hello box)
        (let ((at (tuatara.frontend::begin-message box :attach)))
          (tuatara.frontend::put-facts box (tuatara.frontend.fake:make-fake-frontend) :whole t)
          (apply #'put-raw-request box request-keys)
          (tuatara.frontend::end-message box at)
          ;; one at each octet from the attach's first: one passing carries one descriptor
          (loop for pipe in pipes for k from 0
                do (push (cons (+ at k) (sb-posix:dup (second pipe))) (tuatara.frontend::outbox-descriptors box))))
        (unwind-protect
             (let* ((sender (sb-thread:make-thread (lambda () (tuatara.frontend::send-outbox theirs box 5))))
                    (remote (handler-case (tuatara.frontend:make-remote-frontend mine)
                              ;; refused, the socket is the caller's to close
                              (tuatara.frontend:remote-refused () (sb-posix:close mine) nil)))
                    (got (if remote :taken :refused)))
               (sb-thread:join-thread sender :timeout 10 :default nil)
               (when remote
                 (let ((input (tuatara.frontend:request-standard-input (tuatara.frontend:remote-request remote))))
                   (when (integerp input) (sb-posix:close input)))
                 (tuatara.frontend:frontend-stop remote))
               (dolist (pipe pipes) (sb-posix:close (second pipe)))
               (cond ((not (eq got outcome)) (format nil "~(~a~), not ~(~a~), for ~s" got outcome keys))
                     ((notevery (lambda (pipe) (pipe-closed-everywhere-p (first pipe))) pipes)
                      (format nil "a descriptor passed left open for ~s" keys))))
          (sb-posix:close theirs))))))

(defun hostile-attaches ()
  "Every request of *HOSTILE-ATTACHES*, in turn: NIL when each is taken or
refused as it should be and leaves no descriptor open, else what went wrong."
  (loop for (outcome . keys) in *hostile-attaches*
        thereis (hostile-attach outcome keys)))

(defparameter *hostile-rounds* (list #'paste-past-the-most #'more-pictures-than-kept #'flood #'editor-busy
                                     #'editor-at-the-bounds #'hostile-attaches)
  "The hostile but well-made rounds, run in turn.")

;;; The run

(defun keep-failed (octets round)
  "Write OCTETS, which failed in ROUND, under build/fuzz/; its path."
  (let ((path (merge-pathnames (format nil "build/fuzz/remote-failed-~d-~d.bin" *seed* round) *root*)))
    (ensure-directories-exist path)
    (alexandria:write-byte-vector-into-file octets path :if-exists :supersede)
    path))

(defun fail (round octets side control &rest arguments)
  "Say ROUND failed reading OCTETS as SIDE, for the reason CONTROL and ARGUMENTS make, and end the run."
  (format t "~&FAILED round ~d (~(~a~)): ~? (octets in ~a)~%" round side control arguments (keep-failed octets round))
  (finish-output)
  (sb-ext:exit :code 1 :abort t))

(defun run ()
  "Read *ROUNDS* spoilt streams, each as the side its seed is read by."
  (let ((sockets 0) (hostile 0) (descriptors nil))
    (dotimes (round *rounds*)
      (when (zerop (mod (1+ round) 5000))
        (let* ((check (nth (mod hostile (length *hostile-rounds*)) *hostile-rounds*))
               (failure (handler-case (funcall check) (error (condition) (princ-to-string condition)))))
          (incf hostile)
          (when failure (fail round (tuatara.text:make-octets 0) :hostile "~a: ~a" check failure))))
      (destructuring-bind (side . seed) (nth (random 2 *random*) *seeds*)
        (let ((octets (spoil seed)))
          (if (zerop (mod round 50))
              (multiple-value-bind (in out) (if (zerop (random 3 *random*)) (sb-posix:pipe) (values nil nil))
                (incf sockets)
                (unless (over-a-socket octets side out (and out (zerop (random 2 *random*))))
                  (fail round octets side "its connection did not end"))
                (when in (sb-posix:close in) (sb-posix:close out)))
              (let ((began (get-internal-real-time)))
                (handler-case (sb-ext:with-timeout *deadline* (read-as octets side))
                  (sb-ext:timeout () (fail round octets side "no end in ~d s" *deadline*))
                  (error (condition) (fail round octets side "~a" condition)))
                (when (> (tuatara.runtime:seconds-since began) *deadline*)
                  (fail round octets side "~,1f s" (tuatara.runtime:seconds-since began))))))
        (when (zerop (mod (1+ round) 10000))
          (sb-ext:gc :full t)
          (when (> (sb-kernel:dynamic-usage) *heap-cap*)
            (fail round (tuatara.text:make-octets 0) side "~d octets of heap in use" (sb-kernel:dynamic-usage)))
          ;; descriptors passed are all closed: as many open as after the first ten thousand rounds
          (let ((open (open-descriptors)))
            (if descriptors
                (when (> open (+ descriptors 16))
                  (fail round (tuatara.text:make-octets 0) side "~d descriptors open, ~d before" open descriptors))
                (setf descriptors open)))
          (format t "~&~:d rounds~%" (1+ round))
          (finish-output))))
    (format t "~&~:d spoilt streams read, ~:d of them over a socket, ~:d hostile rounds, seed ~d~%"
            *rounds* sockets hostile *seed*)))

(run)
