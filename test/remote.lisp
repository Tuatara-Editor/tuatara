;;;; test/remote.lisp --- A frontend in another process: the painter interface as messages, and back.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.tests)

(define-test remote
  :description "A remote frontend sends every call of the painter interface over a local socket to a client,
which paints it on its own frontend as it would be painted directly, and sends back the events read there; a
client that does not read never holds the editor up; whatever the other end sends that breaks the messages'
format ends that connection and nothing more.")

;;; A client's screen: a fake frontend whose events wait in a queue

(defclass queued-fake-frontend (tuatara.frontend.fake:fake-frontend)
  ((typed :initform (tuatara.frontend:make-event-queue) :reader queued-typed
          :documentation "The events typed at the fake screen, waited for as a terminal's are."))
  (:documentation "A fake frontend whose events are waited for in a queue and which FRONTEND-WAKE wakes:
the screen of a client in the tests."))

(defmethod tuatara.frontend:frontend-read-event ((f queued-fake-frontend))
  (tuatara.frontend:queue-pop (queued-typed f) nil))

(defmethod tuatara.frontend:frontend-wake ((f queued-fake-frontend))
  (tuatara.frontend:queue-push (queued-typed f) (tuatara.frontend:make-event :tick)))

(defun type-at (local &rest events)
  "Type EVENTS at LOCAL, a QUEUED-FAKE-FRONTEND."
  (dolist (event events)
    (tuatara.frontend:queue-push (queued-typed local) event)))

(defun connect-remote (local &key (run t) queue)
  "A remote frontend and a client painting it on LOCAL, a fake frontend
started here, over a new local socket, once both have said hello; three
values: the remote frontend, its events going into QUEUE when given, the
client, and with RUN the thread running the client (RUN-REMOTE-CLIENT),
which returns the client's exit code."
  (tuatara.frontend:frontend-start local)
  (multiple-value-bind (mine theirs) (tuatara.io:socket-pair)
    (let* ((maker (sb-thread:make-thread (lambda () (tuatara.frontend:make-remote-client theirs local))))
           (remote (tuatara.frontend:make-remote-frontend mine :queue queue))
           (client (sb-thread:join-thread maker)))
      (values remote client
              (and run (sb-thread:make-thread (lambda () (tuatara.frontend:run-remote-client client))))))))

(defun client-done (runner)
  "What the client thread RUNNER returned, waiting ten seconds at most; :LATE when it has not ended."
  (sb-thread:join-thread runner :timeout 10 :default :late))

;;; Painting both ways at once

(defclass tee-frontend ()
  ((direct :initarg :direct :reader tee-direct
           :documentation "A fake frontend painted directly, whose events the editor reads.")
   (remote :initarg :remote :reader tee-remote
           :documentation "A remote frontend painted alongside."))
  (:documentation "A frontend painting each frame both on a fake frontend and through a remote one, read and
asked as the fake one is: the two then paint for the same calls."))

(defmethod tuatara.frontend:frontend-start ((f tee-frontend))
  (tuatara.frontend:frontend-start (tee-direct f)))

(defmethod tuatara.frontend:frontend-stop ((f tee-frontend))
  (tuatara.frontend:frontend-stop (tee-direct f))
  (tuatara.frontend:frontend-stop (tee-remote f)))

(defmethod (setf tuatara.frontend:frontend-frame) (frame (f tee-frontend))
  (setf (tuatara.frontend:frontend-frame (tee-remote f)) frame))

(defmethod tuatara.frontend:frontend-size ((f tee-frontend))
  (tuatara.frontend:frontend-size (tee-direct f)))

(defmethod tuatara.frontend:frontend-read-event ((f tee-frontend))
  (tuatara.frontend:frontend-read-event (tee-direct f)))

(defmethod tuatara.frontend:frontend-read-event-timeout ((f tee-frontend) seconds)
  (tuatara.frontend:frontend-read-event-timeout (tee-direct f) seconds))

(defmethod tuatara.frontend:frontend-input-pending-p ((f tee-frontend))
  (tuatara.frontend:frontend-input-pending-p (tee-direct f)))

(defmethod tuatara.frontend:frontend-colour-count ((f tee-frontend))
  (tuatara.frontend:frontend-colour-count (tee-direct f)))

(defmethod tuatara.frontend:frontend-images-p ((f tee-frontend))
  (tuatara.frontend:frontend-images-p (tee-direct f)))

(defmethod tuatara.frontend:frontend-cell-pixels ((f tee-frontend))
  (tuatara.frontend:frontend-cell-pixels (tee-direct f)))

(defmethod tuatara.frontend:frontend-clipboard ((f tee-frontend))
  (tuatara.frontend:frontend-clipboard (tee-direct f)))

(defmethod tuatara.frontend:frontend-begin-frame ((f tee-frontend))
  (tuatara.frontend:frontend-begin-frame (tee-direct f))
  (tuatara.frontend:frontend-begin-frame (tee-remote f)))

(defmethod tuatara.frontend:frontend-put-row ((f tee-frontend) row string &rest keys &key face spans scales over parts)
  (declare (ignore face spans scales over parts))
  (apply #'tuatara.frontend:frontend-put-row (tee-direct f) row string keys)
  (apply #'tuatara.frontend:frontend-put-row (tee-remote f) row string keys))

(defmethod tuatara.frontend:frontend-set-cursor ((f tee-frontend) row column shape &optional colour)
  (tuatara.frontend:frontend-set-cursor (tee-direct f) row column shape colour)
  (tuatara.frontend:frontend-set-cursor (tee-remote f) row column shape colour))

(defmethod tuatara.frontend:frontend-put-image ((f tee-frontend) image row column columns rows &optional offset)
  (tuatara.frontend:frontend-put-image (tee-direct f) image row column columns rows offset)
  (tuatara.frontend:frontend-put-image (tee-remote f) image row column columns rows offset))

(defmethod tuatara.frontend:frontend-end-frame ((f tee-frontend))
  (tuatara.frontend:frontend-end-frame (tee-direct f))
  (tuatara.frontend:frontend-end-frame (tee-remote f))
  ;; a client that keeps up: each frame taken to be sent before the next is painted, none left out for a newer
  (loop repeat 1000
        until (zerop (tuatara.frontend::outbox-fill (tuatara.frontend::remote-unsent (tee-remote f))))
        do (sleep 0.001)))

(defmethod tuatara.frontend:frontend-set-title ((f tee-frontend) title)
  (tuatara.frontend:frontend-set-title (tee-direct f) title)
  (tuatara.frontend:frontend-set-title (tee-remote f) title))

(defmethod tuatara.frontend:frontend-set-clipboard ((f tee-frontend) text)
  (tuatara.frontend:frontend-set-clipboard (tee-direct f) text)
  (tuatara.frontend:frontend-set-clipboard (tee-remote f) text))

(defun tee-session (queue &key (text "") cell-pixels)
  "Run an editor session on a scratch buffer holding TEXT, painting both on a
fake frontend and through a remote frontend on a client's fake one, alike,
showing pictures when CELL-PIXELS (a cell's size) is given; QUEUE, a function
of the direct fake, queues what is typed. Four values: the direct fake, the
client's fake, the remote frontend, and the client's exit code."
  (let ((direct (tuatara.frontend.fake:make-fake-frontend :width 40 :height 12 :cell-pixels cell-pixels))
        (local (make-instance 'queued-fake-frontend :width 40 :height 12 :cell-pixels cell-pixels)))
    (multiple-value-bind (remote client runner) (connect-remote local)
      (declare (ignore client))
      (let ((scratch (tuatara.buffer:find-buffer "*scratch*")))
        (when scratch (tuatara.buffer:kill-buffer scratch)))
      (tuatara.buffer:make-buffer "*scratch*" :text text)
      (setf tuatara.defaults::*command-history* '() tuatara.defaults::*pattern-history* '()
            tuatara.defaults::*search-lit* nil tuatara.defaults::*search-lit-shown* nil)
      (funcall queue direct)
      (without-grammar-questions
        (tuatara.command:run-editor (make-instance 'tee-frontend :direct direct :remote remote)
                                    :setup #'tuatara.defaults:install-defaults
                                    :buffer (tuatara.buffer:find-buffer "*scratch*")))
      (values direct local remote (client-done runner)))))

(defun painted (f)
  "Every frame fake frontend F painted, oldest first, each as a list of its
rows, their spans, their whole-row faces and its pictures, each picture as
its number, size, channels, pixels and place: lists EQUAL compares."
  (loop for screen in (reverse (tuatara.frontend.fake:fake-history f))
        for (spans . faces) in (reverse (tuatara.frontend.fake:fake-face-history f))
        for pictures in (reverse (tuatara.frontend.fake:fake-image-history f))
        collect (list (coerce screen 'list) (coerce spans 'list) (coerce faces 'list)
                      (loop for (image . place) in pictures
                            collect (list* (tuatara.frontend:image-id image) (tuatara.frontend:image-width image)
                                           (tuatara.frontend:image-height image)
                                           (tuatara.frontend:image-channels image)
                                           (coerce (tuatara.frontend:image-pixels image) 'list)
                                           place)))))

(define-test (remote paints-as-painted-directly)
  ;; a session typed, its every frame painted on a fake directly and through the remote one: the same, row for row
  (multiple-value-bind (direct local remote code)
      (tee-session (lambda (f)
                     (tuatara.frontend.fake:fake-queue-keys
                      f "i k i w i RET t u i RET k e a C-x 2 C-x o M-< C-s t u RET C-x 1"))
                   :text (format nil "ruru~%"))
    (is eql 0 code)
    (true (> (tuatara.frontend.fake:fake-frames direct) 10))
    (true (tuatara.frontend.fake:fake-frame-showing direct "kiwi"))
    (is = (tuatara.frontend.fake:fake-frames direct) (tuatara.frontend.fake:fake-frames local))
    (is equal (painted direct) (painted local))
    (is equal (tuatara.frontend.fake:fake-cursor direct) (tuatara.frontend.fake:fake-cursor local))
    (is eq (tuatara.frontend.fake:fake-cursor-shape direct) (tuatara.frontend.fake:fake-cursor-shape local))
    (is equal (tuatara.frontend.fake:fake-cursor-colour direct) (tuatara.frontend.fake:fake-cursor-colour local))
    (is equal (tuatara.frontend.fake:fake-titles direct) (tuatara.frontend.fake:fake-titles local))
    ;; rows painted as before went as such, not again in full
    (true (plusp (length (tuatara.frontend::remote-kept-calls remote))))))

(define-test (remote faces-taken-as-sent)
  ;; every face sent and taken again is left as it is; one the client lacks is made as sent
  (let ((names (tuatara.frontend::make-face-names)) (box (tuatara.frontend::make-outbox))
        (by-number (make-array (1+ tuatara.frontend::+most-names+) :initial-element nil))
        (generation tuatara.display:*face-generation*))
    (maphash (lambda (name face) (declare (ignore name)) (tuatara.frontend::put-face box names face))
             tuatara.display:*faces*)
    (maphash (lambda (name number) (setf (svref by-number number) name)) (tuatara.frontend::face-names-table names))
    (let* ((inbox (tuatara.frontend::make-inbox nil :client :octets (tuatara.frontend::outbox-octets box)
                                                          :end (tuatara.frontend::outbox-fill box)))
           (faces (progn (setf (tuatara.frontend::inbox-stop inbox) (tuatara.frontend::outbox-fill box))
                         (loop repeat (hash-table-count tuatara.display:*faces*)
                               collect (tuatara.frontend::take-face inbox by-number)))))
      (tuatara.frontend::take-faces faces)
      (is = generation tuatara.display:*face-generation*)
      (unwind-protect
           (progn (tuatara.frontend::take-faces (list (list* :remote-test-face (cdr (first faces)))))
                  (true (tuatara.display:find-face :remote-test-face))
                  (is equal (cdr (first faces))
                      (tuatara.frontend::face-now (tuatara.display:find-face :remote-test-face))))
        (remhash :remote-test-face tuatara.display:*faces*)))))

(define-test (remote pictures-as-painted-directly)
  ;; a picture shown: sent once, then painted by its number, as painted directly
  (with-test-directory (dir)
    (let ((file (merge-pathnames "photo.png" dir)) (start (get-internal-real-time)))
      (alexandria:write-byte-vector-into-file *tiny-png* file)
      (image-forget)
      (multiple-value-bind (direct local remote code)
          (tee-session (lambda (f)
                         (tuatara.frontend.fake:fake-queue-keys
                          f (format nil "C-x C-f C-a C-k ~a RET" (rimu-typed (namestring file))))
                         (labels ((check ()
                                    (if (and (some #'tuatara.defaults::image-draw-job-p tuatara.runtime:*jobs*)
                                             (< (- (get-internal-real-time) start) (* 20 internal-time-units-per-second)))
                                        (progn (sleep 0.02) (tuatara.frontend.fake:fake-queue-action f #'check))
                                        (tuatara.frontend.fake:fake-queue-keys f "j k"))))
                           (tuatara.frontend.fake:fake-queue-action f #'check)))
                       :cell-pixels '(10 20))
        (is eql 0 code)
        (true (tuatara.frontend.fake:fake-images direct))
        (true (> (count-if #'identity (tuatara.frontend.fake:fake-image-history direct)) 1))
        (is equal (painted direct) (painted local))
        (is = 1 (length (tuatara.frontend::remote-pictures remote)))))))

(define-test (remote every-painter-call-carried)
  ;; every call of the painter interface reaches the client's frontend as it was made, and the
  ;; editor's questions are answered from what the client said or asked of it
  (let ((local (make-instance 'queued-fake-frontend :width 30 :height 6 :cell-pixels '(8 16) :font-steps 0
                                                    :fonts '("Mono A" "Mono B") :desktop-style :dark
                                                    :font-faces '(("Mono A" t (("Regular" "Mono A 11"))))))
        (image (tuatara.frontend:make-image 2 1 3 (coerce #(255 0 0 0 255 0) 'tuatara.text:octets))))
    (multiple-value-bind (remote client runner) (connect-remote local)
      (declare (ignore client))
      (is equal '(30 6) (multiple-value-list (tuatara.frontend:frontend-size remote)))
      (is eql 256 (tuatara.frontend:frontend-colour-count remote))
      (is equal '(8 16) (multiple-value-list (tuatara.frontend:frontend-cell-pixels remote)))
      (true (tuatara.frontend:frontend-images-p remote))
      (is eq :dark (tuatara.frontend:frontend-desktop-style remote))
      (is eql 0 (tuatara.frontend:frontend-font-steps remote))
      (is equal '("Mono A" "Mono B") (tuatara.frontend:frontend-fonts remote))
      (is equal '(("Mono A" t (("Regular" "Mono A 11")))) (tuatara.frontend:frontend-font-faces remote))
      ;; asked of the client
      (setf (tuatara.frontend.fake:fake-clipboard local) "from elsewhere"
            (tuatara.frontend.fake:fake-primary local) "chosen elsewhere")
      (is equal "from elsewhere" (tuatara.frontend:frontend-clipboard remote))
      (is equal "chosen elsewhere" (tuatara.frontend:frontend-primary-selection remote))
      (is equal "Mono B 11" (tuatara.frontend:frontend-set-font remote "Mono B"))
      (is eql 2 (setf (tuatara.frontend:frontend-font-steps remote) 2))
      ;; painted
      (tuatara.frontend:frontend-begin-frame remote)
      (tuatara.frontend:frontend-put-row remote 0 "kiwi tūī" :face :mode-line
                                                             :spans '((0 4 :keyword) (5 8 (:string :bold))))
      (tuatara.frontend:frontend-put-row remote 1 "big" :scales '((0 3 3/2 1/2 3/2)) :over '((1 "x" ((0 1 :region)))))
      (tuatara.frontend:frontend-put-row remote 2 "parts" :face '(:region :mode-line) :parts '(2 4))
      (tuatara.frontend:frontend-put-image remote image 3 0 2 1 '(0 1))
      (tuatara.frontend:frontend-set-cursor remote 2 3 :bar "#ff8800")
      (tuatara.frontend:frontend-end-frame remote)
      (tuatara.frontend:frontend-set-title remote "Tuatara: kiwi")
      (tuatara.frontend:frontend-set-clipboard remote "copied")
      (tuatara.frontend:frontend-set-primary-selection remote "selected")
      (true (tuatara.frontend:frontend-toggle-state remote :fullscreen))
      (tuatara.frontend:frontend-raise remote)
      (setf (tuatara.frontend::remote-exit-code remote) 3)
      (tuatara.frontend:frontend-stop remote)
      (is eql 3 (client-done runner))
      (is = 1 (tuatara.frontend.fake:fake-frames local))
      (is equal "kiwi tūī" (tuatara.frontend.fake:fake-screen-line local 0))
      (is equal "bxg" (tuatara.frontend.fake:fake-screen-line local 1))
      (is equal '((0 4 :keyword) (5 8 (:string :bold))) (aref (tuatara.frontend.fake:fake-spans local) 0))
      (is eq :mode-line (aref (tuatara.frontend.fake:fake-row-faces local) 0))
      (is equal '(:region :mode-line) (aref (tuatara.frontend.fake:fake-row-faces local) 2))
      (is equal '((0 3 3/2 1/2 3/2)) (aref (tuatara.frontend.fake:fake-row-scales local) 1))
      (destructuring-bind ((shown row column columns rows offset)) (tuatara.frontend.fake:fake-images local)
        (is equal (list (tuatara.frontend:image-id image) 2 1 3 '(255 0 0 0 255 0))
            (list (tuatara.frontend:image-id shown) (tuatara.frontend:image-width shown)
                  (tuatara.frontend:image-height shown) (tuatara.frontend:image-channels shown)
                  (coerce (tuatara.frontend:image-pixels shown) 'list)))
        (is equal '(3 0 2 1 (0 1)) (list row column columns rows offset)))
      (is equal '(2 . 3) (tuatara.frontend.fake:fake-cursor local))
      (is eq :bar (tuatara.frontend.fake:fake-cursor-shape local))
      (is equal "#ff8800" (tuatara.frontend.fake:fake-cursor-colour local))
      (is equal '("Tuatara: kiwi") (tuatara.frontend.fake:fake-titles local))
      (is equal "copied" (tuatara.frontend.fake:fake-clipboard local))
      (is equal "selected" (tuatara.frontend.fake:fake-primary local))
      (is equal '(:fullscreen) (tuatara.frontend.fake:fake-toggles local))
      (is = 1 (tuatara.frontend.fake:fake-raised local)))))

(define-test (remote events-carried-back)
  ;; what is typed at the client reaches the editor as it was read there, each event naming its frame
  (let* ((local (make-instance 'queued-fake-frontend :width 30 :height 6))
         (long (with-output-to-string (out) (dotimes (i 9000) (write-string "tūī kiwi " out))))
         (typed (list (tuatara.frontend:make-event :key :key (tuatara.frontend:make-key #\x :ctrl t :meta t))
                      (tuatara.frontend:make-event :key :key (tuatara.frontend:make-key :f5 :shift t))
                      (tuatara.frontend:make-event :key :key (tuatara.frontend:make-key :down-mouse-1 :column 3 :row 2))
                      (tuatara.frontend:make-event :key :key (tuatara.frontend:make-key (code-char #x1F95D)))
                      (tuatara.frontend:make-event :resize :width 50 :height 10)
                      (tuatara.frontend:make-event :focus)
                      (tuatara.frontend:make-event :paste :text long)
                      (tuatara.frontend:make-event :drop :column 4 :row 1
                                                         :files (list "/tmp/a b.txt"
                                                                      (format nil "/tmp/~c" (code-char #xDCE9)))))))
    (multiple-value-bind (remote client runner) (connect-remote local)
      (declare (ignore client))
      (setf (tuatara.frontend:frontend-frame remote) :a-frame)
      ;; the window's close button, then the client gone: each closes the client's frame, never the editor
      (apply #'type-at local (append typed (list (tuatara.frontend:make-event :close)
                                                 (tuatara.frontend:make-event :quit))))
      (let ((got (loop for event = (tuatara.frontend::queue-pop (tuatara.frontend::remote-queue remote) 5)
                       while (and event (not (eq (tuatara.frontend:event-kind event) :detached)))
                       collect event))
            (gone (tuatara.frontend::queue-pop (tuatara.frontend::remote-queue remote) 5)))
        (is eq :detached (and gone (tuatara.frontend:event-kind gone)))
        (is eq :a-frame (and gone (tuatara.frontend:event-frame gone)))
        (flet ((seen (event)
                 (list (tuatara.frontend:event-kind event)
                       (let ((key (tuatara.frontend:event-key event)))
                         (and key (list (tuatara.frontend:key-spec key) (tuatara.frontend:key-column key)
                                        (tuatara.frontend:key-row key))))
                       (tuatara.frontend:event-width event) (tuatara.frontend:event-height event)
                       (tuatara.frontend:event-text event) (tuatara.frontend:event-files event)
                       (tuatara.frontend:event-column event) (tuatara.frontend:event-row event))))
          (is equal (mapcar #'seen typed) (mapcar #'seen got))
          (true (every (lambda (event) (eq :a-frame (tuatara.frontend:event-frame event))) got))))
      (is equal '(50 10) (multiple-value-list (tuatara.frontend:frontend-size remote)))
      (is = (1+ (length typed)) (tuatara.frontend::remote-received remote))
      ;; the client left: so said, and the frontend ends with nothing more to say
      (is eql nil (client-done runner))
      (true (tuatara.frontend::remote-ended remote))
      (tuatara.frontend:frontend-stop remote))))

(define-test (remote unanswered-questions-wait-no-longer)
  ;; a client that never answers: its clipboard is taken as none within a moment
  (let ((local (make-instance 'queued-fake-frontend)))
    (multiple-value-bind (remote client) (connect-remote local :run nil)
      (let ((began (get-internal-real-time)))
        (is eql nil (tuatara.frontend:frontend-clipboard remote))
        (true (< (tuatara.runtime:seconds-since began) 1)))
      (tuatara.frontend::close-remote-client client)
      (tuatara.frontend:frontend-stop remote))))

(defun raw-peer (fd &key (width 200) (height 50) (hello #'tuatara.frontend::put-hello))
  "Say what a client says first over the socket FD: HELLO (a function of the
box) and a WIDTH by HEIGHT fake screen."
  (let ((box (tuatara.frontend::make-outbox)))
    (funcall hello box)
    (tuatara.frontend::put-facts-message box (tuatara.frontend.fake:make-fake-frontend :width width :height height)
                                         :attach)
    (true (tuatara.frontend::send-outbox fd box 5))))

(define-test (remote frames-merged-for-a-client-not-reading)
  ;; a client that reads nothing: painting goes on, the frames not sent are replaced by newer ones, the
  ;; buffer stays bounded; once read, the last frame painted is there whole
  (multiple-value-bind (mine theirs) (tuatara.io:socket-pair)
    (raw-peer theirs)
    (let* ((remote (tuatara.frontend:make-remote-frontend mine))
           (rows (loop for n below 300
                       collect (loop for row below 50
                                     collect (format nil "~d.~d ~a" n row
                                                     (make-string 180 :initial-element
                                                                      (code-char (+ 945 (mod (+ n row) 20))))))))
           (began (get-internal-real-time)) (most-unsent 0))
      (dolist (frame rows)
        (tuatara.frontend:frontend-begin-frame remote)
        (loop for text in frame for row from 0
              do (tuatara.frontend:frontend-put-row remote row text :spans '((0 3 :keyword))))
        (tuatara.frontend:frontend-set-cursor remote 0 0 :block)
        (tuatara.frontend:frontend-end-frame remote)
        (setf most-unsent (max most-unsent (tuatara.frontend::outbox-fill (tuatara.frontend::remote-unsent remote)))))
      ;; never held up: 300 frames of 50 rows each sent whole, the socket full after a few
      (true (< (tuatara.runtime:seconds-since began) 5))
      (true (< most-unsent (+ tuatara.frontend::+most-unsent+ 100000)))
      (let* ((inbox (tuatara.frontend::make-inbox theirs :client
                                                  :deadline (tuatara.runtime:deadline-in 10)))
             (client (tuatara.frontend::%make-remote-client theirs nil inbox))
             (frames '()))
        (tuatara.frontend::read-hello inbox)
        (loop for kind = (tuatara.frontend::read-message inbox)
              for item = (and kind (tuatara.frontend::take-from-editor client kind inbox))
              when (eq (first item) :frame) do (push (rest item) frames)
              until (or (null kind)
                        (and frames (equal (third (first (first frames))) (first (car (last rows)))))))
        (true (< 0 (length frames) 300))
        (is equal (car (last rows))
            (loop for call in (first frames) when (eq (first call) :row) collect (third call))))
      (tuatara.frontend:frontend-stop remote)
      (sb-posix:close theirs))))

(define-test (remote refuses-another-build)
  ;; a client of another build, or of another format, is refused with both versions said
  (dolist (hello (list (lambda (box)
                         (let ((at (tuatara.frontend::begin-message box :hello)))
                           (tuatara.frontend::put-number box tuatara.frontend::+protocol+ 4)
                           (tuatara.frontend::put-text box "Tuatara 0.0.1 (elsewhere)")
                           (tuatara.frontend::end-message box at)))
                       (lambda (box)
                         (let ((at (tuatara.frontend::begin-message box :hello)))
                           (tuatara.frontend::put-number box (1+ tuatara.frontend::+protocol+) 4)
                           (tuatara.frontend::put-text box (tuatara:version-line))
                           (tuatara.frontend::end-message box at)))))
    (multiple-value-bind (mine theirs) (tuatara.io:socket-pair)
      (raw-peer theirs :hello hello)
      (let ((refused (handler-case (progn (tuatara.frontend:make-remote-frontend mine) nil)
                       (tuatara.frontend:remote-refused (condition) (tuatara.frontend:remote-refused-words condition)))))
        (true refused)
        (true (search (tuatara:version-line) refused)))
      (sb-posix:close mine)
      (sb-posix:close theirs))))

(defun send-raw (fd octets &optional descriptors)
  "Send OCTETS, a list, over the socket FD, DESCRIPTORS passed with them."
  (let ((vector (coerce octets 'tuatara.text:octets)))
    (tuatara.io:with-octets-sap (sap vector)
      (is = (length vector) (tuatara.io:send-with-descriptors fd sap (length vector) descriptors)))))

(defun ended-after (sent)
  "Whether a remote frontend whose client sends SENT, a function of the
client's socket, once both have said hello, ends its connection: true when
the editor is told no more events come within five seconds, and no error
escaped."
  (multiple-value-bind (mine theirs) (tuatara.io:socket-pair)
    (raw-peer theirs)
    (let ((remote (tuatara.frontend:make-remote-frontend mine)))
      (funcall sent theirs)
      (prog1 (loop for event = (tuatara.frontend::queue-pop (tuatara.frontend::remote-queue remote) 5)
                   while event
                   thereis (eq (tuatara.frontend:event-kind event) :detached))
        (tuatara.frontend:frontend-stop remote)
        (sb-posix:close theirs)))))

(define-test (remote malformed-ends-the-connection)
  ;; whatever breaks the format ends the connection and nothing else
  (dolist (octets '((1 0 0 0 99)                                  ; a kind never sent
                    (#x70 #x11 1 0 4)                              ; an event far past its bound
                    (4 0 0 0 4 0 8 200)                            ; a key of no number
                    (6 0 0 0 4 1 0 0 1 0)                          ; a size of no width
                    (2 0 0 0 1 7)                                  ; a hello again
                    (3 0 0 0 4 0 0)                                ; an event cut short
                    (0 0 0 0 4)))                                  ; a message of no octets
    (true (ended-after (lambda (fd) (send-raw fd octets))) "Not ended by ~a" octets))
  ;; one that goes halfway through a message: ended too
  (true (ended-after (lambda (fd) (send-raw fd '(9 0 0 0 4 0)) (tuatara.io:shut-socket fd))))
  ;; a descriptor passed where none is taken: ended, and the editor's copy closed
  (multiple-value-bind (in out) (sb-posix:pipe)
    (true (ended-after (lambda (fd) (send-raw fd '(2 0 0 0 4 5) (list out)))))
    (sb-posix:close out)
    (true (pipe-ended-p in))))

(defun pipe-ended-p (in)
  "True when the pipe whose reading end is IN has no writing end open
anywhere: every copy of it passed was closed. IN is closed after."
  (unwind-protect
       (and (plusp (tuatara.io:poll-fd in 1 1000))
            (let ((octet (tuatara.text:make-octets 1)))
              (zerop (tuatara.io:with-octets-sap (sap octet) (sb-posix:read in sap 1)))))
    (sb-posix:close in)))

(defun message-octets (kind write)
  "The octets of a message of KIND whose fields WRITE, a function of the box, writes."
  (let* ((box (tuatara.frontend::make-outbox)) (at (tuatara.frontend::begin-message box kind)))
    (funcall write box)
    (tuatara.frontend::end-message box at)
    (coerce (subseq (tuatara.frontend::outbox-octets box) 0 (tuatara.frontend::outbox-fill box)) 'list)))

(define-test (remote descriptors-passed-in-a-hello-closed)
  ;; a descriptor passed with a hello, to the editor or to a client, is closed whether the hello is taken or not
  (dotimes (i 3)
    (multiple-value-bind (in out) (sb-posix:pipe)
      (multiple-value-bind (mine theirs) (tuatara.io:socket-pair)
        (let ((box (tuatara.frontend::make-outbox)))
          (tuatara.frontend::put-hello box)
          (tuatara.frontend::put-facts-message box (tuatara.frontend.fake:make-fake-frontend) :attach)
          (push (cons 0 out) (tuatara.frontend::outbox-descriptors box))
          (tuatara.frontend::send-outbox theirs box 5))
        (true (handler-case (progn (tuatara.frontend:frontend-stop (tuatara.frontend:make-remote-frontend mine)) nil)
                (tuatara.frontend:remote-refused () t)))
        (true (pipe-ended-p in))
        (sb-posix:close mine) (sb-posix:close theirs))))
  (multiple-value-bind (in out) (sb-posix:pipe)
    (multiple-value-bind (mine theirs) (tuatara.io:socket-pair)
      (let ((local (tuatara.frontend.fake:make-fake-frontend))
            (box (tuatara.frontend::make-outbox)))
        (tuatara.frontend:frontend-start local)
        (let ((at (tuatara.frontend::begin-message box :hello)))
          (tuatara.frontend::put-number box tuatara.frontend::+protocol+ 4)
          (tuatara.frontend::put-text box (format nil "Tuatara 0.0.1~c[2J" #\Esc))
          (tuatara.frontend::end-message box at))
        (push (cons 0 out) (tuatara.frontend::outbox-descriptors box))
        (tuatara.frontend::send-outbox theirs box 5)
        (let ((refused (handler-case (progn (tuatara.frontend:make-remote-client mine local) nil)
                         (tuatara.frontend:remote-refused (condition) (tuatara.frontend:remote-refused-words condition)))))
          (true (search "Tuatara 0.0.1[2J" refused))
          (false (find #\Esc refused)))
        (true (pipe-ended-p in))
        (sb-posix:close mine) (sb-posix:close theirs)))))

(define-test (remote answers-of-the-kind-asked)
  ;; an answer that is not of the kind its question asks for ends the connection; none is taken from it
  (dolist (answer (list (lambda (box) (tuatara.frontend::put-answer box 1 5))
                        (lambda (box)
                          (tuatara.frontend::put-answer box 1 "abc" :more t)
                          (tuatara.frontend::put-answer box 1 7))))
    (multiple-value-bind (mine theirs) (tuatara.io:socket-pair)
      (raw-peer theirs)
      (let* ((remote (tuatara.frontend:make-remote-frontend mine))
             (asker (sb-thread:make-thread
                     (lambda () (let ((tuatara.frontend::*answer-seconds* 2))
                                  (tuatara.frontend::ask-client remote :clipboard))))))
        (loop repeat 200 until (tuatara.frontend::remote-asked remote) do (sleep 0.01))
        (let ((box (tuatara.frontend::make-outbox)))
          (funcall answer box)
          (tuatara.frontend::send-outbox theirs box 5))
        (is eq nil (sb-thread:join-thread asker))
        (true (ended-already-p remote))
        (tuatara.frontend:frontend-stop remote)
        (sb-posix:close theirs)))))

(defun ended-already-p (remote)
  "True when REMOTE's connection ends within five seconds, its frame said to be gone."
  (loop for event = (tuatara.frontend:queue-pop (tuatara.frontend::remote-queue remote) 5)
        while event
        thereis (eq (tuatara.frontend:event-kind event) :detached)))

(define-test (remote pastes-held-to-sixteen-megabytes)
  ;; a paste of exactly the most octets is taken; one octet more is left out whole, and said
  (dolist (extra '(0 1))
    (multiple-value-bind (mine theirs) (tuatara.io:socket-pair)
      (raw-peer theirs)
      (let* ((remote (tuatara.frontend:make-remote-frontend mine))
             (text (make-string (+ tuatara.frontend::+most-text+ extra) :initial-element #\k))
             (sender (sb-thread:make-thread
                      (lambda ()
                        (let ((box (tuatara.frontend::make-outbox)) (paste (tuatara.frontend:make-event :paste :text "")))
                          (loop for (piece . more) on (tuatara.frontend::text-pieces text)
                                do (tuatara.frontend::put-event box paste :text piece :more more))
                          (tuatara.frontend::send-outbox theirs box 30)))))
             (event (tuatara.frontend:queue-pop (tuatara.frontend::remote-queue remote) 30)))
        (sb-thread:join-thread sender)
        (if (zerop extra)
            (progn (is eq :paste (tuatara.frontend:event-kind event))
                   (is = tuatara.frontend::+most-text+ (length (tuatara.frontend:event-text event))))
            (progn (is eq :said (tuatara.frontend:event-kind event))
                   (true (search "left out" (tuatara.frontend:event-text event)))))
        (tuatara.frontend:frontend-stop remote)
        (sb-posix:close theirs)))))

(define-test (remote floods-wait-for-the-editor)
  ;; a client sending far more events than the editor takes is read only as fast as they are taken
  (multiple-value-bind (mine theirs) (tuatara.io:socket-pair)
    (raw-peer theirs)
    (let* ((remote (tuatara.frontend:make-remote-frontend mine))
           (sender (sb-thread:make-thread
                    (lambda ()
                      (let ((box (tuatara.frontend::make-outbox))
                            (key (tuatara.frontend:make-event :key :key (tuatara.frontend:make-key #\a))))
                        (dotimes (i 200000) (tuatara.frontend::put-event box key))
                        (tuatara.frontend::send-outbox theirs box 10))))))
      (sleep 1)
      (true (<= (tuatara.frontend::remote-received remote) (1+ tuatara.frontend::+most-waiting-events+)))
      ;; as the editor takes them, more are read
      (dotimes (i 100) (tuatara.frontend:queue-pop (tuatara.frontend::remote-queue remote) 1))
      (sleep 0.2)
      (true (> (tuatara.frontend::remote-received remote) tuatara.frontend::+most-waiting-events+))
      (let ((began (get-internal-real-time)))
        (tuatara.frontend:frontend-stop remote)
        (true (< (tuatara.runtime:seconds-since began) 5)))
      (sb-thread:join-thread sender :timeout 15 :default nil)
      (sb-posix:close theirs))))

(defun raw-editor (local &rest messages)
  "A client painting on LOCAL, a fake frontend, made over a new socket whose
other end, here, says hello as an editor and then sends MESSAGES (lists of
octets); two values: the client and the editor's end of the socket."
  (tuatara.frontend:frontend-start local)
  (multiple-value-bind (mine theirs) (tuatara.io:socket-pair)
    (let ((maker (sb-thread:make-thread (lambda () (tuatara.frontend:make-remote-client theirs local))))
          (box (tuatara.frontend::make-outbox)))
      (tuatara.frontend::put-hello box)
      (tuatara.frontend::send-outbox mine box 5)
      (let ((client (sb-thread:join-thread maker)))
        (dolist (octets messages) (send-raw mine octets))
        (values client mine)))))

(define-test (remote pictures-not-sealed-refused)
  ;; a client given a pipe, or a file in memory not sealed, as a picture's pixels ends the connection
  (dolist (make (list (lambda () (multiple-value-bind (in out) (sb-posix:pipe) (sb-posix:close out) in))
                      (lambda ()
                        (let ((fd (sb-alien:alien-funcall
                                   (sb-alien:extern-alien "memfd_create" (function sb-alien:int sb-alien:c-string
                                                                                   sb-alien:unsigned-int))
                                   "unsealed" 3)))
                          (sb-posix:ftruncate fd 12)
                          fd))))
    (let ((fd (funcall make)))
      (multiple-value-bind (client mine) (raw-editor (tuatara.frontend.fake:make-fake-frontend))
        (let ((box (tuatara.frontend::make-outbox)) (at nil))
          (setf at (tuatara.frontend::begin-message box :picture))
          (push (cons at fd) (tuatara.frontend::outbox-descriptors box))
          (dolist (n '((1 4) (2 4) (2 4) (3 1) (0 1) (0 4))) (apply #'tuatara.frontend::put-number box n))
          (tuatara.frontend::end-message box at)
          (tuatara.frontend::send-outbox mine box 5))
        (true (not (eq :late (sb-thread:join-thread (tuatara.frontend::remote-client-reader client)
                                                    :timeout 3 :default :late))))
        (true (tuatara.frontend::remote-client-ended client))
        (tuatara.frontend::close-remote-client client)
        (sb-posix:close mine)))))

(define-test (remote more-pictures-than-kept)
  ;; seventeen pictures in a frame: sixteen shown, the connection kept; the next frame shows the seventeenth,
  ;; the client letting go of the one the editor did, picture 0 too
  (let ((local (make-instance 'queued-fake-frontend :cell-pixels '(8 16)))
        ;; numbered from 0, as the session's first picture is
        (images (loop for id below 17 collect (tuatara.frontend::%make-image 2 2 3 (tuatara.text:make-octets 12) id))))
    (multiple-value-bind (remote client runner) (connect-remote local)
      (flet ((paint (pictures)
               (tuatara.frontend:frontend-begin-frame remote)
               (loop for image in pictures for row from 0
                     do (tuatara.frontend:frontend-put-image remote image row 0 1 1))
               (tuatara.frontend:frontend-end-frame remote)
               ;; taken to be sent before the next, so that the client paints both
               (loop repeat 1000
                     until (zerop (tuatara.frontend::outbox-fill (tuatara.frontend::remote-unsent remote)))
                     do (sleep 0.001))))
        (paint images)
        (paint (rest images)))
      (tuatara.frontend:frontend-stop remote)
      (is eql 0 (client-done runner))
      (destructuring-bind (newer older) (subseq (tuatara.frontend.fake:fake-image-history local) 0 2)
        (is = 16 (length older))
        (is equal (mapcar #'tuatara.frontend:image-id (rest images))
            (mapcar (lambda (shown) (tuatara.frontend:image-id (first shown))) newer)))
      (is = 16 (length (tuatara.frontend::remote-client-pictures client))))))

(define-test (remote client-descriptors-held-to-pictures)
  ;; more descriptors passed to a client than pictures it may be sent end the connection, every one closed
  (multiple-value-bind (client mine) (raw-editor (tuatara.frontend.fake:make-fake-frontend))
    (let ((ins '()))
      (dotimes (i 20)
        (multiple-value-bind (in out) (sb-posix:pipe)
          (push in ins)
          (let ((octet (tuatara.text:make-octets 1)))
            (tuatara.io:with-octets-sap (sap octet)
              (tuatara.io:send-with-descriptors mine sap 1 (list out))))
          (sb-posix:close out)))
      (true (not (eq :late (sb-thread:join-thread (tuatara.frontend::remote-client-reader client)
                                                  :timeout 3 :default :late))))
      ;; those taken closed by the client, those still on their way with its end of the socket
      (tuatara.frontend::close-remote-client client)
      (true (every #'pipe-ended-p ins))
      (sb-posix:close mine))))

(define-test (remote spoilt-messages-end-cleanly)
  ;; a session's messages both ways, cut short and spoilt: each read ends cleanly or as malformed
  (multiple-value-bind (to-client to-editor) (spoilable-messages)
    (let ((random (sb-ext:seed-random-state 7)) (failures '()))
      (dotimes (n 3000)
        (multiple-value-bind (octets side) (if (evenp n) (values to-client :client) (values to-editor :editor))
          (let ((spoilt (copy-seq octets)))
            (dotimes (k (1+ (random 4 random)))
              (setf (aref spoilt (random (length spoilt) random)) (random 256 random)))
            (let ((failure (read-spoilt (subseq spoilt 0 (random (1+ (length spoilt)) random)) side)))
              (when failure (push failure failures))))))
      (is equal '() (subseq failures 0 (min 3 (length failures)))))))

(define-test (remote places-held-to-their-bounds)
  ;; a frame naming a place far off the screen, a span far past its row's text or a scale past any face's
  ;; is malformed, never painted: painting it would take as much memory as the number says
  (flet ((frame-read (left start scale)
           ;; a frame of one row: a span from START, a part drawn SCALE times as large, a list from LEFT over it
           (let* ((box (tuatara.frontend::make-outbox)) (at (tuatara.frontend::begin-message box :frame)))
             (loop for (value count) in `((1 1) (0 2)) do (tuatara.frontend::put-number box value count))
             (tuatara.frontend::put-text box "row")
             (loop for (value count) in `((0 1) (1 4) (,start 4) (3 4) (0 1)
                                          (1 2) (0 4) (3 4) (,(numerator scale) 4) (,(denominator scale) 2)
                                          (0 4) (1 2) (1 4) (1 2)
                                          (1 2) (,left 4))
                   do (tuatara.frontend::put-number box value count))
             (tuatara.frontend::put-text box "list")
             (loop for (value count) in '((0 4) (0 2)) do (tuatara.frontend::put-number box value count))
             (tuatara.frontend::end-message box at)
             (let* ((octets (subseq (tuatara.frontend::outbox-octets box) 0 (tuatara.frontend::outbox-fill box)))
                    (inbox (tuatara.frontend::make-inbox nil :client :octets octets :end (length octets)))
                    (client (tuatara.frontend::%make-remote-client -1 nil inbox)))
               (handler-case (progn (tuatara.frontend::take-from-editor
                                     client (tuatara.frontend::read-message inbox) inbox)
                                    :read)
                 (tuatara.frontend:malformed-message (condition)
                   (tuatara.frontend:malformed-message-reason condition)))))))
    (is eq :read (frame-read 2 0 3/2))
    (dolist (case '((#xFF0000FF 0 3/2) (2 #xFFFFFF 3/2) (2 0 4000)))
      (let ((said (apply #'frame-read case)))
        (true (and (stringp said) (search "past" said)) "~a read as ~a" case said)))))

(defun spoilable-messages ()
  "The octets of what an editor sends a client in a short session (names,
faces, frames) and of what a client sends an editor (a hello, its screen,
events), as two values."
  (let ((remote (make-instance 'tuatara.frontend:remote-frontend :fd -1))
        (client (tuatara.frontend::make-outbox)))
    (setf (tuatara.frontend::remote-faces-generation remote) -1)
    (dotimes (n 3)
      (tuatara.frontend:frontend-begin-frame remote)
      (tuatara.frontend:frontend-put-row remote 0 (format nil "frame ~d" n) :face :mode-line
                                                                            :spans '((0 5 (:keyword :bold))))
      (tuatara.frontend:frontend-put-row remote 1 "kept" :scales '((0 2 3/2 0 3/2)) :over '((1 "x" ((0 1 :region))))
                                                         :parts '(1))
      (tuatara.frontend:frontend-set-cursor remote 1 1 :bar "#102030")
      (tuatara.frontend:frontend-end-frame remote))
    (tuatara.frontend::put-hello client)
    (tuatara.frontend::put-facts-message client (tuatara.frontend.fake:make-fake-frontend :font-steps 1 :fonts '("A"))
                                         :attach)
    (dolist (event (list (tuatara.frontend:make-event :key :key (tuatara.frontend:make-key #\a :ctrl t))
                         (tuatara.frontend:make-event :key :key (tuatara.frontend:make-key :wheel-up :column 1 :row 1))
                         (tuatara.frontend:make-event :resize :width 9 :height 9)
                         (tuatara.frontend:make-event :drop :files '("/tmp/x") :column 1 :row 1)
                         (tuatara.frontend:make-event :focus)))
      (tuatara.frontend::put-event client event))
    (tuatara.frontend::put-event client (tuatara.frontend:make-event :paste :text "pasted") :more t)
    (tuatara.frontend::put-answer client 1 "answer")
    (let ((unsent (tuatara.frontend::remote-unsent remote)))
      (values (subseq (tuatara.frontend::outbox-octets unsent) 0 (tuatara.frontend::outbox-fill unsent))
              (subseq (tuatara.frontend::outbox-octets client) 0 (tuatara.frontend::outbox-fill client))))))

(defun read-spoilt (octets side)
  "Read OCTETS as SIDE (:CLIENT or :EDITOR) reads what the other end sends,
painting what a client would on a fake frontend; NIL when that ended
cleanly or as malformed, else the error, in words."
  (let ((inbox (tuatara.frontend::make-inbox nil side :octets (copy-seq octets) :end (length octets))))
    (handler-case
        (progn
          (if (eq side :client)
              (let* ((local (tuatara.frontend.fake:make-fake-frontend))
                     (client (tuatara.frontend::%make-remote-client -1 local inbox)))
                (tuatara.frontend:frontend-start local)
                (loop for kind = (tuatara.frontend::read-message inbox)
                      while kind
                      do (let ((item (tuatara.frontend::take-from-editor client kind inbox)))
                           (when (and item (not (eq (first item) :ask)))
                             (push item (tuatara.frontend::remote-client-arrived client)))))
                (tuatara.frontend::paint-arrived client))
              (let ((remote (make-instance 'tuatara.frontend:remote-frontend :fd -1)))
                ;; no one takes the events here: none is waited for, and a question awaits its answer
                (setf (tuatara.frontend::remote-inbox remote) inbox
                      (tuatara.frontend::remote-closing remote) t
                      (tuatara.frontend::remote-asked remote) 1
                      (tuatara.frontend::remote-asked-question remote) :clipboard)
                (loop for kind = (tuatara.frontend::read-message inbox)
                      while kind
                      do (case kind
                           (:hello (tuatara.frontend::take-hello inbox))
                           (:attach (tuatara.frontend::take-facts inbox :whole t)
                            (tuatara.frontend::message-read inbox))
                           (t (unless (tuatara.frontend::take-from-client remote kind inbox) (return)))))))
          nil)
      (tuatara.frontend:malformed-message () nil)
      (error (condition) (princ-to-string condition)))))

(define-test (remote typed-at-the-client)
  ;; a session whose only screen is a client's: keys typed there edit the buffer, which shows there
  (let ((local (make-instance 'queued-fake-frontend :width 40 :height 8)))
    (multiple-value-bind (remote client runner) (connect-remote local)
      (declare (ignore client))
      (let ((scratch (tuatara.buffer:find-buffer "*scratch*")))
        (when scratch (tuatara.buffer:kill-buffer scratch)))
      (tuatara.buffer:make-buffer "*scratch*" :text "")
      (flet ((keys (keys)
               (mapcar (lambda (spec) (tuatara.frontend:make-event :key :key (tuatara.frontend:spec-key spec)))
                       (tuatara.frontend:parse-key-sequence keys))))
        (apply #'type-at local (append (keys "i k i w i RET t u i ESC")
                                       (list (tuatara.frontend:make-event :resize :width 30 :height 6))))
        ;; once what was typed shows at the client, the editor is left
        (sb-thread:make-thread
         (lambda ()
           (loop repeat 1000
                 until (and (tuatara.frontend.fake:fake-screen local)
                            (equal "tui" (tuatara.frontend.fake:fake-screen-line local 1))
                            (equal "" (tuatara.frontend.fake:fake-screen-line local 6)))
                 do (sleep 0.01))
           (apply #'type-at local (keys "C-x C-c")))))
      (let ((editor (without-grammar-questions
                      (tuatara.command:run-editor remote :setup #'tuatara.defaults:install-defaults
                                                         :buffer (tuatara.buffer:find-buffer "*scratch*")))))
        (is equal (format nil "kiwi~%tui")
            (tuatara.buffer:buffer-string (tuatara.display:window-buffer (tuatara.command:editor-window editor)))))
      (is eql 0 (client-done runner))
      (true (tuatara.frontend.fake:fake-frame-showing local "kiwi"))
      (true (tuatara.frontend.fake:fake-frame-showing local "tui"))
      ;; the last frame at the size the client last had: its mode line on row 4, its two last rows left blank
      (is equal '(30 6) (multiple-value-list (tuatara.frontend:frontend-size remote)))
      (true (search "TUATARA" (tuatara.frontend.fake:fake-screen-line local 4)))
      (is equal "" (tuatara.frontend.fake:fake-screen-line local 6)))))

(defparameter *remote-painting*
  '((tuatara.frontend:frontend-begin-frame 1) (tuatara.frontend:frontend-put-row 1)
    (tuatara.frontend:frontend-set-cursor 1) (tuatara.frontend:frontend-end-frame 1) (tuatara.frontend::send-faces -1))
  "The remote frontend's painting, each with 1 for its octets counted, -1 for
those left out (the faces sent in a frame's beginning).")

(defun remote-painting-garbage (count)
  "The octets the remote frontend's painting (*REMOTE-PAINTING*) made in a
session of COUNT keys typed, each frame drawn, the client's end of the socket
read and let go of."
  (multiple-value-bind (mine theirs) (tuatara.io:socket-pair)
    (raw-peer theirs :width 80 :height 24)
    (let* ((remote (tuatara.frontend:make-remote-frontend mine))
           (draining (sb-thread:make-thread
                      (lambda ()
                        (let ((octets (tuatara.text:make-octets 65536)))
                          (tuatara.io:with-octets-sap (sap octets)
                            (loop (let ((got (sb-unix:unix-read theirs sap 65536)))
                                    (unless (and got (plusp got)) (return)))))))))
           (octets 0) (counting nil))
      (let ((scratch (tuatara.buffer:find-buffer "*scratch*")))
        (when scratch (tuatara.buffer:kill-buffer scratch)))
      (tuatara.buffer:make-buffer "*scratch*" :text (format nil "~{~a~%~}" (loop repeat 30 collect "kiwi tūī ruru kea")))
      (dolist (spec (append (tuatara.frontend:parse-key-sequence "i")
                            (loop repeat count append (tuatara.frontend:parse-key-sequence "a DEL"))))
        (tuatara.frontend:queue-push (tuatara.frontend::remote-queue remote)
                                     (tuatara.frontend:make-event :key :key (tuatara.frontend:spec-key spec))))
      (tuatara.frontend:queue-push (tuatara.frontend::remote-queue remote) (tuatara.frontend:make-event :quit))
      ;; the faces sent when a theme changes them are left out: that is once, not for each key
      (loop for (name sign) in *remote-painting*
            do (let ((sign sign))
                 (sb-int:encapsulate name 'remote-garbage
                                     (lambda (function frontend &rest arguments)
                                       (if (and counting (typep frontend 'tuatara.frontend:remote-frontend))
                                           (let ((before (sb-ext:get-bytes-consed)))
                                             (multiple-value-prog1 (apply function frontend arguments)
                                               (incf octets (* sign (- (sb-ext:get-bytes-consed) before)))))
                                           (apply function frontend arguments))))))
      (unwind-protect
           (let ((tuatara.command::*frame-at-least-every* 0))
             (without-grammar-questions
               (tuatara.command:run-editor remote
                                           :setup #'tuatara.defaults:install-defaults
                                           :buffer (tuatara.buffer:find-buffer "*scratch*")
                                           :start (lambda (editor) (declare (ignore editor)) (setf counting t)))))
        (loop for (name) in *remote-painting* do (sb-int:unencapsulate name 'remote-garbage)))
      (sb-thread:join-thread draining :timeout 10 :default nil)
      (sb-posix:close theirs)
      octets)))

(define-test (remote paints-without-garbage-per-key)
  ;; the remote frontend's painting of a frame makes no garbage for each cell or row: what one more key
  ;; costs it, in octets, stays small (each key two frames of 24 rows; the first session warms the calls
  ;; up; the least of two sessions of each length, as the heap is counted in pages of 32 KB)
  (remote-painting-garbage 5)
  (let* ((few (min (remote-painting-garbage 100) (remote-painting-garbage 100)))
         (more (min (remote-painting-garbage 700) (remote-painting-garbage 700)))
         (per-key (/ (- more few) 600.0)))
    (true (< per-key 256) "~,1f octets a key" per-key)))

(define-test (remote client-gone-closes-its-frame)
  ;; an editor whose two frames are two clients': one client's window closed closes its frame alone, and
  ;; that client is told to end; the editor goes on in the other
  (let ((one (make-instance 'queued-fake-frontend :width 40 :height 8))
        (two (make-instance 'queued-fake-frontend :width 40 :height 8)))
    (multiple-value-bind (first-remote client-one runner-one) (connect-remote one)
      (declare (ignore client-one))
      (multiple-value-bind (second-remote client-two runner-two)
          (connect-remote two :queue (tuatara.frontend::remote-queue first-remote))
        (declare (ignore client-two))
        (let ((scratch (tuatara.buffer:find-buffer "*scratch*")))
          (when scratch (tuatara.buffer:kill-buffer scratch)))
        (tuatara.buffer:make-buffer "*scratch*" :text "")
        (let ((editor
                (without-grammar-questions
                  (tuatara.command:run-editor
                   first-remote :setup #'tuatara.defaults:install-defaults
                                :buffer (tuatara.buffer:find-buffer "*scratch*")
                                :start (lambda (editor)
                                         (tuatara.command::add-frame editor second-remote)
                                         (type-at two (tuatara.frontend:make-event :close))
                                         ;; once that frame is gone, the editor is left from the first
                                         (sb-thread:make-thread
                                          (lambda ()
                                            (loop repeat 500
                                                  while (rest (tuatara.command::editor-frames editor))
                                                  do (sleep 0.01))
                                            (apply #'type-at one
                                                   (mapcar (lambda (spec)
                                                             (tuatara.frontend:make-event
                                                              :key :key (tuatara.frontend:spec-key spec)))
                                                           (tuatara.frontend:parse-key-sequence "C-x C-c"))))))))))
          (is = 1 (length (tuatara.command::editor-frames editor)))
          (is eql 0 (client-done runner-two))
          (is eql 0 (client-done runner-one)))))))

(define-test (remote more-faces-than-names)
  ;; an editor with more faces than a connection numbers names sends those it can, and the client stays
  (let ((made (loop for i below (+ 100 tuatara.frontend::+most-names+)
                    collect (intern (format nil "REMOTE-TEST-FACE-~d" i) :keyword))))
    (unwind-protect
         (let ((local (make-instance 'queued-fake-frontend)))
           (dolist (name made) (tuatara.display:set-face-attributes name :fg :red))
           (multiple-value-bind (remote client runner) (connect-remote local)
             (declare (ignore client))
             (tuatara.frontend:frontend-begin-frame remote)
             (tuatara.frontend:frontend-put-row remote 0 "kiwi" :face :mode-line)
             (tuatara.frontend:frontend-end-frame remote)
             (tuatara.frontend:frontend-stop remote)
             (is eql 0 (client-done runner))
             (is equal "kiwi" (tuatara.frontend.fake:fake-screen-line local 0))))
      (dolist (name made) (remhash name tuatara.display:*faces*))
      (incf tuatara.display:*face-generation*))))

(define-test (remote dropped-names-refused)
  ;; a file dropped whose name is empty or holds a control character: a client never sends it, and the editor
  ;; ends the connection of one that does
  (let ((box (tuatara.frontend::make-outbox)))
    (tuatara.frontend::put-event box (tuatara.frontend:make-event :drop :column 1 :row 1
                                                                        :files (list "/tmp/kiwi" (format nil "a~cb" #\Nul)
                                                                                     "" (format nil "x~%y"))))
    (let* ((octets (subseq (tuatara.frontend::outbox-octets box) 0 (tuatara.frontend::outbox-fill box)))
           (inbox (tuatara.frontend::make-inbox nil :editor :octets octets :end (length octets))))
      (tuatara.frontend::read-message inbox)
      (is equal '("/tmp/kiwi") (tuatara.frontend:event-files (tuatara.frontend::take-event inbox nil)))))
  (dolist (name (list (format nil "a~cb" #\Nul) "" (format nil "~c[2J" #\Esc)))
    (true (ended-after (lambda (fd)
                         (send-raw fd (message-octets :event
                                                      (lambda (box)
                                                        (dolist (n '((3 1) (0 2) (0 2) (1 2)))
                                                          (apply #'tuatara.frontend::put-number box n))
                                                        (tuatara.frontend::put-text box name))))))
          "Not ended by ~s" name)))

(defun key-octets (count &optional (base #\a))
  "COUNT key events of BASE written as a client sends them, as octets."
  (let ((box (tuatara.frontend::make-outbox))
        (key (tuatara.frontend:make-event :key :key (tuatara.frontend:make-key base))))
    (dotimes (i count) (tuatara.frontend::put-event box key))
    (subseq (tuatara.frontend::outbox-octets box) 0 (tuatara.frontend::outbox-fill box))))

(define-test (remote reader-waits-past-a-busy-editor)
  ;; an editor that takes no event for seconds holds a client's reading back, never ends the connection;
  ;; once it takes them, every event comes
  (multiple-value-bind (mine theirs) (tuatara.io:socket-pair)
    (raw-peer theirs)
    (let* ((remote (tuatara.frontend:make-remote-frontend mine))
           (octets (key-octets 3000))
           (sender (sb-thread:make-thread
                    (lambda () (tuatara.io:with-octets-sap (sap octets)
                                 (loop with at = 0 while (< at (length octets))
                                       do (let ((sent (tuatara.io:send-with-descriptors
                                                       theirs (sb-sys:sap+ sap at) (- (length octets) at))))
                                            (if (plusp sent) (incf at sent) (tuatara.io:poll-fd theirs 4 1000)))))))))
      (sleep 2.5)
      (false (tuatara.frontend::remote-ended remote))
      (true (sb-thread:thread-alive-p (tuatara.frontend::remote-reader remote)))
      (is = 3000 (loop repeat 3000
                       for event = (tuatara.frontend:queue-pop (tuatara.frontend::remote-queue remote) 2)
                       count (and event (eq :key (tuatara.frontend:event-kind event)))))
      (false (tuatara.frontend::remote-ended remote))
      (sb-thread:join-thread sender :timeout 5 :default nil)
      (tuatara.frontend:frontend-stop remote)
      (sb-posix:close theirs))))

(define-test (remote unanswered-while-the-lock-is-busy)
  ;; a question unanswered while other threads keep taking the frontend's lock ends with none, its lock let go
  (let ((local (make-instance 'queued-fake-frontend)))
    (multiple-value-bind (remote client) (connect-remote local :run nil)
      (let* ((busy t)
             (holder (sb-thread:make-thread
                      (lambda () (loop while busy
                                       do (sb-thread:with-mutex ((tuatara.frontend::remote-lock remote)) (sleep 0.02))
                                          (sleep 0.001))))))
        (dotimes (i 3)
          (let ((tuatara.frontend::*answer-seconds* 0.3))
            (is eq nil (tuatara.frontend::ask-client remote :clipboard))))
        (false (sb-thread:holding-mutex-p (tuatara.frontend::remote-lock remote)))
        (setf busy nil)
        (sb-thread:join-thread holder))
      (tuatara.frontend::close-remote-client client)
      (tuatara.frontend:frontend-stop remote))))

(define-test (remote flood-while-another-frame-asks)
  ;; while one client's frame asks a question, another's keys wait in the editor, at most a bound of them
  ;; read; once the question is done they are all handled
  (let ((one (make-instance 'queued-fake-frontend :width 40 :height 8))
        (two (make-instance 'queued-fake-frontend :width 40 :height 8))
        (count 5000) (most 0))
    (multiple-value-bind (first-remote client-one runner-one) (connect-remote one)
      (declare (ignore client-one))
      (multiple-value-bind (second-remote client-two runner-two)
          (connect-remote two :queue (tuatara.frontend::remote-queue first-remote))
        (declare (ignore client-two))
        (let ((scratch (tuatara.buffer:find-buffer "*scratch*")))
          (when scratch (tuatara.buffer:kill-buffer scratch)))
        (tuatara.buffer:make-buffer "*scratch*" :text "")
        (flet ((keys (text)
                 (mapcar (lambda (spec) (tuatara.frontend:make-event :key :key (tuatara.frontend:spec-key spec)))
                         (tuatara.frontend:parse-key-sequence text))))
          (let ((editor
                  (without-grammar-questions
                    (tuatara.command:run-editor
                     first-remote
                     :setup #'tuatara.defaults:install-defaults :buffer (tuatara.buffer:find-buffer "*scratch*")
                     :start (lambda (editor)
                              (tuatara.command::add-frame editor second-remote)
                              ;; the first asks (M-x); the second floods; then the first gives up asking and leaves
                              (apply #'type-at one (keys "M-x"))
                              (sb-thread:make-thread
                               (lambda ()
                                 (sleep 0.5)
                                 (apply #'type-at two (make-list count :initial-element (first (keys "i"))))
                                 (loop repeat 30
                                       do (sleep 0.1)
                                          (setf most (max most (tuatara.frontend::remote-received second-remote))))
                                 (apply #'type-at one (keys "C-g"))
                                 (loop repeat 600
                                       until (= count (tuatara.frontend::remote-received second-remote))
                                       do (sleep 0.05))
                                 (sleep 0.5)
                                 (apply #'type-at one (keys "C-x C-c")))))))))
            (declare (ignorable editor))
            ;; while the first asked, no more than the bound were read of the second's
            (true (<= most (+ 2 tuatara.frontend::+most-waiting-events+)) "~d read while held" most)
            (is = count (tuatara.frontend::remote-received second-remote))
            (client-done runner-two)
            (client-done runner-one)))))))

(define-test (remote screen-too-large-says-so)
  ;; a screen too large to send whole: rows sent plainer first, then a word in place of those left out, said
  ;; once; never a blank row standing for text
  (let ((local (make-instance 'queued-fake-frontend :width 4096 :height 2048)))
    (multiple-value-bind (remote client runner) (connect-remote local)
      (declare (ignore client))
      (let ((text (make-string 4096 :initial-element (code-char #x101))))
        (tuatara.frontend:frontend-begin-frame remote)
        (dotimes (row 2048)
          (tuatara.frontend:frontend-put-row remote row text :face :mode-line :spans '((0 10 :keyword))))
        (tuatara.frontend:frontend-end-frame remote)
        (let ((said (loop for event = (tuatara.frontend:queue-pop (tuatara.frontend::remote-queue remote) 2)
                          while event
                          when (eq :said (tuatara.frontend:event-kind event)) collect event)))
          (is = 1 (length said)))
        (tuatara.frontend:frontend-stop remote)
        (is eql 0 (client-done runner))
        (let ((rows (map 'list #'identity (tuatara.frontend.fake:fake-screen local))))
          (true (every (lambda (row) (or (string= row text) (string= row tuatara.frontend::*row-left-out*))) rows))
          (true (find tuatara.frontend::*row-left-out* rows :test #'string=))
          (true (find text rows :test #'string=))
          ;; the first rows kept their faces, those sent plainer lost them
          (is eq :mode-line (aref (tuatara.frontend.fake:fake-row-faces local) 0)))))))

(define-test (remote paused-client-resumes)
  ;; a client that takes nothing for longer than half a minute (stopped, as by C-z) is kept, and given the
  ;; newest frame once it reads again
  (multiple-value-bind (mine theirs) (tuatara.io:socket-pair)
    (raw-peer theirs)
    (let ((remote (tuatara.frontend:make-remote-frontend mine))
          (row (make-string 190 :initial-element #\k)))
      (dotimes (n 200)
        (tuatara.frontend:frontend-begin-frame remote)
        (dotimes (r 50)
          (tuatara.frontend:frontend-put-row remote r (format nil "~d ~a" n row)))
        (tuatara.frontend:frontend-end-frame remote))
      (sleep 32)
      (false (tuatara.frontend::remote-ended remote))
      (let* ((inbox (tuatara.frontend::make-inbox theirs :client :deadline (tuatara.runtime:deadline-in 10)))
             (client (tuatara.frontend::%make-remote-client theirs nil inbox))
             (last nil))
        (tuatara.frontend::read-hello inbox)
        (loop for kind = (tuatara.frontend::read-message inbox)
              for item = (and kind (tuatara.frontend::take-from-editor client kind inbox))
              when (eq (first item) :frame) do (setf last (rest item))
              until (or (null kind) (and last (search "199 " (third (first last))))))
        (true (and last (eql 0 (search "199 " (third (first last)))))))
      (false (tuatara.frontend::remote-ended remote))
      (tuatara.frontend:frontend-stop remote)
      (sb-posix:close theirs))))
