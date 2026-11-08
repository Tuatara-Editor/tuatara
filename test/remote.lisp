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

(define-test (remote far-words-hide-nothing)
  ;; what the other end of a connection says, a paste from another machine and a name shown in a list leave
  ;; out, or show as their code, the same invisible characters: those that turn text round or draw nothing
  (let ((hiding (map 'string #'code-char '(97 #x202E 98 #x200B 99 #x2066 100 #xFEFF 101)))
        (kept (format nil "a~cb" (code-char #xAD))))
    (is string= "abcde" (tuatara.frontend::shown-words hiding))
    (is string= "abcde" (tuatara.frontend::far-text hiding))
    (is string= "a<202E>b<200B>c<2066>d<FEFF>e" (tuatara.command:escape-controls hiding))
    (is string= "words" (tuatara.frontend::shown-words (format nil "wo~crds~c" (code-char 27) (code-char 7))))
    ;; a soft hyphen draws nothing either, but is shown as it is everywhere
    (is string= kept (tuatara.frontend::shown-words kept))
    (is string= kept (tuatara.frontend::far-text kept))
    (is string= kept (tuatara.command:escape-controls kept))
    (true (tuatara.display:invisible-code-p #xAD))
    (false (tuatara.display:hiding-code-p #xAD))
    (true (tuatara.display:hiding-code-p #x202E))))

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

(defun put-editor-hello (box)
  "Put in BOX what an editor says first: its hello, and the settings its
clients are shown by."
  (tuatara.frontend::put-hello box)
  (let ((at (tuatara.frontend::begin-message box :settings)))
    (tuatara.frontend::put-settings box)
    (tuatara.frontend::end-message box at)))

(defun raw-editor (local &rest messages)
  "A client painting on LOCAL, a fake frontend, made over a new socket whose
other end, here, says hello as an editor and then sends MESSAGES (lists of
octets); two values: the client and the editor's end of the socket."
  (tuatara.frontend:frontend-start local)
  (multiple-value-bind (mine theirs) (tuatara.io:socket-pair)
    (let ((maker (sb-thread:make-thread (lambda () (tuatara.frontend:make-remote-client theirs local))))
          (box (tuatara.frontend::make-outbox)))
      (put-editor-hello box)
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
           (loop repeat 6000
                 until (and (tuatara.frontend.fake:fake-screen local)
                            (equal "tui" (tuatara.frontend.fake:fake-screen-line local 1))
                            (search "TUATARA" (tuatara.frontend.fake:fake-screen-line local 4)))
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
  ;; an editor whose frames are two clients': one client's window closed closes the frames on its screen
  ;; alone, and that client is told to end; the editor goes on in the other
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
                                         ;; two frames taking turns on the second client's screen
                                         (tuatara.command::add-frame editor second-remote)
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
  ;; while one client's frame asks inside another's question, the other's keys wait in the editor for their
  ;; turn, at most a bound of them read; once the inner question is done they are all handled
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
                              ;; the second asks (M-x), then the first, inside it; the second floods, its keys
                              ;; waiting for its turn while the first asks; then both give up asking and the
                              ;; first leaves
                              (sb-thread:make-thread
                               (lambda ()
                                 (apply #'type-at two (keys "M-x"))
                                 (sleep 0.5)
                                 (apply #'type-at one (keys "M-x"))
                                 (sleep 0.5)
                                 (apply #'type-at two (make-list count :initial-element (first (keys "i"))))
                                 (loop repeat 30
                                       do (sleep 0.1)
                                          (setf most (max most (tuatara.frontend::remote-received second-remote))))
                                 (apply #'type-at one (keys "C-g"))
                                 (loop repeat 600
                                       until (= (1+ count) (tuatara.frontend::remote-received second-remote))
                                       do (sleep 0.05))
                                 (sleep 0.5)
                                 (apply #'type-at two (keys "C-g"))
                                 (sleep 0.5)
                                 (apply #'type-at one (keys "C-x C-c")))))))))
            (declare (ignorable editor))
            ;; while the first asked, no more than the bound were read of the second's
            (true (<= most (+ 3 tuatara.frontend::+most-waiting-events+)) "~d read while held" most)
            (is = (+ 2 count) (tuatara.frontend::remote-received second-remote))
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

;;; Settings, told by the editor to its clients

(defun display-settings-bound (function)
  "Call FUNCTION with each of *DISPLAY-SETTINGS* bound to its value, so that
what it sets is undone after."
  (let ((names (mapcar #'first tuatara.frontend::*display-settings*)))
    (progv names (mapcar (lambda (name) (and (boundp name) (symbol-value name))) names)
      (funcall function))))

(defun settings-read-back ()
  "The settings as a client reads them from a message of the values here
\(TUATARA.FRONTEND::TAKE-SETTINGS)."
  (let* ((box (tuatara.frontend::make-outbox)) (at (tuatara.frontend::begin-message box :settings)))
    (tuatara.frontend::put-settings box)
    (tuatara.frontend::end-message box at)
    (let* ((octets (subseq (tuatara.frontend::outbox-octets box) 0 (tuatara.frontend::outbox-fill box)))
           (inbox (tuatara.frontend::make-inbox nil :client :octets octets :end (length octets))))
      (is eq :settings (tuatara.frontend::read-message inbox))
      (tuatara.frontend::take-settings inbox))))

(define-test (remote settings-carried-to-a-client)
  ;; the window's font and padding, the terminal's colours and the rest, as the editor's init file set them
  (display-settings-bound
   (lambda ()
     (setf tuatara-user:*gui-font* "Kiwi Mono 13" tuatara-user:*gui-padding* '(4 12)
           tuatara-user:*window-colours-from-desktop* t tuatara-user:*gui-renderer* :cairo
           tuatara-user:*tui-padding* 1 tuatara-user:*tui-mouse* nil tuatara-user:*tui-colours* 16
           tuatara-user:*tui-pictures* nil tuatara-user:*kitty-keyboard* t)
     (let ((read (settings-read-back)))
       (is equal "Kiwi Mono 13" (cdr (assoc 'tuatara-user:*gui-font* read)))
       (is equal '(4 12 4 12) (cdr (assoc 'tuatara-user:*gui-padding* read)))
       (is equal '(1 1 1 1) (cdr (assoc 'tuatara-user:*tui-padding* read)))
       (is eq t (cdr (assoc 'tuatara-user:*window-colours-from-desktop* read)))
       (is eq :cairo (cdr (assoc 'tuatara-user:*gui-renderer* read)))
       (is eql 16 (cdr (assoc 'tuatara-user:*tui-colours* read)))
       (is eq t (cdr (assoc 'tuatara-user:*kitty-keyboard* read)))
       (true (assoc 'tuatara-user:*tui-mouse* read))
       (false (cdr (assoc 'tuatara-user:*tui-mouse* read))))
     ;; a value a setting may not take, or a font named with a control character, is not told: the client's stays
     (setf tuatara-user:*tui-colours* 24 tuatara-user:*gui-font* (format nil "Mono~c[2J" #\Esc))
     (let ((read (settings-read-back)))
       (false (assoc 'tuatara-user:*tui-colours* read))
       (false (assoc 'tuatara-user:*gui-font* read)))
     ;; a client takes them, but those it was told before unchanged, and those taken as its screen opens
     (setf tuatara-user:*gui-font* "Kiwi Mono 13" tuatara-user:*gui-renderer* :gl)
     (let ((told (list (cons 'tuatara-user:*gui-font* "Weka Mono 9") (cons 'tuatara-user:*gui-renderer* :vulkan)
                       (cons 'tuatara-user:*tui-mouse* t))))
       (true (tuatara.frontend::use-settings told :before (list (cons 'tuatara-user:*tui-mouse* t))))
       (is equal "Weka Mono 9" tuatara-user:*gui-font*)
       (is eq :gl tuatara-user:*gui-renderer*)
       (false tuatara-user:*tui-mouse* "told as before: left as the client has it")
       (true (tuatara.frontend::use-settings told :at-start t))
       (is eq :vulkan tuatara-user:*gui-renderer*)))))

(define-test (remote settings-told-again-when-changed)
  ;; after a command the settings are looked at: told again only when one has another value
  (display-settings-bound
   (lambda ()
     (let ((remote (make-instance 'tuatara.frontend:remote-frontend :fd -1)))
       (flet ((settings-sent ()
                (let* ((box (tuatara.frontend::remote-unsent remote))
                       (octets (subseq (tuatara.frontend::outbox-octets box) 0 (tuatara.frontend::outbox-fill box)))
                       (inbox (tuatara.frontend::make-inbox nil :client :octets octets :end (length octets))))
                  (setf (tuatara.frontend::outbox-fill box) 0)
                  (loop for kind = (tuatara.frontend::read-message inbox)
                        while kind
                        count (eq kind :settings)
                        do (setf (tuatara.frontend::inbox-at inbox) (tuatara.frontend::inbox-stop inbox))))))
         (setf (tuatara.frontend::remote-settings-sent remote) (tuatara.frontend::settings-now))
         (tuatara.frontend:frontend-settings-changed remote)
         (is = 0 (settings-sent))
         (setf tuatara-user:*gui-padding* 17)
         (tuatara.frontend:frontend-settings-changed remote)
         (tuatara.frontend:frontend-settings-changed remote)
         (is = 1 (settings-sent))
         (setf tuatara-user:*gui-padding* 17)
         (tuatara.frontend:frontend-settings-changed remote)
         (is = 0 (settings-sent)))))))

(define-test (remote settings-told-with-the-hello)
  ;; a client knows the settings before its screen opens: an editor that says none is refused
  (multiple-value-bind (mine theirs) (tuatara.io:socket-pair)
    (let ((box (tuatara.frontend::make-outbox)))
      (tuatara.frontend::put-hello box)
      (tuatara.frontend::send-outbox mine box 5)
      (let ((tuatara.frontend::*hello-seconds* 1))
        (let ((refused (handler-case (progn (tuatara.frontend:greet-editor theirs) nil)
                         (tuatara.frontend:remote-refused (condition)
                           (tuatara.frontend:remote-refused-words condition)))))
          (true (search "settings" refused) "~a" refused))))
    (sb-posix:close mine) (sb-posix:close theirs))
  ;; the editor's settings, here, read by a client, on a thread of its own, before it says anything
  (display-settings-bound
   (lambda ()
     (setf tuatara-user:*gui-font* "Tui Mono 11")
     (multiple-value-bind (mine theirs) (tuatara.io:socket-pair)
       (let ((client (sb-thread:make-thread
                      (lambda ()
                        (display-settings-bound
                         (lambda ()
                           (prog1 (list (second (tuatara.frontend:greet-editor theirs)) tuatara-user:*gui-font*)
                             (sb-posix:close theirs))))))))
         (false (handler-case (tuatara.frontend:make-remote-frontend mine)
                  (tuatara.frontend:remote-refused () nil)))
         (destructuring-bind (told taken) (sb-thread:join-thread client :timeout 10 :default '(nil nil))
           (is equal "Tui Mono 11" (cdr (assoc 'tuatara-user:*gui-font* told)))
           (is equal "Tui Mono 11" taken "taken by the client"))
         (sb-posix:close mine))))))

(define-test (remote settings-looked-at-without-garbage)
  ;; the settings looked at after every command, none changed: nothing made for each look
  (let ((remote (make-instance 'tuatara.frontend:remote-frontend :fd -1)))
    (setf (tuatara.frontend::remote-settings-sent remote) (tuatara.frontend::settings-now))
    (flet ((garbage (count)
             (sb-ext:gc :full t)
             (let ((before (sb-ext:get-bytes-consed)))
               (dotimes (i count) (tuatara.frontend:frontend-settings-changed remote))
               (- (sb-ext:get-bytes-consed) before))))
      (garbage 100)
      ;; the count is every thread's: another's garbage can only raise it, so the least of a few rounds
      (let ((per-look (loop repeat 5
                            minimize (/ (- (min (garbage 20000) (garbage 20000)) (min (garbage 2000) (garbage 2000)))
                                        18000.0))))
        (true (< per-look 1) "~,2f octets a look" per-look)))))

(define-test (remote window-asked-for-by-its-process-alone)
  ;; a window asked of a client is taken by its word, from the process of the connection asked, once; one
  ;; not come in time is no longer waited for
  (let ((remote (make-instance 'tuatara.frontend:remote-frontend :fd -1)))
    (setf (tuatara.frontend::facts-window (tuatara.frontend::remote-facts remote)) t
          (tuatara.frontend:remote-pid remote) 4242)
    (true (tuatara.frontend:frontend-ask-another remote :shown 1))
    (let ((word (first (find remote tuatara.frontend::*windows-asked-for* :key #'second))))
      (false (tuatara.frontend:take-window-asked-for word 4243) "another process")
      (false (tuatara.frontend:take-window-asked-for "NOSUCHWORD" 4242) "another word")
      (multiple-value-bind (asker what) (tuatara.frontend:take-window-asked-for word 4242)
        (is eq remote asker)
        (is eq :shown what))
      (false (tuatara.frontend:take-window-asked-for word 4242) "taken once"))
    (let ((tuatara.frontend:*window-seconds* 0))
      (true (tuatara.frontend:frontend-ask-another remote :late 1)))
    (let ((word (first (find remote tuatara.frontend::*windows-asked-for* :key #'second))))
      ;; with any others of earlier sessions not come
      (true (member :late (tuatara.frontend:windows-not-come)))
      (false (tuatara.frontend:take-window-asked-for word 4242) "too late"))
    ;; a terminal is asked for none
    (setf (tuatara.frontend::facts-window (tuatara.frontend::remote-facts remote)) nil)
    (false (tuatara.frontend:frontend-ask-another remote :shown 1))))

(define-test (remote windows-asked-for-held-to-their-most)
  ;; a client is asked for no more windows than it may show, and for a few at a time: past either, the
  ;; request says why, and nothing is asked
  (let ((remote (make-instance 'tuatara.frontend:remote-frontend :fd -1)))
    (setf (tuatara.frontend::facts-window (tuatara.frontend::remote-facts remote)) t)
    (flet ((asked () (count remote tuatara.frontend::*windows-asked-for* :key #'second))
           (forget () (setf tuatara.frontend::*windows-asked-for*
                            (remove remote tuatara.frontend::*windows-asked-for* :key #'second))))
      (unwind-protect
           (progn
             (dotimes (i tuatara.frontend::+most-windows-asked+)
               (is eq t (tuatara.frontend:frontend-ask-another remote i 1)))
             (let ((refused (tuatara.frontend:frontend-ask-another remote :one-more 1)))
               (true (and (stringp refused) (search "still to come" refused)) "~s" refused))
             (is = tuatara.frontend::+most-windows-asked+ (asked))
             ;; two still to come, beside thirty windows shown: no more
             (forget)
             (tuatara.frontend:frontend-ask-another remote :a 30)
             (tuatara.frontend:frontend-ask-another remote :b 30)
             (let ((refused (tuatara.frontend:frontend-ask-another remote :c 30)))
               (true (and (stringp refused) (search "32 windows" refused)) "~s" refused))
             (is = 2 (asked)))
        (forget)))))

;;; Pictures to a client on another machine

(define-test (remote deflate-makes-what-inflate-reads)
  ;; packed and unpacked again to the same octets, for runs, noise, lengths at a block's edge and nothing
  (let ((*random-state* (sb-ext:seed-random-state 7)))
    (flet ((same (octets)
             (let ((packed (tuatara.io:deflate octets)))
               (is equalp octets (tuatara.io:inflate packed :limit (max 1 (length octets)) :size (length octets))))))
      (same (tuatara.text:make-octets 0))
      (same (coerce #(1) 'tuatara.text:octets))
      (same (tuatara.text:make-octets 100000))
      (dolist (size '(258 259 32767 32768 32769 65535 65536 70000))
        (let ((noise (tuatara.text:make-octets size)) (pattern (tuatara.text:make-octets size)))
          (dotimes (i size) (setf (aref noise i) (random 256) (aref pattern i) (mod (* i i) 7)))
          (same noise)
          (same pattern)))
      ;; noise is stored, a run packs to almost nothing
      (let ((noise (tuatara.text:make-octets 70000)))
        (dotimes (i (length noise)) (setf (aref noise i) (random 256)))
        (true (<= (length (tuatara.io:deflate noise)) (+ (length noise) 32))))
      (true (< (length (tuatara.io:deflate (tuatara.text:make-octets 100000))) 200)))))

(defun far-remote (local)
  "An editor's remote frontend and a client on another machine painting on the
fake frontend LOCAL, over a socket: the remote frontend and the client, not yet running."
  (multiple-value-bind (mine theirs) (tuatara.io:socket-pair)
    (let* ((maker (sb-thread:make-thread
                   (lambda () (tuatara.frontend:make-remote-client
                               theirs local :request (tuatara.frontend:make-request :directory "/" :far t)))))
           (remote (tuatara.frontend:make-remote-frontend mine)))
      (values remote (sb-thread:join-thread maker)))))

(defun two-colour-image (width height channels)
  "A picture WIDTH by HEIGHT of CHANNELS octets, red above and blue below, as octets."
  (let ((pixels (tuatara.text:make-octets (* width height channels))))
    (dotimes (y height)
      (dotimes (x width)
        (let ((at (* channels (+ x (* y width)))))
          (setf (aref pixels at) (if (< y (/ height 2)) 200 0)
                (aref pixels (+ at 2)) (if (< y (/ height 2)) 0 200))
          (when (= channels 4) (setf (aref pixels (+ at 3)) 255)))))
    (tuatara.frontend:make-image width height channels pixels)))

(defun paint-picture (remote image &optional (columns 40) (rows 12) offset)
  "Have REMOTE paint a frame showing IMAGE in COLUMNS by ROWS cells, or none when IMAGE is NIL."
  (tuatara.frontend:frontend-begin-frame remote)
  (when image (tuatara.frontend:frontend-put-image remote image 0 0 columns rows offset))
  (tuatara.frontend:frontend-end-frame remote))

(defun far-picture-of (remote source)
  "REMOTE's record of the picture numbered SOURCE, its newest."
  (find source (tuatara.frontend::remote-far-pictures remote) :key #'tuatara.frontend::far-picture-source))

(defun far-ready-p (remote source)
  "True when REMOTE's client has the picture numbered SOURCE whole."
  (let ((far (far-picture-of remote source)))
    (and far (eq (tuatara.frontend::far-picture-state far) :ready))))

(define-test (remote far-picture-made-small-and-shown)
  ;; a picture four times the size of its window goes once, the size it shows at, and is shown with the pixels
  ;; it was given, a good deal smaller than the picture; the frame before it arrives has a blank, not a picture
  (let* ((local (make-instance 'queued-fake-frontend :width 40 :height 12 :cell-pixels '(8 16)))
         (image (two-colour-image 1280 768 3)))
    (multiple-value-bind (remote client) (far-remote local)
      (let ((runner (sb-thread:make-thread (lambda () (tuatara.frontend:run-remote-client client)))))
        (paint-picture remote image)
        (true (tuatara.runtime:wait-until (lambda () (far-ready-p remote (tuatara.frontend:image-id image))) 30)
              "the picture came")
        (let ((far (far-picture-of remote (tuatara.frontend:image-id image))))
          ;; the window is 320 by 192 pixels: the picture fitted to it
          (is = 320 (tuatara.frontend::far-picture-width far))
          (is = 192 (tuatara.frontend::far-picture-height far))
          (isnt = (tuatara.frontend:image-id image) (tuatara.frontend::far-picture-id far))
          (true (< (tuatara.frontend::far-picture-total far) 2000)
                "~d octets for two colours" (tuatara.frontend::far-picture-total far))
          (paint-picture remote image)
          (true (tuatara.runtime:wait-until (lambda () (tuatara.frontend.fake:fake-images local)) 30) "shown")
          (destructuring-bind ((shown row column columns rows offset)) (tuatara.frontend.fake:fake-images local)
            (is equal '(0 0 40 12 nil) (list row column columns rows offset))
            (is = 320 (tuatara.frontend:image-width shown))
            (is = 192 (tuatara.frontend:image-height shown))
            (is = 3 (tuatara.frontend:image-channels shown))
            (is equal '(200 0 0) (coerce (subseq (tuatara.frontend:image-pixels shown) 0 3) 'list) "red above")
            (is equal '(0 0 200)
                (coerce (subseq (tuatara.frontend:image-pixels shown) (- (* 320 192 3) 3)) 'list) "blue below"))
          ;; the next frame of the picture asks nothing more of the connection
          (let ((count (length (tuatara.frontend::remote-far-pictures remote))))
            (paint-picture remote image)
            (is = count (length (tuatara.frontend::remote-far-pictures remote)))))
        (tuatara.frontend:frontend-stop remote)
        (client-done runner)))))

(define-test (remote far-picture-at-own-size-with-offset)
  ;; shown from an offset, a picture goes whole: the view scrolls over it without asking again
  (let* ((local (make-instance 'queued-fake-frontend :width 40 :height 12 :cell-pixels '(8 16)))
         (image (two-colour-image 1000 1600 3)))
    (multiple-value-bind (remote client) (far-remote local)
      (let ((runner (sb-thread:make-thread (lambda () (tuatara.frontend:run-remote-client client)))))
        (paint-picture remote image 40 12 '(4 0))
        (true (tuatara.runtime:wait-until (lambda () (far-ready-p remote (tuatara.frontend:image-id image))) 30))
        (paint-picture remote image 40 12 '(4 0))
        (true (tuatara.runtime:wait-until (lambda () (tuatara.frontend.fake:fake-images local)) 30))
        (destructuring-bind ((shown row column columns rows offset)) (tuatara.frontend.fake:fake-images local)
          (declare (ignore row column columns rows))
          (is equal '(4 0) offset)
          (is = 1000 (tuatara.frontend:image-width shown))
          (is equalp (tuatara.frontend:image-pixels image) (tuatara.frontend:image-pixels shown)))
        (tuatara.frontend:frontend-stop remote)
        (client-done runner)))))

(define-test (remote far-picture-cancelled-off-screen)
  ;; with a client that takes in nothing no more than the window of a picture is queued; a frame without the
  ;; picture cancels it, the client forgets what came, the connection goes on, and a smaller picture comes
  (let* ((local (make-instance 'queued-fake-frontend :width 40 :height 12 :cell-pixels '(8 16)))
         (noise (tuatara.text:make-octets (* 320 192 3)))
         (image (let ((*random-state* (sb-ext:seed-random-state 11)))
                  (dotimes (i (length noise)) (setf (aref noise i) (random 256)))
                  (tuatara.frontend:make-image 320 192 3 noise)))
         (other (two-colour-image 64 64 3)))
    (multiple-value-bind (remote client) (far-remote local)
      (paint-picture remote image)
      (true (tuatara.runtime:wait-until
             (lambda ()
               (let ((far (far-picture-of remote (tuatara.frontend:image-id image))))
                 (and far (eq (tuatara.frontend::far-picture-state far) :sending))))
             30)
            "begun")
      ;; held at the window: not everything is put among the messages
      (sleep 0.3)
      (let ((queued (+ (tuatara.frontend::outbox-fill (tuatara.frontend::remote-unsent remote))
                       (tuatara.frontend::outbox-fill (tuatara.frontend::remote-sending remote)))))
        (true (<= queued (+ tuatara.frontend::+far-pictures-in-flight+ 70000))
              "~d octets waiting for a client that took nothing" queued))
      (paint-picture remote nil)
      (true (tuatara.runtime:wait-until (lambda () (null (far-picture-of remote (tuatara.frontend:image-id image)))) 10)
            "cancelled")
      (let ((runner (sb-thread:make-thread (lambda () (tuatara.frontend:run-remote-client client)))))
        (paint-picture remote other 8 8)
        (true (tuatara.runtime:wait-until (lambda () (far-ready-p remote (tuatara.frontend:image-id other))) 30)
              "the next one came")
        (is equal nil (tuatara.frontend::remote-client-arriving client))
        (is = 1 (length (tuatara.frontend::remote-client-pictures client)) "the cancelled one is not kept")
        (false (tuatara.frontend::remote-client-ended client))
        (tuatara.frontend:frontend-stop remote)
        (client-done runner)))))

(define-test (remote far-picture-refused-past-the-most)
  ;; a picture past what a client on another machine takes (at its own size, shown from an offset) is left out
  ;; once: no error, no sending, and the frame goes on without it
  (let* ((local (make-instance 'queued-fake-frontend :width 40 :height 12 :cell-pixels '(8 16)))
         (image (tuatara.frontend::%make-image 20000 3 3 (tuatara.text:make-octets (* 20000 3 3))
                                               (sb-ext:atomic-incf (car tuatara.frontend::**image-count**)))))
    (multiple-value-bind (remote client) (far-remote local)
      (let ((runner (sb-thread:make-thread (lambda () (tuatara.frontend:run-remote-client client)))))
        (dotimes (n 3) (paint-picture remote image 40 12 '(0 0)))
        (let ((far (far-picture-of remote (tuatara.frontend:image-id image))))
          (true far)
          (is eq :refused (tuatara.frontend::far-picture-state far)))
        (is = 1 (count (tuatara.frontend:image-id image) (tuatara.frontend::remote-far-pictures remote)
                       :key #'tuatara.frontend::far-picture-source)
            "asked for once")
        (false (tuatara.frontend::remote-far-sender remote) "no thread for what is not sent")
        (tuatara.frontend:frontend-stop remote)
        (client-done runner)))))

(defun client-reading (build)
  "A client with an inbox of the octets the function BUILD writes to an outbox,
as messages from an editor; the client."
  (let ((box (tuatara.frontend::make-outbox)))
    (funcall build box)
    (tuatara.frontend::%make-remote-client
     -1 (tuatara.frontend.fake:make-fake-frontend :cell-pixels '(8 16))
     (tuatara.frontend::make-inbox nil :client
                                   :octets (subseq (tuatara.frontend::outbox-octets box)
                                                   0 (tuatara.frontend::outbox-fill box))
                                   :end (tuatara.frontend::outbox-fill box)))))

(defun read-all-as-client (client)
  "Read all CLIENT's inbox holds, as the reading thread does; the items for its frontend."
  (let ((inbox (tuatara.frontend::remote-client-inbox client)))
    (loop for kind = (tuatara.frontend::read-message inbox)
          while kind
          collect (tuatara.frontend::take-from-editor client kind inbox))))

(defun refused-as-malformed-p (build)
  "True when a client reading what BUILD writes takes it as malformed."
  (handler-case (progn (read-all-as-client (client-reading build)) nil)
    (tuatara.frontend:malformed-message () t)))

(defun put-picture-begin-raw (box id width height channels total)
  "Put a picture's beginning in BOX with whatever numbers it is given."
  (let ((at (tuatara.frontend::begin-message box :picture-begin)))
    (dolist (field (list (list id 4) (list width 4) (list height 4) (list channels 1) (list total 4)))
      (tuatara.frontend::put-number box (first field) (second field)))
    (tuatara.frontend::end-message box at)))

(defun put-picture-piece-raw (box id start octets)
  "Put a piece of a picture in BOX with whatever octets and place it is given."
  (let ((at (tuatara.frontend::begin-message box :picture-piece)))
    (tuatara.frontend::put-number box id 4)
    (tuatara.frontend::put-number box start 4)
    (tuatara.frontend::put-octets box octets)
    (tuatara.frontend::end-message box at)))

(define-test (remote far-picture-from-a-hostile-editor)
  ;; every number and length of a picture's messages held to its bound; the octets of what comes unpack, or
  ;; end the connection, and never to more than the size it said; a picture given whole is taken
  (let ((pixels (coerce #(1 2 3 4 5 6 7 8 9 10 11 12) 'tuatara.text:octets)))
    ;; a good picture of 2 by 2: rows against those above, deflated
    (let* ((packed (tuatara.io:deflate (tuatara.frontend::rows-against-above pixels 6)))
           (client (client-reading
                    (lambda (box)
                      (put-picture-begin-raw box 9 2 2 3 (length packed))
                      (put-picture-piece-raw box 9 0 packed)))))
      (read-all-as-client client)
      (true (tuatara.runtime:wait-until (lambda () (tuatara.frontend::remote-client-pictures client)) 10))
      (let ((kept (first (tuatara.frontend::remote-client-pictures client))))
        (true kept)
        (is equalp pixels (tuatara.frontend:image-pixels kept))
        (is = 9 (tuatara.frontend:image-id kept))))
    (let ((packed (tuatara.io:deflate (tuatara.text:make-octets 12))))
      (macrolet ((refused (&body body) `(true (refused-as-malformed-p (lambda (box) ,@body)))))
        ;; sizes past the most
        (refused (put-picture-begin-raw box 1 0 2 3 8))
        (refused (put-picture-begin-raw box 1 2 0 3 8))
        (refused (put-picture-begin-raw box 1 2 2 5 8))
        (refused (put-picture-begin-raw box 1 2 2 2 8))
        (refused (put-picture-begin-raw box 1 16385 1 3 100))
        (refused (put-picture-begin-raw box 1 16384 16384 4 100))
        ;; more packed than the pixels can come to
        (refused (put-picture-begin-raw box 1 2 2 3 100000))
        (refused (put-picture-begin-raw box 1 2 2 3 0))
        ;; a piece before any begin, of another picture, out of order, past its end
        (refused (put-picture-piece-raw box 9 0 packed))
        (refused (put-picture-begin-raw box 9 2 2 3 (length packed))
                 (put-picture-piece-raw box 8 0 packed))
        (refused (put-picture-begin-raw box 9 2 2 3 (length packed))
                 (put-picture-piece-raw box 9 1 packed))
        (refused (put-picture-begin-raw box 9 2 2 3 (length packed))
                 (put-picture-piece-raw box 9 0 (concatenate 'tuatara.text:octets packed #(1))))
        ;; a piece of nothing
        (refused (put-picture-begin-raw box 9 2 2 3 (length packed))
                 (put-picture-piece-raw box 9 0 (tuatara.text:make-octets 0)))
        ;; a second begun over the first
        (refused (put-picture-begin-raw box 9 2 2 3 (length packed))
                 (put-picture-begin-raw box 8 2 2 3 (length packed)))
        ;; a forgotten picture never sent
        (refused (let ((at (tuatara.frontend::begin-message box :picture-forget)))
                   (tuatara.frontend::put-number box 3 4)
                   (tuatara.frontend::end-message box at))))
      ;; what does not unpack, and what unpacks to more or fewer pixels than it said
      (flet ((unpacks-p (octets)
               (let ((arriving (tuatara.frontend::make-arriving-picture 9 2 2 3 (length octets))))
                 (setf (tuatara.frontend::arriving-picture-octets arriving) (coerce octets 'tuatara.text:octets)
                       (tuatara.frontend::arriving-picture-filled arriving) (length octets))
                 (handler-case (tuatara.frontend::arrived-picture arriving)
                   (tuatara.frontend:malformed-message () nil)))))
        (true (unpacks-p packed))
        (false (unpacks-p (coerce #(255 255) 'tuatara.text:octets)))
        (false (unpacks-p (tuatara.io:deflate (tuatara.text:make-octets 13))))
        (false (unpacks-p (tuatara.io:deflate (tuatara.text:make-octets 11))))))))

(define-test (remote far-picture-acks-held-to-its-size)
  ;; a client saying more was taken than a picture holds ends the connection, and ack with no picture is ignored
  (let ((remote (make-instance 'tuatara.frontend:remote-frontend :fd -1))
        (far (tuatara.frontend::make-far-picture 5 2 2 3 nil 0)))
    (setf (tuatara.frontend::far-picture-id far) 5
          (tuatara.frontend::far-picture-state far) :sending
          (tuatara.frontend::far-picture-total far) 10
          (tuatara.frontend::remote-far-pictures remote) (list far))
    (tuatara.frontend::picture-taken remote 5 4)
    (is = 4 (tuatara.frontend::far-picture-taken far))
    (fail (tuatara.frontend::picture-taken remote 5 11) 'tuatara.frontend:malformed-message)
    (fail (tuatara.frontend::picture-taken remote 5 3) 'tuatara.frontend:malformed-message)
    (tuatara.frontend::picture-taken remote 77 999)))

(defun send-to-far-client (remote write)
  "Have REMOTE send its client the messages WRITE, a function of the box, writes."
  (sb-thread:with-mutex ((tuatara.frontend::remote-lock remote))
    (funcall write (tuatara.frontend::remote-unsent remote))
    (sb-thread:condition-broadcast (tuatara.frontend::remote-moved remote))))

(defun put-whole-picture (box id width height channels packed)
  "Put in BOX a picture numbered ID, WIDTH by HEIGHT by CHANNELS, its packed octets PACKED in pieces as sent."
  (put-picture-begin-raw box id width height channels (length packed))
  (loop for start from 0 below (length packed) by tuatara.frontend::+far-picture-piece+
        for end = (min (length packed) (+ start tuatara.frontend::+far-picture-piece+))
        do (let ((at (tuatara.frontend::begin-message box :picture-piece)))
             (tuatara.frontend::put-picture-piece-of box id packed start end)
             (tuatara.frontend::end-message box at))))

(defun put-frame-naming (box id)
  "Put in BOX a frame that shows the picture numbered ID and nothing else."
  (let ((at (tuatara.frontend::begin-message box :frame)))
    (tuatara.frontend::put-picture-call box id 0 0 4 4 nil)
    (tuatara.frontend::end-message box at)))

(defun put-forget (box id)
  "Put in BOX word to forget the picture numbered ID."
  (let ((at (tuatara.frontend::begin-message box :picture-forget)))
    (tuatara.frontend::put-number box id 4)
    (tuatara.frontend::end-message box at)))

(defun put-title (box title)
  "Put in BOX a message giving the window TITLE."
  (let ((at (tuatara.frontend::begin-message box :title)))
    (tuatara.frontend::put-text box title)
    (tuatara.frontend::end-message box at)))

(defun arrived-title-p (client title)
  "True when the message giving TITLE is among what CLIENT has read and not painted."
  (find title (tuatara.frontend::remote-client-arrived client) :key #'second :test #'equal))

(define-test (remote far-picture-resent-under-a-number-of-its-own)
  ;; a picture sent at its own size, cancelled and sent again goes under another number, so that what the client
  ;; says of the first is not taken for the second
  (let* ((local (make-instance 'queued-fake-frontend :width 40 :height 12 :cell-pixels '(8 16)))
         (noise (tuatara.text:make-octets 90000))
         (image (let ((*random-state* (sb-ext:seed-random-state 13)))
                  (dotimes (i (length noise)) (setf (aref noise i) (random 256)))
                  (tuatara.frontend:make-image 300 100 3 noise))))
    (multiple-value-bind (remote client) (far-remote local)
      (flet ((sending ()
               (let ((far (far-picture-of remote (tuatara.frontend:image-id image))))
                 (and far (eq (tuatara.frontend::far-picture-state far) :sending) far))))
        (paint-picture remote image 40 12 '(0 0))
        (true (tuatara.runtime:wait-until #'sending 30))
        (let ((first-id (tuatara.frontend::far-picture-id (sending))))
          (paint-picture remote nil)
          (true (tuatara.runtime:wait-until
                 (lambda () (null (far-picture-of remote (tuatara.frontend:image-id image)))) 10))
          (paint-picture remote image 40 12 '(0 0))
          (true (tuatara.runtime:wait-until #'sending 30))
          (isnt = first-id (tuatara.frontend::far-picture-id (sending)) "each send its own number")
          ;; a late word about the first send changes nothing
          (tuatara.frontend::picture-taken remote first-id 80000)
          (is = 0 (tuatara.frontend::far-picture-taken (sending)))))
      (tuatara.frontend:frontend-stop remote)
      (tuatara.frontend::close-remote-client client))))

(define-test (remote far-picture-from-an-offset-only-at-its-own-size)
  ;; a copy made smaller for a window is not shown from an offset: the client would crop it in the wrong pixels
  (let* ((local (make-instance 'queued-fake-frontend :width 40 :height 12 :cell-pixels '(8 16)))
         (image (two-colour-image 1000 800 3)))
    (multiple-value-bind (remote client) (far-remote local)
      (let ((runner (sb-thread:make-thread (lambda () (tuatara.frontend:run-remote-client client)))))
        (paint-picture remote image)
        (true (tuatara.runtime:wait-until (lambda () (far-ready-p remote (tuatara.frontend:image-id image))) 30))
        (tuatara.frontend:frontend-begin-frame remote)
        (false (tuatara.frontend::far-picture-to-show remote image 40 12 '(2 0)) "the smaller copy is not shown")
        (tuatara.frontend:frontend-end-frame remote)
        (true (tuatara.runtime:wait-until
               (lambda ()
                 (tuatara.frontend:frontend-begin-frame remote)
                 (prog1 (tuatara.frontend::far-picture-to-show remote image 40 12 '(2 0))
                   (tuatara.frontend:frontend-end-frame remote)))
               30)
              "the one of its own size comes")
        (tuatara.frontend:frontend-stop remote)
        (client-done runner)))))

(define-test (remote far-picture-begin-takes-no-room-for-what-has-not-come)
  ;; eighteen octets saying a huge picture is coming make no huge room; the room grows with its pieces
  (let* ((total 100000000)
         (client (client-reading (lambda (box) (put-picture-begin-raw box 9 8000 4000 4 total)))))
    (read-all-as-client client)
    (let ((arriving (tuatara.frontend::remote-client-arriving client)))
      (true arriving)
      (true (<= (length (tuatara.frontend::arriving-picture-octets arriving)) 65536)))))

(define-test (remote inflate-refuses-oversubscribed-codes)
  ;; a block whose code lengths promise more codes than bits can tell apart is damaged, and says so
  (let ((writer (tuatara.io::make-bit-writer 64)))
    (flet ((bits (value count) (tuatara.io::write-bits writer value count)))
      (bits 1 1) (bits 2 2) (bits 0 5) (bits 0 5) (bits 0 4)
      (dotimes (i 4) (bits 1 3))
      (dotimes (i 40) (bits 0 8))
      (bits 0 7))
    (let ((words (handler-case (progn (tuatara.io:inflate (subseq (tuatara.io::bit-writer-octets writer)
                                                                  0 (tuatara.io::bit-writer-fill writer))
                                                          :limit 1000)
                                      "unpacked")
                   (tuatara.io:zip-error (condition) (princ-to-string condition)))))
      (true (search "more codes than" words) "said ~a" words))))

(define-test (remote far-picture-out-of-memory-keeps-the-sender)
  ;; no memory while packing one picture: that one is left out, the thread goes on, and the next is sent
  (let* ((local (make-instance 'queued-fake-frontend :width 40 :height 12 :cell-pixels '(8 16)))
         (bad (two-colour-image 64 64 3)) (good (two-colour-image 32 32 3)) (failed nil))
    (sb-int:encapsulate 'tuatara.frontend::packed-picture 'test-no-memory
                        (lambda (function image)
                          (when (and (not failed) (= (tuatara.frontend:image-width image) 64))
                            (setf failed t)
                            (error (make-condition 'storage-condition)))
                          (funcall function image)))
    (unwind-protect
         (multiple-value-bind (remote client) (far-remote local)
           (let ((runner (sb-thread:make-thread (lambda () (tuatara.frontend:run-remote-client client)))))
             (paint-picture remote bad 8 8)
             (true (tuatara.runtime:wait-until
                    (lambda ()
                      (let ((far (far-picture-of remote (tuatara.frontend:image-id bad))))
                        (and far (eq (tuatara.frontend::far-picture-state far) :refused))))
                    10)
                   "left out")
             (paint-picture remote good 8 8)
             (true (tuatara.runtime:wait-until (lambda () (far-ready-p remote (tuatara.frontend:image-id good))) 20)
                   "the next is sent")
             (tuatara.frontend:frontend-stop remote)
             (client-done runner)))
      (sb-int:unencapsulate 'tuatara.frontend::packed-picture 'test-no-memory))))

(define-test (remote far-picture-unpacks-behind-the-frames)
  ;; a picture being unpacked keeps nothing waiting: what comes after it is read, and painted, meanwhile
  (let* ((local (make-instance 'queued-fake-frontend :width 40 :height 12 :cell-pixels '(8 16)))
         (held (sb-thread:make-semaphore))
         (packed (tuatara.io:deflate (tuatara.frontend::rows-against-above (tuatara.text:make-octets 48) 12))))
    (sb-int:encapsulate 'tuatara.frontend::arrived-picture 'test-held
                        (lambda (function arriving)
                          (sb-thread:wait-on-semaphore held :timeout 30)
                          (funcall function arriving)))
    (unwind-protect
         (multiple-value-bind (remote client) (far-remote local)
           (let ((runner (sb-thread:make-thread (lambda () (tuatara.frontend:run-remote-client client)))))
             (send-to-far-client remote (lambda (box)
                                (put-whole-picture box 40 4 4 3 packed)
                                (put-title box "behind the picture")))
             (true (tuatara.runtime:wait-until
                    (lambda () (equal "behind the picture" (first (tuatara.frontend.fake:fake-titles local)))) 10)
                   "what came after is painted while the picture unpacks")
             (false (tuatara.frontend::remote-client-pictures client) "still unpacking")
             (sb-thread:signal-semaphore held)
             (true (tuatara.runtime:wait-until (lambda () (tuatara.frontend::remote-client-pictures client)) 10))
             (tuatara.frontend:frontend-stop remote)
             (client-done runner)))
      (sb-int:unencapsulate 'tuatara.frontend::arrived-picture 'test-held))))

(define-test (remote far-pictures-pinned-by-unpainted-frames-bounded)
  ;; a client not painting: frames naming pictures since let go of hold their pixels; past the most kept,
  ;; reading stops until they are painted, and then goes on
  (let* ((local (make-instance 'queued-fake-frontend :width 40 :height 12 :cell-pixels '(8 16)))
         (width 8192) (height 5461)
         (packed (tuatara.io:deflate (tuatara.text:make-octets (* width height 3)))))
    (multiple-value-bind (remote client) (far-remote local)
      (flet ((picture-and-frame (id)
               (send-to-far-client remote (lambda (box)
                                  (put-whole-picture box id width height 3 packed)))
               (true (tuatara.runtime:wait-until
                      (lambda () (find id (tuatara.frontend::remote-client-pictures client)
                                       :key #'tuatara.frontend:image-id))
                      60)
                     "picture ~d kept" id)
               (send-to-far-client remote (lambda (box) (put-frame-naming box id) (put-forget box id)))))
        (picture-and-frame 50)
        (picture-and-frame 51)
        (picture-and-frame 52)
        (send-to-far-client remote (lambda (box) (put-title box "after the flood")))
        (false (tuatara.runtime:wait-until (lambda () (arrived-title-p client "after the flood")) 2)
               "reading waits for painting")
        (tuatara.frontend::paint-arrived client)
        (true (tuatara.runtime:wait-until (lambda () (arrived-title-p client "after the flood")) 20)
              "and goes on once painted"))
      (tuatara.frontend:frontend-stop remote)
      (tuatara.frontend::close-remote-client client))))

(define-test (remote far-pictures-told-to-forget-wait-for-a-place)
  ;; more pictures told to forget than may unpack at once, as after a stall: reading waits for a place and goes on,
  ;; the connection kept
  (let* ((local (make-instance 'queued-fake-frontend :width 40 :height 12 :cell-pixels '(8 16)))
         (held (sb-thread:make-semaphore))
         (packed (tuatara.io:deflate (tuatara.frontend::rows-against-above (tuatara.text:make-octets 48) 12))))
    (sb-int:encapsulate 'tuatara.frontend::arrived-picture 'test-held
                        (lambda (function arriving)
                          (sb-thread:wait-on-semaphore held :timeout 30)
                          (funcall function arriving)))
    (unwind-protect
         (multiple-value-bind (remote client) (far-remote local)
           (let ((runner (sb-thread:make-thread (lambda () (tuatara.frontend:run-remote-client client)))))
             (send-to-far-client remote (lambda (box)
                                          (loop for id from 60 below 66
                                                do (put-whole-picture box id 4 4 3 packed)
                                                   (put-forget box id))
                                          (put-title box "after the stall")))
             (false (tuatara.runtime:wait-until
                     (lambda () (equal "after the stall" (first (tuatara.frontend.fake:fake-titles local)))) 1)
                    "reading waits while none is done")
             (false (tuatara.frontend::remote-client-ended client) "the connection is kept")
             (sb-thread:signal-semaphore held 6)
             (true (tuatara.runtime:wait-until
                    (lambda () (equal "after the stall" (first (tuatara.frontend.fake:fake-titles local)))) 20)
                   "and goes on as they are done")
             (false (tuatara.frontend::remote-client-ended client))
             (false (tuatara.frontend::remote-client-pictures client) "all forgotten")
             (tuatara.frontend:frontend-stop remote)
             (client-done runner)))
      (sb-int:unencapsulate 'tuatara.frontend::arrived-picture 'test-held))))

(define-test (remote far-picture-unpacking-leaves-a-closed-socket-alone)
  ;; a picture that does not unpack, found out once the connection is being closed here, shuts no descriptor
  (multiple-value-bind (mine theirs) (tuatara.io:socket-pair)
    (let ((client (tuatara.frontend::%make-remote-client
                   theirs (tuatara.frontend.fake:make-fake-frontend) (tuatara.frontend::make-inbox theirs :client)))
          (arriving (tuatara.frontend::make-arriving-picture 9 2 2 3 2)))
      (setf (tuatara.frontend::arriving-picture-octets arriving) (coerce #(255 255) 'tuatara.text:octets)
            (tuatara.frontend::arriving-picture-filled arriving) 2
            (tuatara.frontend::remote-client-closing client) t)
      (unwind-protect
           (progn (tuatara.frontend::unpack-picture client arriving)
                  (is = 0 (tuatara.io:poll-fd mine 1 50) "the socket was not shut"))
        (sb-posix:close mine)
        (sb-posix:close theirs)))))

(define-test (remote far-picture-failed-once-begun-is-forgotten)
  ;; a send that fails after its beginning went: the client is told to forget it, and the next picture comes
  (let* ((local (make-instance 'queued-fake-frontend :width 40 :height 12 :cell-pixels '(8 16)))
         (bad (two-colour-image 64 64 3)) (good (two-colour-image 32 32 3)) (failed nil))
    (sb-int:encapsulate 'tuatara.frontend::queue-message 'test-fails
                        (lambda (function remote kind &rest more)
                          (when (and (eq kind :picture-piece) (not failed))
                            (setf failed t)
                            (error "a send failing"))
                          (apply function remote kind more)))
    (unwind-protect
         (multiple-value-bind (remote client) (far-remote local)
           (let ((runner (sb-thread:make-thread (lambda () (tuatara.frontend:run-remote-client client)))))
             (paint-picture remote bad 8 8)
             (true (tuatara.runtime:wait-until
                    (lambda ()
                      (let ((far (far-picture-of remote (tuatara.frontend:image-id bad))))
                        (and far (eq (tuatara.frontend::far-picture-state far) :refused))))
                    10)
                   "left out")
             (paint-picture remote good 8 8)
             (true (tuatara.runtime:wait-until (lambda () (far-ready-p remote (tuatara.frontend:image-id good))) 20)
                   "the next comes")
             (false (tuatara.frontend::remote-client-ended client) "the connection is kept")
             (tuatara.frontend:frontend-stop remote)
             (client-done runner)))
      (sb-int:unencapsulate 'tuatara.frontend::queue-message 'test-fails))))
