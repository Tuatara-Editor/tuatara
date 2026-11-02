;;;; test/frames.lisp --- Several frames in one editor: keys, repeats, recordings, messages and windows.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.tests)

(define-test frames
  :description "An editor with several frames sharing one queue of events: each event runs in its frame, which holds its own last command, repeat keys and keys recorded; work begun in a frame says its messages there; killing a buffer and keeping windows in place hold in every frame; the list of frames is replaced, never changed.")

(defun run-frames (typing &key (count (1+ (reduce #'max typing :key #'first))) (text "") (width 40) (height 8))
  "Run a session of an editor with COUNT frames, each painted by a fake
frontend, all reading one queue of events, the first frame on a scratch buffer
holding TEXT and each other showing it too, each buffer in the modal layer's
NORMAL state at first. TYPING is a list of (INDEX KEYS):
KEYS, a key sequence typed in the INDEXth frame (0 the first), or a function
called in its turn. Returns the editor, its frames and their frontends."
  (let* ((first (tuatara.frontend.fake:make-fake-frontend :width width :height height))
         (frontends (cons first (loop repeat (1- count)
                                      collect (tuatara.frontend.fake:make-fake-frontend :width width :height height
                                                                                        :joining first))))
         (frames (make-array count)))
    (let ((scratch (tuatara.buffer:find-buffer "*scratch*")))
      (when scratch (tuatara.buffer:kill-buffer scratch)))
    (dolist (name '("kiwi"))
      (let ((left (tuatara.buffer:find-buffer name)))
        (when left (tuatara.buffer:kill-buffer left))))
    (tuatara.buffer:make-buffer "*scratch*" :text text)
    (setf tuatara.defaults::*command-history* '() tuatara.defaults::*pattern-history* '())
    (let ((editor (without-grammar-questions
                    (tuatara.command:run-editor
                     first :setup #'tuatara.defaults:install-defaults :buffer (tuatara.buffer:find-buffer "*scratch*")
                     :start (lambda (editor)
                              (setf (aref frames 0) (tuatara.command::editor-frame editor))
                              (loop for i from 1 below count
                                    for f in (rest frontends)
                                    do (setf (aref frames i) (tuatara.command::add-frame editor f)))
                              (loop for (index keys) in typing
                                    do (if (functionp keys)
                                           (tuatara.frontend.fake:fake-queue-action first keys)
                                           (tuatara.frontend.fake:fake-queue-keys first keys
                                                                                  :frame (aref frames index)))))))))
      (values editor (coerce frames 'list) frontends))))

(defmacro with-frames ((editor frames &rest options) typing &body body)
  "Run TYPING in an editor of several frames (RUN-FRAMES, with OPTIONS), then
BODY with EDITOR bound to the editor and each of FRAMES, a list of names, to
its frame in turn."
  (let ((all (gensym "FRAMES")) (others (gensym "OTHERS")))
    `(multiple-value-bind (,editor ,all) (run-frames ,typing ,@options)
       (declare (ignorable ,editor))
       (destructuring-bind (&optional ,@frames &rest ,others) ,all
         (declare (ignorable ,@frames) (ignore ,others))
         ,@body))))

(defun frame-buffer-text (frame)
  "The name of the buffer FRAME's selected window shows, and its text, as a list."
  (let ((buffer (tuatara.display:window-buffer (tuatara.command::frame-window frame))))
    (list (tuatara.buffer:buffer-name buffer) (tuatara.buffer:buffer-string buffer))))

(defstruct (frames-test-job (:include tuatara.runtime:job) (:constructor make-frames-test-job ()))
  "A job of the frames' tests, with no buffer.")

(defmethod tuatara.runtime:job-doing ((job frames-test-job))
  "Testing")

(defun start-frames-test-job (work)
  "Start a FRAMES-TEST-JOB on the editor running, which runs WORK, a function
of no arguments; returns the job."
  (tuatara.runtime:start-job-thread (make-frames-test-job) (lambda (job) (declare (ignore job)) (funcall work))))

(defun wait-for-job (job)
  "Wait, five seconds at most, for JOB to end."
  (loop repeat 500 while (tuatara.runtime:job-running-p job) do (sleep 0.01)))

(define-test (frames keys-run-in-their-frame)
  ;; each key runs in the frame it was typed in: its buffer, its window, its question
  (with-frames (editor (first second)) '((1 "C-x b k i w i C-j i k i w i") (0 "i t u i"))
    (is equal '("kiwi" "kiwi") (frame-buffer-text second))
    (is equal '("*scratch*" "tui") (frame-buffer-text first))
    (is eq first (tuatara.command::editor-frame editor))))

(define-test (frames repeat-keys-of-their-frame)
  ;; a key repeating the command just run repeats it only in the frame it ran in: typed in
  ;; another frame, it does what it does there, and the first frame can still repeat
  (with-frames (editor (first second)) '((1 "C-x b k i w i C-j i") (0 "i C-x ( a C-x ) C-x e") (1 "e") (0 "e"))
    (is equal '("kiwi" "e") (frame-buffer-text second))
    (is equal '("*scratch*" "aaa") (frame-buffer-text first))))

(define-test (frames last-command-of-their-frame)
  ;; a kill following a kill in another frame starts a kill of its own, never joined to that one
  (with-frames (editor (first second) :text "kea") '((1 "C-x b k i w i C-j i t u i C-a") (0 "C-k") (1 "C-k"))
    (is equal '("tui" "kea") (mapcar #'princ-to-string (subseq (tuatara.command:editor-kill-ring editor) 0 2)))))

(define-test (frames keys-recorded-in-their-frame)
  ;; keys recorded in one frame are its own: those typed in another meanwhile are not taken
  (with-frames (editor (first second)) '((0 "i C-x (") (1 "C-x b k i w i C-j i x y") (0 "a C-x ) C-x e"))
    (is equal '("kiwi" "xy") (frame-buffer-text second))
    (is equal '("*scratch*" "aa") (frame-buffer-text first))))

(define-test (frames keys-wait-for-their-frame-while-another-asks)
  ;; a key of one frame typed while a question is open in another waits, and runs in its own frame
  (with-frames (editor (first second)) '((1 "C-x b k i w i C-j i") (0 "M-x") (1 "x") (0 "C-g"))
    (is equal '("kiwi" "x") (frame-buffer-text second))
    (is equal '("*scratch*" "") (frame-buffer-text first))))

(define-test (frames job-messages-shown-where-begun)
  ;; a message said by work in the background shows in the frame the work was begun in, though
  ;; another frame is the one worked in when it comes; the frame worked in shows none of it
  (let ((go (sb-thread:make-semaphore)) (job nil))
    (with-frames (editor (first second))
        (list '(1 "C-x b k i w i C-j")
              (list 1 (lambda ()
                        (setf job (start-frames-test-job (lambda ()
                                                           (sb-thread:wait-on-semaphore go :timeout 5)
                                                           (tuatara.command:message "Pīwakawaka"))))))
              '(0 "C-n")
              (list 0 (lambda () (sb-thread:signal-semaphore go) (wait-for-job job))))
      (is eq second (tuatara.runtime::job-frame job))
      (is equal "Pīwakawaka" (tuatara.command::frame-message second))
      (false (equal "Pīwakawaka" (tuatara.command::frame-message first))))))

(define-test (frames job-messages-shown-here-once-their-frame-is-gone)
  ;; a message of work begun in a frame since taken away shows in the frame worked in
  (let ((go (sb-thread:make-semaphore)) (job nil))
    (with-frames (editor (first second))
        (list '(1 "C-x b k i w i C-j")
              (list 1 (lambda ()
                        (setf job (start-frames-test-job (lambda ()
                                                           (sb-thread:wait-on-semaphore go :timeout 5)
                                                           (tuatara.command:message "Pīwakawaka"))))))
              (list 0 (lambda ()
                        (let ((editor tuatara.command:*editor*))
                          (tuatara.command::remove-frame editor (second (tuatara.command::editor-frames editor))))
                        (sb-thread:signal-semaphore go)
                        (wait-for-job job))))
      (true (tuatara.command::frame-deleted second))
      (is equal (list first) (tuatara.command::editor-frames editor))
      (is equal "Pīwakawaka" (tuatara.command::frame-message first)))))

(define-test (frames deferred-actions-run-where-begun)
  ;; work handed on from the background by work begun in a frame runs with that frame the one
  ;; worked in, and the frame worked in before is so again after
  (let ((go (sb-thread:make-semaphore)) (job nil) (seen '()))
    (with-frames (editor (first second))
        (list '(1 "C-x b k i w i C-j")
              (list 1 (lambda ()
                        (setf job (start-frames-test-job
                                   (lambda ()
                                     (sb-thread:wait-on-semaphore go :timeout 5)
                                     (tuatara.command:defer-action
                                      (lambda ()
                                        (push (tuatara.command::editor-frame tuatara.command:*editor*) seen))))))))
              '(0 "C-n")
              (list 0 (lambda () (sb-thread:signal-semaphore go) (wait-for-job job))))
      (is equal (list second) seen)
      (is eq first (tuatara.command::editor-frame editor)))))

(define-test (frames killed-buffer-left-in-no-frame)
  ;; a buffer killed in one frame is shown in no window of any frame
  (with-frames (editor (first second)) '((1 "C-x b k i w i C-j") (0 "C-x b k i w i C-j C-x k RET"))
    (false (tuatara.buffer:find-buffer "kiwi"))
    (dolist (frame (list first second))
      (true (tuatara.buffer:live-buffer-p (tuatara.display:window-buffer (tuatara.command::frame-window frame)))))))

(define-test (frames windows-of-every-frame)
  ;; the windows showing a buffer are those of every frame, each with a point of its own
  (with-frames (editor (first second) :text "kea tūī") '((1 "C-e") (0 "C-f"))
    (let* ((buffer (tuatara.buffer:find-buffer "*scratch*"))
           (windows (tuatara.command:buffer-windows buffer editor)))
      (is = 2 (length windows))
      (is equal (list (tuatara.command::frame-window first) (tuatara.command::frame-window second)) windows)
      (is = 1 (tuatara.display:window-point (tuatara.command::frame-window first)))
      (is = (length (sb-ext:string-to-octets "kea tūī" :external-format :utf-8))
          (tuatara.display:window-point (tuatara.command::frame-window second))))))

(define-test (frames list-replaced-never-changed)
  ;; adding and taking away frames replaces the list of frames: a list read before is as it was,
  ;; and threads reading it all the while, as waking the editor and asking for keys do, meet no harm
  (let ((before nil) (with-two nil) (errors '()) (reads 0))
    (with-frames (editor (first))
        (list (list 0 (lambda ()
                        (let* ((editor tuatara.command:*editor*)
                               (buffer (tuatara.buffer:find-buffer "*scratch*"))
                               (done nil)
                               (reader (sb-thread:make-thread
                                        (lambda ()
                                          (loop until done
                                                do (handler-case
                                                       (progn (tuatara.command:key-waiting-p)
                                                              (tuatara.command:wake-editor editor)
                                                              (tuatara.command:buffer-windows buffer editor)
                                                              (dolist (frame (tuatara.command::editor-frames editor))
                                                                (unless (tuatara.command::frame-p frame)
                                                                  (error "Not a frame: ~s" frame)))
                                                              (incf reads))
                                                     (error (e) (push e errors))))))))
                          (setf before (tuatara.command::editor-frames editor))
                          (let ((added (tuatara.command::add-frame
                                        editor (tuatara.frontend.fake:make-fake-frontend
                                                :joining (tuatara.command:editor-frontend editor)))))
                            (setf with-two (tuatara.command::editor-frames editor))
                            (tuatara.command::remove-frame editor added))
                          (dotimes (i 2000)
                            (tuatara.command::remove-frame
                             editor (tuatara.command::add-frame
                                     editor (tuatara.frontend.fake:make-fake-frontend
                                             :joining (tuatara.command:editor-frontend editor)))))
                          (setf done t)
                          (sb-thread:join-thread reader)))))
      (is equal (list first) before)
      (is = 2 (length with-two))
      (is eq first (first with-two))
      (is equal (list first) (tuatara.command::editor-frames editor))
      (true (plusp reads))
      (is equal '() errors))))

(define-test (frames last-frame-stays)
  ;; the only frame is never taken away
  (with-frames (editor (first)) '((0 "C-n"))
    (fail (tuatara.command::remove-frame editor first))
    (is equal (list first) (tuatara.command::editor-frames editor))
    (false (tuatara.command::frame-deleted first))))

(define-test (frames keys-wait-for-their-frame-while-a-question-takes-in-work)
  ;; a question whose candidates come from the background (finding a file) reads keys as wakes
  ;; come; a key typed in another frame meanwhile still waits for that frame, the question keeping
  ;; its own frame and text
  (with-frames (editor (first second)) '((1 "C-x b k i w i C-j i") (0 "C-x C-f") (1 "x") (0 "C-g"))
    (is equal '("kiwi" "x") (frame-buffer-text second))
    (is equal '("*scratch*" "") (frame-buffer-text first))
    (false (tuatara.command::frame-minibuffer-active first))
    (false (tuatara.command::frame-minibuffer-active second))
    ;; the key waiting in the second frame ran last, there
    (is eq second (tuatara.command::editor-frame editor))))

(define-test (frames recorded-keys-hold-no-frame)
  ;; keys recorded in a frame are kept without it, and play in another frame
  (with-frames (editor (first second)) '((1 "C-x b k i w i C-j i C-x ( a b C-x )") (0 "i C-x e"))
    (true (plusp (length tuatara.defaults::*last-recorded-keys*)))
    (true (every (lambda (event) (null (tuatara.frontend:event-frame event))) tuatara.defaults::*last-recorded-keys*))
    (is equal '("kiwi" "ab") (frame-buffer-text second))
    (is equal '("*scratch*" "ab") (frame-buffer-text first))))

(defstruct (frames-taken-job (:include frames-test-job) (:constructor make-frames-taken-job ()))
  "A job of the frames' tests that notes the frame it is taken in with, and says so.")

(defvar *frames-taken-in* '()
  "The frames the FRAMES-TAKEN-JOBs were taken in with, the latest first.")

(defmethod tuatara.runtime:job-take-in ((job frames-taken-job))
  "Note the frame worked in as JOB is taken in, and say Kōkako."
  (push (tuatara.command::editor-frame tuatara.command:*editor*) *frames-taken-in*)
  (tuatara.command:message "Kōkako"))

(define-test (frames jobs-taken-in-where-begun)
  ;; a job begun in a frame is taken in with that frame the one worked in, what it says shown
  ;; there, though another frame is worked in when it ends; that one is worked in again after
  (let ((go (sb-thread:make-semaphore)) (job nil) (*frames-taken-in* '()))
    (with-frames (editor (first second))
        (list '(1 "C-x b k i w i C-j")
              (list 1 (lambda ()
                        (setf job (tuatara.runtime:start-job-thread
                                   (make-frames-taken-job)
                                   (lambda (job)
                                     (declare (ignore job))
                                     (sb-thread:wait-on-semaphore go :timeout 5))))))
              '(0 "C-n")
              (list 0 (lambda () (sb-thread:signal-semaphore go) (wait-for-job job)))
              '(0 "C-n"))
      (is equal (list second) *frames-taken-in*)
      (is equal "Kōkako" (tuatara.command::frame-message second))
      (is eq first (tuatara.command::editor-frame editor)))))

(defstruct (frames-deferring-job (:include frames-test-job) (:constructor make-frames-deferring-job (taken)))
  "A job of the frames' tests whose taking in calls TAKEN, a function of no arguments."
  (taken nil))

(defmethod tuatara.runtime:job-take-in ((job frames-deferring-job))
  "Call JOB's TAKEN."
  (funcall (frames-deferring-job-taken job)))

(defun run-deferring-job (taken)
  "Run a session of two frames: a FRAMES-DEFERRING-JOB begun in the second
frame, calling TAKEN as it is taken in, ends while the first is worked in.
Returns the editor and its frames, as RUN-FRAMES does."
  (let ((go (sb-thread:make-semaphore)) (job nil))
    (run-frames (list '(1 "C-x b k i w i C-j")
                      (list 1 (lambda ()
                                (setf job (tuatara.runtime:start-job-thread
                                           (make-frames-deferring-job taken)
                                           (lambda (job)
                                             (declare (ignore job))
                                             (sb-thread:wait-on-semaphore go :timeout 5))))))
                      '(0 "C-n")
                      (list 0 (lambda () (sb-thread:signal-semaphore go) (wait-for-job job)))
                      '(0 "C-n")))))

(define-test (frames work-deferred-at-taking-in-runs-in-its-frame)
  ;; work a job's taking in hands on runs at once, with the job's frame the one worked in, though
  ;; nothing more is typed there
  (let ((seen '()))
    (multiple-value-bind (editor frames)
        (run-deferring-job (lambda ()
                             (tuatara.command:defer-action
                              (lambda () (push (tuatara.command::editor-frame tuatara.command:*editor*) seen)))))
      (is equal (list (second frames)) seen)
      (is eq (first frames) (tuatara.command::editor-frame editor)))))

(define-test (frames work-deferred-in-a-closed-frame-still-runs)
  ;; work handed on to be done in a frame that closes before it runs is done in the frame left
  (let ((seen '()))
    (multiple-value-bind (editor frames)
        (run-deferring-job (lambda ()
                             (let ((editor tuatara.command:*editor*))
                               (tuatara.command:defer-action
                                (lambda () (push (tuatara.command::editor-frame tuatara.command:*editor*) seen)))
                               (tuatara.command::remove-frame editor (tuatara.command::editor-frame editor)))))
      (is equal (list (first frames)) seen)
      (is equal (list (first frames)) (tuatara.command::editor-frames editor)))))

;;; Frames opened and closed by their commands, in a window

(defun run-window-keys (typing &key (text "") (width 40) (height 8) files start)
  "Run a session of an editor in a fake window, which can open other frames,
on a scratch buffer holding TEXT (or FILES), START called once they are shown.
TYPING is a list of keys, as a string, or (FRAME-INDEX KEYS) for keys typed
in the INDEXth frame open then (0 the first), or a function called in its
turn. Returns the editor, its frames' frontends as they were opened, and the
first frontend."
  (let ((first (tuatara.frontend.fake:make-fake-frontend :width width :height height :font-steps 0)))
    (let ((scratch (tuatara.buffer:find-buffer "*scratch*")))
      (when scratch (tuatara.buffer:kill-buffer scratch)))
    (dolist (name '("kiwi"))
      (let ((left (tuatara.buffer:find-buffer name)))
        (when left (tuatara.buffer:kill-buffer left))))
    (tuatara.buffer:make-buffer "*scratch*" :text text)
    (setf tuatara.defaults::*command-history* '() tuatara.defaults::*pattern-history* '())
    (dolist (item typing)
      (cond ((stringp item) (tuatara.frontend.fake:fake-queue-keys first item))
            ((functionp item) (tuatara.frontend.fake:fake-queue-action first item))
            (t (destructuring-bind (index keys) item
                 (tuatara.frontend.fake:fake-queue-action
                  first (lambda ()
                          (let ((frame (nth index (tuatara.command:editor-frames tuatara.command:*editor*))))
                            (dolist (spec (reverse (tuatara.frontend:parse-key-sequence keys)))
                              (push (tuatara.frontend:make-event :key :key (tuatara.frontend:spec-key spec) :frame frame)
                                    (car (tuatara.frontend.fake::fake-source first)))))))))))
    (let ((editor (without-grammar-questions
                    (tuatara.command:run-editor first :files files :setup #'tuatara.defaults:install-defaults
                                                      :buffer (tuatara.buffer:find-buffer "*scratch*")
                                                      :start start))))
      (values editor (mapcar #'tuatara.command::frame-frontend (tuatara.command:editor-frames editor)) first))))

(defun frame-shows (frame)
  "The name of the buffer FRAME's selected window shows."
  (tuatara.buffer:buffer-name (tuatara.display:window-buffer (tuatara.command:frame-window frame))))

(define-test (frames make-frame-opens-another)
  ;; C-x 5 2 opens a frame showing the current buffer, brought before the others and worked in:
  ;; what is typed then goes there, into the same buffer
  (multiple-value-bind (editor frontends) (run-window-keys '("C-x 5 2 i k e a"))
    (destructuring-bind (first second) (tuatara.command:editor-frames editor)
      (is eq second (tuatara.command:editor-frame editor))
      (is equal "*scratch*" (frame-shows first))
      (is equal "*scratch*" (frame-shows second))
      (is equal "kea" (tuatara.buffer:buffer-string (tuatara.buffer:find-buffer "*scratch*")))
      (is = 1 (tuatara.frontend.fake:fake-raised (second frontends)))
      (true (tuatara.frontend.fake:fake-frame-showing (second frontends) "kea")))))

(define-test (frames make-frame-refused-in-a-terminal)
  ;; a terminal shows one frame: asking for another says how to have them
  (multiple-value-bind (f editor) (run-keys "C-x 5 2")
    (is = 1 (length (tuatara.command:editor-frames editor)))
    (true (tuatara.frontend.fake:fake-frame-showing-across-rows f "start Tuatara with --gui for frames"))))

(define-test (frames delete-frame-keeps-the-last)
  ;; C-x 5 0 closes the frame worked in, its buffer staying; the last frame stays, said in plain words
  (multiple-value-bind (editor frontends first-frontend)
      (run-window-keys '("C-x 5 2 C-x b k i w i C-j C-x 5 0 C-x 5 0"))
    (declare (ignore frontends))
    (is = 1 (length (tuatara.command:editor-frames editor)))
    (is equal "*scratch*" (frame-shows (tuatara.command:editor-frame editor)))
    (true (tuatara.buffer:find-buffer "kiwi"))
    (true (tuatara.frontend.fake:fake-frame-showing first-frontend "This is the only frame; C-x C-c quits"))))

(define-test (frames delete-other-frames-keeps-this-one)
  ;; C-x 5 1 closes every frame but the one worked in
  (multiple-value-bind (editor frontends) (run-window-keys '("C-x 5 2 C-x 5 2 C-x b k i w i C-j C-x 5 1"))
    (declare (ignore frontends))
    (is = 1 (length (tuatara.command:editor-frames editor)))
    (is equal "kiwi" (frame-shows (tuatara.command:editor-frame editor)))))

(define-test (frames close-button-closes-its-frame)
  ;; a frame's close button closes that frame only, the editor going on in the others; the last
  ;; one's quits, as C-x C-c does
  (let ((closed nil) (running nil))
    (multiple-value-bind (editor frontends first-frontend)
        (run-window-keys (list "C-x 5 2"
                               (lambda ()
                                 (let ((editor tuatara.command:*editor*))
                                   (setf closed (second (tuatara.command:editor-frames editor)))
                                   (push (tuatara.frontend:make-event :close :frame closed)
                                         (car (tuatara.frontend.fake::fake-source
                                               (tuatara.command:editor-frontend editor))))))
                               "i t u i"
                               (lambda () (setf running (tuatara.command:editor-running tuatara.command:*editor*)))
                               (lambda ()
                                 (push (tuatara.frontend:make-event
                                        :close :frame (first (tuatara.command:editor-frames tuatara.command:*editor*)))
                                       (car (tuatara.frontend.fake::fake-source
                                             (tuatara.command:editor-frontend tuatara.command:*editor*)))))
                               "i k e a"))
      (declare (ignore frontends first-frontend))
      (true (tuatara.command::frame-deleted closed))
      (true running)
      (is = 1 (length (tuatara.command:editor-frames editor)))
      ;; the last frame's close button ended the editor before what came after it
      (false (tuatara.command:editor-running editor))
      (is equal "tui" (tuatara.buffer:buffer-string (tuatara.buffer:find-buffer "*scratch*"))))))

(define-test (frames other-frame-goes-round)
  ;; C-x 5 o goes to the next frame, the first after the last, and brings its window before the others
  (let ((seen '()))
    (flet ((note () (push (position (tuatara.command:editor-frame tuatara.command:*editor*)
                                    (tuatara.command:editor-frames tuatara.command:*editor*))
                          seen)))
      (multiple-value-bind (editor frontends first-frontend)
          (run-window-keys (list "C-x 5 o" "C-x 5 2" #'note "C-x 5 o" #'note "C-x 5 o" #'note "C-u - 1 C-x 5 o" #'note))
        (declare (ignore editor))
        (is equal '(1 0 1 0) (reverse seen))
        (is = 2 (tuatara.frontend.fake:fake-raised first-frontend))
        (is = 2 (tuatara.frontend.fake:fake-raised (second frontends)))
        (true (tuatara.frontend.fake:fake-frame-showing first-frontend "There is no other frame"))))))

(define-test (frames focus-selects-its-frame)
  ;; the frame whose window is given the keyboard is the one worked in, where a message said by
  ;; no frame's work shows
  (with-frames (editor (first second))
      (list '(1 "C-x b k i w i C-j")
            '(0 "C-n")
            (list 0 (lambda ()
                      (push (tuatara.frontend:make-event :focus :frame (second (tuatara.command:editor-frames
                                                                                 tuatara.command:*editor*)))
                            (car (tuatara.frontend.fake::fake-source
                                  (tuatara.command:editor-frontend tuatara.command:*editor*))))))
            (list 0 (lambda ()
                      (sb-thread:join-thread (sb-thread:make-thread (lambda () (tuatara.command:message "Ruru")))))))
    (is eq second (tuatara.command:editor-frame editor))
    (is equal "Ruru" (tuatara.command::frame-message second))
    (false (equal "Ruru" (tuatara.command::frame-message first)))))

(define-test (frames others-painted-only-on-change)
  ;; a frame not worked in is painted again only when what it shows changed: typing in another
  ;; buffer leaves it alone, typing in its buffer paints it, the text shown
  (let ((tuatara.command::*frame-at-least-every* 0) (counts '()))
    (flet ((note-paints (editor)
             (push (tuatara.frontend.fake:fake-frames
                    (tuatara.command::frame-frontend (second (tuatara.command:editor-frames editor))))
                   counts)))
      (with-frames (editor (first second))
          (list '(1 "C-x b k i w i C-j")
                '(0 "i")
                (list 0 (lambda ()
                          (push #'note-paints (tuatara.command:editor-post-command-functions tuatara.command:*editor*))))
                '(0 "a b c d e f")
                (list 0 (lambda ()
                          (setf (tuatara.command:editor-post-command-functions tuatara.command:*editor*)
                                (remove #'note-paints (tuatara.command:editor-post-command-functions
                                                       tuatara.command:*editor*)))))
                '(0 "C-x b k i w i C-j i t i e k e"))
        ;; noted after each of a to f: the second frame painted no more meanwhile
        (is = 6 (length counts))
        (is = 1 (length (remove-duplicates counts)) "~s" counts)
        (true (> (tuatara.frontend.fake:fake-frames (tuatara.command::frame-frontend second)) (first counts)))
        (true (tuatara.frontend.fake:fake-frame-showing (tuatara.command::frame-frontend second) "tieke"))))))

(define-test (frames find-file-in-another)
  ;; C-x 5 f, C-x 5 b and C-x 5 d open a file, a buffer and a folder in a new frame, worked in;
  ;; the frame they were asked in shows what it showed
  (with-test-directory (dir)
    (let ((path (put-file (merge-pathnames "kea.txt" dir) "kea")))
      (multiple-value-bind (editor frontends)
          (run-window-keys (list (format nil "C-x 5 f C-a C-k ~a RET" (rimu-typed (namestring path)))
                                 '(0 "C-x 5 b k i w i C-j")
                                 (format nil "C-x 5 d C-a C-k ~a RET" (rimu-typed (namestring dir)))))
        (declare (ignore frontends))
        (destructuring-bind (first second third fourth) (tuatara.command:editor-frames editor)
          (is equal "*scratch*" (frame-shows first))
          (is equal "kea.txt" (frame-shows second))
          (is equal "kiwi" (frame-shows third))
          (true (tuatara.defaults::rimu-list-p (tuatara.display:window-buffer (tuatara.command:frame-window fourth))))
          (is eq fourth (tuatara.command:editor-frame editor)))))))

(define-test (frames start-frames-spread-files)
  ;; --frames N opens N frames as the editor starts, the files one to each in turn, those left
  ;; over behind the first; the first frame is the one worked in
  (with-test-directory (dir)
    (let ((paths (loop for name in '("a.txt" "b.txt" "c.txt" "d.txt")
                       collect (namestring (put-file (merge-pathnames name dir) name)))))
      (let ((editor (run-window-keys '() :files paths :start (tuatara.terminal::start-function :frames 3))))
        (is equal '("a.txt" "b.txt" "c.txt") (mapcar #'frame-shows (tuatara.command:editor-frames editor)))
        (is eq (first (tuatara.command:editor-frames editor)) (tuatara.command:editor-frame editor))
        (true (tuatara.buffer:find-buffer "d.txt")))
      (let ((editor (run-window-keys '() :files (list (first paths))
                                         :start (tuatara.terminal::start-function :frames 3))))
        (is equal '("a.txt" "a.txt" "a.txt") (mapcar #'frame-shows (tuatara.command:editor-frames editor)))))))
