;;;; test/session.lisp --- The session kept: files and windows opened again as they were.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.tests)

(define-test session
  :description "With *keep-session* the editor writes the files, directories and windows it shows as it
stops, for you alone, and opens them again as they were when started without files: points, splits
and their sizes, the selected window, a file opened safely opened safely again, a file gone left out;
a session file not of its own kind is left alone, nothing in it evaluated; off, nothing is written.")

(defun forget-session-buffers ()
  "Kill every buffer visiting a file or listing a directory, as a new start of the editor has none."
  (dolist (buffer (copy-list tuatara.buffer:*buffers*))
    (when (or (tuatara.buffer:buffer-file buffer) (tuatara.buffer:buffer-local buffer 'tuatara.defaults::rimu-directory))
      (tuatara.buffer:kill-buffer buffer)))
  (setf tuatara.command::*buffer-order* (remove-if-not #'tuatara.command:live-buffer-p tuatara.command::*buffer-order*)))

(defun session-start (keys &key (width 120) (height 30))
  "Start the editor without files, type KEYS, and stop it. Returns the frontend and the editor."
  (let ((f (tuatara.frontend.fake:make-fake-frontend :width width :height height)))
    (when keys (queue-test-keys f (list keys)))
    (values f (tuatara.command:run-editor f :setup #'tuatara.defaults:install-defaults))))

(define-test (session kept-and-opened-again)
  (with-test-directory (dir)
    (let ((one (namestring (merge-pathnames "one.txt" dir)))
          (two (namestring (merge-pathnames "two.txt" dir)))
          (evidence (namestring (merge-pathnames "evidence.txt" dir)))
          (tuatara-user:*keep-session* t)
          (tuatara.defaults::*modal-editing* nil))
      (alexandria:write-string-into-file (format nil "one~%line two~%line three~%") one)
      (alexandria:write-string-into-file (format nil "two~%second~%") two)
      (alexandria:write-string-into-file "evidence" evidence)
      (ignore-errors (delete-file (tuatara.defaults::session-path)))
      (forget-session-buffers)
      ;; one.txt on the left at its third line, two.txt on the right, selected; evidence opened safely, not shown
      (session-start (format nil "C-u C-x C-f C-a C-k ~a RET C-x C-f C-a C-k ~a RET M-> C-p C-x 3 C-x o C-x C-f C-a C-k ~a RET C-n"
                             (rimu-typed evidence) (rimu-typed one) (rimu-typed two)))
      (let ((kept (alexandria:read-file-into-string (tuatara.defaults::session-path))))
        (true (uiop:string-prefix-p "tuatara session 1" kept))
        (true (search "split side-by-side 2" kept))
        (true (search (format nil "window 1 0 file 4 ~a" two) kept) "~a" kept)
        (true (search (format nil "buffer safe 0 ~a" evidence) kept) "~a" kept)
        (is = #o600 (logand #o777 (sb-posix:stat-mode (sb-posix:stat (tuatara.defaults::session-path))))))
      ;; started again without files: as it was
      (forget-session-buffers)
      (delete-file two)
      (alexandria:write-string-into-file (format nil "two~%second~%") two)
      (multiple-value-bind (f editor) (session-start nil)
        (let* ((windows (tuatara.command:editor-windows editor))
               (selected (tuatara.command:editor-window editor)))
          (is = 2 (length windows))
          (is string= "two.txt" (tuatara.buffer:buffer-name (tuatara.display:window-buffer selected)))
          (is = 4 (tuatara.display:window-point selected))
          (let ((left (find selected windows :test-not #'eq)))
            (is string= "one.txt" (tuatara.buffer:buffer-name (tuatara.display:window-buffer left)))
            (true (< (tuatara.display:area-left left) (tuatara.display:area-left selected)))
            (true (<= 55 (tuatara.display:area-width left) 62)))
          (let ((safe (tuatara.buffer:find-buffer "evidence.txt")))
            (true safe)
            (true (and safe (tuatara.io:safe-visit-p safe))))
          (true (tuatara.frontend.fake:fake-frame-showing f "Session opened again"))))
      ;; started with a file: the session's files opened behind it, its windows its own
      (forget-session-buffers)
      (let ((f (tuatara.frontend.fake:make-fake-frontend :width 120 :height 30)))
        (let ((editor (tuatara.command:run-editor f :setup #'tuatara.defaults:install-defaults :files (list two))))
          (is = 1 (length (tuatara.command:editor-windows editor)))
          (is string= "two.txt" (tuatara.buffer:buffer-name (tuatara.display:window-buffer (tuatara.command:editor-window editor))))
          (true (tuatara.buffer:find-buffer "one.txt"))
          (true (tuatara.frontend.fake:fake-frame-showing f "of the session kept opened again"))))
      ;; a view kept by the file it shows
      (let ((view (tuatara.buffer:make-buffer "kea.pdf")))
        (setf (tuatara.buffer:buffer-local view :viewed-file) one)
        (is equal (list :file 0 one) (tuatara.defaults::session-entry view))
        (tuatara.buffer:kill-buffer view))
      ;; a file no longer there: left out, said
      (forget-session-buffers)
      (delete-file evidence)
      (let ((f (session-start nil)))
        (true (tuatara.frontend.fake:fake-frame-showing f "1 file no longer there")))
      (forget-session-buffers))))

(define-test (session frames-kept-and-opened-again)
  ;; each frame's windows kept in a section of its own, the frame worked in first; opened again
  ;; as frames, in a window and in a terminal alike
  (with-test-directory (dir)
    (let ((one (namestring (put-file (merge-pathnames "one.txt" dir) "one")))
          (two (namestring (put-file (merge-pathnames "two.txt" dir) "two")))
          (tuatara-user:*keep-session* t)
          (tuatara.defaults::*modal-editing* nil))
      (ignore-errors (delete-file (tuatara.defaults::session-path)))
      (forget-session-buffers)
      (let ((f (tuatara.frontend.fake:make-fake-frontend :width 120 :height 30 :font-steps 0)))
        (queue-test-keys f (list (format nil "C-x C-f C-a C-k ~a RET C-x 5 f C-a C-k ~a RET"
                                         (rimu-typed one) (rimu-typed two))))
        (tuatara.command:run-editor f :setup #'tuatara.defaults:install-defaults))
      (let ((kept (alexandria:read-file-into-string (tuatara.defaults::session-path))))
        (true (search (format nil "frame~%window 1 0 file 0 ~a~%frame~%window 1 0 file 0 ~a~%end" two one) kept)
              "~a" kept))
      (multiple-value-bind (buffers tree others) (tuatara.defaults::read-session)
        (declare (ignore buffers))
        (is equal (list :window t 0 (list :file 0 two)) tree)
        (is equal (list (list :window t 0 (list :file 0 one))) others))
      ;; in a window: both frames, the one worked in first
      (forget-session-buffers)
      (let* ((f (tuatara.frontend.fake:make-fake-frontend :width 120 :height 30 :font-steps 0))
             (editor (tuatara.command:run-editor f :setup #'tuatara.defaults:install-defaults)))
        (is equal '("two.txt" "one.txt")
            (mapcar (lambda (frame)
                      (tuatara.buffer:buffer-name (tuatara.display:window-buffer (tuatara.command:frame-window frame))))
                    (tuatara.command:editor-frames editor)))
        (is eq (first (tuatara.command:editor-frames editor)) (tuatara.command:editor-frame editor)))
      ;; in a terminal: both frames, taking turns on it, the one worked in shown
      (forget-session-buffers)
      (multiple-value-bind (f editor) (session-start nil)
        (is equal '("two.txt" "one.txt")
            (mapcar (lambda (frame)
                      (tuatara.buffer:buffer-name (tuatara.display:window-buffer (tuatara.command:frame-window frame))))
                    (tuatara.command:editor-frames editor)))
        (is eq (first (tuatara.command:editor-frames editor)) (tuatara.command:editor-frame editor))
        (true (tuatara.frontend.fake:fake-frame-showing f "] F1 two.txt")))
      (let ((kept (alexandria:read-file-into-string (tuatara.defaults::session-path))))
        (true (search (format nil "frame~%window 1 0 file 0 ~a~%frame~%window 1 0 file 0 ~a~%end" two one) kept)
              "~a" kept))
      ;; and so in a window again: both frames
      (forget-session-buffers)
      (let* ((f (tuatara.frontend.fake:make-fake-frontend :width 120 :height 30 :font-steps 0))
             (editor (tuatara.command:run-editor f :setup #'tuatara.defaults:install-defaults)))
        (is = 2 (length (tuatara.command:editor-frames editor))))
      (forget-session-buffers))))

(define-test (session frames-of-a-session-bounded)
  ;; a session of a thousand frames opens sixteen, in a window; the others are kept as they
  ;; were, never dropped, and the session written back never holds more frames than it had
  (let ((tuatara-user:*keep-session* t)
        (tuatara.defaults::*modal-editing* nil)
        (text (with-output-to-string (out)
                (format out "tuatara session 1~%")
                (dotimes (i 1000) (format out "frame~%window 1 ~d scratch 0 *scratch*~%" i))
                (format out "end~%"))))
    (flet ((frames-written ()
             (count "frame" (uiop:split-string (alexandria:read-file-into-string (tuatara.defaults::session-path))
                                               :separator '(#\Newline))
                    :test #'string=)))
      (forget-session-buffers)
      (alexandria:write-string-into-file text (tuatara.defaults::session-path) :if-exists :supersede)
      (let* ((f (tuatara.frontend.fake:make-fake-frontend :width 80 :height 20 :font-steps 0))
             (editor (progn (queue-test-keys f (list "C-x 5 2"))
                            (tuatara.command:run-editor f :setup #'tuatara.defaults:install-defaults))))
        ;; sixteen opened, and one more by C-x 5 2
        (is = 17 (length (tuatara.command:editor-frames editor))))
      (is = 1000 (frames-written))
      ;; the sections not opened are those read, as they were, but the last, which the frame
      ;; opened since took the room of
      (let ((written (alexandria:read-file-into-string (tuatara.defaults::session-path))))
        (true (search (format nil "frame~%window 1 998 scratch 0 *scratch*~%end") written))
        (false (search "window 1 999 " written)))
      ;; in a terminal: sixteen too, every other section kept
      (multiple-value-bind (f editor) (session-start nil)
        (declare (ignore f))
        (is = 16 (length (tuatara.command:editor-frames editor))))
      (is = 1000 (frames-written))
      (forget-session-buffers))))

(define-test (session never-evaluated-and-off)
  (with-test-directory (dir)
    (let ((one (namestring (merge-pathnames "one.txt" dir))))
      (alexandria:write-string-into-file "one" one)
      ;; not of its own kind: left alone, nothing in it evaluated, the editor starts as usual
      (let ((tuatara-user:*keep-session* t))
        (dolist (text (list (format nil "(progn (setf *session-file-evaluated* t))~%")
                            (format nil "tuatara session 1~%frame~%split stacked 2 #.(setf *session-file-evaluated* t) 0.5~%")
                            (format nil "tuatara session 1~%frame~%window 1 0 file 0 ~a~%window 0 0 file 0 ~a~%" one one)
                            (format nil "tuatara session 1~%frame~%split stacked 100000 0.5 0.5~%")))
          (forget-session-buffers)
          (alexandria:write-string-into-file text (tuatara.defaults::session-path) :if-exists :supersede)
          (multiple-value-bind (f editor) (session-start nil)
            (declare (ignore f))
            (is = 1 (length (tuatara.command:editor-windows editor))))
          (false (boundp '*session-file-evaluated*))))
      ;; off: nothing written, nothing opened again
      (forget-session-buffers)
      (ignore-errors (delete-file (tuatara.defaults::session-path)))
      (let ((tuatara-user:*keep-session* nil))
        (session-start (format nil "C-x C-f C-a C-k ~a RET" (rimu-typed one)))
        (false (probe-file (tuatara.defaults::session-path))))
      (forget-session-buffers))))

(define-test (session startup-file-that-cannot-open)
  ;; a file given at the start that cannot be read is said, and the others open
  (with-test-directory (dir)
    (let ((locked (put-file (merge-pathnames "locked.txt" dir) "hidden"))
          (open (put-file (merge-pathnames "open.txt" dir) "shown")))
      (sb-posix:chmod (namestring locked) 0)
      (unwind-protect
           (let ((f (tuatara.frontend.fake:make-fake-frontend :width 120 :height 8)))
             (tuatara.frontend.fake:fake-queue-keys f "C-g")
             (let ((editor (tuatara.command:run-editor f :setup #'tuatara.defaults:install-defaults
                                                         :files (list (namestring locked) (namestring open)))))
               (is string= "open.txt" (tuatara.buffer:buffer-name (session-buffer editor)))
               (true (tuatara.frontend.fake:fake-frame-showing f "locked.txt:"))))
        (sb-posix:chmod (namestring locked) #o600)))))
