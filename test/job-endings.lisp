;;;; test/job-endings.lisp --- Work in the background always ends, and waiting for it never freezes the editor.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.tests)

(define-test job-endings
  :description "A job whose thread runs out of memory or stack ends as failed, never left running; waiting for a save that does not end can be stopped with C-g, the kill or exit then not done.")

(defun wait-for-status (read &optional (seconds 5))
  "Wait until READ, a function of no arguments, gives something other than :RUNNING; what it gave."
  (loop repeat (* seconds 100)
        for status = (funcall read)
        while (eq status :running) do (sleep 0.01)
        finally (return status)))

(define-test (job-endings out-of-memory-ends-the-job)
  ;; a lack of memory (a storage condition, which is no error) in a job's work: failed, said
  (multiple-value-bind (f editor) (run-keys "" :text (format nil "b~%a~%") :width 80 :height 8)
    (declare (ignore f))
    (let ((tuatara.command:*editor* editor)
          (tuatara.defaults::*sort-wait* 1/100)
          (b (tuatara.display:window-buffer (tuatara.command:editor-window editor))))
      (let ((job (tuatara.defaults::start-sort b 0 (tuatara.buffer:buffer-size b) "Sorting"
                                               (lambda (job rope start end)
                                                 (declare (ignore job rope start end))
                                                 (error 'storage-condition)))))
        (is eq :failed (wait-for-status (lambda () (tuatara.defaults::sort-job-status job))))
        (tuatara.runtime:forget-job job))
      ;; a sum likewise
      (let ((job (tuatara.defaults::start-hash b "the test" 10 (lambda (&rest arguments) (declare (ignore arguments))
                                                                  (error 'storage-condition)))))
        (is eq :failed (wait-for-status (lambda () (tuatara.defaults::hash-job-status job))))
        (tuatara.runtime:forget-job job)))))

(define-test (job-endings waiting-for-a-save-can-be-stopped)
  ;; a save that does not end (a file system that does not answer): C-g stops the waiting,
  ;; the exit or kill not done, the editor never frozen
  (let* ((thread (sb-thread:make-thread (lambda () (sleep 30)) :name "a save that hangs"))
         (result :not-asked))
    (unwind-protect
         (let ((f (tuatara.frontend.fake:make-fake-frontend :width 80 :height 8)))
           ;; keys queued count as pressed while the editor waits
           (setf (tuatara.frontend.fake:fake-pending f) (lambda (f) (tuatara.frontend.fake::fake-events f)))
           (let ((old (tuatara.buffer:find-buffer "*scratch*"))) (when old (tuatara.buffer:kill-buffer old)))
           (tuatara.buffer:make-buffer "*scratch*" :text "")
           (tuatara.frontend.fake:fake-queue-action
            f (lambda ()
                (let ((job (tuatara.io::%make-save-job :buffer (tuatara.buffer:find-buffer "*scratch*") :thread thread)))
                  (setf result (tuatara.defaults::wait-for-saves (list job))))))
           (tuatara.frontend.fake:fake-queue-keys f "C-g")
           (let ((start (get-internal-real-time)))
             (tuatara.command:run-editor f :setup #'tuatara.defaults:install-defaults
                                           :buffer (tuatara.buffer:find-buffer "*scratch*"))
             (is eq nil result)
             ;; well before the save would have ended
             (true (< (- (get-internal-real-time) start) (* 10 internal-time-units-per-second)))
             (true (tuatara.frontend.fake:fake-frame-showing f "Waiting for *scratch* to be saved"))))
      (ignore-errors (sb-thread:terminate-thread thread)))))
