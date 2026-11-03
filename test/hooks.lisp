;;;; test/hooks.lisp --- Running hooks: a function that fails is said once and the others still run.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.tests)

(define-test hooks
  :description "Hooks: every function of a hook is called though one fails; the failure is said
once, in the echo area and the debug log; a question asked of a hook counts a failure as no
answer, or as no to going ahead; C-g still gives up.")

(defmacro with-hook-failures-said ((said) &body body)
  "Run BODY with the failures of hook functions forgotten and SAID bound to a
list, newest first, of what is said of those that fail in it."
  `(let ((,said '()))
     (clrhash tuatara.runtime::*hook-failures*)
     (let ((tuatara.runtime:*host* (make-answering-host 'tuatara.runtime:host-say-hook-failure
                                                        (lambda (text) (push text ,said)))))
       ,@body)))

(define-test (hooks failure-said-once-others-run)
  (with-hook-failures-said (said)
    (let* ((ran '())
           (functions (list (lambda (x) (push (list :first x) ran))
                            (lambda (x) (error "no room for ~a" x))
                            (lambda (x) (push (list :last x) ran)))))
      ;; the others still run; the first failure is given back
      (let ((failure (tuatara.runtime:run-hook functions 1)))
        (true (typep failure 'error))
        (true (search "no room for 1" (princ-to-string failure))))
      (is equal '((:last 1) (:first 1)) ran)
      (is = 1 (length said))
      (true (search "failed and was left out: no room for 1" (first said)) "~a" said)
      ;; failing again, the same function is not said again
      (tuatara.runtime:run-hook functions 2)
      (is = 1 (length said))
      (is equal '((:last 2) (:first 2) (:last 1) (:first 1)) ran))))

(defun hooks-test-failing (buffer)
  "A hook function for the tests that always fails."
  (declare (ignore buffer))
  (error "no room on the test's device"))

(define-test (hooks named-function-said-by-name)
  (with-hook-failures-said (said)
    (false (tuatara.runtime:call-hook-function 'hooks-test-failing nil))
    (is equal '("hooks-test-failing failed and was left out: no room on the test's device") said)))

(define-test (hooks questions-count-a-failure)
  (with-hook-failures-said (said)
    (let ((failing (lambda (x) (error "broken for ~a" x))))
      ;; the first answer is taken; a failure is no answer
      (is eq :yes (tuatara.runtime:run-hook-until-success (list failing (constantly nil) (constantly :yes)) 1))
      (false (tuatara.runtime:run-hook-until-success (list failing) 1))
      (is = 1 (length said))
      ;; one function called alone: NIL and the error
      (multiple-value-bind (answer failure) (tuatara.runtime:call-hook-function failing 2)
        (false answer)
        (true (typep failure 'error))))))

(define-test (hooks work-offered-fails-the-command)
  ;; a function offered a command's work that fails fails the command, said by it every time
  (with-hook-failures-said (said)
    (is eq :done (tuatara.runtime:run-hook-until-done (list (constantly nil) (constantly :done)) 1))
    (fail (tuatara.runtime:run-hook-until-done (list (lambda (x) (error "broken for ~a" x)) (constantly :done)) 1))
    (false said)))

(define-test (hooks failures-in-the-debug-log)
  ;; every failure goes to the debug log, the calls under way the first time only
  (drain-debug-reports)
  (unwind-protect
       (with-hook-failures-said (said)
         (tuatara.runtime:set-debug t)
         (let ((failing (lambda () (error "logged on purpose"))))
           (tuatara.runtime:run-hook (list failing))
           (tuatara.runtime:run-hook (list failing)))
         (let ((reports (loop for report = (sb-concurrency:dequeue tuatara.runtime:*debug-reports*)
                              while report
                              when (search "logged on purpose" (fourth report)) collect (fourth report))))
           (is = 2 (length reports))
           (true (search (format nil "~%    ") (first reports)))
           (false (search (format nil "~%    ") (second reports))))
         (is = 1 (length said)))
    (drain-debug-reports)))

(define-test (hooks function-without-a-name)
  (let ((name (tuatara.runtime:hook-function-name (let ((n 1)) (lambda () n)))))
    (false (search "#<" name) "~a" name)))

(define-test (hooks quit-goes-on)
  ;; C-g in a hook function gives up what is going on: not a failure to say
  (with-hook-failures-said (said)
    (let ((ran '()))
      (fail (tuatara.runtime:run-hook (list (lambda () (error 'tuatara.command:quit))
                                            (lambda () (push :after ran))))
          tuatara.command:quit)
      (false ran)
      (false said))))

(define-test (hooks after-save-failure-said)
  ;; a function run after saving that fails leaves the file saved, the others run and the failure said
  (clrhash tuatara.runtime::*hook-failures*)
  (with-test-directory (dir)
    (let* ((path (put-file (merge-pathnames "doc.txt" dir) "original"))
           (ran '())
           (tuatara.defaults::*after-save-functions*
             (list* 'hooks-test-failing
                    (lambda (buffer) (push (tuatara.buffer:buffer-name buffer) ran))
                    tuatara.defaults::*after-save-functions*)))
      (multiple-value-bind (f editor) (run-keys "i X C-g C-x C-s" :file path :width 160)
        (is string= "Xoriginal" (file-text path))
        (is equal '("doc.txt") ran)
        (true (tuatara.frontend.fake:fake-frame-showing
               f "hooks-test-failing failed and was left out: no room on the test's device"))
        (tuatara.buffer:kill-buffer (session-buffer editor))))))

(define-test (hooks refusal-check-failing-refuses)
  ;; a refusal question that fails refuses the command, saying it could not be checked
  (let ((tuatara.defaults::*command-refusal-functions*
          (list (lambda (command buffer)
                  (declare (ignore buffer))
                  (when (string= command "END-OF-BUFFER") (error "no index on the test's device"))))))
    (multiple-value-bind (f editor) (run-keys "M->" :text "abc" :width 160)
      (is = 0 (tuatara.buffer:buffer-point (session-buffer editor)))
      (true (tuatara.frontend.fake:fake-frame-showing
             f "end-of-buffer could not be checked (no index on the test's device): not run")))))

(define-test (hooks failing-kill-question-asks)
  ;; a question before killing that fails asks whether to kill all the same; yes kills
  (let* ((asked nil)
         (tuatara.defaults::*kill-buffer-query-functions*
           (list (lambda (buffer) (setf asked buffer) (error "cannot tell")))))
    (multiple-value-bind (f editor) (run-keys "C-x k y e s RET" :text "abc" :width 160)
      (declare (ignore editor))
      (true (tuatara.frontend.fake:fake-frame-showing f "failed (cannot tell); kill anyway?"))
      (true asked)
      (false (tuatara.buffer:live-buffer-p asked)))))

(define-test (hooks failing-exit-question-asks)
  ;; a question before leaving that fails asks whether to leave all the same; yes leaves,
  ;; what the other questions said to do once leaving is certain done
  (let* ((stopped nil)
         (tuatara.defaults::*quit-query-functions*
           (list (lambda () (error "cannot tell"))
                 (lambda () (lambda () (setf stopped t))))))
    (multiple-value-bind (f editor) (run-keys "C-x C-c y e s RET" :text "" :width 160)
      (true (tuatara.frontend.fake:fake-frame-showing f "failed (cannot tell); exit anyway?"))
      (false (tuatara.command:editor-running editor))
      (true stopped))))

(define-test (hooks failing-hex-view-fails-the-command)
  ;; a view of its own that fails as the hex view is asked for fails the command, said each time,
  ;; and no hex view is shown
  (with-hook-failures-said (said)
    (let ((tuatara.defaults::*hex-view-functions* (list (lambda (b) (declare (ignore b)) (error "pages not read")))))
      (multiple-value-bind (f editor) (run-keys "M-x h e x - m o d e RET" :text "abc" :width 160)
        (true (tuatara.frontend.fake:fake-frame-showing f "pages not read"))
        (false (tuatara.display:hex-view-p (session-buffer editor)))
        (false said)))))

(define-test (hooks failing-gutter-left-out)
  ;; a gutter that fails is left out, said once; the text is still drawn
  (with-hook-failures-said (said)
    (let ((tuatara.command:*gutter-providers*
            (cons (list :name :broken
                        :width (lambda (editor window) (declare (ignore editor window)) (error "no width"))
                        :cell (lambda (&rest arguments) (declare (ignore arguments)) (values "" nil)))
                  tuatara.command:*gutter-providers*)))
      (multiple-value-bind (f editor) (run-keys "j k j" :text (format nil "one~%two~%") :width 60 :height 6)
        (declare (ignore editor))
        (true (search "one" (tuatara.frontend.fake:fake-screen-line f 0)))
        (is = 1 (length said))
        (true (search "failed and was left out: no width" (first said)))))))
