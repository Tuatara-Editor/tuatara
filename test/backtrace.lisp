;;;; test/backtrace.lisp --- Errors shown with the calls under way: toggle-debug-on-error, *Backtrace*.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.tests)

(define-test backtrace
  :description "With debug-on-error on, an error in Lisp evaluated in the editor or in a command
shows the calls under way in *Backtrace*, innermost first, the signalling itself and the editor's
own frames below left out; RET goes to the definition of the function called on the line.")

(defmacro with-debug-on-error-restored (&body body)
  "Run BODY, then put *DEBUG-ON-ERROR* back as it was: sessions set it for every thread."
  (let ((old (gensym)))
    `(let ((,old tuatara-user:*debug-on-error*))
       (unwind-protect (progn ,@body)
         (setf tuatara-user:*debug-on-error* ,old)))))

(define-test (backtrace errors-in-evaluation)
  (with-debug-on-error-restored
    ;; switched on, an evaluation's error shows *Backtrace*, the message too
    (multiple-value-bind (f editor)
        (fresh-start (format nil "M-x ~a RET o ~a C-g C-x C-e" (key-string "toggle-debug-on-error") (key-string "(goto-char 'x)"))
                     :width 160 :height 20)
      (true (showing f "Debug on Error enabled globally"))
      (is string= "*Backtrace*" (current-name editor))
      (let ((text (session-text editor)))
        (true (search "Debugger entered--Lisp error: Wrong type argument" text))
        ;; the innermost call first, the function it was in after it
        (true (search "(GOTO-CHAR X)" text))
        ;; neither the signalling nor the editor's own evaluation
        (false (search "%SIGNAL" text))
        (false (search "EVALUATE-FORMS" text)))
      ;; its lines name the calls; the one of goto-char leads to its definition
      (let* ((b (session-buffer editor))
             (text (tuatara.buffer:buffer-string b))
             (at (search "(GOTO-CHAR X)" text)))
        (is string= "GOTO-CHAR" (tuatara.defaults::backtrace-call-name b at))
        (false (tuatara.defaults::backtrace-call-name b 0))
        (true (search "user-lisp.lisp"
                      (tuatara.defaults::definition-group
                       (first (tuatara.defaults::mode-find-definitions (tuatara.modes:buffer-major-mode b) b "GOTO-CHAR")))))))
    ;; switched off again: the message alone
    (setf tuatara-user:*debug-on-error* nil)
    (multiple-value-bind (f editor) (fresh-start (format nil "o ~a C-g C-x C-e" (key-string "(goto-char 'x)")) :width 160)
      (true (showing f "Error: Wrong type argument"))
      (is string= "*lisp-scratch*" (current-name editor)))))

(define-test (backtrace errors-in-commands)
  (with-debug-on-error-restored
    (setf tuatara-user:*debug-on-error* t)
    ;; a command's error: its calls, down to the command, not the command loop's
    (unwind-protect
         (multiple-value-bind (f editor)
             (fresh-start (format nil "M-: ~a RET M-x ~a RET"
                                  (key-string "(tuatara.command:define-command backtrace-test-command () \"Fail.\" (error \"backtrace test\"))")
                                  (key-string "backtrace-test-command"))
                          :width 160 :height 20)
           (true (showing f "backtrace test"))
           (is string= "*Backtrace*" (current-name editor))
           (let ((text (session-text editor)))
             (true (search "(ERROR \"backtrace test\")" text))
             (true (search "(BACKTRACE-TEST-COMMAND)" text))
             (false (search "RUN-COMMAND" text))
             (false (search "COMMAND-LOOP" text))))
      (let ((symbol (find-symbol "BACKTRACE-TEST-COMMAND" :tuatara-user)))
        (when symbol
          (remhash symbol tuatara.command:*commands*)
          (fmakunbound symbol)
          (unintern symbol :tuatara-user))))
    ;; C-g is no error to show
    (multiple-value-bind (f editor) (fresh-start "C-x C-g" :width 160)
      (declare (ignore f))
      (is string= "*lisp-scratch*" (current-name editor)))))
