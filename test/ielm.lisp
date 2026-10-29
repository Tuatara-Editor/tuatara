;;;; test/ielm.lisp --- IELM: typing Lisp to the running editor, a line at a time.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.tests)

(define-test ielm
  :description "M-x ielm shows *ielm*: RET evaluates a whole expression typed after the prompt and puts
what it printed and its values after it, then a new prompt; an open expression goes on over lines;
M-p and M-n bring back inputs; C-c C-b chooses the working buffer; Modal editing leaves the buffer alone.")

(defun in-ielm (keys &key (width 120) (height 20))
  "Start in *lisp-scratch*, kill any *ielm*, run M-x ielm and type KEYS; return the frontend and the editor."
  (let ((old (tuatara.buffer:find-buffer "*ielm*")))
    (when old (tuatara.buffer:kill-buffer old)))
  (fresh-start (format nil "M-x i e l m RET ~a" keys) :width width :height height))

(defun ielm-text (editor)
  "The text of *ielm* after EDITOR's session."
  (declare (ignore editor))
  (tuatara.buffer:buffer-string (tuatara.buffer:find-buffer "*ielm*")))

(defun count-substrings (part text)
  "How many times PART occurs in TEXT, without overlapping."
  (loop with start = 0
        for at = (search part text :start2 start)
        while at
        count t
        do (setf start (+ at (length part)))))

(define-test (ielm evaluating)
  ;; the banner and a prompt naming the package
  (multiple-value-bind (f editor) (in-ielm "")
    (declare (ignore f))
    (is string= "*ielm*" (current-name editor))
    (true (search "*** Welcome to IELM ***" (ielm-text editor)))
    (true (search (format nil "~%TUATARA-USER> ") (ielm-text editor))))
  ;; RET after a whole expression: its value, then a new prompt
  (let ((text (ielm-text (nth-value 1 (in-ielm (format nil "~a RET" (key-string "(+ 1 2)")))))))
    (true (search (format nil "TUATARA-USER> (+ 1 2)~%3~%TUATARA-USER> ") text)))
  ;; an open expression goes on over lines, then is evaluated whole
  (let ((text (ielm-text (nth-value 1 (in-ielm (format nil "~a RET ~a RET" (key-string "(+ 1") (key-string "2)")))))))
    (true (search (format nil "2)~%3~%TUATARA-USER> ") text)))
  ;; what it printed, then its values, each on a line
  (let ((text (ielm-text (nth-value 1 (in-ielm (format nil "~a RET" (key-string "(progn (print 'hi) (values 1 2))")))))))
    (true (search (format nil "~%HI~%1~%2~%TUATARA-USER> ") text)))
  ;; an error, said
  (let ((text (ielm-text (nth-value 1 (in-ielm (format nil "~a RET" (key-string "(car 5)")))))))
    (true (search "*** Eval error ***  The value" text)))
  ;; C-j evaluates as it is, open or not
  (let ((text (ielm-text (nth-value 1 (in-ielm (format nil "~a C-j" (key-string "(+ 1")))))))
    (true (search "*** Eval error ***  The text ends inside a form" text)))
  ;; the package follows IN-PACKAGE
  (let ((text (ielm-text (nth-value 1 (in-ielm (format nil "~a RET" (key-string "(in-package :cl-user)")))))))
    (true (search (format nil "~%COMMON-LISP-USER> ") text)))
  ;; *, **, *** as a Lisp REPL keeps them
  (let ((text (ielm-text (nth-value 1 (in-ielm (format nil "~a RET ~a RET" (key-string "(+ 1 2)") (key-string "(* * 10)")))))))
    (true (search (format nil "~%30~%") text))))

(define-test (ielm history-and-old-input)
  ;; M-p brings back the input before, M-n goes on again
  (let ((text (ielm-text (nth-value 1 (in-ielm (format nil "~a RET M-p RET" (key-string "(+ 20 3)")))))))
    (is = 2 (count-substrings (format nil "~%23~%") text)))
  (let ((f (in-ielm (format nil "~a RET M-n" (key-string "(+ 20 3)")))))
    (true (showing f "End of history; no next item")))
  (let ((f (in-ielm "M-p")))
    (true (showing f "No history")))
  ;; RET on an earlier input evaluates it again
  (let ((text (ielm-text (nth-value 1 (in-ielm (format nil "~a RET C-p C-p RET" (key-string "(+ 40 2)")))))))
    (is = 2 (count-substrings (format nil "~%42~%") text)))
  ;; C-c C-u takes the input away
  (let ((text (ielm-text (nth-value 1 (in-ielm (format nil "~a C-c C-u" (key-string "(+ 1 1)")))))))
    (false (search "(+ 1 1)" text))))

(define-test (ielm working-buffer)
  ;; the Lisp typed sees the buffer M-x ielm was typed in as current, C-c C-b chooses another
  (let ((text (ielm-text (nth-value 1 (in-ielm (format nil "~a RET" (key-string "(buffer-name)")))))))
    (true (search (format nil "~%\"*lisp-scratch*\"~%") text)))
  (let ((text (ielm-text (nth-value 1 (in-ielm (format nil "C-c C-b ~a RET ~a RET" (key-string "*org-scratch*") (key-string "(buffer-name)")))))))
    (true (search (format nil "~%\"*org-scratch*\"~%") text))))

(define-test (ielm modal-keys-leave-it-alone)
  ;; letters type, whatever state other buffers are in
  (let ((text (ielm-text (nth-value 1 (in-ielm "j k")))))
    (true (search "TUATARA-USER> jk" text))))

(define-test (ielm old-input-over-lines)
  ;; RET on any line of an earlier input evaluates all of it again
  (let ((text (ielm-text (nth-value 1 (in-ielm (format nil "~a RET ~a RET C-p C-p RET" (key-string "(+ 1") (key-string "2)")))))))
    (is = 2 (count-substrings (format nil "~%3~%") text))))

(define-test (ielm tab-indents-or-completes)
  (flet ((typed (keys) (ielm-text (nth-value 1 (in-ielm keys)))))
    ;; after a name TAB completes it
    (true (search "TUATARA-USER> (buffer-substring" (typed (format nil "~a TAB" (key-string "(buffer-subs")))))
    ;; right after the prompt it does nothing, the prompt never indented
    (true (search (format nil "~%TUATARA-USER> ") (typed "TAB")))
    ;; on a line of the input with only blanks before point it indents the line
    (true (search (format nil "(let ((a 1)~%      ") (typed (format nil "~a RET C-a TAB" (key-string "(let ((a 1)")))))))

(define-test (ielm characters-of-any-size)
  ;; values and prompts beside text of characters wider than an octet are shown
  (multiple-value-bind (f editor)
      (in-ielm (format nil "~a RET ~a RET" (key-string "(format nil \"~a\" (make-string 150 :initial-element (code-char 233)))")
                       (key-string "(princ (coerce (list (code-char 26085) (code-char 26412)) 'string))")))
    (declare (ignore editor))
    (false (showing f "cannot be shown"))
    (true (showing f "TUATARA-USER> ")))
  ;; RET on an earlier input longer than 256 octets, wide characters across the 256th
  (let ((text (ielm-text (nth-value 1 (in-ielm (format nil "~a RET C-p C-p RET"
                                                          (key-string (format nil "(length \"~a\")" (make-string 140 :initial-element (code-char 233))))))))))
    (is = 2 (count-substrings (format nil "~%140~%") text))))

(define-test (ielm errors-kept-whole)
  ;; with debug-on-error, the backtrace shown is the error's, *ielm* ready for more
  (let ((old tuatara-user:*debug-on-error*))
    (unwind-protect
         (progn
           (setf tuatara-user:*debug-on-error* t)
           (multiple-value-bind (f editor) (in-ielm (format nil "~a RET" (key-string "(car 5)")))
             (declare (ignore f))
             (is string= "*Backtrace*" (current-name editor))
             (true (search "Debugger entered--Lisp error: The value" (session-text editor)))
             (true (search (format nil "~%TUATARA-USER> ") (ielm-text editor)))))
      (setf tuatara-user:*debug-on-error* old)))
  ;; a value that cannot be printed says so, and a prompt follows
  (unwind-protect
       (let ((text (ielm-text (nth-value 1 (in-ielm (format nil "~a RET" (key-string "(progn (defstruct (ielm-test-unprintable (:print-object (lambda (o s) (declare (ignore o s)) (error \"not printable\"))))) (make-ielm-test-unprintable))")))))))
         (true (search "#<a value that cannot be printed: not printable>" text))
         (true (search (format nil "not printable>~%TUATARA-USER> ") text)))
    (let ((symbol (find-symbol "IELM-TEST-UNPRINTABLE" :tuatara-user)))
      (when symbol (setf (find-class symbol) nil)))))

(define-test (ielm history-kept-as-every-history)
  ;; the inputs remembered are the newest *HISTORY-KEPT*, as in every other history
  (let ((tuatara.defaults::*history-kept* 3))
    (in-ielm (format nil "~{~a RET ~}" (loop for i from 1 to 5 collect (key-string (format nil "(+ ~d 0)" i)))))
    (is equal '("(+ 5 0)" "(+ 4 0)" "(+ 3 0)")
        (tuatara.buffer:buffer-local (tuatara.buffer:find-buffer "*ielm*") 'tuatara.defaults::ielm-history))))
