;;;; test/lisp-repl.lisp --- Lisp REPL: typing Lisp to the running editor, a line at a time.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.tests)

(define-test lisp-repl
  :description "M-x lisp-repl shows *Lisp REPL*: RET evaluates a whole expression typed after the prompt and puts
what it printed and its values after it, then a new prompt; an open expression goes on over lines;
M-p and M-n bring back inputs; C-c C-b chooses the working buffer; Modal editing leaves the buffer alone.")

(defun in-lisp-repl (keys &key (width 120) (height 20))
  "Start in *lisp-scratch*, kill any *Lisp REPL*, run M-x lisp-repl and type KEYS; return the frontend and the editor."
  (let ((old (tuatara.buffer:find-buffer "*Lisp REPL*")))
    (when old (tuatara.buffer:kill-buffer old)))
  (fresh-start (format nil "M-x l i s p - r e p l RET ~a" keys) :width width :height height))

(defun lisp-repl-text (editor)
  "The text of *Lisp REPL* after EDITOR's session."
  (declare (ignore editor))
  (tuatara.buffer:buffer-string (tuatara.buffer:find-buffer "*Lisp REPL*")))

(defun count-substrings (part text)
  "How many times PART occurs in TEXT, without overlapping."
  (loop with start = 0
        for at = (search part text :start2 start)
        while at
        count t
        do (setf start (+ at (length part)))))

(define-test (lisp-repl settings-put-back-said)
  ;; a setting given a value it does not take is put back, and said, even when showing the results
  ;; fails
  (let ((width tuatara-user:*fill-column*))
    (sb-int:encapsulate 'tuatara.defaults::lisp-repl-results 'failing
                        (lambda (f result) (declare (ignore f result)) (error "results not shown")))
    (unwind-protect
         (with-messages-said (said)
           (in-lisp-repl (format nil "~a RET" (key-string "(setf *fill-column* \"wide\")")) :width 200)
           (is eql width tuatara-user:*fill-column*)
           (true (search "*fill-column* is \"wide\", but it takes" (said)) "~a" (said)))
      (sb-int:unencapsulate 'tuatara.defaults::lisp-repl-results 'failing)
      (setf tuatara-user:*fill-column* width))))

(define-test (lisp-repl evaluating)
  ;; the banner and a prompt naming the package
  (multiple-value-bind (f editor) (in-lisp-repl "")
    (declare (ignore f))
    (is string= "*Lisp REPL*" (current-name editor))
    (true (search "Lisp REPL: " (lisp-repl-text editor)))
    (true (search (format nil "~%TUATARA-USER> ") (lisp-repl-text editor))))
  ;; RET after a whole expression: its value, then a new prompt
  (let ((text (lisp-repl-text (nth-value 1 (in-lisp-repl (format nil "~a RET" (key-string "(+ 1 2)")))))))
    (true (search (format nil "TUATARA-USER> (+ 1 2)~%3~%TUATARA-USER> ") text)))
  ;; an open expression goes on over lines, then is evaluated whole
  (let ((text (lisp-repl-text (nth-value 1 (in-lisp-repl (format nil "~a RET ~a RET"
                                                                 (key-string "(+ 1") (key-string "2)")))))))
    (true (search (format nil "2)~%3~%TUATARA-USER> ") text)))
  ;; what it printed, then its values, each on a line
  (let ((text (lisp-repl-text (nth-value 1 (in-lisp-repl
                                            (format nil "~a RET" (key-string "(progn (print 'hi) (values 1 2))")))))))
    (true (search (format nil "~%HI~%1~%2~%TUATARA-USER> ") text)))
  ;; an error, said
  (let ((text (lisp-repl-text (nth-value 1 (in-lisp-repl (format nil "~a RET" (key-string "(car 5)")))))))
    (true (search "*** Eval error ***  The value" text)))
  ;; C-j evaluates as it is, open or not
  (let ((text (lisp-repl-text (nth-value 1 (in-lisp-repl (format nil "~a C-j" (key-string "(+ 1")))))))
    (true (search "*** Eval error ***  The text ends inside a form" text)))
  ;; the package follows IN-PACKAGE
  (let ((text (lisp-repl-text (nth-value 1 (in-lisp-repl (format nil "~a RET" (key-string "(in-package :cl-user)")))))))
    (true (search (format nil "~%COMMON-LISP-USER> ") text)))
  ;; *, **, *** as a Lisp REPL keeps them
  (let ((text (lisp-repl-text (nth-value 1
                                    (in-lisp-repl (format nil
                                                     "~a RET ~a RET"
                                                     (key-string "(+ 1 2)")
                                                     (key-string "(* * 10)")))))))
    (true (search (format nil "~%30~%") text))))

(define-test (lisp-repl history-and-old-input)
  ;; M-p brings back the input before, M-n goes on again
  (let ((text (lisp-repl-text (nth-value 1 (in-lisp-repl (format nil "~a RET M-p RET" (key-string "(+ 20 3)")))))))
    (is = 2 (count-substrings (format nil "~%23~%") text)))
  (let ((f (in-lisp-repl (format nil "~a RET M-n" (key-string "(+ 20 3)")))))
    (true (showing f "End of history; no next item")))
  (let ((f (in-lisp-repl "M-p")))
    (true (showing f "No history")))
  ;; RET on an earlier input evaluates it again
  (let ((text (lisp-repl-text (nth-value 1 (in-lisp-repl (format nil "~a RET C-p C-p RET" (key-string "(+ 40 2)")))))))
    (is = 2 (count-substrings (format nil "~%42~%") text)))
  ;; C-c C-u takes the input away
  (let ((text (lisp-repl-text (nth-value 1 (in-lisp-repl (format nil "~a C-c C-u" (key-string "(+ 1 1)")))))))
    (false (search "(+ 1 1)" text))))

(define-test (lisp-repl working-buffer)
  ;; the Lisp typed sees the buffer M-x lisp-repl was typed in as current, C-c C-b chooses another
  (let ((text (lisp-repl-text (nth-value 1 (in-lisp-repl
                                            (format nil "~a RET" (key-string "(buffer-name (current-buffer))")))))))
    (true (search (format nil "~%\"*lisp-scratch*\"~%") text)))
  (let ((text (lisp-repl-text (nth-value 1
                                    (in-lisp-repl (format nil
                                                     "C-c C-b ~a RET ~a RET"
                                                     (key-string "*org-scratch*")
                                                     (key-string "(buffer-name (current-buffer))")))))))
    (true (search (format nil "~%\"*org-scratch*\"~%") text))))

(define-test (lisp-repl modal-keys-leave-it-alone)
  ;; letters type, whatever state other buffers are in
  (let ((text (lisp-repl-text (nth-value 1 (in-lisp-repl "j k")))))
    (true (search "TUATARA-USER> jk" text))))

(define-test (lisp-repl old-input-over-lines)
  ;; RET on any line of an earlier input evaluates all of it again
  (let ((text (lisp-repl-text (nth-value 1
                                    (in-lisp-repl (format nil
                                                     "~a RET ~a RET C-p C-p RET"
                                                     (key-string "(+ 1")
                                                     (key-string "2)")))))))
    (is = 2 (count-substrings (format nil "~%3~%") text))))

(define-test (lisp-repl tab-indents-or-completes)
  (flet ((typed (keys) (lisp-repl-text (nth-value 1 (in-lisp-repl keys)))))
    ;; after a name TAB completes it
    (true (search "TUATARA-USER> (buffer-forward-line" (typed (format nil "~a TAB" (key-string "(buffer-forward-l")))))
    ;; right after the prompt it does nothing, the prompt never indented
    (true (search (format nil "~%TUATARA-USER> ") (typed "TAB")))
    ;; on a line of the input with only blanks before point it indents the line
    (true (search (format nil "(let ((a 1)~%      ")
            (typed (format nil "~a RET C-a TAB" (key-string "(let ((a 1)")))))))

(define-test (lisp-repl characters-of-any-size)
  ;; values and prompts beside text of characters wider than an octet are shown
  (multiple-value-bind (f editor)
      (in-lisp-repl (format nil
                       "~a RET ~a RET"
                       (key-string "(format nil \"~a\" (make-string 150 :initial-element (code-char 233)))")
                       (key-string "(princ (coerce (list (code-char 26085) (code-char 26412)) 'string))")))
    (declare (ignore editor))
    (false (showing f "cannot be shown"))
    (true (showing f "TUATARA-USER> ")))
  ;; RET on an earlier input longer than 256 octets, wide characters across the 256th
  (let* ((input (format nil "(length \"~a\")" (make-string 140 :initial-element (code-char 233))))
         (text (lisp-repl-text (nth-value 1 (in-lisp-repl (format nil "~a RET C-p C-p RET" (key-string input)))))))
    (is = 2 (count-substrings (format nil "~%140~%") text))))

(define-test (lisp-repl errors-kept-whole)
  ;; with *backtrace-on-error*, the backtrace shown is the error's, *Lisp REPL* ready for more
  (let ((old tuatara-user:*backtrace-on-error*))
    (unwind-protect
         (progn
           (setf tuatara-user:*backtrace-on-error* t)
           (multiple-value-bind (f editor) (in-lisp-repl (format nil "~a RET" (key-string "(car 5)")))
             (declare (ignore f))
             (is string= "*Backtrace*" (current-name editor))
             (true (search "An error: The value" (session-text editor)))
             (true (search (format nil "~%TUATARA-USER> ") (lisp-repl-text editor)))))
      (setf tuatara-user:*backtrace-on-error* old)))
  ;; a value that cannot be printed says so, and a prompt follows
  (unwind-protect
       (let ((text (lisp-repl-text
                    (nth-value 1 (in-lisp-repl
                                  (format nil "~a RET"
                                          (key-string (concatenate 'string
                                                                   "(progn (defstruct (lisp-repl-test-unprintable "
                                                                   "(:print-object (lambda (o s) (declare (ignore o s)) "
                                                                   "(error \"not printable\"))))) "
                                                                   "(make-lisp-repl-test-unprintable))"))))))))
         (true (search "#<a value that cannot be printed: not printable>" text))
         (true (search (format nil "not printable>~%TUATARA-USER> ") text)))
    (let ((symbol (find-symbol "LISP-REPL-TEST-UNPRINTABLE" :tuatara-user)))
      (when symbol (setf (find-class symbol) nil)))))

(define-test (lisp-repl history-kept-as-every-history)
  ;; the inputs remembered are the newest *HISTORY-KEPT*, as in every other history
  (let ((tuatara.defaults::*history-kept* 3))
    (in-lisp-repl (format nil "~{~a RET ~}" (loop for i from 1 to 5 collect (key-string (format nil "(+ ~d 0)" i)))))
    (is equal '("(+ 5 0)" "(+ 4 0)" "(+ 3 0)")
        (tuatara.buffer:buffer-local (tuatara.buffer:find-buffer "*Lisp REPL*") 'tuatara.defaults::lisp-repl-history))))
