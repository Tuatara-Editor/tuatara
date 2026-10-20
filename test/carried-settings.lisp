;;;; test/carried-settings.lisp --- Settings carried to the work a command starts on a thread of its own.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.tests)

(define-test carried-settings
  :description "Work started on a thread of its own sees each setting it reads as the command that started
it had it, bound around the command or not: the agenda, the days the calendar marks for the agenda and the
journal, dynamic blocks, a PDF's text, an image, rimu's preview, sorting, sums and counts.")

(defvar *carried-probe* :global
  "A setting for the tests of carried settings: :GLOBAL unless bound.")

(defun after-jobs ()
  "Keys for QUEUE-TEST-KEYS: wait for the jobs running to end, then wake the
editor once, to take them in."
  (list (lambda () (loop while (find-if #'tuatara.runtime:job-running-p tuatara.runtime:*jobs*) do (sleep 0.01)))
        :tick))

(defun on-a-thread (function &rest arguments)
  "What FUNCTION returns, called with ARGUMENTS on a new thread, which sees only
the global values of settings."
  (sb-thread:join-thread (sb-thread:make-thread (lambda () (apply function arguments)))))

(defun seen-elsewhere (name setting function)
  "Call FUNCTION; the value SETTING had the last time the function NAME was
called meanwhile on a thread other than this one, or :NEVER when it was not."
  (let ((seen :never) (here sb-thread:*current-thread*))
    (sb-int:encapsulate name 'carried-probe
                        (lambda (next &rest arguments)
                          (unless (eq sb-thread:*current-thread* here)
                            (setf seen (symbol-value setting)))
                          (apply next arguments)))
    (unwind-protect (funcall function)
      (sb-int:unencapsulate name 'carried-probe))
    seen))

(defun marked-days ()
  "The days the calendar's buffer marks, a hash table of day numbers, or NIL."
  (let ((calendar (tuatara.buffer:find-buffer "*Calendar*")))
    (and calendar (tuatara.buffer:buffer-local calendar 'tuatara.defaults::calendar-marked))))

(define-test (carried-settings carry-settings-binds)
  ;; on another thread, the values as they were when carried, each time it runs; the arguments passed on
  (let ((carried (let ((*carried-probe* :bound))
                   (tuatara.runtime:carry-settings '(*carried-probe*) (lambda (x) (list x *carried-probe*))))))
    (is eq :global (on-a-thread (lambda () *carried-probe*)))
    (is equal '(1 :bound) (on-a-thread carried 1))
    (is equal '(2 :bound) (on-a-thread carried 2))
    ;; on this thread too, whatever is bound here now; left as it was after
    (let ((*carried-probe* :later))
      (is equal '(3 :bound) (funcall carried 3))
      (is eq :later *carried-probe*)))
  ;; nothing carried: the global values
  (is equal '(:global) (on-a-thread (tuatara.runtime:carry-settings '() (lambda () (list *carried-probe*))))))

(define-test (carried-settings carried-settings-lists)
  ;; each kind's carried settings are special variables, all of them with a value
  (dolist (settings (list tuatara.defaults::*org-agenda-carried-settings* tuatara.defaults::*dblock-carried-settings*
                          tuatara.defaults::*journal-carried-settings* tuatara.defaults::*decoding-carried-settings*
                          tuatara.defaults::*sort-carried-settings* tuatara.defaults::*hash-carried-settings*
                          tuatara.defaults::*count-carried-settings*))
    (true settings)
    (dolist (setting settings)
      (true (boundp setting) "~s" setting))))

(define-test (carried-settings agenda-carried)
  (with-test-directory (dir)
    (let ((file (merge-pathnames "carried.org" dir)))
      (alexandria:write-string-into-file (format nil "* TODO Carried along~%SCHEDULED: <2026-10-01 Thu>~%") file)
      (let ((tuatara-user:*org-agenda-files* (list (namestring file)))
            (tuatara.defaults::*org-now* (encode-universal-time 0 0 12 3 10 2026))
            (tuatara.defaults::*org-agenda-at-once* 0))
        ;; the agenda worked out in the background, from the agenda files bound here
        (let ((f (run-keys (list* "M-x o r g - a g e n d a - l i s t RET" (after-jobs)) :width 100 :height 30)))
          (true (tuatara.frontend.fake:fake-frame-showing f "Carried along")))
        ;; the calendar's days marked in the background, from them too, on the day the calendar binds as today
        (is eql (encode-universal-time 0 0 12 3 10 2026)
            (seen-elsewhere 'tuatara.defaults::org-agenda-day 'tuatara.defaults::*org-now* (lambda () (calendar-keys ""))))
        (let ((marked (marked-days)))
          (true (and marked (gethash (calendar-day 2026 10 1) marked)))
          (false (and marked (gethash (calendar-day 2026 10 6) marked))))))))

(define-test (carried-settings dynamic-blocks-carried)
  ;; a block made on a thread of its own, with the tags' column bound here
  (tuatara-user:define-dblock-writer "carried" (params)
    (declare (ignore params))
    (format nil "Tags at ~a~%" tuatara-user:*org-tags-column*))
  (unwind-protect
       (let ((tuatara-user:*org-tags-column* -61))
         (let ((editor (nth-value 1 (run-keys (list* "M-x o r g - m o d e RET C-c C-c" (after-jobs))
                                              :text (format nil "#+BEGIN: carried~%#+END:~%") :width 60 :height 12))))
           (is string= (format nil "#+BEGIN: carried~%Tags at -61~%#+END:~%") (session-text editor))))
    (remhash "carried" tuatara.defaults::*dblock-writers*)))

(define-test (carried-settings journal-carried)
  ;; the days with an entry, found in the background, in the journal's directory and by the names bound here
  (with-journal (dir)
    (let ((tuatara-user:*journal-directory* (namestring (merge-pathnames "diary/" dir)))
          (tuatara-user:*journal-file-name* "%Y-%m-%d.org"))
      (alexandria:write-string-into-file "my day" (ensure-directories-exist (merge-pathnames "diary/2026-10-01.org" dir)))
      (calendar-keys "")
      (let ((marked (marked-days)))
        (true (and marked (member :calendar-journal (gethash (calendar-day 2026 10 1) marked))))
        (false (and marked (gethash (calendar-day 2026 10 2) marked)))))))

(define-test (carried-settings decoding-carried)
  ;; the sandbox's time limit bound here: the programs drawing and decoding in the background are held to it
  (with-test-directory (dir)
    (let ((image (merge-pathnames "photo.png" dir))
          (pdf (merge-pathnames "report.pdf" dir)))
      (alexandria:write-byte-vector-into-file *tiny-png* image)
      (alexandria:write-byte-vector-into-file (pdf-octets '("Hello from page one")) pdf)
      (let ((tuatara-user:*sandbox-seconds* 0))
        ;; an image
        (image-forget)
        (let ((f (image-session image nil :cell-pixels '(10 20))))
          (true (tuatara.frontend.fake:fake-frame-showing f "not shown: " "was stopped after 0 seconds"))
          (false (tuatara.frontend.fake:fake-images f)))
        (image-forget)
        ;; a PDF's text
        (pdf-forget)
        (let ((f (pdf-session pdf nil nil)))
          (false (tuatara.frontend.fake:fake-frame-showing f "Hello from page one")))
        (pdf-forget)
        ;; rimu's preview
        (let ((f (pdf-preview dir "report.pdf" (pdf-octets '("Hello from page one")))))
          (true (tuatara.frontend.fake:fake-frame-showing f "not decoded: " "was stopped after 0 seconds"))
          (false (tuatara.frontend.fake:fake-frame-showing f "report.pdf (preview, text)")))
        (rimu-forget))
      ;; as they are by default, the same decoded
      (let ((f (pdf-session pdf nil nil)))
        (true (tuatara.frontend.fake:fake-frame-showing f "Hello from page one")))
      (pdf-forget)
      (let ((f (pdf-preview dir "report.pdf" (pdf-octets '("Hello from page one")))))
        (true (tuatara.frontend.fake:fake-frame-showing f "report.pdf (preview, text)")))
      (rimu-forget))))

(define-test (carried-settings sorting-carried)
  ;; numbers read in the base bound here, in memory and on disk
  (let ((text (lines-text "a" "10" "9")))
    (dolist (on-disk '(nil t))
      (let ((tuatara.defaults::*sort-numeric-base* 16))
        (is string= (lines-text "9" "a" "10") (sorted-text "M-x s o r t - n u m e r i c - f i e l d s RET" text :on-disk on-disk)))
      (is string= (lines-text "a" "9" "10") (sorted-text "M-x s o r t - n u m e r i c - f i e l d s RET" text :on-disk on-disk)))))

(define-test (carried-settings sums-carried)
  ;; summed by the program bound here
  (let ((tuatara.defaults::*hash-library* nil) (tuatara.defaults::*hash-program* "false"))
    (let ((f (run-keys "M-x s h a 2 5 6 - s u m RET" :text "abc" :width 140)))
      (true (tuatara.frontend.fake:fake-frame-showing f "Could not sum *scratch*'s text (3 octets): false failed")))))

(define-test (carried-settings counts-carried)
  ;; counted in the pieces bound here, on the count's own thread
  (let ((tuatara.defaults::*count-piece* 3))
    (is eql 3 (seen-elsewhere 'tuatara.defaults::count-text 'tuatara.defaults::*count-piece*
                              (lambda ()
                                (let ((f (run-keys (list* "M-=" (after-jobs)) :text "Hello there, world." :width 100)))
                                  (true (tuatara.frontend.fake:fake-frame-showing
                                         f "Buffer has 1 line, 1 sentence, 3 words, and 19 characters"))))))))
