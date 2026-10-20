;;;; test/tui.lisp --- The terminal frontend and the program's start: names for shared pictures, the random numbers of each start.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.tests)

(define-test tui
  :description "The terminal frontend and the program's start, as frontends/tui has them.")

(define-test (tui random-numbers-new-at-each-start)
  ;; two starts of one saved executable begin with the same state; each draws its own numbers
  (let ((saved (make-random-state nil)))
    (flet ((first-number ()
             (let ((*random-state* (make-random-state saved)))
               (tuatara.terminal::fresh-random-state)
               (random (expt 2 64)))))
      (isnt = (first-number) (first-number)))))

(define-test (tui picture-names-new-at-each-start)
  ;; the names of shared pictures do not repeat a state saved in the executable
  (let ((saved (make-random-state nil)))
    (flet ((name () (let ((*random-state* (make-random-state saved))) (tuatara.frontend.tui::shared-picture-name))))
      (let ((one (name)) (two (name)))
        (isnt string= one two)
        (true (uiop:string-prefix-p (format nil "tuatara-~d-" (sb-posix:getpid)) one))))))

(define-test (tui window-only-without-a-terminal)
  ;; a window of its own only when neither the input nor the output is a terminal, on a desktop
  (flet ((window (input output desktop)
           (tuatara.terminal::window-wanted-p :input input :output output :desktop desktop)))
    (true (window nil nil ":0"))
    (false (window nil t ":0"))
    (false (window t nil ":0"))
    (false (window t t ":0"))
    (false (window nil nil nil))))

(define-test (tui input-pending-counts-what-is-kept)
  ;; with nothing on the terminal's input, a resize kept to hand over and keys handed
  ;; back to ncurses are input waiting all the same
  (multiple-value-bind (empty writer) (sb-posix:pipe)
    (let ((saved (sb-posix:dup 0))
          (f (tuatara.frontend.tui:make-tui-frontend)))
      (unwind-protect
           (progn
             (sb-posix:dup2 empty 0)
             (let ((tuatara.frontend.tui::*keys-given-back* nil))
               (false (tuatara.frontend:frontend-input-pending-p f))
               (push (tuatara.frontend:make-event :resize :width 80 :height 24) (tuatara.frontend.tui::tui-pending f))
               (true (tuatara.frontend:frontend-input-pending-p f))
               (setf (tuatara.frontend.tui::tui-pending f) '()
                     tuatara.frontend.tui::*keys-given-back* t)
               (true (tuatara.frontend:frontend-input-pending-p f))))
        (sb-posix:dup2 saved 0)
        (mapc #'sb-posix:close (list saved empty writer))))))

(define-test (tui file-names-not-utf-8-on-the-command-line)
  ;; a file name that is not UTF-8 on the command line: every argument kept, that
  ;; name's octets as its raw bytes' characters, the runtime's options left out
  (with-test-directory (directory)
    (let ((cmdline (merge-pathnames "cmdline" directory)))
      (with-open-file (o cmdline :direction :output :element-type '(unsigned-byte 8))
        (write-sequence (concatenate '(vector (unsigned-byte 8))
                                     (sb-ext:string-to-octets "tuatara") #(0)
                                     (sb-ext:string-to-octets "--dynamic-space-size") #(0) (sb-ext:string-to-octets "3GB") #(0)
                                     (sb-ext:string-to-octets "--gui") #(0)
                                     (sb-ext:string-to-octets "bad") #(255 254) (sb-ext:string-to-octets "name.txt") #(0)
                                     (sb-ext:string-to-octets "a.txt") #(0))
                        o))
      (is equal (list "tuatara" "--gui" (format nil "bad~c~cname.txt" (code-char #xDCFF) (code-char #xDCFE)) "a.txt")
          (tuatara.terminal::command-line-arguments :argv nil :from (uiop:native-namestring cmdline)))
      (is equal '("tuatara" "x") (tuatara.terminal::command-line-arguments :argv '("tuatara" "x") :from "/nonexistent")))))

(define-test (tui heap-restart-with-a-name-not-utf-8)
  ;; such a name is handed on as its octets; with no program there to run, this
  ;; start goes on with its heap
  (let ((sb-ext:*posix-argv* (list "tuatara" (format nil "bad~cname" (code-char #xDCFF))))
        (tuatara.io:*saved-program* "/nonexistent/tuatara"))
    (unwind-protect (finish (tuatara.terminal::restart-with-heap 100))
      (sb-posix:unsetenv "TUATARA_HEAP_DECIDED"))))
