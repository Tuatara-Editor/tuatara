;;;; test/tui.lisp --- The terminal frontend and the program's start: names for shared pictures, the random numbers of
;;;; each start.
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

(define-test (tui first-line-of-a-closed-file)
  ;; a file not there and one that may not be read both give no line; only the second is
  ;; kept in the debug log, as a failure
  (let ((path (namestring (merge-pathnames "first-line" *test-state-directory*))))
    (with-open-file (out path :direction :output :if-exists :supersede) (format out "max~%"))
    (drain-debug-reports)
    (unwind-protect
         (progn
           (is string= "max" (tuatara.terminal::file-first-line path))
           (sb-posix:chmod path 0)
           (tuatara.runtime:set-debug t)
           (false (tuatara.terminal::file-first-line (format nil "~a.missing" path)))
           (true (sb-concurrency:queue-empty-p tuatara.runtime:*debug-reports*))
           (false (tuatara.terminal::file-first-line path))
           (let ((texts (loop for report = (sb-concurrency:dequeue tuatara.runtime:*debug-reports*)
                              while report collect (princ-to-string (fourth report)))))
             (true (find-if (lambda (text) (search (format nil "reading ~a failed" path) text)) texts) "~s" texts)))
      (drain-debug-reports)
      (delete-file path))))

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
                                     (sb-ext:string-to-octets "--dynamic-space-size")
                                     #(0)
                                     (sb-ext:string-to-octets "3GB")
                                     #(0)
                                     (sb-ext:string-to-octets "--gui") #(0)
                                     (sb-ext:string-to-octets "bad")
                                     #(255 254)
                                     (sb-ext:string-to-octets "name.txt")
                                     #(0)
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

(define-test (tui terminal-given-back-alike)
  ;; stopping and lending the terminal give it back the same way, and a lent terminal is taken
  ;; again as the editor had it: the kitty keyboard, the mouse, pastes, rows shown in the order sent, the
  ;; screen painted whole
  (let ((calls '()))
    (with-functions-replaced ((tuatara.frontend.tui::%endwin (lambda () (push :endwin calls)))
                              (tuatara.frontend.tui::%clearok (lambda (window on)
                                                                (declare (ignore window on))
                                                                (push :clearok calls)))
                              (tuatara.frontend.tui::%wrefresh (lambda (window)
                                                                 (declare (ignore window))
                                                                 (push :refresh calls)))
                              (tuatara.frontend.tui::forget-images (lambda (f all)
                                                                     (declare (ignore f))
                                                                     (push (list :forget all) calls))))
      (flet ((sent (function)
               (let ((f (tuatara.frontend.tui:make-tui-frontend))
                     (tuatara.frontend.tui::*kitty-keyboard-on* t)
                     (tuatara.frontend.tui::*kitty-flags* 31)
                     (tuatara.frontend.tui::*altgr-held* t)
                     (tuatara.frontend.tui::*shown-cursor-shape* :bar)
                     (tuatara.frontend.tui::*shown-cursor-colour* "#123456")
                     (tuatara.frontend.tui::*saved-stderr* nil)
                     (tuatara.frontend.tui::*shared-pictures* '()))
                 (setf (tuatara.frontend.tui::tui-window f) :window (tuatara.frontend.tui::tui-mouse f) t
                       calls '())
                 (values (with-output-to-string (*standard-output*) (funcall function f))
                         (reverse calls)
                         tuatara.frontend.tui::*kitty-keyboard-on*
                         tuatara.frontend.tui::*shown-cursor-shape*
                         tuatara.frontend.tui::*altgr-held*))))
        (let* ((esc (string #\Escape))
               (given-back (concatenate 'string esc "[0 q" esc "]112" (string (code-char 7)) esc "[<u"
                                        esc "[?1004l" esc "[?1002l" esc "[?1006l" esc "[?2004l" esc "[8h")))
          (multiple-value-bind (out calls kitty shape) (sent #'tuatara.frontend:frontend-stop)
            (is string= given-back out)
            (is equal '((:forget t) :endwin) calls)
            (false kitty)
            (false shape))
          ;; AltGr held when the terminal was lent: not held when it comes back, its letting go unseen
          (multiple-value-bind (out calls kitty shape held)
              (sent (lambda (f) (tuatara.frontend:frontend-lend-terminal f (lambda () (write-string "program")))))
            (is string= (concatenate 'string given-back "program" esc "[>31u" esc "[?1004h"
                                     esc "[?1002h" esc "[?1006h" esc "[?2004h" esc "[8l")
                out)
            (is equal '((:forget t) :endwin :clearok :refresh) calls)
            (true kitty)
            (false shape)
            (false held)))))))

(define-test (tui row-written-without-garbage)
  ;; a row's spans and the face painted past its text are written to the terminal from the row itself,
  ;; no piece of it copied: little garbage for each span more (faces combined as they always are)
  (let ((written (make-string-output-stream)) (recording t))
    (with-functions-replaced ((tuatara.frontend.tui::%wmove (lambda (window y x) (declare (ignore window y x)) 0))
                              (tuatara.frontend.tui::%getmaxx (lambda (window) (declare (ignore window)) 300))
                              (tuatara.frontend.tui::%getmaxy (lambda (window) (declare (ignore window)) 50))
                              (tuatara.frontend.tui::%getcurx (lambda (window) (declare (ignore window)) 200))
                              (tuatara.frontend.tui::%wclrtoeol (lambda (window) (declare (ignore window)) 0))
                              (tuatara.frontend.tui::attributes-on (lambda (window attrs)
                                                                     (declare (ignore window attrs))))
                              (tuatara.frontend.tui::attributes-off (lambda (window) (declare (ignore window))))
                              (tuatara.frontend.tui::spec-attributes (lambda (spec) (declare (ignore spec)) 1))
                              (tuatara.display:combine-faces (lambda (face base) (declare (ignore base)) face))
                              (tuatara.frontend.tui::%waddstr-octets
                               (lambda (window octets) (declare (ignore window))
                                 (when recording (write-string (tuatara.runtime:sap-string octets) written))
                                 0)))
      (let ((f (tuatara.frontend.tui:make-tui-frontend))
            (text (make-string 200 :initial-element #\x)))
        (setf (tuatara.frontend.tui::tui-window f) :window)
        (flet ((consed (count)
                 (let ((spans (loop for k below count collect (list (* k 5) (+ (* k 5) 3) :bold))))
                   (tuatara.frontend:frontend-put-row f 0 text :face :mode-line :spans spans)
                   (let ((before (tuatara.bench::bytes-consed)))
                     (dotimes (k 100) (tuatara.frontend:frontend-put-row f 0 text :face :mode-line :spans spans))
                     (/ (- (tuatara.bench::bytes-consed) before) 100)))))
          (tuatara.frontend:frontend-put-row f 0 text :face :mode-line :spans '((0 3 :bold)))
          ;; the row's text, then spaces to the edge
          (is string=
              (concatenate 'string text (make-string 100 :initial-element #\Space))
              (get-output-stream-string written))
          (setf recording nil)
          (let ((few (consed 2)) (many (consed 40)))
            (true (< (/ (- many few) 38) 48) "~d octets of garbage for a row of 40 spans, ~d for one of 2" many few)))))))

(define-test (tui frontends-call-c-one-way)
  ;; the terminal and the window call their C libraries as the rest of the editor
  ;; does, through SBCL's own foreign calls: no library of their own to do it
  (let ((libraries '("cffi" "babel" "trivial-features")))
    (labels ((needed (name)
               (let ((system (asdf:find-system name nil)))
                 (when system
                   (cons (asdf:primary-system-name system)
                         (loop for dependency in (asdf:system-depends-on system)
                               when (stringp dependency) append (needed dependency)))))))
      (dolist (system '("tuatara/terminal" "tuatara/frontend-tui" "tuatara/frontend-gtk"))
        (is equal '() (intersection libraries (needed system) :test #'string=) "~a needs ~a" system
            (intersection libraries (needed system) :test #'string=)))))
  (dolist (file (directory (merge-pathnames "frontends/**/*.lisp" (asdf:system-source-directory :tuatara))))
    (false (search "cffi:" (uiop:read-file-string file)) "~a calls C through cffi" (enough-namestring file))))

(define-test (tui lent-terminal-words-written-out)
  ;; the words shown on the terminal lent to a program have their control characters written out
  (let ((f (tuatara.frontend.tui:make-tui-frontend))
        (said (make-string-output-stream)))
    (setf (tuatara.frontend.tui::tui-window f) t)
    (sb-int:encapsulate 'tuatara.frontend.tui::give-terminal-back 'lending-nothing
                        (lambda (function f) (declare (ignore function f))))
    (sb-int:encapsulate 'tuatara.frontend.tui::take-terminal-again 'lending-nothing
                        (lambda (function &rest arguments) (declare (ignore function arguments))))
    (unwind-protect
         (let ((*standard-output* said))
           (tuatara.frontend:frontend-lend-terminal
            f (constantly t) :words (format nil "decrypting a~c]0;owned~c.gpg" #\Esc (code-char 7))))
      (sb-int:unencapsulate 'tuatara.frontend.tui::give-terminal-back 'lending-nothing)
      (sb-int:unencapsulate 'tuatara.frontend.tui::take-terminal-again 'lending-nothing))
    (let ((text (get-output-stream-string said)))
      (true (search "decrypting a^[]0;owned^G.gpg" text) "~s" text)
      (false (find #\Esc text)))))

(define-test (tui terminal-resize-wakes-the-wait)
  ;; a resize (SIGWINCH) wakes the wait for events, so that it needs no timeout to notice one
  (let ((f (tuatara.frontend.tui:make-tui-frontend))
        (wake (tuatara.io:make-wake-pipe)))
    (setf (tuatara.frontend.tui::tui-wake f) wake)
    (setf (car tuatara.frontend.tui::**terminal-resized**) nil)
    (unwind-protect
         (progn
           (tuatara.frontend.tui::watch-terminal-size f)
           (sb-posix:kill (sb-posix:getpid) sb-unix:sigwinch)
           (true (consp (tuatara.io:wait-for-fds (list (car wake)) 5)) "the wake pipe was written")
           (true (car tuatara.frontend.tui::**terminal-resized**)))
      (sb-sys:enable-interrupt sb-unix:sigwinch :default)
      (setf (car tuatara.frontend.tui::**terminal-resized**) nil
            (tuatara.frontend.tui::tui-wake f) nil)
      (tuatara.io:close-wake-pipe wake))))

(define-test (tui wake-and-resize-drained-together-still-tick)
  ;; a wake written beside a resize is not lost: the tick it asks for is given
  (setf (car tuatara.frontend.tui::**frontend-woken**) nil
        (car tuatara.frontend.tui::**terminal-resized**) t)
  (unwind-protect
       (progn
         (false (tuatara.frontend.tui::wake-gives-tick-p) "a resize alone gives no tick")
         (setf (car tuatara.frontend.tui::**frontend-woken**) t)
         (true (tuatara.frontend.tui::wake-gives-tick-p) "a wake with a resize gives one")
         (false (car tuatara.frontend.tui::**frontend-woken**)))
    (setf (car tuatara.frontend.tui::**terminal-resized**) nil)))

(define-test (tui frame-the-same-not-sent-again)
  ;; a timer that wakes the editor with nothing changed (the mode line's signs fading) paints a frame the same as
  ;; the one on the screen: it is not sent, so the terminal gets no empty frame half a second after each key
  (let ((sent '()))
    (with-functions-replaced ((tuatara.frontend.tui::%wmove (lambda (window y x) (declare (ignore window y x)) 0))
                              (tuatara.frontend.tui::%getmaxx (lambda (window) (declare (ignore window)) 80))
                              (tuatara.frontend.tui::%getmaxy (lambda (window) (declare (ignore window)) 24))
                              (tuatara.frontend.tui::%getcurx (lambda (window) (declare (ignore window)) 5))
                              (tuatara.frontend.tui::%wclrtoeol (lambda (window) (declare (ignore window)) 0))
                              (tuatara.frontend.tui::%werase (lambda (window) (declare (ignore window)) 0))
                              (tuatara.frontend.tui::%wbkgd (lambda (window ch) (declare (ignore window ch)) 0))
                              (tuatara.frontend.tui::%wrefresh (lambda (window) (declare (ignore window)) 0))
                              (tuatara.frontend.tui::%curs-set (lambda (visibility) (declare (ignore visibility)) 0))
                              (tuatara.frontend.tui::%waddstr-octets (lambda (window octets)
                                                                       (declare (ignore window octets))
                                                                       0))
                              (tuatara.frontend.tui::attributes-on (lambda (window attrs)
                                                                     (declare (ignore window attrs))))
                              (tuatara.frontend.tui::attributes-off (lambda (window) (declare (ignore window))))
                              (tuatara.frontend.tui::spec-attributes (lambda (spec) (declare (ignore spec)) 1))
                              (tuatara.frontend.tui::background-character (lambda (attrs) (declare (ignore attrs)) 32))
                              (tuatara.frontend.tui::cursor-colour-code (lambda (colour) (declare (ignore colour))
                                                                          :default))
                              (tuatara.frontend.tui::forget-images (lambda (f all) (declare (ignore f all))))
                              (tuatara.frontend.tui::terminal-control (lambda (control &rest arguments)
                                                                        (declare (ignore arguments))
                                                                        (push control sent))))
      (let ((f (tuatara.frontend.tui:make-tui-frontend))
            (tuatara.frontend.tui::*shown-frame* nil))
        (setf (tuatara.frontend.tui::tui-window f) :window)
        (flet ((frame (text column)
                 (setf sent '())
                 (tuatara.frontend:frontend-begin-frame f)
                 (tuatara.frontend:frontend-put-row f 0 text :spans '((0 2 :bold)))
                 (tuatara.frontend:frontend-set-cursor f 0 column :block)
                 (tuatara.frontend:frontend-end-frame f)
                 (and (member "[?2026h" sent :test #'string=) t)))
          (true (frame "hello" 5) "the first frame is sent")
          (false (frame "hello" 5) "the same frame again is not")
          (true (frame "hullo" 5) "a row changed is sent")
          (true (frame "hullo" 4) "the cursor moved is sent")
          (false (frame "hullo" 4) "and the same again is not"))))))

(define-test (tui kitty-key-past-the-last-character)
  ;; a CSI u key whose code, shifted code, base or text is past the last character is no key, not an error
  (dolist (parameters '("57;1;1114112u" "1114112;1" "97:1114112;2" "97::1114112;1" "97;;97:1114112"))
    (false (tuatara.frontend.tui::kitty-key (string-right-trim "u" parameters)) "~a" parameters)))

(define-test (tui frame-hash-reads-every-face-of-a-list)
  ;; two rows whose spans differ only in a fifth face of a list hash apart, so the second is sent
  (flet ((hash (fifth)
           (tuatara.frontend.tui::frame-row-hash 0 0 "words" nil
                                                 (list (list 0 5 (list :bold :italic :underline :region fifth))))))
    (isnt = (hash :highlight) (hash :warning))
    (isnt = (tuatara.frontend.tui::frame-row-hash 0 0 "words" '(:a :b :c :d :highlight) '())
        (tuatara.frontend.tui::frame-row-hash 0 0 "words" '(:a :b :c :d :warning) '()))))

(defun tui-read-typed (text)
  "The events the terminal frontend makes of TEXT read after an ESC, as if the
terminal sent it, each character at once, then nothing."
  (tuatara.frontend.tui::load-ncurses)
  (let ((left (coerce text 'list))
        (f (make-instance 'tuatara.frontend.tui::tui-frontend))
        (window (sb-sys:int-sap 0)))
    (sb-int:encapsulate 'tuatara.frontend.tui::read-wide-char 'typed
                        (lambda (function window)
                          (declare (ignore function window))
                          (if left (values 0 (char-code (pop left))) (values -1 0))))
    (unwind-protect
         (let ((first (let ((tuatara.frontend.tui::*kitty-keyboard-on* nil))
                        (tuatara.frontend.tui::read-escape-sequence f window))))
           (values (remove nil (cons first (tuatara.frontend.tui::tui-pending f))) left))
      (sb-int:unencapsulate 'tuatara.frontend.tui::read-wide-char 'typed))))

(define-test (tui late-picture-answer-is-no-key)
  ;; a picture's answer coming late (ESC _ G ... ESC \) is read whole and is no key; M-_ typed is still a key
  (multiple-value-bind (events left) (tui-read-typed (format nil "_Gi=31;OK~c\\" #\Escape))
    (is equal '() events)
    (is equal '() left))
  (let ((events (tui-read-typed "_x")))
    (is = 2 (length events))
    (is equal '("M-_" "x") (mapcar (lambda (event) (tuatara.frontend:key-description
                                                   (tuatara.frontend:key-spec (tuatara.frontend:event-key event))))
                                   events))))

(define-test (tui clipboard-wait-given-up-on-c-g)
  ;; a paste waiting for a change of the clipboard being read is given up on with C-g, at once
  (let ((kept (tuatara.frontend:make-clipboard-kept))
        (answer :none)
        (seconds nil))
    (tuatara.frontend:clipboard-read-done kept "before")
    (tuatara.frontend:clipboard-read-begun kept)
    (run-keys (list (lambda ()
                      ;; the C-g queued after this counts as typed while it waits
                      (setf (tuatara.frontend.fake:fake-pending
                             (tuatara.command:editor-frontend tuatara.command:*editor*))
                            (lambda (f) (tuatara.frontend.fake::fake-events f)))
                      (let ((start (get-internal-real-time)))
                        (setf answer (multiple-value-list (tuatara.command:wait-for-clipboard-text kept))
                              seconds (/ (- (get-internal-real-time) start) internal-time-units-per-second))))
                    "C-g"))
    (is equal '(nil t) answer)
    (true (and seconds (< seconds 1/2)) "waited ~a s" seconds)))

(define-test (tui clipboard-written-in-the-background)
  ;; a copy hands the text to the clipboard's program and returns at once; the newest text given meanwhile
  ;; is the one written
  (with-test-directory (dir)
    (let* ((out (tuatara.io:native (merge-pathnames "clipboard" dir)))
           (writer (put-program (merge-pathnames "copy.sh" dir) (format nil "#!/bin/sh~%sleep 1~%cat > ~a~%" out)))
           (tuatara.frontend.tui::*clipboard-programs* (list (list writer) (list writer)))
           (tuatara.frontend.tui::*clipboard-writer* nil)
           (f (make-instance 'tuatara.frontend.tui::tui-frontend)))
      (let ((start (get-internal-real-time)))
        (true (tuatara.frontend:frontend-set-clipboard f "first"))
        (true (tuatara.frontend:frontend-set-clipboard f "second"))
        (true (tuatara.frontend:frontend-set-clipboard f "third"))
        (true (< (- (get-internal-real-time) start) (* 1/2 internal-time-units-per-second))))
      (loop repeat 100 until (equal (ignore-errors (file-text out)) "third") do (sleep 0.1))
      (is equal "third" (ignore-errors (file-text out))))))

(define-test (tui clipboard-read-after-a-copy)
  ;; the clipboard read straight after a copy gives the text copied, while its program is still writing it
  (with-test-directory (dir)
    (let* ((out (tuatara.io:native (merge-pathnames "clipboard" dir)))
           (writer (put-program (merge-pathnames "copy.sh" dir) (format nil "#!/bin/sh~%sleep 1~%cat > ~a~%" out)))
           (reader (put-program (merge-pathnames "paste.sh" dir) (format nil "#!/bin/sh~%cat ~a~%" out)))
           (tuatara.frontend.tui::*clipboard-programs* (list (list writer) (list reader)))
           (tuatara.frontend.tui::*clipboard-writer* nil)
           (f (make-instance 'tuatara.frontend.tui::tui-frontend)))
      (alexandria:write-string-into-file "old" out)
      (true (tuatara.frontend:frontend-set-clipboard f "new"))
      (is equal "new" (tuatara.frontend:frontend-clipboard f))
      (loop repeat 100 until (equal (ignore-errors (file-text out)) "new") do (sleep 0.1))
      (is equal "new" (tuatara.frontend:frontend-clipboard f)))))

(define-test (tui clipboard-writer-goes-on-after-an-error)
  ;; a copy whose writing goes wrong is let go: a read asks the clipboard again, and the next copy is written
  (with-test-directory (dir)
    (let* ((out (tuatara.io:native (merge-pathnames "clipboard" dir)))
           (writer (put-program (merge-pathnames "copy.sh" dir) (format nil "#!/bin/sh~%cat > ~a~%" out)))
           (tuatara.frontend.tui::*clipboard-programs* (list (list writer) (list writer)))
           (tuatara.frontend.tui::*clipboard-writer* nil)
           (f (make-instance 'tuatara.frontend.tui::tui-frontend)))
      (sb-int:encapsulate 'tuatara.io:program-output 'failing
                          (lambda (function command &rest keys)
                            (if (equal (getf keys :input) "goes wrong")
                                (error "a stand-in failure")
                                (apply function command keys))))
      (unwind-protect
           (progn
             (true (tuatara.frontend:frontend-set-clipboard f "goes wrong"))
             (true (tuatara.runtime:wait-until
                    (lambda () (null (tuatara.frontend.tui::pending-clipboard-text (list writer))))
                    10))
             (true (tuatara.frontend:frontend-set-clipboard f "written"))
             (true (tuatara.runtime:wait-until (lambda () (equal (ignore-errors (file-text out)) "written")) 10))
             (is equal "written" (ignore-errors (file-text out))))
        (sb-int:unencapsulate 'tuatara.io:program-output 'failing)))))

(define-test (tui clipboard-secret-kept-from-debug-log)
  ;; a secret copied while a copy that was no secret came first stays out of the debug log when the clipboard's
  ;; program does not take it
  (drain-debug-reports)
  (with-test-directory (dir)
    (let* ((takes (put-program (merge-pathnames "takes.sh" dir) (format nil "#!/bin/sh~%cat > /dev/null~%")))
           (quits (put-program (merge-pathnames "quits.sh" dir) (format nil "#!/bin/sh~%exit 0~%")))
           (tuatara.frontend.tui::*clipboard-writer* nil)
           (secret (concatenate 'string "tuatara-secret-marker " (make-string 300000 :initial-element #\x))))
      ;; the calls of a failure are written the first time it comes from its place: as if it never came before
      (sb-thread:with-mutex (tuatara.runtime::*error-report-lock*)
        (let ((places tuatara.runtime::*error-report-places*))
          (loop for key being the hash-keys of places
                when (equal (car key) "giving ~a its input") do (remhash key places))))
      (unwind-protect
           (progn
             (tuatara.runtime:set-debug t)
             (tuatara.frontend.tui::give-clipboard-text (list takes) "no secret")
             (true (tuatara.runtime:wait-until
                    (lambda () (null (tuatara.frontend.tui::pending-clipboard-text (list takes))))
                    10))
             (let ((tuatara.runtime:*secret-at-hand* t))
               (tuatara.frontend.tui::give-clipboard-text (list quits) secret))
             (true (tuatara.runtime:wait-until
                    (lambda () (null (tuatara.frontend.tui::pending-clipboard-text (list quits))))
                    10))
             (let ((texts (loop for report = (sb-concurrency:dequeue tuatara.runtime:*debug-reports*)
                                while report collect (princ-to-string (fourth report)))))
               (true (find-if (lambda (text) (search "its input failed" text)) texts) "~s" texts)
               (false (find-if (lambda (text) (search "tuatara-secret-marker" text)) texts))))
        (drain-debug-reports)))))

(define-test (tui clipboard-written-before-stop)
  ;; quitting just after a copy waits for the clipboard's program to write it, a second at most
  (with-test-directory (dir)
    (flet ((stop-after-copy (pause)
             (let* ((out (tuatara.io:native (merge-pathnames (format nil "clipboard-~a" pause) dir)))
                    (writer (put-program (merge-pathnames (format nil "copy-~a.sh" pause) dir)
                                         (format nil "#!/bin/sh~%sleep ~a~%cat > ~a~%" pause out)))
                    (tuatara.frontend.tui::*clipboard-programs* (list (list writer) (list writer)))
                    (tuatara.frontend.tui::*clipboard-writer* nil)
                    (tuatara.frontend.tui::*clipboard-watch* nil)
                    (f (make-instance 'tuatara.frontend.tui::tui-frontend))
                    (start (get-internal-real-time)))
               (true (tuatara.frontend:frontend-set-clipboard f "copied"))
               (tuatara.frontend:frontend-stop f)
               (values (ignore-errors (file-text out))
                       (/ (- (get-internal-real-time) start) internal-time-units-per-second)))))
      (is equal "copied" (stop-after-copy "0.2"))
      (multiple-value-bind (text seconds) (stop-after-copy "4")
        (false text)
        (true (< seconds 3) "stopping took ~,1f s" seconds)))))
