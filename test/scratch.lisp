;;;; test/scratch.lisp --- The startup scratch buffers and evaluating Lisp in the editor.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.tests)

(define-test scratch
  :description "The editor starts in *lisp-scratch*, with *org-scratch* beside it; Lisp buffers evaluate
expressions in the running editor.")

(defun fresh-start (keys &key (startup "*lisp-scratch*") pending (width 80) (height 10))
  "Kill the scratch buffers, start a session without files or buffer, type KEYS; return the frontend and editor.
With PENDING the frontend says input waits whenever keys are queued, as a terminal does while
the editor is busy, so that C-g can stop work and keys typed meanwhile are kept."
  (dolist (name '("*lisp-scratch*" "*org-scratch*"))
    (let ((b (tuatara.buffer:find-buffer name))) (when b (tuatara.buffer:kill-buffer b))))
  (let ((f (tuatara.frontend.fake:make-fake-frontend :width width :height height))
        (tuatara-user:*startup-buffer* startup))
    (when pending
      (setf (tuatara.frontend.fake:fake-pending f) (lambda (f) (tuatara.frontend.fake::fake-events f))))
    (tuatara.frontend.fake:fake-queue-keys f keys)
    (values f (tuatara.command:run-editor f :setup #'tuatara.defaults:install-defaults))))

(define-test (scratch startup-buffers)
  (multiple-value-bind (f editor) (fresh-start "")
    (is string= "*lisp-scratch*" (current-name editor))
    (true (typep (tuatara.modes:buffer-major-mode (tuatara.buffer:find-buffer "*lisp-scratch*"))
                 'tuatara.defaults::lisp-mode))
    (true (typep (tuatara.modes:buffer-major-mode (tuatara.buffer:find-buffer "*org-scratch*"))
                 'tuatara.defaults::org-mode))
    (true (tuatara.frontend.fake:fake-frame-showing f "*lisp-scratch*" "Lisp"))
    ;; the first text is not a change: nothing to undo, not modified
    (false (tuatara.buffer:buffer-undo-stack (tuatara.buffer:find-buffer "*lisp-scratch*"))))
  (multiple-value-bind (f editor) (fresh-start "" :startup "*org-scratch*")
    (declare (ignore f))
    (is string= "*org-scratch*" (current-name editor))))

(define-test (scratch auto-made)
  ;; *auto-scratch-buffers* says which are made by themselves: *org-scratch* alone
  (let ((tuatara-user:*auto-scratch-buffers* '("*org-scratch*")))
    (multiple-value-bind (f editor) (fresh-start "")
      (declare (ignore f))
      (is string= "*org-scratch*" (current-name editor))
      (false (tuatara.buffer:find-buffer "*lisp-scratch*"))
      (is string= "*org-scratch*" (tuatara.defaults::auto-scratch-name))))
  ;; none: an empty *scratch*; M-x lisp-scratch makes its buffer all the same
  (let ((tuatara-user:*auto-scratch-buffers* '()))
    (let ((s (tuatara.buffer:find-buffer "*scratch*"))) (when s (tuatara.buffer:kill-buffer s)))
    (multiple-value-bind (f editor) (fresh-start "")
      (declare (ignore f))
      (is string= "*scratch*" (current-name editor))
      (false (tuatara.buffer:find-buffer "*lisp-scratch*"))
      (false (tuatara.buffer:find-buffer "*org-scratch*"))
      (false (tuatara.defaults::auto-scratch-name)))
    (multiple-value-bind (f editor) (fresh-start "M-x l i s p - s c r a t c h RET")
      (declare (ignore f))
      (is string= "*lisp-scratch*" (current-name editor))))
  ;; as standard, both
  (fresh-start "")
  (true (tuatara.buffer:find-buffer "*lisp-scratch*"))
  (true (tuatara.buffer:find-buffer "*org-scratch*")))

(define-test (scratch text-kept-when-asked)
  ;; with *scratch-text-kept* on, what was typed in a scratch buffer is written as the editor stops and put back
  ;; at the next start; off, the first text comes back
  (with-fresh-state
    (let ((tuatara-user:*scratch-text-kept* t))
      (fresh-start "i x y z C-g")
      (let ((kept (cdr (assoc "*lisp-scratch*" (tuatara.defaults::state-list-entries tuatara.defaults::*scratch-texts*)
                              :test #'string=))))
        (true (and kept (uiop:string-suffix-p kept "xyz")) "kept: ~s" kept))
      (let ((editor (nth-value 1 (fresh-start ""))))
        (true (uiop:string-suffix-p (session-text editor) "xyz"))
        (false (tuatara.buffer:buffer-undo-stack (session-buffer editor)))))
    (let ((editor (nth-value 1 (fresh-start ""))))
      (false (search "xyz" (session-text editor)))
      (true (uiop:string-prefix-p ";; Lisp to try out:" (session-text editor))))))

(define-test (scratch text-kept-only-when-changed-and-short)
  ;; the first text is not kept, so a new header reaches users of the setting; a text over the limit is left out
  (with-fresh-state
    (let ((tuatara-user:*scratch-text-kept* t))
      (fresh-start "")
      (tuatara.defaults::keep-scratch-texts)
      (false (tuatara.defaults::state-list-entries tuatara.defaults::*scratch-texts*))
      (fresh-start "i x C-g")
      (tuatara.defaults::keep-scratch-texts)
      (true (tuatara.defaults::state-list-entries tuatara.defaults::*scratch-texts*))))
  (with-fresh-state
    (let ((tuatara-user:*scratch-text-kept* t)
          (tuatara.defaults::*rewrite-threshold* 10))
      (fresh-start "i x y z C-g")
      (tuatara.defaults::keep-scratch-texts)
      (false (tuatara.defaults::state-list-entries tuatara.defaults::*scratch-texts*)))))

(define-test (scratch shown-and-made-anew)
  ;; killed, M-x lisp-scratch makes a new one as at startup, and says so
  (multiple-value-bind (f editor) (fresh-start "C-x k M-x l i s p - s c r a t c h RET")
    (let ((b (tuatara.buffer:find-buffer "*lisp-scratch*")))
      (is string= "*lisp-scratch*" (current-name editor))
      (true (tuatara.frontend.fake:fake-frame-showing f "Made a new *lisp-scratch*"))
      (true (typep (tuatara.modes:buffer-major-mode b) 'tuatara.defaults::lisp-mode))
      (is string= ";; Lisp to try out: C-x C-e after an expression evaluates it."
          (subseq (tuatara.buffer:buffer-string b) 0 (position #\Newline (tuatara.buffer:buffer-string b))))
      (false (tuatara.buffer:buffer-undo-stack b))
      (false (tuatara.buffer:buffer-modified-p b))))
  (multiple-value-bind (f editor) (fresh-start "M-x o r g - s c r a t c h RET C-x k M-x o r g - s c r a t c h RET")
    (is string= "*org-scratch*" (current-name editor))
    (true (tuatara.frontend.fake:fake-frame-showing f "Made a new *org-scratch*"))
    (true (typep (tuatara.modes:buffer-major-mode (tuatara.buffer:find-buffer "*org-scratch*"))
                 'tuatara.defaults::org-mode))
    ;; the header comment, word for word, with the keys bound now: its text is not kept, and how to keep it
    (is string= (format nil "# Notes in Org that are not kept: they go when Tuatara quits, unless~%~
                             # *scratch-text-kept* is on. C-x C-s saves them to a file;~%# C-x C-f opens one.~%~%")
        (tuatara.buffer:buffer-string (tuatara.buffer:find-buffer "*org-scratch*"))))
  ;; there already: shown as it is, what was typed in it kept, nothing said
  (multiple-value-bind (f editor)
      (fresh-start "i ( + SPC 1 ) C-g M-x o r g - s c r a t c h RET M-x l i s p - s c r a t c h RET")
    (is string= "*lisp-scratch*" (current-name editor))
    (true (search "(+ 1)" (session-text editor)))
    (false (tuatara.frontend.fake:fake-frame-showing f "Made a new"))))

(define-modal-test (scratch evaluation)
  ;; C-x C-e after an expression shows its value
  (multiple-value-bind (f editor) (fresh-start "i ( + SPC 1 SPC 2 ) C-g C-x C-e")
    (declare (ignore editor))
    (true (tuatara.frontend.fake:fake-frame-showing f "=> 3")))
  ;; with C-u the value is inserted after point
  (multiple-value-bind (f editor) (fresh-start "i ( * SPC 6 SPC 7 ) C-g C-u C-x C-e")
    (declare (ignore f))
    (true (search "(* 6 7) 42" (session-text editor))))
  ;; several values, strings printed readably
  (multiple-value-bind (f editor) (fresh-start "i ( v a l u e s SPC 1 SPC \" a \" ) C-g C-x C-e")
    (declare (ignore editor))
    (true (tuatara.frontend.fake:fake-frame-showing f "=> 1 ; \"a\"")))
  ;; an error is a message, not a crash
  (multiple-value-bind (f editor) (fresh-start "i ( c a r SPC 1 ) C-g C-x C-e")
    (declare (ignore editor))
    (true (tuatara.frontend.fake:fake-frame-showing f "Error:")))
  ;; C-M-x evaluates the top-level form around point; the definition is then usable
  (multiple-value-bind (f editor)
      (fresh-start "i ( d e f u n SPC t t - f n SPC ( ) RET SPC SPC 4 2 ) C-g k C-M-x g e o ( t t - f n ) C-g C-x C-e")
    (declare (ignore editor))
    (true (tuatara.frontend.fake:fake-frame-showing f "=> TT-FN"))
    (true (tuatara.frontend.fake:fake-frame-showing f "=> 42")))
  ;; a defvar: C-M-x sets its variable again, C-x C-e leaves it as it was
  (fresh-start "i ( d e f v a r SPC * t t - v * SPC 1 ) C-g C-x C-e")
  (fresh-start "i ( d e f v a r SPC * t t - v * SPC 2 ) C-g C-x C-e")
  (is = 1 (symbol-value (find-symbol "*TT-V*" :tuatara-user)))
  (fresh-start "i ( d e f v a r SPC * t t - v * SPC 3 ) C-g C-M-x")
  (is = 3 (symbol-value (find-symbol "*TT-V*" :tuatara-user))))

(define-test (scratch evaluation-reads-within-reach)
  ;; an expression whose top-level form starts further back than the reach is not read for:
  ;; C-x C-e and C-M-x say so at once, and one within reach is still evaluated
  (let ((tuatara.defaults::*lisp-indent-reach* 1000)
        (far (format nil "~{~a~%~}  (+ 1 2)" (loop repeat 1000 collect "  x"))))
    (dolist (keys '("M-x l i s p - m o d e RET M-> C-x C-e" "M-x l i s p - m o d e RET M-> C-M-x"))
      (true (tuatara.frontend.fake:fake-frame-showing (run-keys keys :text far :width 80)
                                                      "No expression within reach")))
    (let ((buffer (tuatara.buffer:make-buffer "*tt-reach*" :text far)))
      (unwind-protect
           (progn
             (is eq :far (nth-value 1 (tuatara.defaults::expression-before buffer (length far))))
             (is eq :far (nth-value 1 (tuatara.defaults::top-level-form-bounds buffer (length far)))))
        (tuatara.buffer:kill-buffer buffer)))
    (true (tuatara.frontend.fake:fake-frame-showing
           (run-keys "M-x l i s p - m o d e RET M-> C-x C-e" :text (format nil "  x~%  (+ 1 2)") :width 80)
           "=> 3"))))

(define-test (scratch quitting)
  ;; typing in a scratch buffer does not make quitting ask
  (multiple-value-bind (f editor) (fresh-start "i x C-g C-x C-c")
    (false (tuatara.command:editor-running editor))
    (false (tuatara.frontend.fake:fake-frame-showing f "Save the changed buffer")))
  ;; a modified file does
  (let ((path (merge-pathnames (format nil "tuatara-quit-~a.txt" (random 1000000)) (uiop:temporary-directory))))
    (with-open-file (o path :direction :output :if-exists :supersede) (write-string "a" o))
    (unwind-protect
         (multiple-value-bind (f editor) (run-keys "i x C-g C-x C-c" :file path)
           (true (tuatara.frontend.fake:fake-frame-showing f "Save the changed buffer"))
           (tuatara.buffer:kill-buffer (tuatara.display:window-buffer (tuatara.command:editor-window editor))))
      (ignore-errors (delete-file path)))))

(define-test (scratch quitting-asks-for-a-typed-answer)
  ;; the question before leaving with changes not saved says its answer is typed and ended with RET:
  ;; y alone waits for RET (C-g then leaves the question); y RET and yes RET save and leave; n RET leaves
  ;; without saving; C-g stays
  (with-test-directory (dir)
    (let ((path (namestring (merge-pathnames "quit.txt" dir))))
      (flet ((quit-with (answer)
               ;; the frontend, whether the editor still runs, the text, and the file, once ANSWER is typed
               (alexandria:write-string-into-file "a" path :if-exists :supersede)
               (multiple-value-bind (f editor) (run-keys (format nil "i x C-g C-x C-c ~a" answer) :file path :width 80)
                 (multiple-value-prog1 (values f (tuatara.command:editor-running editor) (session-text editor)
                                               (file-text path))
                   (forget-file-buffers)))))
        (true (tuatara.frontend.fake:fake-frame-showing (quit-with "y")
                                                        "Save the changed buffer quit.txt? (y or n, then RET)"))
        (let ((text (nth-value 2 (quit-with "y C-g i z"))))
          (true (search "z" text))
          (false (search "y" text)))
        ;; left: what is typed after the answer reaches nothing
        (multiple-value-bind (f running text file) (quit-with "y RET i z")
          (declare (ignore f running))
          (false (search "z" text))
          (is string= "xa" file))
        (is string= "xa" (nth-value 3 (quit-with "y e s RET")))
        (multiple-value-bind (f running text file) (quit-with "n RET i z")
          (declare (ignore f running))
          (false (search "z" text))
          (is string= "a" file))
        (multiple-value-bind (f running text file) (quit-with "C-g i z")
          (declare (ignore f running))
          (true (search "z" text))
          (is string= "a" file))))))

(define-test (scratch quitting-without-saving-keeps-a-copy)
  ;; n at the question before leaving writes every change to its auto-saved copy first, though the copy was
  ;; not due yet, so that recover-file takes it back
  (with-test-directory (dir)
    (let ((path (namestring (merge-pathnames "kept.txt" dir)))
          (tuatara-user:*auto-save* t)
          (tuatara-user:*auto-save-directory* (namestring (merge-pathnames "auto-saves/" dir)))
          (tuatara-user:*auto-save-idle* 3600))
      (alexandria:write-string-into-file "a" path)
      (unwind-protect
           (let ((editor (nth-value 1 (run-keys "i x C-g C-x C-c n RET" :file path :width 80))))
             (false (tuatara.command:editor-running editor))
             (is string= "a" (file-text path))
             (is string= "xa" (file-text (tuatara.defaults::auto-save-path path))))
        (forget-file-buffers)))))

(define-test (scratch quitting-with-a-save-refused-stays)
  ;; y at the question before leaving, with a save that is refused or fails, leaves nothing behind: the
  ;; editor stays, says which buffer is not saved, and the change is in its auto-saved copy
  (with-test-directory (dir)
    (let* ((shut (merge-pathnames "shut/" dir))
           (path (namestring (merge-pathnames "kept.txt" shut)))
           (tuatara-user:*auto-save* t)
           (tuatara-user:*auto-save-directory* (namestring (merge-pathnames "auto-saves/" dir)))
           (tuatara-user:*auto-save-idle* 3600))
      (ensure-directories-exist shut)
      (alexandria:write-string-into-file "a" path)
      (unwind-protect
           (dolist (anyway '("n" "y"))
             (sb-posix:chmod (namestring shut) #o755)
             (sb-posix:chmod path #o644)
             (multiple-value-bind (f editor)
                 ;; the file and its directory are shut once the change is made, before the save
                 (run-keys (list "i x C-g"
                                 (lambda ()
                                   (sb-posix:chmod path #o444)
                                   (sb-posix:chmod (namestring shut) #o555))
                                 (format nil "C-x C-c y RET ~a RET i z" anyway))
                           :file path :width 100)
               ;; stayed: what is typed after it reaches the buffer
               (true (search "z" (session-text editor)))
               (true (tuatara.frontend.fake:fake-frame-showing f "Not exited" "kept.txt"))
               (is string= "a" (file-text path))
               (is string= "xa" (file-text (tuatara.defaults::auto-save-path path))))
             (forget-file-buffers))
        (sb-posix:chmod (namestring shut) #o755)
        (sb-posix:chmod path #o644)))))

;; the last message of a refused exit is its outcome, not the count of the files saved first
(define-test (scratch quitting-with-one-save-declined-says-not-exited)
  (with-test-directory (dir)
    (let ((locked (put-file (merge-pathnames "b1.txt" dir) "one"))
          (free (put-file (merge-pathnames "b2.txt" dir) "two")))
      (sb-posix:chmod (namestring locked) #o444)
      (unwind-protect
           (multiple-value-bind (f editor)
               (run-keys (format nil "C-x C-q i x C-g C-x C-f ~a RET i y C-g C-x C-c y RET n RET" (key-string (namestring free)))
                         :file locked :width 120 :height 10)
             (declare (ignore editor))
             (is string= "ytwo" (file-text free))
             (let ((rows (loop for row below 10 collect (tuatara.frontend.fake:fake-screen-line f row))))
               (true (some (lambda (row) (search "Not exited: b1.txt not saved" row)) rows))
               (false (some (lambda (row) (search "Saved 1 file" row)) rows)))
             (forget-file-buffers))
        (sb-posix:chmod (namestring locked) #o644)))))

(define-test (scratch typed-answer-asked-again)
  ;; a typed answer other than y, yes, n or no, none at all too, says which it takes and asks again
  (with-test-directory (dir)
    (let ((path (namestring (merge-pathnames "again.txt" dir))))
      (alexandria:write-string-into-file "a" path)
      (flet ((quit-with (answer)
               ;; the frontend, whether the editor still runs, and the text, once ANSWER is typed
               (multiple-value-bind (f editor) (run-keys (format nil "i x C-g C-x C-c ~a" answer) :file path :width 100)
                 (multiple-value-prog1 (values f (tuatara.command:editor-running editor) (session-text editor))
                   (forget-file-buffers)))))
        (multiple-value-bind (f running text) (quit-with "y y RET C-g i z")
          (declare (ignore running))
          (true (search "z" text))
          (true (tuatara.frontend.fake:fake-frame-showing-across-rows
                 f "Answer y or n. Save the changed buffer again.txt?")))
        (true (search "z" (nth-value 2 (quit-with "RET C-g i z"))))
        (false (nth-value 1 (quit-with "y y RET n RET")))
        (false (nth-value 1 (quit-with "RET N O RET")))
        (true (search "z" (nth-value 2 (quit-with "y y RET C-g i z"))))
        (is string= "a" (file-text path))))))

(defun evaluating (code &key (then "") pending)
  "Start in *lisp-scratch*, type CODE on a new line, evaluate it with C-x C-e,
then type THEN; return the frontend and the editor."
  (fresh-start (format nil "o ~a C-x C-e ~a" (key-string code) then) :pending pending :width 160))

(defun messages-text ()
  "The text of *Messages*."
  (tuatara.buffer:buffer-string (tuatara.buffer:find-buffer "*Messages*")))

(defun showing (frontend &rest texts)
  "True when a frame FRONTEND painted had a row with all of TEXTS."
  (apply #'tuatara.frontend.fake:fake-frame-showing frontend texts))

(define-test (scratch evaluation-output-and-errors)
  ;; what the code prints goes to *Messages*, never to the terminal, and is said
  (let ((f (evaluating "(print 'hello)")))
    (true (showing f "=> HELLO" "printed 1 line: C-h e"))
    (true (search "HELLO" (messages-text))))
  (let ((f (evaluating "(format t \"one~%two~%\")")))
    (true (showing f "=> NIL" "printed 2 lines"))
    (true (search (format nil "one~%two") (messages-text))))
  ;; warnings counted, their text kept
  (let ((f (evaluating "(warn \"careful\")")))
    (true (showing f "=> NIL" "1 warning, printed 1 line"))
    (true (search "WARNING: careful" (messages-text))))
  ;; errors reported, the editor going on
  (true (showing (evaluating "(/ 1 0)") "Error: arithmetic error DIVISION-BY-ZERO"))
  ;; on one line, a space between words where the report breaks its lines
  (true (showing (evaluating "(car 1)") "Error: The value 1 is not of type LIST"))
  (is string= "two words"
      (tuatara.runtime:condition-words (make-condition 'simple-error :format-control (format nil "  two~%    words~%")
                                                                    :format-arguments '())))
  (true (showing (evaluating "(values)") "=> ; No values"))
  ;; running out of stack is reported, not the end of the editor
  (let ((f (evaluating "(labels ((deep (n) (1+ (deep n)))) (deep 1))" :then "o ( + SPC 1 SPC 1 ) C-x C-e")))
    (true (showing f "Error: Control stack exhausted"))
    (true (showing f "=> 2")))
  ;; reading finds an empty input rather than the terminal, and the debugger is not entered
  (true (showing (evaluating "(read-line)") "Error: end of file: code evaluated in the editor has no input to read"))
  (true (showing (evaluating "(break)") "Error: break")))

(define-test (scratch evaluation-package)
  ;; IN-PACKAGE holds in that buffer, for the evaluations after it, and nowhere else
  (let ((before *package*))
    (multiple-value-bind (f editor)
        (evaluating "(in-package :cl-user)" :then "o ( p a c k a g e - n a m e SPC * p a c k a g e * ) C-x C-e")
      (true (showing f "=> \"COMMON-LISP-USER\""))
      (is string= "COMMON-LISP-USER" (tuatara.buffer:buffer-local (tuatara.display:window-buffer
                                                                   (tuatara.command:editor-window editor))
                                                                  :lisp-package)))
    (is eq before *package*))
  ;; symbols print as typed there, without their package
  (true (showing (evaluating "'hello") "=> HELLO")))

(define-test (scratch evaluation-stopped-and-typeahead)
  ;; C-g stops an evaluation that runs on; the editor carries on
  (let ((f (evaluating "(loop)" :then "C-g o ( + SPC 2 SPC 2 ) C-x C-e" :pending t)))
    (true (showing f "Evaluating... (C-g stops it)"))
    (true (showing f "Evaluation stopped"))
    (true (showing f "=> 4")))
  ;; keys typed while an evaluation runs are kept, and handled after it, in order
  (multiple-value-bind (f editor) (evaluating "(sleep 1/2)" :then "x y" :pending t)
    (true (showing f "=> NIL"))
    (true (search "(sleep 1/2)xy" (session-text editor)))))

(define-modal-test (scratch evaluation-boundaries)
  ;; brackets in strings, character names and comments count for nothing
  (true (showing (evaluating "(format nil \"(\")") "=> \"(\""))
  (true (showing (evaluating "(list #\\( #\\))") "=> (#\\( #\\))"))
  (true (showing (evaluating "(+ 1 2) ; )") "=> 3"))
  (true (showing (evaluating "'(a \"b)\" c)") "=> (A \"b)\" C)"))
  ;; C-M-x takes the whole top-level form around point, strings with brackets in it
  (let ((f (fresh-start (format nil "o ~a RET ~a C-M-x" (key-string "(defun g () \"a ) b\"") (key-string "  7)"))
                        :width 160)))
    (true (showing f "=> G"))))

(define-modal-test (scratch eval-region-buffer-expression)
  ;; eval-region: the forms selected, one after another; nothing without a selection
  (let ((f (fresh-start (format nil "o ~a C-x h M-x e v a l - r e g i o n RET"
                                (key-string "(defparameter *seven* 7) (+ *seven* 1)"))
                        :width 160)))
    (true (showing f "=> 8  (2 forms)")))
  (true (showing (fresh-start "M-x e v a l - r e g i o n RET" :width 160) "No selection: eval-region was not run"))
  ;; a form not closed is said plainly
  (true (showing (fresh-start (format nil "o ~a C-x h M-x e v a l - r e g i o n RET" (key-string "(+ 1 2) (list 3"))
                              :width 160)
                 "Error: The text ends inside a form: a bracket or a string is not closed."))
  ;; eval-buffer: every form in the buffer, the header's comments skipped
  (let ((f (fresh-start (format nil "o ~a M-x e v a l - b u f f e r RET"
                                (key-string "(defparameter *eight* 8) (* *eight* 2)"))
                        :width 160)))
    (true (showing f "=> 16  (2 forms)")))
  ;; M-: reads an expression and shows its value; with C-u it is inserted at point
  (true (showing (fresh-start "M-: ( * SPC 6 SPC 7 ) RET" :width 160) "=> 42"))
  (multiple-value-bind (f editor) (fresh-start "o C-u M-: ( * SPC 6 SPC 7 ) RET" :width 160)
    (declare (ignore f))
    (true (search (format nil "~%42") (session-text editor)))))

(defvar *evaluated-by-hostile-text* nil
  "Set by the text of a file that must never be evaluated.")

(define-test (scratch evaluation-refused-where-unsafe)
  ;; a Lisp file opened safely is never run, whichever way evaluation is asked for, its mode
  ;; made Lisp's or not
  (with-test-directory (dir)
    (let ((file (namestring (put-file (merge-pathnames "hostile.lisp" dir)
                                      (format nil "(setf tuatara.tests::*evaluated-by-hostile-text* t)~%"))))
          (refusals '()))
      (setf *evaluated-by-hostile-text* nil)
      (flet ((refusals-here (where)
               (lambda ()
                 (push (cons where (mapcar #'tuatara.defaults::mode-command-refusal
                                           tuatara.defaults::*buffer-evaluating-commands*))
                       refusals))))
        (dolist (then '("M-> C-x C-e" "M-< C-M-x"))
          (multiple-value-bind (f editor)
              (run-keys (list (format nil "C-u C-x C-f C-a C-k ~a RET M-x l i s p - m o d e RET" (key-string file))
                              (refusals-here :lisp-mode) then)
                        :width 160 :height 12)
            (let ((b (tuatara.display:window-buffer (tuatara.command:editor-window editor))))
              (true (tuatara.io:safe-visit-p b))
              (true (typep (tuatara.modes:buffer-major-mode b) 'tuatara.defaults::lisp-mode)))
            (false *evaluated-by-hostile-text*)
            (true (tuatara.frontend.fake:fake-frame-showing f "is opened safely: nothing was evaluated")))
          (kill-buffers-visiting dir))
        (run-keys (list (format nil "C-u C-x C-f C-a C-k ~a RET" (key-string file)) (refusals-here :as-opened))
                  :width 160 :height 12)
        (kill-buffers-visiting dir))
      (false *evaluated-by-hostile-text*)
      (is = 7 (length (cdr (assoc :lisp-mode refusals))))
      (true (every (lambda (said) (search "is opened safely: nothing was evaluated" said))
                   (cdr (assoc :lisp-mode refusals))))
      (true (every (lambda (said) (search "works in Lisp buffers only" said)) (cdr (assoc :as-opened refusals))))))
  ;; nor is the text of a buffer that is not Lisp
  (let ((said nil))
    (run-keys (list (lambda () (setf said (tuatara.defaults::mode-command-refusal 'tuatara.defaults::eval-buffer))))
              :text "(setf tuatara.tests::*evaluated-by-hostile-text* t)" :width 160)
    (is string= "eval-buffer works in Lisp buffers only" said)))

(define-test (scratch evaluate-and-print)
  ;; C-j in *lisp-scratch* puts what was printed and the value on the lines after the expression
  (multiple-value-bind (f editor) (fresh-start (format nil "g e o ~a C-j" (key-string "(print \"hello\")")) :width 160)
    (declare (ignore f))
    (true (search (format nil "(print \"hello\")~%~%\"hello\"~%\"hello\"~%") (session-text editor))))
  (multiple-value-bind (f editor) (fresh-start (format nil "g e o ~a C-j" (key-string "(+ 1 2)")) :width 160)
    (declare (ignore f))
    (true (search (format nil "(+ 1 2)~%3~%") (session-text editor))))
  ;; its mode is Lisp mode's, C-x C-e too
  (true (typep (tuatara.modes:buffer-major-mode (tuatara.buffer:find-buffer "*lisp-scratch*"))
               'tuatara.defaults::lisp-interaction-mode))
  (true (showing (fresh-start (format nil "g e o ~a C-x C-e" (key-string "(+ 1 2)")) :width 160) "=> 3"))
  ;; an error says so and puts nothing in
  (multiple-value-bind (f editor) (fresh-start (format nil "g e o ~a C-j" (key-string "(car 5)")) :width 160)
    (true (showing f "Error:"))
    (true (search (format nil "(car 5)") (session-text editor)))
    (false (search (format nil "(car 5)~%") (session-text editor))))
  ;; message is there for the Lisp one types: M-:, the scratch buffer, the init file
  (true (showing (fresh-start (format nil "M-: ~a RET" (key-string "(message \"hi ~a\" 42)")) :width 160) "hi 42"))
  (true (eq 'tuatara.command:message 'tuatara-user::message)))

(define-test (scratch integers-shown-several-ways)
  ;; M-: shows an integer with its octal, hex and character
  (flet ((shown (expression)
           (multiple-value-bind (f editor) (run-keys (format nil "M-: ~a RET" (key-string expression))
                                                     :text ""
                                                     :width 100)
             (declare (ignore editor))
             (echo-text f))))
    (is string= "=> 65 (#o101, #x41, #\\A)" (shown "65"))
    (is string= "=> 10 (#o12, #xa, #\\Newline)" (shown "10"))
    (is string= "=> 1000 (#o1750, #x3e8)" (shown "1000"))
    (is string= "=> -5 (#o-5, #x-5)" (shown "-5"))
    (is string= "=> \"x\"" (shown "\"x\""))
    (is string= "(values 1 2) => 1 ; 2" (shown "(values 1 2)")))
  ;; C-u M-: puts the value alone at point
  (is string= "65"
      (session-text (nth-value 1 (run-keys (format nil "C-u M-: ~a RET" (key-string "65")) :text "" :width 100)))))

(define-test (scratch value-said-with-its-expression)
  ;; the value after what was evaluated, in the echo area and in *Messages*; a long one cut
  (let ((f (evaluating "(setf *print-pretty* *print-pretty*)")))
    (true (showing f "(setf *print-pretty* *print-pretty*) => "))
    (true (search "(setf *print-pretty* *print-pretty*) => " (messages-text))))
  (true (showing (evaluating (format nil "(list 1~%   2)")) "(list 1 2) => (1 2)"))
  (true (showing (evaluating (format nil "(list ~{~a~^ ~})" (loop for i below 40 collect i)))
                 "(list 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20… => ")))

(define-test (scratch stopped-but-not-at-once)
  ;; C-g on an evaluation that cannot be interrupted for a while: never ended outright, said to end on its own, and ends
  (let ((before (count "tuatara evaluation" (sb-thread:list-all-threads) :key #'sb-thread:thread-name :test #'equal))
        (start (get-internal-real-time)))
    (let ((f (evaluating "(sb-sys:without-interrupts (loop repeat 400 do (sleep 0.01)))" :then "C-g" :pending t)))
      (true (showing f "Evaluation stopped"))
      (true (search "ends on its own" (messages-text)))
      (true (< (- (get-internal-real-time) start) (* 4 internal-time-units-per-second))))
    (true (loop repeat 600
                thereis (<= (count "tuatara evaluation"
                                   (sb-thread:list-all-threads)
                                   :key #'sb-thread:thread-name
                                   :test #'equal)
                           before)
                do (sleep 0.01)))))

(defun abandoned-evaluation-sample ()
  "Give *FILL-COLUMN* 66, then, six seconds later and not to be stopped meanwhile, a value it
does not take: longer than the editor waits for an evaluation it stopped to end."
  (sb-sys:without-interrupts
    (setf tuatara-user:*fill-column* 66)
    (loop repeat 600 do (sleep 0.01))
    (setf tuatara-user:*fill-column* "wide")))

(define-test (scratch abandoned-evaluation-settings-put-back)
  ;; an evaluation given up on that gives a setting a value it does not take as its thread ends:
  ;; the setting goes back to what it held when the evaluation was given up on, and that is said
  (let ((width tuatara-user:*fill-column*))
    (unwind-protect
         (progn
           (auto-save-session
            (list (format nil "M-: ~a RET" (key-string "(tuatara.tests::abandoned-evaluation-sample)"))
                  (list :until (lambda () (eql tuatara-user:*fill-column* 66)) 30)
                  "C-g"
                  (list :until (lambda () (search "*fill-column* is \"wide\"" (messages-text))) 30))
            :width 200 :pending t)
           (is eql 66 tuatara-user:*fill-column* "~a" (messages-text)))
      (setf tuatara-user:*fill-column* width))))

(define-test (scratch stop-after-the-forms-does-nothing)
  ;; asked to stop once its forms are done, as it ends, an evaluation's thread is not thrown out of
  ;; where there is nothing to catch it
  (let* ((go (list nil))
         (thread (sb-thread:make-thread (lambda ()
                                          (handler-case (progn (tuatara.runtime:wait-until (lambda () (car go)) 10)
                                                               :ended)
                                            (control-error () :thrown)))
                                        :name "tuatara stop test")))
    (tuatara.defaults::stop-evaluation-thread thread (list :running nil nil))
    (sleep 0.1)
    (setf (car go) t)
    (is eq :ended (sb-thread:join-thread thread :default :failed))))

(defstruct (unprintable-sample (:print-function (lambda (object stream depth)
                                                  (declare (ignore object stream depth))
                                                  (error "no printing here")))))

(define-test (scratch unprintable-value)
  ;; a value whose printing fails is said to be so, in the echo area, inserted, after the
  ;; expression, in *Lisp REPL* and beside a completion alike
  (let ((call (key-string "(tuatara.tests::make-unprintable-sample)")))
    (multiple-value-bind (f editor) (fresh-start (format nil "i ~a C-g C-x C-e" call) :width 200)
      (declare (ignore editor))
      (true (tuatara.frontend.fake:fake-frame-showing f "=> #<a value that cannot be printed: no printing here>")))
    (multiple-value-bind (f editor) (fresh-start (format nil "i ~a C-g C-u C-x C-e" call) :width 200)
      (declare (ignore f))
      (true (search ") #<a value that cannot be printed: no printing here>" (session-text editor)))))
  (is string= (format nil "printed~%1~%#<a value that cannot be printed: no printing here>~%")
      (with-output-to-string (out)
        (tuatara.defaults::write-output-and-values out (format nil "printed  ") (list 1 (make-unprintable-sample)))))
  (is string= (format nil "~a~%*** Eval error ***  x~%" "1")
      (tuatara.defaults::lisp-repl-results
       (tuatara.defaults::make-evaluation :kind :error :condition (make-condition 'simple-error :format-control "x")
                                          :output "1" :package *package*)))
  (is string= (format nil "; No values~%")
      (tuatara.defaults::lisp-repl-results
       (tuatara.defaults::make-evaluation :kind :values :count 1 :output "" :package *package*)))
  (is string= "#<a value that cannot be printed: no printing here>"
      (tuatara.defaults::value-preview (make-unprintable-sample)))
  (is string= "(1 2 3) ; \"a b\"" (tuatara.defaults::format-values (list '(1 2 3) (format nil "a~%b")))))
