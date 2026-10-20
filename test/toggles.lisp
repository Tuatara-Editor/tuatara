;;;; test/toggles.lisp --- The display toggles, each switched and set the same way.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.tests)

(define-test toggles
  :description "Soft wrap, the line at the fill column, centred text, the highlighted line and column, line numbers and the characters that draw nothing shown as their codes: a command for the buffer and one for every buffer, one message form, the list by major mode through the modes made from it, the setting for every buffer, the buffer's own choice first, the mode line's word; and the one lookup of settings by major mode.")

(defparameter *toggle-cases*
  '((tuatara.defaults::soft-wrap "soft-wrap-mode" "Soft wrap" "Wrap" t)
    (tuatara.defaults::visual-line "visual-line-mode" "Visual line" nil t)
    (tuatara.defaults::centred-text "centred-text-mode" "Centred text" "Centred" t)
    (tuatara.defaults::hl-line "hl-line-mode" "Hl-line" nil t)
    (tuatara.defaults::cursor-column "cursor-column-mode" "Cursor column" nil t)
    (tuatara.defaults::spellcheck "spellcheck-mode" "Spellcheck" nil t)
    (tuatara.defaults::invisible-characters "invisible-characters-mode" "Invisible characters" "Invisible" t)
    (tuatara.defaults::line-numbers "display-line-numbers-mode" "Line numbers" nil :absolute))
  "Each display toggle tested: its name, its command for the buffer, the label
its messages begin with, the word the mode line shows while it is on (NIL for
none), and the value it takes when its command switches it on.")

(defun typed (&rest names)
  "Keys that run each of NAMES (commands' names) with M-x, one after another."
  (format nil "~{M-x ~{~a~^ ~} RET~^ ~}" (mapcar (lambda (name) (map 'list #'string name)) names)))

(defun toggle-session (name keys &key global modes)
  "Run KEYS on a scratch buffer with the display toggle called NAME set to
GLOBAL for every buffer and to MODES by major mode, the others as the tests
have them; return the frontend, the editor, and what NAME's lookup gives in
the buffer shown at the end."
  (let ((toggle (tuatara.defaults::find-display-toggle name)))
    (progv (list (tuatara.defaults::display-toggle-global toggle) (tuatara.defaults::display-toggle-modes toggle))
        (list global modes)
      (multiple-value-bind (f editor) (run-keys keys :text (format nil "one two three four five six seven~%x~%") :width 60 :height 8)
        (values f editor (funcall (tuatara.defaults::display-toggle-lookup toggle) (session-buffer editor)))))))

(defun mode-line-row (frontend)
  "The mode line's row on FRONTEND's screen: the one naming the scratch buffer."
  (loop for row below 8
        for line = (tuatara.frontend.fake:fake-screen-line frontend row)
        when (search "*scratch*" line) return line))

(define-test (toggles switched-in-one-buffer)
  ;; each toggle's command switches it in the buffer and says so in one form;
  ;; again, and it is off; with a count, on when positive, off when zero or less
  (loop for (name command label nil on) in *toggle-cases*
        do (multiple-value-bind (f editor value) (toggle-session name (typed command))
             (is eq on value "~a on" command)
             (true (tuatara.frontend.fake:fake-frame-showing
                    f (format nil "~a ~a" label (tuatara.defaults::display-toggle-words (tuatara.defaults::find-display-toggle name)
                                                                                       on (session-buffer editor))))))
           (multiple-value-bind (f editor value) (toggle-session name (typed command command))
             (declare (ignore editor))
             (false value "~a twice" command)
             (true (tuatara.frontend.fake:fake-frame-showing f (format nil "~a off" label))))
           (is eq on (nth-value 2 (toggle-session name (format nil "C-u 1 ~a C-u 1 ~a" (typed command) (typed command)))))
           (false (nth-value 2 (toggle-session name (format nil "C-u 0 ~a" (typed command)) :global on)))))

(define-test (toggles switched-in-every-buffer)
  ;; global-COMMAND sets the setting for every buffer, says so, and a buffer
  ;; that chose for itself keeps its choice
  (loop for (name command label nil on) in *toggle-cases*
        for global = (format nil "global-~a" command)
        for setting = (tuatara.defaults::display-toggle-global (tuatara.defaults::find-display-toggle name))
        do (multiple-value-bind (f editor value) (toggle-session name (typed global))
             (declare (ignore editor))
             (is eq on value "~a" global)
             (true (tuatara.frontend.fake:fake-frame-showing
                    f (format nil "~a ~a in every buffer" label
                              (tuatara.defaults::display-toggle-words (tuatara.defaults::find-display-toggle name) on nil)))))
           (multiple-value-bind (f editor value) (toggle-session name (typed global) :global on)
             (declare (ignore editor))
             (false value)
             (true (tuatara.frontend.fake:fake-frame-showing f (format nil "~a off in every buffer" label))))
           ;; the buffer chose on; every buffer switched on and off again; still on in it
           (is eq on (nth-value 2 (toggle-session name (typed command global global))))
           ;; the setting is the one the init file sets
           (true (find-symbol (symbol-name setting) :tuatara-user))))

(define-test (toggles default-and-own-choice)
  (loop for (name command label nil on) in *toggle-cases*
        do ;; the setting for every buffer is what a buffer that has not chosen shows
           (is eq on (nth-value 2 (toggle-session name "" :global on)) "~a by default" name)
           (false (nth-value 2 (toggle-session name "")))
           ;; the buffer's own choice before it, and before the list by major mode
           (false (nth-value 2 (toggle-session name (typed command) :global on)))
           (false (nth-value 2 (toggle-session name (typed command) :global on
                                                                :modes (list (cons (intern "FUNDAMENTAL-MODE" :tuatara-user) on)))))
           (is eq on (nth-value 2 (toggle-session name (typed command) :modes '((fundamental-mode . nil)))))))

(define-test (toggles by-major-mode)
  ;; the list by major mode, its names written as in the init file (read in
  ;; TUATARA-USER, with or without -mode), counts for the modes made from them,
  ;; the nearest one listed deciding, and before the setting for every buffer
  (true (member 'tuatara.defaults::text-mode
                (tuatara.modes:mode-precedence (make-instance 'tuatara.defaults::org-mode))))
  (let ((*package* (find-package :tuatara-user)))
    (loop for (name nil nil nil on) in *toggle-cases*
          do (is eq on (nth-value 2 (toggle-session name (typed "org-mode") :modes (read-from-string (format nil "((text-mode . ~s))" on))))
                 "~a by a mode Org is made from" name)
             (is eq on (nth-value 2 (toggle-session name (typed "org-mode") :modes (read-from-string (format nil "((text . ~s))" on)))))
             (false (nth-value 2 (toggle-session name (typed "org-mode") :global on :modes (read-from-string "((org-mode . nil))"))))
             (false (nth-value 2 (toggle-session name (typed "org-mode")
                                                  :modes (read-from-string (format nil "((text-mode . ~s) (org-mode . nil))" on)))))
             ;; a mode the buffer is not in says nothing
             (false (nth-value 2 (toggle-session name "" :modes (read-from-string (format nil "((csv-mode . ~s))" on))))))))

(define-test (toggles mode-line-word)
  ;; those that move the text about say so in the mode line (Wrap, Centred);
  ;; the others leave it as it was
  (loop for (name command nil word) in *toggle-cases*
        for before = (mode-line-row (toggle-session name ""))
        for after = (mode-line-row (toggle-session name (typed command)))
        do (true before)
           (if word
               (progn (true (search word after) "~a: ~s" command after)
                      (false (search word before)))
               (is string= before after "~a" command))))

(define-test (toggles line-numbers-values)
  ;; line numbers take two values: the commands for each, the cycle, and the
  ;; list by major mode giving either
  (flet ((said (keys &rest texts)
           (let ((f (toggle-session 'tuatara.defaults::line-numbers keys)))
             (dolist (text texts) (true (tuatara.frontend.fake:fake-frame-showing f text) "~a: ~a" keys text)))))
    (said (typed "display-relative-line-numbers-mode") "Line numbers relative")
    (said (typed "display-relative-line-numbers-mode" "display-relative-line-numbers-mode") "Line numbers off")
    (said (typed "toggle-line-numbers" "toggle-line-numbers" "toggle-line-numbers")
          "Line numbers absolute" "Line numbers relative" "Line numbers off"))
  (is eq :absolute (nth-value 2 (toggle-session 'tuatara.defaults::line-numbers (typed "display-line-numbers-mode") :global :relative)))
  (is eq :relative (nth-value 2 (toggle-session 'tuatara.defaults::line-numbers (typed "org-mode")
                                                :modes (list (cons (intern "TEXT-MODE" :tuatara-user) :relative)))))
  (let ((f (toggle-session 'tuatara.defaults::line-numbers "" :modes '((tuatara.modes:fundamental-mode . :absolute)))))
    (is string= "1 one two three" (subseq (tuatara.frontend.fake:fake-screen-line f 0) 0 15)))
  ;; under a folded heading, the next heading's own number: the hidden lines counted
  (let ((f (run-keys "M-x o r g - m o d e RET M-x d i s p l a y - l i n e - n u m b e r s - m o d e RET TAB"
                     :text (format nil "* A~%a~%a~%* B~%b~%") :width 40)))
    (true (uiop:string-prefix-p "4 " (string-left-trim " " (tuatara.frontend.fake:fake-screen-line f 1)))))
  ;; the calendar has none, whatever every buffer is set to
  (let ((tuatara-user:*line-numbers-default* :absolute)
        (calendar (tuatara.defaults::calendar-buffer)))
    (unwind-protect (false (tuatara.defaults::line-numbers-mode calendar))
      (tuatara.buffer:kill-buffer calendar))))

(define-test (toggles mode-setting)
  ;; one lookup of a setting by major mode, for lists of pairs, lists of names and tables
  (let ((org (make-instance 'tuatara.defaults::org-mode)))
    (true (tuatara.modes:names-mode-p 'tuatara.defaults::org-mode 'tuatara.defaults::org-mode))
    (true (tuatara.modes:names-mode-p (intern "ORG" :tuatara-user) 'tuatara.defaults::org-mode))
    (true (tuatara.modes:names-mode-p "org-mode" 'tuatara.defaults::org-mode))
    (false (tuatara.modes:names-mode-p "or" 'tuatara.defaults::org-mode))
    (false (tuatara.modes:names-mode-p nil 'tuatara.defaults::org-mode))
    (false (tuatara.modes:names-mode-p t 'tuatara.defaults::org-mode))
    (is equal '(5 t) (multiple-value-list (tuatara.modes:mode-setting org '((prog-mode . 3) (text-mode . 5)))))
    (is equal '(nil t) (multiple-value-list (tuatara.modes:mode-setting org '((text-mode . 5) ("org-mode")  (org-mode . nil)))))
    (is equal '(t t) (multiple-value-list (tuatara.modes:mode-setting org '(prog-mode text))))
    (is equal '(nil nil) (multiple-value-list (tuatara.modes:mode-setting org '(prog-mode))))
    (is equal '(nil nil) (multiple-value-list (tuatara.modes:mode-setting nil '(text-mode))))
    (let ((table (make-hash-table :test 'eq)))
      (setf (gethash 'tuatara.defaults::text-mode table) 8)
      (is equal '(8 t) (multiple-value-list (tuatara.modes:mode-setting org table))))
    ;; set-colouring's rules are read by it too: the nearest mode, else every buffer
    (let ((tuatara.defaults::*colouring-rules* '((tuatara.defaults::text-mode :sparse 2) (t :none nil))))
      (is equal '(:sparse 2) (multiple-value-list (tuatara.defaults::colouring-rule org)))
      (is equal '(:none nil) (multiple-value-list (tuatara.defaults::colouring-rule (make-instance 'tuatara.defaults::prog-mode))))
      (is equal '(:none nil) (multiple-value-list (tuatara.defaults::colouring-rule nil))))))

(define-test (toggles counts-on-hand-made-switches)
  ;; a count sets a switch as every toggle takes one: positive on, zero off, whatever it was
  (multiple-value-bind (f editor) (run-keys "C-u 1 M-x r e a d - o n l y - m o d e RET C-u 1 M-x r e a d - o n l y - m o d e RET" :text "x")
    (declare (ignore f))
    (true (tuatara.buffer:buffer-local (session-buffer editor) :read-only)))
  (multiple-value-bind (f editor) (run-keys "C-u 0 M-x r e a d - o n l y - m o d e RET" :text "x")
    (declare (ignore f))
    (false (tuatara.buffer:buffer-local (session-buffer editor) :read-only))))

(defun covered-by-face (spans face start end)
  "True when SPANS, a row's (START END FACE) spans, give FACE, alone or among
others, to every character from START below END."
  (loop for i from start below end
        always (some (lambda (span)
                       (destructuring-bind (from to faces) span
                         (and (<= from i) (< i to) (if (listp faces) (member face faces) (eq face faces)))))
                     spans)))

(define-test (toggles invisible-characters-shown)
  ;; laid out with INVISIBLE, a character that draws nothing is its code, in its own face, in that many columns
  (let* ((text (format nil "a~cb~c~cc" (code-char #xFEFF) (code-char #x200B) (code-char #xE0041)))
         (on (aref (tuatara.display:layout (tuatara.text:string->rope text) 0 40 1 :invisible t) 0))
         (off (aref (tuatara.display:layout (tuatara.text:string->rope text) 0 40 1) 0)))
    (is string= "a<FEFF>b<200B><E0041>c" (coerce (tuatara.display:row-string on) 'string))
    (is = (length "a<FEFF>b<200B><E0041>c") (tuatara.display:row-columns on))
    (true (every (lambda (face) (eq face :invisible-character)) (subseq (tuatara.display:row-faces on) 1 7)))
    (is eq nil (aref (tuatara.display:row-faces on) 7))
    (is string= text (coerce (tuatara.display:row-string off) 'string))
    (is = 4 (tuatara.display:row-columns off))
    ;; the columns counted along a line agree with the rows
    (is = 7 (tuatara.display::line-screen-column (tuatara.text:string->rope text) 0 4 :invisible t))
    (is = 1 (tuatara.display::line-screen-column (tuatara.text:string->rope text) 0 4)))
  ;; combining marks, wide characters and ordinary text are not codes
  (is = 0 (tuatara.display:char-columns (code-char #x301) t))
  (is = 2 (tuatara.display:char-columns (code-char #x4E2D) t))
  (is = 6 (tuatara.display:char-columns (code-char #xAD) t))
  (is = 1 (tuatara.display:char-columns #\a t))
  ;; in a buffer that shows them: on screen as the code, the cursor on its first column, x deletes it
  (let ((text (format nil "a~cb" (code-char #xFEFF))))
    (multiple-value-bind (f editor) (run-keys "SPC t i l" :text text :width 40 :height 6)
      (true (tuatara.defaults::invisible-characters-p (session-buffer editor)))
      (is string= "a<FEFF>b" (subseq (tuatara.frontend.fake:fake-screen-line f 0) 0 8))
      (true (covered-by-face (aref (tuatara.frontend.fake:fake-spans f) 0) :invisible-character 1 7))
      (is equal '(0 . 1) (tuatara.frontend.fake:fake-cursor f))
      (true (search "Invisible" (mode-line-row f))))
    (is equal '(0 . 7) (tuatara.frontend.fake:fake-cursor (run-keys "SPC t i l l" :text text :width 40 :height 6)))
    (multiple-value-bind (f editor) (run-keys "SPC t i l d" :text text :width 40 :height 6)
      (is string= "ab" (session-text editor))
      (is string= "ab" (string-right-trim " " (tuatara.frontend.fake:fake-screen-line f 0))))
    ;; off, as before: nothing shown for it
    (multiple-value-bind (f editor) (run-keys "l" :text text :width 40 :height 6)
      (false (tuatara.defaults::invisible-characters-p (session-buffer editor)))
      (false (search "<FEFF>" (tuatara.frontend.fake:fake-screen-line f 0)))
      (false (search "Invisible" (mode-line-row f))))))

(define-test (toggles invisible-characters-when-safe)
  ;; a file opened safely shows them unless it chose otherwise, or the setting says not to
  (with-test-directory (dir)
    (let ((file (namestring (merge-pathnames "evidence.txt" dir))))
      (alexandria:write-string-into-file (format nil "~cname~cx~%" (code-char #xFEFF) (code-char #x200D)) file :external-format :utf-8)
      (multiple-value-bind (f editor) (run-keys (format nil "C-u C-x C-f C-a C-k ~a RET" (key-string file)) :width 60 :height 6)
        (true (tuatara.io:safe-visit-p (session-buffer editor)))
        (true (tuatara.defaults::invisible-characters-p (session-buffer editor)))
        (true (tuatara.frontend.fake:fake-frame-showing f "<FEFF>name<200D>x")))
      (multiple-value-bind (f editor) (run-keys (format nil "C-u C-x C-f C-a C-k ~a RET SPC t i" (key-string file)) :width 60 :height 6)
        (declare (ignore f))
        (false (tuatara.defaults::invisible-characters-p (session-buffer editor))))
      (let ((tuatara-user:*invisible-characters-when-safe* nil))
        (multiple-value-bind (f editor) (run-keys (format nil "C-u C-x C-f C-a C-k ~a RET" (key-string file)) :width 60 :height 6)
          (declare (ignore f))
          (false (tuatara.defaults::invisible-characters-p (session-buffer editor)))))
      ;; another opened as any file (one opened safely opens so again): off
      (multiple-value-bind (f editor) (run-keys "" :file (put-file (merge-pathnames "plain.txt" dir) (format nil "~cx~%" (code-char #xFEFF)))
                                                   :width 60 :height 6)
        (declare (ignore f))
        (false (tuatara.io:safe-visit-p (session-buffer editor)))
        (false (tuatara.defaults::invisible-characters-p (session-buffer editor)))))))

(define-test (toggles defined-by-a-package)
  ;; a package's display toggle: its setting for every buffer and its list by major
  ;; mode are settings of the package, in TUATARA-USER, and it switches as every toggle does
  (let ((package (make-package "TOGGLETEST" :use '("CL" "TUATARA"))))
    (unwind-protect
         (let ((*package* package))
           (eval (read-from-string "(define-display-toggle toggletest-ruler \"Ruler\"
                                      \"Show a ruler in the current buffer, or stop.\"
                                      :command toggletest-ruler-mode :lookup toggletest-ruler-p
                                      :global *toggletest-ruler* :what \"show a ruler\" :example \"'((text-mode . t))\")"))
           (dolist (name '("*TOGGLETEST-RULER*" "*TOGGLETEST-RULER-MODES*"))
             (multiple-value-bind (symbol status) (find-symbol name :tuatara-user)
               (is eq :external status name)
               (true (boundp symbol))
               (is equal "TOGGLETEST" (get symbol 'tuatara.defaults::package-setting))
               (is eq symbol (find-symbol name package))))
           (multiple-value-bind (f editor value) (toggle-session (find-symbol "TOGGLETEST-RULER" package) (typed "toggletest-ruler-mode"))
             (declare (ignore editor))
             (true value)
             (true (tuatara.frontend.fake:fake-frame-showing f "Ruler on"))))
      (setf tuatara.defaults::*display-toggles*
            (remove "TOGGLETEST-RULER" tuatara.defaults::*display-toggles*
                    :key (lambda (toggle) (symbol-name (tuatara.defaults::display-toggle-name toggle))) :test #'string=))
      (dolist (name '("TOGGLETEST-RULER-MODE" "GLOBAL-TOGGLETEST-RULER-MODE"))
        (let ((command (find-symbol name package)))
          (when command (remhash command tuatara.command::*commands*))))
      (dolist (name '("*TOGGLETEST-RULER*" "*TOGGLETEST-RULER-MODES*"))
        (let ((symbol (find-symbol name :tuatara-user)))
          (when symbol
            (unexport symbol :tuatara-user)
            (unintern symbol :tuatara-user))))
      (delete-package package))))
