;;;; test/config.lisp --- The init file and bind-key.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.tests)

(define-test config
  :description "The init file is loaded in TUATARA-USER; bind-key binds in the global, NORMAL and leader maps; an error in it becomes a message.")

(define-modal-test (config bind-key)
  (let ((file (merge-pathnames (format nil "tuatara-init-~a.lisp" (random 1000000)) (uiop:temporary-directory)))
        ;; what the leader's ; is before "; ;" makes it a prefix, put back after
        (semicolon (tuatara.command:lookup-key tuatara.defaults::*tuatara-leader-keymap*
                                               (tuatara.frontend:parse-key-sequence ";"))))
    (unwind-protect
         (progn
           (with-open-file (o file :direction :output)
             (format o "(bind-key \"C-c t\" 'end-of-buffer)~%(bind-key \"SPC .\" 'beginning-of-buffer :map :normal)~%(bind-key \"; ;\" 'end-of-line :map :leader)~%"))
           (let ((tuatara.defaults:*config-file* file))
             (multiple-value-bind (f editor) (run-keys "C-c t" :text "abc")
               (declare (ignore f))
               (is = 3 (session-point editor)))
             (multiple-value-bind (f editor) (run-keys "g e SPC ." :text "abc")
               (declare (ignore f))
               (is = 0 (session-point editor)))
             (multiple-value-bind (f editor) (run-keys "SPC ; ;" :text "abc")
               (declare (ignore f))
               (is = 3 (session-point editor))))
           ;; faces can be changed from the init file
           (with-open-file (o file :direction :output :if-exists :supersede)
             (format o "(set-face :comment :fg :cyan :italic nil)~%"))
           (let ((tuatara.defaults:*config-file* file))
             (run-keys "" :text "")
             (is eq :cyan (tuatara.display:face-attribute :comment :fg))
             (false (tuatara.display:face-attribute :comment :italic)))
           ;; set-face is your own setting, kept across themes: clear it for the other tests
           (setf tuatara.display:*user-face-settings* '())
           (tuatara.display:apply-theme nil)
           ;; special buffers' maps are reachable by name
           (with-open-file (o file :direction :output :if-exists :supersede)
             (format o "(bind-key \"z\" 'quit-window :map :buffers)~%"))
           (let ((tuatara.defaults:*config-file* file))
             (multiple-value-bind (f editor) (fresh-list-session "C-x C-b z")
               (declare (ignore f))
               (is string= "*scratch*" (current-name editor))))
           ;; a broken init file reports and the editor still runs
           (with-open-file (o file :direction :output :if-exists :supersede)
             (format o "(bind-key \"C-c u\" 'no-such-command)~%"))
           (let ((tuatara.defaults:*config-file* file))
             (multiple-value-bind (f editor) (run-keys "" :text "abc")
               (declare (ignore editor))
               (true (tuatara.frontend.fake:fake-frame-showing f "Error in" "no-such-command")))))
      (ignore-errors (delete-file file))
      ;; leave no binding behind for the other tests
      (tuatara-user:unbind-key "C-c t")
      (tuatara-user:unbind-key "SPC ." :map :normal)
      (tuatara-user:unbind-key "; ;" :map :leader)
      (tuatara.command:define-key tuatara.defaults::*tuatara-leader-keymap* ";" semicolon)
      (tuatara-user:unbind-key "z" :map :buffers))))

(define-modal-test (config key-labels)
  ;; the leader's prefixes are labelled; bind-key labels a key, either way it
  ;; takes a label, and a prefix; the key hints show the labels
  (flet ((hints (keys) (run-keys keys :text "abc" :width 140 :height 20)))
    ;; the standard prefixes are named
    (let ((f (let ((tuatara-user:*key-hint-rows* 30)) (run-keys "C-x" :text "abc" :width 140 :height 40))))
      (true (tuatara.frontend.fake:fake-frame-showing f "r +rectangles"))
      (true (tuatara.frontend.fake:fake-frame-showing f "m +bookmarks"))
      (true (tuatara.frontend.fake:fake-frame-showing f "4 +other window"))
      (true (tuatara.frontend.fake:fake-frame-showing f "n +notes")))
    (let ((f (hints "SPC")))
      (true (tuatara.frontend.fake:fake-frame-showing f "t +toggle"))
      (true (tuatara.frontend.fake:fake-frame-showing f "w +window"))
      (true (tuatara.frontend.fake:fake-frame-showing f "i +input")))
    (let ((file (merge-pathnames (format nil "tuatara-init-~a.lisp" (random 1000000)) (uiop:temporary-directory))))
      (unwind-protect
           (progn
             (with-open-file (o file :direction :output)
               (format o "(bind-key \"t z\" 'end-of-buffer :map :leader :label \"to the end\")~%~
                          (bind-key \"t y\" '(\"to the top\" . beginning-of-buffer) :map :leader)~%~
                          (bind-key \"t\" nil :map :leader :label \"toggles\")~%"))
             (let ((tuatara.defaults:*config-file* file))
               (true (tuatara.frontend.fake:fake-frame-showing (hints "SPC") "t +toggles"))
               (let ((f (hints "SPC t")))
                 (true (tuatara.frontend.fake:fake-frame-showing f "z to the end"))
                 (true (tuatara.frontend.fake:fake-frame-showing f "y to the top"))
                 (true (tuatara.frontend.fake:fake-frame-showing f "h current-line-mode")))
               ;; a labelled key still runs its command
               (is = 3 (session-point (nth-value 1 (run-keys "SPC t z" :text "abc"))))))
        (ignore-errors (delete-file file))
        (tuatara-user:unbind-key "t z" :map :leader)
        (tuatara-user:unbind-key "t y" :map :leader)
        (tuatara-user:bind-key "t" nil :map :leader :label "toggle")))
    ;; unbound, a key keeps no label
    (let ((leader tuatara.defaults::*tuatara-leader-keymap*))
      (false (tuatara.command:key-label (tuatara.command:lookup-key leader (tuatara.frontend:parse-key-sequence "t"))
                                        (first (tuatara.frontend:parse-key-sequence "z")))))
    ;; a key bound to a command is no prefix to label under
    (fail (tuatara-user:bind-key "b x" nil :map :leader :label "no"))))

(define-modal-test (config keys-for-modes)
  (flet ((point-after (keys) (session-point (nth-value 1 (run-keys keys :text "abc"))))
         (hints (keys) (run-keys keys :text "abc" :width 140 :height 20)))
    (unwind-protect
         (progn
           ;; SPC o z everywhere, another in Org and Markdown, SPC o y in text modes and those made from them
           (tuatara-user:bind-key "o z" 'end-of-buffer :map :leader)
           (tuatara-user:bind-key "o z" 'beginning-of-buffer :map :leader :mode '(org-mode markdown-mode))
           (tuatara-user:bind-key "o y" 'end-of-line :map :leader :mode 'text-mode)
           (tuatara-user:bind-key "o" nil :map :leader :label "own")
           (is = 3 (point-after "SPC o z"))
           (is = 0 (point-after "M-x o r g - m o d e RET g e SPC o z"))
           (is = 0 (point-after "M-x m a r k d o w n - m o d e RET g e SPC o z"))
           (is = 3 (point-after "M-x o r g - m o d e RET SPC o y"))
           (is = 0 (point-after "SPC o y"))
           ;; the key hints: the mode's keys over the others, the prefix's label as everywhere
           (true (tuatara.frontend.fake:fake-frame-showing (hints "M-x o r g - m o d e RET SPC") "o +own"))
           (let ((f (hints "M-x o r g - m o d e RET SPC o")))
             (true (tuatara.frontend.fake:fake-frame-showing f "z beginning-of-buffer"))
             (true (tuatara.frontend.fake:fake-frame-showing f "y end-of-line")))
           (true (tuatara.frontend.fake:fake-frame-showing (hints "SPC o") "z end-of-buffer"))
           ;; a mode's keys in a state; a map that is no state's nor the leader's refused with :mode
           (tuatara-user:bind-key "C-c z" 'beginning-of-buffer :map :normal :mode 'org-mode)
           (is = 0 (point-after "M-x o r g - m o d e RET g e C-c z"))
           (fail (tuatara-user:bind-key "x" 'end-of-line :map :buffers :mode 'org-mode))
           (fail (tuatara-user:bind-key "x" 'end-of-line :map :leader :mode 'no-such-mode))
           ;; unbound for the mode, the key everywhere is the key there too
           (tuatara-user:unbind-key "o z" :map :leader :mode '(org-mode markdown-mode))
           (is = 3 (point-after "M-x o r g - m o d e RET SPC o z")))
      (tuatara-user:unbind-key "o" :map :leader)
      (tuatara-user:unbind-key "o" :map :leader :mode '(org-mode markdown-mode text-mode))
      (tuatara-user:unbind-key "C-c z" :map :normal :mode 'org-mode))))

(define-modal-test (config leader-on-another-key)
  (unwind-protect
       (let ((tuatara-user:*leader-key* ","))
         (tuatara-user:bind-key "o z" 'end-of-buffer :map :leader)
         (tuatara-user:bind-key "o y" 'end-of-line :map :leader :mode 'org-mode)
         ;; the leader's keys after , in NORMAL state and in a mode's own; SPC no longer opens them
         (is = 3 (session-point (nth-value 1 (run-keys ", o z" :text "abc"))))
         (is = 3 (session-point (nth-value 1 (run-keys "M-x o r g - m o d e RET , o y" :text "abc"))))
         (true (tuatara.frontend.fake:fake-frame-showing (run-keys "," :text "abc" :width 140 :height 20) "o +"))
         (false (eq tuatara.defaults::*tuatara-leader-keymap*
                    (tuatara.command:lookup-key tuatara.defaults::*tuatara-normal-keymap*
                                                (tuatara.frontend:parse-key-sequence "SPC")))))
    (tuatara-user:unbind-key "o" :map :leader)
    (tuatara-user:unbind-key "o" :map :leader :mode 'org-mode)
    (tuatara.defaults::put-leader-on "SPC")
    (true (eq tuatara.defaults::*tuatara-leader-keymap*
              (tuatara.command:lookup-key tuatara.defaults::*tuatara-normal-keymap*
                                          (tuatara.frontend:parse-key-sequence "SPC"))))))

(define-test (config leader-on-c-x-spc)
  ;; C-x SPC opens the leader's keys, with their key hints, in NORMAL and INSERT state, with modal
  ;; editing off and in a list; moving the leader to another key leaves it there
  (flet ((point-after (keys &key (modal t))
           (let ((tuatara.defaults::*modal-editing* modal))
             (session-point (nth-value 1 (run-keys keys :text "abc")))))
         (hints-after (keys &key (modal t))
           (let ((tuatara.defaults::*modal-editing* modal))
             (run-keys keys :text "abc" :width 140 :height 20))))
    (unwind-protect
         (progn
           (tuatara-user:bind-key "o z" 'end-of-buffer :map :leader)
           (tuatara-user:bind-key "o" nil :map :leader :label "own")
           (is = 3 (point-after "C-x SPC o z" :modal nil))
           (is = 3 (point-after "i C-x SPC o z"))
           (is = 3 (point-after "C-x SPC o z"))
           (is = 3 (point-after "SPC o z"))
           (true (tuatara.frontend.fake:fake-frame-showing (hints-after "C-x SPC" :modal nil) "o +own"))
           (true (tuatara.frontend.fake:fake-frame-showing (hints-after "i C-x SPC") "o +own"))
           (true (tuatara.frontend.fake:fake-frame-showing (hints-after "C-x SPC o" :modal nil) "z end-of-buffer"))
           ;; in the list of buffers, as M-> there
           (dolist (modal '(t nil))
             (is = (point-after "C-x C-b M->" :modal modal) (point-after "C-x C-b C-x SPC o z" :modal modal))
             (isnt = (point-after "C-x C-b" :modal modal) (point-after "C-x C-b C-x SPC o z" :modal modal))
             (true (tuatara.frontend.fake:fake-frame-showing (hints-after "C-x C-b C-x SPC" :modal modal) "o +own")))
           ;; the leader moved to another key
           (let ((tuatara-user:*leader-key* ","))
             (tuatara.defaults::put-leader-on ",")
             (is = 3 (point-after "C-x SPC o z" :modal nil))
             (is = 3 (point-after "i C-x SPC o z"))
             (is = 3 (point-after ", o z"))))
      (tuatara.defaults::put-leader-on "SPC")
      (tuatara-user:unbind-key "o z" :map :leader)
      (tuatara-user:unbind-key "o" :map :leader))))

(define-test (config mode-leader-on-c-x-spc)
  ;; a leader key bound for one mode is reached after C-x SPC in that mode alone, in INSERT state
  ;; and with modal editing off, with its key hint
  (flet ((point-after (keys &key (modal t))
           (let ((tuatara.defaults::*modal-editing* modal))
             (session-point (nth-value 1 (run-keys keys :text "abc"))))))
    (unwind-protect
         (progn
           (tuatara-user:bind-key "o y" 'end-of-line :map :leader :mode 'org-mode)
           (is = 3 (point-after "M-x o r g - m o d e RET C-x SPC o y" :modal nil))
           (is = 3 (point-after "M-x o r g - m o d e RET i C-x SPC o y"))
           (is = 3 (point-after "M-x o r g - m o d e RET C-x SPC o y"))
           (isnt = 3 (point-after "C-x SPC o y" :modal nil))
           (true (tuatara.frontend.fake:fake-frame-showing
                  (let ((tuatara.defaults::*modal-editing* nil))
                    (run-keys "M-x o r g - m o d e RET C-x SPC o" :text "abc" :width 140 :height 20))
                  "y end-of-line")))
      (tuatara-user:unbind-key "o y" :map :leader :mode 'org-mode))))

(define-test (config options-reach-the-editor)
  ;; the options documented for the init file are, read in
  ;; TUATARA-USER, the very variables the editor uses
  (loop for (name used) in '(("*MESSAGES-KEPT*" tuatara.command:*messages-kept*)
                             ("*SORT-IGNORES-CASE*" tuatara.defaults::*sort-ignores-case*)
                             ("*RELOAD-CHANGED-FILES*" tuatara.defaults::*reload-changed-files*)
                             ("*LINE-NUMBERS-DEFAULT*" tuatara.defaults::*line-numbers-default*)
                             ("*INDENT-NEW-LINES*" tuatara.defaults::*indent-new-lines*)
                             ("*STARTUP-BUFFER*" tuatara-user:*startup-buffer*)
                             ("*DARK-THEME*" tuatara-user:*dark-theme*)
                             ("*LIGHT-THEME*" tuatara-user:*light-theme*))
        do (multiple-value-bind (symbol status) (find-symbol name :tuatara-user)
             (is eq used symbol)
             (is eq :external status)
             (true (boundp symbol))
             (true (documentation symbol 'variable)))))

(define-test (config starter-names-every-setting)
  ;; every setting the starter init file sets is one the init file reaches, an
  ;; external of TUATARA-USER, and every such setting is named there (a package's
  ;; own, in the example of one, aside)
  (let ((starter tuatara.defaults::*starter-config*) (set '()))
    (loop with start = 0
          for at = (search "(setf *" starter :start2 start)
          while at
          do (let ((end (position-if (lambda (c) (member c '(#\Space #\Newline #\)))) starter :start (+ at 6))))
               (pushnew (subseq starter (+ at 6) end) set :test #'string=)
               (setf start end)))
    (dolist (name set)
      (unless (search "*example-" name)
        (multiple-value-bind (symbol status) (find-symbol (string-upcase name) :tuatara-user)
          (declare (ignore symbol))
          (is eq :external status "~a, set in the starter init file" name))))
    (do-external-symbols (symbol :tuatara-user)
      (when (and (boundp symbol) (eq :special (sb-int:info :variable :kind symbol)))
        (true (search (string-downcase (symbol-name symbol)) starter) "~(~a~) in the starter init file" symbol)))))

(define-modal-test (config starting-past-a-slip)
  ;; an init file setting the pairs as one dotted pair: the editor starts, says the value is
  ;; refused, and pairs as standard
  (let* ((file (merge-pathnames (format nil "init-~36r.lisp" (random (expt 36 8))) (tuatara.io:cache-directory)))
         (pairs tuatara-user:*pair-delimiters*)
         (mode tuatara-user:*auto-close-mode*))
    (unwind-protect
         (progn
           (with-open-file (out (ensure-directories-exist file) :direction :output :if-exists :supersede)
             (write-string "(setf *pair-delimiters* '(#\\( . #\\)))
(setf *auto-close-mode* t)
" out))
           (let ((tuatara.defaults:*config-file* file))
             (multiple-value-bind (f editor) (run-keys "i ( a C-g" :text "" :width 140)
               (declare (ignore f))
               (true (search "*pair-delimiters* is " (cdr tuatara.defaults::*init-file-outcome*))
                     "~s" tuatara.defaults::*init-file-outcome*)
               (is string= "(a)" (session-text editor)))))
      (setf tuatara-user:*pair-delimiters* pairs
            tuatara-user:*auto-close-mode* mode)
      (ignore-errors (delete-file file))))
  ;; a step of starting that fails is said, and the rest carries on
  (let ((said nil))
    (false (tuatara.defaults::after-init-step "Binding something" (lambda () (error "it broke"))))
    (setf said (tuatara.defaults::after-init-step "Something else" (lambda () :done)))
    (is eq :done said)))

;; the commented settings of an init file, each read as it would be without its ;;
(defun commented-forms (text)
  "The forms of TEXT, an init file, that are commented out with ;; in front of
each of their lines, read; a form over several lines read whole. A form
commented inside another one commented (;;   ;; (setf ...) in a load-package's
:config) is read too, by itself, its lines being those of its own depth."
  (let ((forms '()) (pending (make-hash-table :test 'equal)))
    (dolist (line (uiop:split-string text :separator '(#\Newline)) (nreverse forms))
      (let* ((run (or (position-if-not (lambda (c) (member c '(#\Space #\;))) line) (length line)))
             (last (position #\; line :end run :from-end t)))
        (when (and last (plusp last) (char= #\; (char line (1- last))))
          (let* ((prefix (subseq line 0 (1+ last)))
                 (content (subseq line (1+ last)))
                 (so-far (gethash prefix pending)))
            (when (or so-far (uiop:string-prefix-p "(" (string-left-trim " " content)))
              (let* ((text (if so-far (format nil "~a~%~a" so-far content) content))
                     (form (handler-case (read-from-string text) (end-of-file () :more))))
                (if (eq form :more)
                    (setf (gethash prefix pending) text)
                    (progn (push form forms) (remhash prefix pending)))))))))))

(define-test (config starter-init-file)
  (let* ((home (ensure-directories-exist
                (merge-pathnames (format nil "config-~36r/" (random (expt 36 8))) (tuatara.io:cache-directory))))
         (file (merge-pathnames "tuatara/init.lisp" home))
         (old (uiop:getenv "XDG_CONFIG_HOME"))
         ;; loading the starter sets tabs for some modes: put back afterwards
         (tabs tuatara-user:*indent-with-tabs-modes*))
    (unwind-protect
         (progn
           ;; the first start without an init file writes one there and says so
           (sb-posix:setenv "XDG_CONFIG_HOME" (namestring home) 1)
           (let ((tuatara.defaults:*config-file* nil))
             (multiple-value-bind (f editor) (run-keys "" :text "abc" :width 140)
               (declare (ignore editor))
               (true (tuatara.frontend.fake:fake-frame-showing f "Wrote a starter init file" "tuatara/init.lisp"))))
           ;; and reads it at once: Makefiles have their tabs from the first start
           (true (tuatara.modes:mode-setting 'tuatara.defaults::makefile-mode tuatara-user:*indent-with-tabs-modes*))
           (let ((text (uiop:read-file-string file)))
             (true (search "(setf *reload-changed-files* t)" text))
             (true (search "(bind-key \"C-c t\" 'toggle-light-dark-theme)" text))
             (true (search "(set-colouring 'rust :sparse)" text))
             ;; every line of it is a comment but the tabs some languages' own
             ;; tools require, an entry a line, and the load-package forms holding
             ;; the settings, by what they are for
             (true (every (lambda (line)
                            (let ((line (string-left-trim " " line)))
                              (or (zerop (length line)) (char= (char line 0) #\;)
                                  (string= line ":config") (string= line ")")
                                  (cl-ppcre:scan "^\\(load-package \"[a-z-]+\"$" line)
                                  (string= line "(setf *indent-with-tabs-modes*")
                                  (cl-ppcre:scan "^'?\\((;;.*|[a-z-]+ \\. t\\)\\)*)$" line))))
                          (uiop:split-string text :separator '(#\Newline))))
             (true (search "(makefile . t)" text))
             (true (search "(go . t)" text)))
           ;; each setting in it works once its ;; is taken away, from every line of it
           (let ((*package* (find-package :tuatara-user)))
             (dolist (form (commented-forms (uiop:read-file-string file)))
               (when (and (consp form) (eq (first form) 'setf))
                 (true (every #'boundp (loop for (place) on (rest form) by #'cddr collect place))))))
           ;; never written again, never written over
           (with-open-file (o file :direction :output :if-exists :supersede) (write-string ";; mine" o))
           (false (tuatara.defaults::write-starter-config file))
           (let ((tuatara.defaults:*config-file* nil))
             (run-keys "" :text "abc"))
           (is string= ";; mine" (uiop:read-file-string file)))
      (if old (sb-posix:setenv "XDG_CONFIG_HOME" old 1) (sb-posix:unsetenv "XDG_CONFIG_HOME"))
      (setf tuatara-user:*indent-with-tabs-modes* tabs)
      (uiop:delete-directory-tree home :validate #'tests-own-path-p))))

(define-test (config bind-key-a-mode-either-way)
  ;; :map 'csv-mode and :map :global :mode 'csv are the same keys: the mode's own
  (is equal (tuatara.defaults::mode-keymaps 'csv-mode nil) (tuatara.defaults::mode-keymaps :global 'csv))
  (is eq (tuatara.defaults::mode-keymap 'tuatara.defaults::csv-mode)
      (first (tuatara.defaults::mode-keymaps 'csv-mode nil))))

(define-test (config init-file-read-past-an-error)
  ;; a form that fails is said with its line, and the forms after it are read all the same
  (let ((file (merge-pathnames (format nil "init-~36r.lisp" (random (expt 36 8))) (uiop:temporary-directory))))
    (unwind-protect
         (progn
           (alexandria:write-string-into-file
            (format nil "(setf (symbol-value 'tuatara-user::*init-test-mark*) :before)~%;; a comment~%~%(load-package \"calendr\")~%(setf (symbol-value 'tuatara-user::*init-test-mark*) :after)~%")
            file :if-exists :supersede)
           (let ((tuatara.defaults:*config-file* file))
             (multiple-value-bind (f editor) (run-keys "" :text "abc" :width 160)
               (declare (ignore editor))
               (true (tuatara.frontend.fake:fake-frame-showing f "line 4" "did you mean calendar?"))))
           (is eq :after (symbol-value 'tuatara-user::*init-test-mark*))
           (true (consp tuatara.defaults::*init-file-outcome*)))
      (ignore-errors (delete-file file))
      (makunbound 'tuatara-user::*init-test-mark*))))

(define-test (config error-line-past-a-block-comment)
  ;; the line said is the failing form's, not that of a block comment before it
  (is = 10 (tuatara.defaults::next-form-start "  #| a |# (x)" 0))
  (is = 23 (tuatara.defaults::next-form-start (format nil "#| a #| nested |# b |#~%(x)") 0))
  (is = 4 (tuatara.defaults::next-form-start "#| a" 0))
  (is = 0 (tuatara.defaults::next-form-start "#'car" 0))
  (let ((said nil) (*package* (find-package :tuatara-user)))
    (tuatara.defaults::map-text-forms (format nil "(a)~%#| one~%two |#~%(b c d)~%")
                                      (lambda (form line) (push (cons form line) said)))
    (is equal '((tuatara-user::a . 1) (tuatara-user::b . 4))
        (mapcar (lambda (each) (cons (first (car each)) (cdr each))) (reverse said)))))

(define-test (config bind-key-maps-said-exist)
  ;; every map the help of bind-key names is one it takes
  (let* ((help (documentation 'tuatara-user:bind-key 'function))
         (named (remove-duplicates
                 (mapcar (lambda (word) (intern (string-upcase (subseq word 1)) :keyword))
                         (cl-ppcre:all-matches-as-strings ":[a-z][a-z-]*" help))))
         (maps (list* :minibuffer (mapcar #'car (tuatara.defaults::keymap-registry)))))
    (true (member :pattern named))
    (is equal '() (set-difference named (list* :map :mode :label maps)))))

(define-test (config data-directory-not-kept-in-the-program)
  ;; a saved program forgets where data/ was when it was built
  (true (member 'tuatara.defaults::forget-data-directory sb-ext:*save-hooks*))
  (let ((tuatara.defaults::*data-directory* tuatara.defaults::*data-directory*))
    (tuatara.defaults::forget-data-directory)
    (is eq nil tuatara.defaults::*data-directory*)))

(define-test (config subdirectories-named-by-config-path)
  ;; every subdirectory of the configuration directory is named by CONFIG-PATH, and so
  ;; follows XDG_CONFIG_HOME alike
  (true (< 100 (length (source-files "lisp"))))
  (dolist (file (source-files "lisp"))
    (unless (equal (pathname-name file) "config")
      (false (search "xdg-config-home" (uiop:read-file-string file)) "~a" (enough-namestring file))))
  (let ((config (tuatara.defaults::config-path ""))
        (tuatara-user:*capture-template-directories* nil)
        (tuatara-user:*logos-directory* nil)
        (tuatara.defaults::*note-templates-directory* nil))
    (dolist (directory (list (first (tuatara.defaults::capture-template-directories))
                          (first (tuatara.defaults::logos-directories))
                          (tuatara.defaults::docx-templates-directory)
                          (tuatara.defaults::latex-templates-directory)
                          (tuatara.defaults::note-templates-directory)))
      (true (uiop:string-prefix-p config directory) "~a" directory))))

(define-test (config budgets-are-not-settings)
  ;; limits the editor decides for itself are neither the init file's settings nor in the starter
  ;; init file; those that became internal are still there, documented, where they are used
  (let ((starter tuatara.defaults::*starter-config*))
    (loop for (name home) in '(("*LSP-EVENTS-KEPT*" :tuatara.lsp)
                               ("*LSP-EVENT-LENGTH*" :tuatara.lsp)
                               ("*LSP-MOST-CHANGES*" :tuatara.lsp)
                               ("*LSP-WATCHED-MOST*" :tuatara.lsp)
                               ("*LSP-LOCATION-FILES-TOTAL*" :tuatara.lsp)
                               ("*LSP-SYNC-DELAY*" nil)
                               ("*SYNTAX-HELPER-MEMORY*" :tuatara.syntax)
                               ("*SYNTAX-HELPER-WAIT*" :tuatara.syntax)
                               ("*SYNTAX-HELPERS-MOST*" :tuatara.syntax)
                               ("*TYPING-PAUSE-OCTETS*" nil)
                               ("*SEARCH-FILTER-SCREEN-SECONDS*" :tuatara.defaults)
                               ("*VC-GUTTER-MAX-BYTES*" :tuatara.defaults)
                               ("*CONFLICT-MARKERS-MAX-BYTES*" :tuatara.defaults)
                               ("*DOCX-READ-MOST-STAND-INS*" :tuatara.defaults)
                               ("*MAGICK-LARGEST-SIDE*" :tuatara.defaults)
                               ("*LATEX-IMPORT-PICTURE-LIMIT*" :tuatara.defaults)
                               ("*ORG-MACRO-LIMIT*" :tuatara.defaults)
                               ("*GLOBAL-MARK-RING-MAX*" nil)
                               ("*PICTURE-SECONDS*" nil)
                               ("*NO-ONE-TO-ASK-SAVE-SECONDS*" :tuatara.defaults)
                               ("*ARCHIVE-LEAST-FILES-LEFT*" :tuatara.defaults))
          do (isnt eq :external (nth-value 1 (find-symbol name :tuatara-user)) "~a a setting" name)
             (false (search (string-downcase name) starter) "~(~a~) in the starter init file" name)
             (when home
               (let ((symbol (find-symbol name home)))
                 (true (and symbol (boundp symbol) (documentation symbol 'variable)) "~a in ~a" name home))))))

(define-test (config definitions-named-in-the-init-file)
  ;; help names the init file as where its commands and recorded keys are defined, and its
  ;; link goes to the line there
  (let* ((file (merge-pathnames (format nil "init-~36r.lisp" (random (expt 36 8))) (tuatara.io:cache-directory)))
         (names '(tuatara-user::test-init-command tuatara-user::test-init-keys)))
    (unwind-protect
         (progn
           (with-open-file (out (ensure-directories-exist file) :direction :output :if-exists :supersede)
             (write-string "(define-command test-init-command () \"Say hello.\" (message \"hello\"))
(define-recorded-keys 'test-init-keys \"C-e\")
" out))
           (let ((tuatara.defaults:*config-file* file))
             (multiple-value-bind (f editor) (run-keys "C-h f t e s t - i n i t - k e y s RET" :text "abc" :width 100)
               (declare (ignore editor))
               (true (tuatara.frontend.fake:fake-frame-showing f (format nil "Defined in ~a" (file-namestring file))))
               (false (tuatara.frontend.fake:fake-frame-showing f "recorded-keys.lisp"))))
           (dolist (name names)
             (let ((words (tuatara.defaults::pieces-text (tuatara.defaults::source-location-pieces name :function))))
               (true (search (format nil "Defined in ~a" (file-namestring file)) words) "~(~a~): ~a" name words)))
           (let ((definitions (tuatara.defaults::lisp-definitions 'tuatara-user::test-init-keys)))
             (is = 1 (length definitions))
             (multiple-value-bind (buffer at) (funcall (tuatara.defaults::definition-locate (first definitions)))
               (unwind-protect
                    (let ((line "(define-recorded-keys 'test-init-keys"))
                      (is string= line (subseq (tuatara.buffer:buffer-string buffer) at (+ at (length line)))))
                 (when buffer (tuatara.buffer:kill-buffer buffer))))))
      (dolist (name names)
        (tuatara.command:forget-command name)
        (remhash name tuatara.defaults::*init-file-definitions*)
        (fmakunbound name))
      (ignore-errors (delete-file file)))))

(define-test (config files-the-init-file-loads-keep-their-own-source)
  ;; a file compiled and loaded, or loaded as it is, while the init file is read is where its
  ;; own definitions are, for help and its link, never the init file
  (let* ((dir (ensure-directories-exist
               (merge-pathnames (format nil "init-~36r/" (random (expt 36 8))) (tuatara.io:cache-directory))))
         (init (merge-pathnames "init.lisp" dir))
         (compiled (merge-pathnames "compiled-helper.lisp" dir))
         (plain (merge-pathnames "plain-helper.lisp" dir))
         (names '(tuatara-user::test-compiled-helper tuatara-user::test-plain-helper)))
    (unwind-protect
         (progn
           (with-open-file (out compiled :direction :output :if-exists :supersede)
             (write-string "(in-package :tuatara-user)
(defun test-compiled-helper () \"One.\" 1)
" out))
           (with-open-file (out plain :direction :output :if-exists :supersede)
             (write-string "(in-package :tuatara-user)
(defun test-plain-helper () \"Two.\" 2)
" out))
           (with-open-file (out init :direction :output :if-exists :supersede)
             (format out "(cl:load (cl:compile-file ~s :output-file ~s))~%(cl:load ~s)~%"
                     (namestring compiled)
                     (namestring (merge-pathnames "compiled-helper.fasl" dir))
                     (namestring plain)))
           (let ((tuatara.defaults:*config-file* init))
             (run-keys "" :text "abc"))
           (loop for name in names
                 for file in (list compiled plain)
                 do (let ((words (tuatara.defaults::pieces-text (tuatara.defaults::source-location-pieces name :function))))
                      (true (search (format nil "Defined in ~a" (file-namestring file)) words) "~(~a~): ~a" name words))
                    (is equal (list (file-namestring file))
                        (mapcar (lambda (d) (file-namestring (tuatara.defaults::definition-group d)))
                                (tuatara.defaults::lisp-definitions name)))))
      (dolist (name names) (fmakunbound name))
      (uiop:delete-directory-tree dir :validate (lambda (p) (search "/init-" (namestring p)))))))

(define-test (config starter-config-failure-reported)
  ;; a directory that may not be looked into: not written, and why; with debug mode on, in its log too
  (with-test-directory (dir)
    (let* ((directory (ensure-directories-exist (merge-pathnames "closed/" dir)))
           (file (namestring (merge-pathnames "tuatara/init.lisp" directory))))
      (drain-debug-reports)
      (sb-posix:chmod (namestring directory) 0)
      (unwind-protect
           (progn
             (multiple-value-bind (written why) (tuatara.defaults::write-starter-config file)
               (false written)
               (true (stringp why)))
             (tuatara.runtime:set-debug t)
             (false (tuatara.defaults::write-starter-config file))
             (let ((texts (loop for report = (sb-concurrency:dequeue tuatara.runtime:*debug-reports*)
                                while report collect (princ-to-string (fourth report)))))
               (true (find-if (lambda (text) (search (format nil "writing the starter init file ~a failed" file) text))
                              texts)
                     "~s" texts)))
        (sb-posix:chmod (namestring directory) #o700)
        (drain-debug-reports)))))

(define-test (config renamed-setting-carried-over)
  ;; a setting set by its name before a rename still takes effect, and the name to write instead is said
  (with-test-directory (dir)
    (let ((file (merge-pathnames "init.lisp" dir))
          (tuatara-user:*directory-tree-depth-limit* 200))
      (alexandria:write-string-into-file (format nil "(setf *folder-tree-depth-limit* 50)~%") file)
      (unwind-protect
           (let ((tuatara.defaults:*config-file* file)
                 (compiler (make-string-output-stream)))
             (with-messages-said (said)
               (let ((*error-output* compiler))
                 (run-keys "" :text ""))
               (is = 50 tuatara-user:*directory-tree-depth-limit*)
               (true (search "*folder-tree-depth-limit*, now called *directory-tree-depth-limit*" (said)) "~a" (said))
               ;; and without a warning of an undefined variable
               (let ((warned (get-output-stream-string compiler)))
                 (false (search "undefined variable" warned) "~a" warned))))
        (let ((old (find-symbol "*FOLDER-TREE-DEPTH-LIMIT*" :tuatara-user)))
          (when old (makunbound old)))))))

(define-test (config removed-setting-said)
  ;; a setting taken away, set or added to by the init file: said to be gone, with what decides instead,
  ;; and the rest of the file read; the directories of *safe-directories* added to *untrusted-directories*;
  ;; a setting taken away reads as nil
  (with-test-directory (dir)
    (let ((file (merge-pathnames "init.lisp" dir))
          (tuatara-user:*untrusted-directories* '(:downloads))
          (tuatara-user:*directory-tree-depth-limit* 200))
      (alexandria:write-string-into-file
       (format nil "(setf *export-trusted-directories* '(\"~~/latex/\"))~%(push \"~~/Evidence/\" *safe-directories*)~%~
                    (setf *trusted-projects* '(\"~~/src/\"))~%(push \"~~/code/\" *lsp-trusted-projects*)~%~
                    (setf *directory-tree-depth-limit* 60)~%~
                    (when *export-trusted-folders* (setf *directory-tree-depth-limit* 1))~%")
       file)
      (let ((tuatara.defaults:*config-file* file)
            (compiler (make-string-output-stream)))
        (with-messages-said (said)
          (let ((*error-output* compiler))
            (run-keys "" :text ""))
          (true (search "*export-trusted-directories*, which is gone: *untrusted-directories* decides" (said))
                "~a" (said))
          (true (search "*safe-directories*, which is gone: its directories are added to *untrusted-directories*"
                        (said))
                "~a" (said))
          (is equal '(:downloads "~/Evidence/") tuatara-user:*untrusted-directories*)
          (true (search "*trusted-projects*, which is gone: *untrusted-directories* decides" (said)) "~a" (said))
          (true (search "*lsp-trusted-projects*, which is gone: *untrusted-directories* decides" (said)) "~a" (said))
          (is = 60 tuatara-user:*directory-tree-depth-limit*)
          (let ((warned (get-output-stream-string compiler)))
            (false (search "undefined variable" warned) "~a" warned))
          (false (boundp (find-symbol "*SAFE-DIRECTORIES*" :tuatara-user))))))))

(define-test (config renamed-setting-added-to)
  ;; a list setting added to by its name before a rename gets what was added, and the name to
  ;; write instead is said; a setting the init file left alone keeps its value
  (with-test-directory (dir)
    (let ((file (merge-pathnames "init.lisp" dir))
          (evidence tuatara-user:*evidence-directories*)
          (tuatara-user:*directory-tree-depth-limit* 200))
      (alexandria:write-string-into-file (format nil "(push \"/nonexistent/tuatara-test-case/\" *evidence-folders*)~%")
                                         file)
      (unwind-protect
           (let ((tuatara.defaults:*config-file* file))
             (setf tuatara-user:*evidence-directories* (list "/nonexistent/tuatara-test-kept/"))
             (with-messages-said (said)
               (run-keys "" :text "")
               (is equal '("/nonexistent/tuatara-test-case/" "/nonexistent/tuatara-test-kept/")
                   tuatara-user:*evidence-directories*)
               (is = 200 tuatara-user:*directory-tree-depth-limit*)
               (true (search "*evidence-folders*, now called *evidence-directories*" (said)) "~a" (said))
               (false (search "*folder-tree-depth-limit*" (said)) "~a" (said))
               (false (boundp (find-symbol "*EVIDENCE-FOLDERS*" :tuatara-user)))))
        (setf tuatara-user:*evidence-directories* evidence)))))

(define-test (config renamed-place-settings)
  ;; an init file setting the old names of the places settings gives their values to the new ones,
  ;; and is told the new names
  (with-test-directory (dir)
    (let ((file (merge-pathnames "init.lisp" dir))
          (tuatara-user:*remember-places* t)
          (tuatara-user:*remembered-places-limit* 400))
      (alexandria:write-string-into-file
       (format nil "(setf *save-place-mode* nil)~%(setf *save-place-limit* 12)~%") file)
      (let ((tuatara.defaults:*config-file* file))
        (with-messages-said (said)
          (run-keys "" :text "")
          (false tuatara-user:*remember-places*)
          (is = 12 tuatara-user:*remembered-places-limit*)
          (true (search "*save-place-mode*, now called *remember-places*" (said)) "~a" (said))
          (true (search "*save-place-limit*, now called *remembered-places-limit*" (said)) "~a" (said))
          (false (boundp (find-symbol "*SAVE-PLACE-MODE*" :tuatara-user))))))))

(define-test (config emacs-named-settings-renamed)
  ;; each setting named the Emacs way has its new name, a setting an init file reaches with its
  ;; documentation; an init file setting the old name gives the value to the new one and is told
  (let ((renamed '(("*DEBUG-ON-ERROR*" "*BACKTRACE-ON-ERROR*") ("*MESSAGE-LOG-MAX*" "*MESSAGES-KEPT*")
                   ("*KILL-RING-MAX*" "*KILL-RING-LENGTH*") ("*KILL-RING-MAX-SIZE*" "*KILL-RING-OCTETS*")
                   ("*MARK-RING-MAX*" "*MARK-RING-LENGTH*") ("*SHOW-PAREN-MODE*" "*SHOW-MATCHING-BRACKET*")
                   ("*BLINK-MATCHING-PAREN*" "*SAY-MATCHING-BRACKET*") ("*ELECTRIC-PAIR-MODE*" "*AUTO-CLOSE-MODE*")
                   ("*ELECTRIC-PAIR-TYPING*" "*AUTO-CLOSE-DELIMITERS*")
                   ("*ELECTRIC-PAIR-SAFE-EXCLUSIONS*" "*AUTO-CLOSE-SAFE-EXCLUSIONS*")
                   ("*INDENT-TABS-MODE*" "*INDENT-WITH-TABS*") ("*TAB-ALWAYS-INDENT*" "*TAB-KEY-ACTION*")
                   ("*GLOBAL-HL-LINE-MODE*" "*GLOBAL-CURRENT-LINE-MODE*") ("*HL-LINE-MODES*" "*CURRENT-LINE-MODES*")
                   ("*GLOBAL-CURSOR-COLUMN-MODE*" "*GLOBAL-CURRENT-COLUMN-MODE*")
                   ("*CURSOR-COLUMN-MODES*" "*CURRENT-COLUMN-MODES*") ("*SORT-FOLD-CASE*" "*SORT-IGNORES-CASE*")
                   ("*DEFAULT-DIRECTORY*" "*NO-FILE-DIRECTORY*"))))
    (loop for (old new) in renamed
          do (multiple-value-bind (symbol status) (find-symbol new :tuatara-user)
               (is eq :external status "~a" new)
               (true (tuatara.runtime:setting-values symbol) "~a" new)
               (true (documentation symbol 'variable) "~a" new))
             (is eq (find-symbol new :tuatara-user)
                 (cdr (assoc old tuatara.defaults::*renamed-settings* :test #'string=)) "~a" old))
    (with-test-directory (dir)
      (let ((file (merge-pathnames "init.lisp" dir))
            (tuatara-user:*sort-ignores-case* nil)
            (tuatara-user:*kill-ring-length* 120))
        (alexandria:write-string-into-file (format nil "(setf *sort-fold-case* t)~%(setf *kill-ring-max* 7)~%") file)
        (let ((tuatara.defaults:*config-file* file))
          (with-messages-said (said)
            (run-keys "" :text "")
            (true tuatara-user:*sort-ignores-case*)
            (is = 7 tuatara-user:*kill-ring-length*)
            (true (search "*sort-fold-case*, now called *sort-ignores-case*" (said)) "~a" (said))
            (true (search "*kill-ring-max*, now called *kill-ring-length*" (said)) "~a" (said))))))))

(define-test (config renamed-setting-not-taken)
  ;; a value given by a setting's old name that the setting does not take is put back, and said
  (with-test-directory (dir)
    (let ((file (merge-pathnames "init.lisp" dir))
          (tuatara-user:*evidence-directories* (list "/cases/")))
      (alexandria:write-string-into-file (format nil "(setf *evidence-folders* 7)~%") file)
      (let ((tuatara.defaults:*config-file* file))
        (with-messages-said (said)
          (run-keys "" :text "")
          (is equal '("/cases/") tuatara-user:*evidence-directories*)
          (true (search "Your init file: *evidence-directories* is 7, but it takes a list" (said)) "~a" (said)))))))

(define-test (config settings-refused-where-given)
  ;; a setting given a value it does not take is refused, with what it takes, and keeps its
  ;; value: by the editor's own setting, M-:, a form of the init file (whose other settings
  ;; stand), a package's :config
  (let ((padding tuatara-user:*gui-padding*) (width tuatara-user:*fill-column*) (said '())
        (message (find-symbol "MESSAGE" :tuatara.packages))
        (file (merge-pathnames (format nil "init-~36r.lisp" (random (expt 36 8))) (uiop:temporary-directory))))
    (flet ((refused (function)
             (handler-case (progn (funcall function) nil)
               (tuatara.runtime:refusal (e) (princ-to-string e)))))
      (unwind-protect
           (progn
             (setf tuatara-user:*gui-padding* 8)
             (dolist (value '(x 4.5 -30))
               (let ((words (refused (lambda () (tuatara.runtime:set-setting 'tuatara-user:*gui-padding* value)))))
                 (true (search "*gui-padding* is " words) "~s" words)
                 (true (search "but it takes a whole number, 0 or more, or a list" words) "~s" words)
                 (true (search "it keeps 8" words) "~s" words))
               (is eql 8 tuatara-user:*gui-padding*))
             (is equal '(4 12) (tuatara.runtime:set-setting 'tuatara-user:*gui-padding* '(4 12)))
             (setf tuatara-user:*gui-padding* padding)
             (multiple-value-bind (f editor)
                 (run-keys (format nil "M-: ~a RET" (key-string "(setf *fill-column* 66 *gui-padding* 'x)")) :width 160)
               (declare (ignore editor))
               (true (tuatara.frontend.fake:fake-frame-showing
                      f "*gui-padding* is x, but it takes a whole number, 0 or more, or a list")))
             (is equal padding tuatara-user:*gui-padding*)
             (is = 66 tuatara-user:*fill-column*)
             (alexandria:write-string-into-file
              (format nil "(setf *gui-padding* \"wide\" *fill-column* 64)~%") file :if-exists :supersede)
             (let ((tuatara.defaults:*config-file* file))
               (multiple-value-bind (f editor) (run-keys "" :text "abc" :width 160)
                 (declare (ignore editor))
                 (true (tuatara.frontend.fake:fake-frame-showing-across-rows f "line 1: *gui-padding* is \"wide\""))))
             (is equal padding tuatara-user:*gui-padding*)
             (is = 64 tuatara-user:*fill-column*)
             (sb-int:encapsulate message 'said (lambda (f control &rest arguments)
                                                 (push (apply #'format nil control arguments) said)
                                                 (apply f control arguments)))
             (tuatara.packages::run-package-forms "kiwi" :config '((setf tuatara-user:*fill-column* "narrow")))
             (is = 64 tuatara-user:*fill-column*)
             (true (find-if (lambda (line) (and (search "kiwi's :config: " line)
                                                (search "*fill-column* is \"narrow\"" line)))
                            said)
                   "~s" said))
        (sb-int:unencapsulate message 'said)
        (ignore-errors (delete-file file))
        (setf tuatara-user:*gui-padding* padding tuatara-user:*fill-column* width)))))

(define-test (config setting-defined-after-a-value-it-does-not-take)
  ;; an init file may set a package's setting before the package defines it: a value the setting
  ;; does not take is then replaced by the standard value, and said
  (let ((name (intern (format nil "*KIWI-LATE-~36r*" (random (expt 36 8))) :tuatara-user)) (said '())
        (message (find-symbol "MESSAGE" :tuatara.packages)))
    (setf (symbol-value name) :x)
    (sb-int:encapsulate message 'said (lambda (f control &rest arguments)
                                        (push (apply #'format nil control arguments) said)
                                        (apply f control arguments)))
    (unwind-protect
         (progn
           (eval `(tuatara.runtime:define-setting ,name 3 "How many kiwi." :values (integer 0 *)))
           (is = 3 (symbol-value name))
           (true (find-if (lambda (line)
                            (search "is :x, but it takes a whole number, 0 or more: it is given its standard value, 3"
                                    line))
                          said)
                 "~s" said)
           (fail (tuatara.runtime:set-setting name -1))
           (is = 3 (symbol-value name)))
      (sb-int:unencapsulate message 'said)
      ;; the setting made for the test is let go
      (let ((settings tuatara.runtime::*settings*))
        (when (and (plusp (length settings)) (eq name (aref settings (1- (length settings)))))
          (vector-pop settings))
        (makunbound name)
        (unintern name :tuatara-user)))))

(define-test (config times-in-words-read-in-linear-time)
  ;; a time written with long runs of spaces is read at once, whether it is one or not
  (let ((spaces (make-string 40000 :initial-element #\Space)))
    (dolist (words (list (concatenate 'string "1" spaces "x" spaces "y") (concatenate 'string "1h" spaces "2")
                         (concatenate 'string spaces "1h" spaces "30m" spaces)))
      (let ((start (get-internal-real-time)))
        (tuatara.runtime::seconds-in-words words)
        (true (< (- (get-internal-real-time) start) (* 1/2 internal-time-units-per-second)))))
    (is eql 5400 (tuatara.runtime::seconds-in-words (concatenate 'string spaces "1h" spaces "30m" spaces)))))

(define-test (config sizes-and-times-in-words)
  ;; a setting holding octets or a time takes words for them, turned into its number where a value is
  ;; looked at: the editor's own setting, a form of the init file, a value given before the setting was
  ;; defined; words that give none it takes are refused and the setting keeps its value
  (let ((g (expt 1024 3)) (m (expt 1024 2)))
    (loop for (words octets) in `(("20G" ,(* 20 g)) ("512M" ,(* 512 m)) ("1.5G" ,(* 3/2 g)) ("20 GB" ,(* 20 g))
                                  ("20GiB" ,(* 20 g)) (" 2t " ,(* 2 1024 g)) ("1.5k" 1536) ("300" 300) ("300B" 300)
                                  (".5K" 512) ("20Q" nil) ("G" nil) ("" nil) ("1.2.3G" nil) ("-5G" nil) ("5 G B" nil))
          do (is eql octets (tuatara.runtime::octets-in-words words) "~s" words)))
  (loop for (words seconds) in '(("90s" 90) ("500ms" 1/2) ("1.5h" 5400) ("30m" 1800) ("2 h" 7200) ("14d" 1209600)
                                 ("10" 10) ("90 minutes" nil) ("h" nil) ("1:30" nil)
                                 ("1h30m" 5400) ("1h 3m 10s" 3790) ("1h30" nil) ("h30m" nil) ("1h30x" nil))
        do (is eql seconds (tuatara.runtime::seconds-in-words words) "~s" words))
  (loop for (description words value) in '(((or (member nil) (octets 1 *)) "1.5G" 1610612736)
                                           ((octets 1 *) "0" "0")
                                           ((minutes 0 *) "90s" 1.5)
                                           ((minutes 0 *) "5" 5)
                                           ((minutes 0 * :whole) "90s" "90s")
                                           ((minutes 0 * :whole) "2h" 120)
                                           ((seconds 0 *) "500ms" 0.5)
                                           ((seconds 1 * :whole) "1.5h" 5400)
                                           ((integer 0 *) "20G" "20G")
                                           ((octets 1 *) "8E" "8E")
                                           ((seconds 0 *) "40000d" "40000d"))
        do (is equalp value (tuatara.runtime::converted-setting-value description words) "~s ~s" description words))
  (let ((limit tuatara-user:*sandbox-memory-copy-limit*) (seconds tuatara-user:*sandbox-seconds*))
    (unwind-protect
         (progn
           (is = (* 512 1024 1024) (tuatara.runtime:set-setting 'tuatara-user:*sandbox-memory-copy-limit* "512M"))
           (is = (* 512 1024 1024) tuatara-user:*sandbox-memory-copy-limit*)
           (let ((words (handler-case (progn (tuatara.runtime:set-setting 'tuatara-user:*sandbox-memory-copy-limit*
                                                                           "lots")
                                             "")
                          (tuatara.runtime:refusal (e) (princ-to-string e)))))
             (true (search "*sandbox-memory-copy-limit* is \"lots\", but it takes" words) "~a" words)
             (true (search "a size in words such as 512M, 1.5G or 20 GB" words) "~a" words))
           (is = (* 512 1024 1024) tuatara-user:*sandbox-memory-copy-limit*)
           (let ((snapshot (tuatara.runtime:settings-snapshot)))
             (setf tuatara-user:*sandbox-memory-copy-limit* "1.5G" tuatara-user:*sandbox-seconds* "2m")
             (is equal '() (tuatara.runtime:put-back-misfit-settings snapshot))
             (is = (* 3/2 1024 1024 1024) tuatara-user:*sandbox-memory-copy-limit*)
             (is = 120 tuatara-user:*sandbox-seconds*))
           (let ((snapshot (tuatara.runtime:settings-snapshot)))
             (setf tuatara-user:*sandbox-memory-copy-limit* "20Q")
             (let ((said (tuatara.runtime:put-back-misfit-settings snapshot)))
               (true (search "*sandbox-memory-copy-limit* is \"20Q\"" (first said)) "~s" said))
             (is = (* 3/2 1024 1024 1024) tuatara-user:*sandbox-memory-copy-limit*))
           ;; sizes and times past any sensible end are refused
           (true (tuatara.runtime:setting-value-fault 'tuatara-user:*sandbox-memory* (expt 2 63)))
           (false (tuatara.runtime:setting-value-fault 'tuatara-user:*sandbox-memory* (expt 2 40)))
           (true (tuatara.runtime:setting-value-fault 'tuatara-user:*sandbox-seconds* (* 200 365 24 3600)))
           ;; a string left in a setting, as a LET gives it, is a fault the health check names
           (true (tuatara.runtime:setting-value-fault 'tuatara-user:*sandbox-seconds* "90s")))
      (setf tuatara-user:*sandbox-memory-copy-limit* limit tuatara-user:*sandbox-seconds* seconds)))
  (let ((name (intern (format nil "*KIWI-SIZE-~36r*" (random (expt 36 8))) :tuatara-user)))
    (setf (symbol-value name) "2G")
    (unwind-protect
         (progn
           (eval `(tuatara.runtime:define-setting ,name 1024 "How large a kiwi may be." :values (octets 1 *)))
           (is = (* 2 1024 1024 1024) (symbol-value name)))
      (let ((settings tuatara.runtime::*settings*))
        (when (and (plusp (length settings)) (eq name (aref settings (1- (length settings)))))
          (vector-pop settings))
        (makunbound name)
        (unintern name :tuatara-user)))))

(define-test (config setting-checks-make-nothing)
  ;; a value is checked, and the settings looked at after a form, without garbage, so that the
  ;; checks after each form of the init file and each evaluation cost no more than looking
  (let* ((snapshot (tuatara.runtime:settings-snapshot))
         (check (lambda (n)
                  (dotimes (i n)
                    (tuatara.runtime:setting-value-fault 'tuatara-user:*gui-padding* '(4 12 4 12))
                    (tuatara.runtime:setting-value-fault 'tuatara-user:*fill-column* 72)
                    (tuatara.runtime:put-back-misfit-settings snapshot)))))
    (flet ((consed (n)
             (loop repeat 3 minimize (nth-value 2 (tuatara.bench::measuring (funcall check n))))))
      (let ((made (- (consed 1100) (consed 100))))
        (true (< made 1000) "~d octets for 1000 checks" made)))))

(define-test (config settings-of-a-name-take-text-too)
  ;; a theme named as text is found as one named by a symbol is, and so taken; a setting with no
  ;; value is refused in words, not with an error of Lisp's
  (dolist (setting '(tuatara-user:*dark-theme* tuatara-user:*light-theme*))
    (false (tuatara.runtime:setting-value-fault setting "terminal")))
  (let ((name (gensym "UNBOUND")))
    (setf (get name 'tuatara.runtime::setting-values) 'boolean)
    (true (search "it has no value" (handler-case (progn (tuatara.runtime:set-setting name :maybe) "")
                                      (tuatara.runtime:refusal (e) (princ-to-string e)))))))
