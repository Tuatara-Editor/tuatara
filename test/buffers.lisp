;;;; test/buffers.lisp --- The buffer list and the SPC leader.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.tests)

(define-test buffers
  :description "SPC , selects a buffer with completion; C-x C-b lists buffers as a support buffer with mark, kill, save, visit and quit.")

(defun current-name (editor) "Name of the editor's current buffer." (tuatara.buffer:buffer-name (tuatara.display:window-buffer (tuatara.command:editor-window editor))))

(defun fresh-list-session (keys &key (width 40) (height 8))
  "Run KEYS with no *Buffers* buffer left over from an earlier session, so the
list starts on its first line, on a screen WIDTH by HEIGHT."
  (let ((l (tuatara.buffer:find-buffer "*Buffers*"))) (when l (tuatara.buffer:kill-buffer l)))
  (run-keys keys :text "abc" :width width :height height))

(define-test (buffers leader-selector)
  ;; SPC , in NORMAL opens the buffer selector; C-j takes the typed name
  (multiple-value-bind (f editor) (run-keys "SPC , n o t e s C-j" :text "")
    (declare (ignore f))
    (is string= "notes" (current-name editor))
    (tuatara.buffer:kill-buffer (tuatara.buffer:find-buffer "notes")))
  ;; in INSERT, SPC is still a space
  (multiple-value-bind (f editor) (run-keys "i a SPC b C-g" :text "")
    (declare (ignore f))
    (is string= "a b" (session-text editor)))
  ;; the key hints show the leader entries
  (let ((f (tuatara.frontend.fake:make-fake-frontend :width 60 :height 10)))
    (let ((scratch (tuatara.buffer:find-buffer "*scratch*"))) (when scratch (tuatara.buffer:kill-buffer scratch)))
    (tuatara.buffer:make-buffer "*scratch*" :text "")
    (tuatara.frontend.fake:fake-queue-keys f "SPC")
    (tuatara.command:run-editor f :setup #'tuatara.defaults:install-defaults :buffer (tuatara.buffer:find-buffer "*scratch*"))
    (true (tuatara.frontend.fake:fake-frame-showing f ", switch-to-buffer"))))

(define-test (buffers leader-list)
  ;; SPC I opens the list of buffers
  (multiple-value-bind (f editor) (run-keys "SPC I" :text "" :width 100 :height 20)
    (declare (ignore f))
    (is string= "*Buffers*" (current-name editor))))

(define-test (buffers free-keys)
  ;; as in every list, moves by word and selections but by lines are free keys, V selecting lines
  (dolist (key tuatara.defaults::*list-free-keys*)
    (multiple-value-bind (f editor) (fresh-list-session (format nil "C-x C-b ~a" key))
      (declare (ignore editor))
      (true (tuatara.frontend.fake:fake-frame-showing f (format nil "~a is undefined" key)))))
  (is eq :undefined (tuatara.command:lookup-key (tuatara.modes:mode-keymap 'tuatara.defaults::jobs-mode)
                                                (tuatara.frontend:parse-key-sequence "w")))
  (false (tuatara.command:lookup-key (tuatara.modes:mode-keymap 'tuatara.defaults::buffers-mode)
                                     (tuatara.frontend:parse-key-sequence "V"))))

(define-test (buffers list)
  ;; start from a known set of buffers: earlier tests may have left some behind
  (dolist (b (copy-list tuatara.buffer:*buffers*))
    (unless (string= (tuatara.buffer:buffer-name b) "*scratch*") (tuatara.buffer:kill-buffer b)))
  (let ((extra (tuatara.buffer:make-buffer "extra" :text "12345")))
    (unwind-protect
         (progn
           ;; the list shows the other buffers with sizes; d marks, x kills; q returns
           (multiple-value-bind (f editor) (fresh-list-session "C-x C-b")
             (is string= "*Buffers*" (current-name editor))
             (true (search "TUATARA[S]" (tuatara.frontend.fake:fake-screen-line f (- (tuatara.command:editor-height editor) 2))))
             (let ((text (session-text editor)))
               (true (search "extra" text)) (true (search "*scratch*" text)) (true (search "5 Fundamental" text))))
           ;; find a buffer's line by searching for its name, then l (or RET) opens it
           (multiple-value-bind (f editor) (fresh-list-session "C-x C-b C-s e x t r a RET l")
             (declare (ignore f))
             (is string= "extra" (current-name editor)))
           (multiple-value-bind (f editor) (fresh-list-session "C-x C-b C-s \\ * s c r RET RET")
             (declare (ignore f))
             (is string= "*scratch*" (current-name editor)))
           ;; o shows it in another window, the list staying in its own
           (multiple-value-bind (f editor) (fresh-list-session "C-x C-b C-s e x t r a RET o" :width 100 :height 16)
             (declare (ignore f))
             (is string= "extra" (current-name editor))
             (true (find "*Buffers*" (tuatara.command:editor-windows editor)
                         :key (lambda (w) (tuatara.buffer:buffer-name (tuatara.display:window-buffer w))) :test #'string=)))
           ;; opened again, the list starts on its first buffer as well
           (multiple-value-bind (f editor) (fresh-list-session "C-x C-b j j q C-x b e x t r a RET C-x C-b" :width 100 :height 16)
             (declare (ignore f))
             (let ((list (tuatara.buffer:find-buffer "*Buffers*")))
               (is = (1+ tuatara.defaults::*buffers-title-lines*)
                   (tuatara.buffer:buffer-line-number list (session-point editor)))
               (true (tuatara.defaults::list-member-at list))))
           ;; h and q go back
           (multiple-value-bind (f editor) (fresh-list-session "C-x C-b h")
             (declare (ignore f))
             (is string= "*scratch*" (current-name editor)))
           (multiple-value-bind (f editor) (fresh-list-session "C-x C-b q")
             (declare (ignore f))
             (is string= "*scratch*" (current-name editor)))
           ;; d marks the line's buffer, x kills it
           (multiple-value-bind (f editor) (fresh-list-session "C-x C-b C-s e x t r a RET d x")
             (declare (ignore f))
             (true (search "Killed 1 buffer" (or (tuatara.command:editor-message editor) "")))
             (false (tuatara.buffer:find-buffer "extra")))
           ;; m marks, as ibuffer's does, and D kills the marked buffers after asking;
           ;; m and d take each other's place, u and U unmark, x leaves the marked
           (let ((one (tuatara.buffer:make-buffer "one" :text "1"))
                 (two (tuatara.buffer:make-buffer "two" :text "2")))
             (flet ((line (editor name)
                      (find-if (lambda (l) (search name l)) (uiop:split-string (session-text editor) :separator '(#\Newline)))))
               (multiple-value-bind (f editor) (fresh-list-session "C-x C-b C-s t w o RET m m" :width 100 :height 16)
                 (declare (ignore f))
                 (true (uiop:string-prefix-p ">" (line editor "one")))
                 (true (uiop:string-prefix-p ">" (line editor "two"))))
               (multiple-value-bind (f editor) (fresh-list-session "C-x C-b C-s o n e RET m k d" :width 100 :height 16)
                 (declare (ignore f))
                 (true (uiop:string-prefix-p "D" (line editor "one"))))
               (multiple-value-bind (f editor) (fresh-list-session "C-x C-b C-s t w o RET m m U" :width 100 :height 16)
                 (declare (ignore f))
                 (false (uiop:string-prefix-p ">" (line editor "one")))
                 (false (uiop:string-prefix-p ">" (line editor "two"))))
               (multiple-value-bind (f editor) (fresh-list-session "C-x C-b C-s o n e RET m x" :width 100 :height 16)
                 (declare (ignore f))
                 (true (tuatara.buffer:find-buffer "one"))
                 (true (search "No buffers marked for deletion" (or (tuatara.command:editor-message editor) ""))))
               (multiple-value-bind (f editor) (fresh-list-session "C-x C-b C-s t w o RET m m D y RET" :width 100 :height 16)
                 (true (tuatara.frontend.fake:fake-frame-showing f "Kill the 2 buffers?"))
                 (true (search "Killed 2 buffers" (or (tuatara.command:editor-message editor) "")))
                 (false (tuatara.buffer:find-buffer "one"))
                 (false (tuatara.buffer:find-buffer "two")))
               ;; marked entries a search filter hides are said in the question
               (setf one (tuatara.buffer:make-buffer "one" :text "1")
                     two (tuatara.buffer:make-buffer "two" :text "2"))
               (multiple-value-bind (f editor)
                   (fresh-list-session "C-x C-b C-s t w o RET m m C-x / s t w o RET D" :width 100 :height 16)
                 (declare (ignore editor))
                 (true (tuatara.frontend.fake:fake-frame-showing f "Kill the 2 buffers (1 hidden by the filter)?"))))
             (dolist (b (list one two)) (when (tuatara.buffer:live-buffer-p b) (tuatara.buffer:kill-buffer b)))))
      (when (find extra tuatara.buffer:*buffers*) (tuatara.buffer:kill-buffer extra))
      (let ((l (tuatara.buffer:find-buffer "*Buffers*"))) (when l (tuatara.buffer:kill-buffer l))))))

(define-test (buffers columns-and-colours)
  (dolist (b (copy-list tuatara.buffer:*buffers*))
    (unless (string= (tuatara.buffer:buffer-name b) "*scratch*") (tuatara.buffer:kill-buffer b)))
  (let ((notes (tuatara.buffer:make-buffer "notes" :text "12345"))
        (long (tuatara.buffer:make-buffer "a-name-longer-than-the-column.lisp" :text "(x)")))
    (setf (tuatara.buffer:buffer-local long :read-only) t)
    (unwind-protect
         (flet ((line-of (text name) (find-if (lambda (l) (search name l)) (uiop:split-string text :separator '(#\Newline))))
                (faces-on (f text)
                  ;; the faces the last frame painted over the first cell showing TEXT
                  (loop for row from 0 below (length (tuatara.frontend.fake:fake-screen f))
                        for column = (search text (tuatara.frontend.fake:fake-screen-line f row))
                        when column
                          return (loop for (start end face) in (aref (tuatara.frontend.fake:fake-spans f) row)
                                       when (and (<= start column) (< column end))
                                         append (if (listp face) face (list face))))))
           ;; notes changed and seen last: first, with * in the M column
           (multiple-value-bind (f editor) (fresh-list-session "C-x b n o t e s RET i x C-g C-x C-b" :width 100 :height 16)
             (let* ((text (session-text editor))
                    (lines (uiop:split-string text :separator '(#\Newline))))
               (is string= (format nil " MR  ~18a ~9@a ~16a Filename/Process" "Name" "Size" "Mode") (first lines))
               (is string= (format nil " --  ~18a ~9@a ~16a ----------------" "----" "----" "----") (second lines))
               ;; the buffers in no project under their heading
               (true (uiop:string-prefix-p "▾ No project" (third lines)))
               (true (uiop:string-prefix-p (format nil " *   ~18a ~9@a Fundamental" "notes" "6") (fourth lines)))
               ;; read-only: %; a long name cut to its column
               (true (uiop:string-prefix-p (format nil "  %  ~a ~9@a Fundamental" "a-name-longer-t..." "3")
                                           (line-of text "a-name-longer")))
               ;; the sum at the end
               (true (search " buffers" (car (last lines 2))))
               (true (search "No files, no projects" (car (last lines 2)))))
             ;; the colours: titles as header lines, names in stars as Tuatara's own, read-only as such
             (true (member :header-line (faces-on f "MR ")))
             (true (member :list-special (faces-on f "*scratch*")))
             (true (member :list-read-only (faces-on f "a-name-longer")))
             (false (faces-on f "notes ")))
           ;; a name marked for deletion, and a marked one, each in a face of its own
           (multiple-value-bind (f editor) (fresh-list-session "C-x C-b C-s n o t e s RET d" :width 100 :height 16)
             (true (uiop:string-prefix-p "D" (line-of (session-text editor) "notes")))
             (true (member :list-deletion (faces-on f "notes "))))
           (multiple-value-bind (f editor) (fresh-list-session "C-x C-b C-s n o t e s RET m" :width 100 :height 16)
             (true (uiop:string-prefix-p ">" (line-of (session-text editor) "notes")))
             (true (member :list-marked (faces-on f "notes ")))))
      (dolist (b (list notes long)) (when (tuatara.buffer:live-buffer-p b) (tuatara.buffer:kill-buffer b)))
      (let ((l (tuatara.buffer:find-buffer "*Buffers*"))) (when l (tuatara.buffer:kill-buffer l))))))

(define-test (buffers list-commands-stay-in-the-list)
  ;; the buffer list's own commands do nothing to another buffer: M-x does not offer
  ;; them there, and the text stays as it was
  (multiple-value-bind (f editor) (run-keys "M-x b u f f e r s - s a v e RET C-g" :text "my text")
    (declare (ignore f))
    (is string= "my text" (session-text editor))
    (false (tuatara.buffer:buffer-local (session-buffer editor) :read-only)))
  ;; reached by a key all the same, they say where they work
  (multiple-value-bind (f editor)
      (run-keys (format nil "M-: ~a RET C-c z" (key-string "(bind-key \"C-c z\" 'tuatara.defaults::buffers-save)"))
                :text "my text" :width 120)
    (is string= "my text" (session-text editor))
    (true (tuatara.frontend.fake:fake-frame-showing f "buffers-save works in the buffer list (C-x C-b) only")))
  ;; the same for the list of fonts' and *Help*'s
  (true (gethash 'tuatara.defaults::font-list-use tuatara.defaults::*mode-commands*))
  (true (gethash 'tuatara.defaults::push-button tuatara.defaults::*mode-commands*)))

(define-test (buffers home-by-either-name)
  ;; a home reached through a link (/home to /var/home) is written ~ by either name
  (let* ((base (merge-pathnames (format nil "tuatara-home-~d/" (random 1000000)) (uiop:temporary-directory)))
         (real (merge-pathnames "real/" base))
         (link (merge-pathnames "link" base))
         (old (uiop:getenv "HOME")))
    (ensure-directories-exist real)
    (sb-posix:symlink (namestring real) (namestring link))
    (unwind-protect
         (progn
           (sb-posix:setenv "HOME" (namestring link) 1)
           (is string= "~/notes.org" (tuatara.defaults::abbreviated-path (merge-pathnames "notes.org" real)))
           (is string= "~/notes.org" (tuatara.defaults::abbreviated-path (format nil "~a/notes.org" (namestring link))))
           (is string= "~/" (tuatara.command:abbreviated-path (namestring real)))
           (is string= "/elsewhere/x" (tuatara.command:abbreviated-path "/elsewhere/x")))
      (sb-posix:setenv "HOME" old 1)
      (uiop:delete-directory-tree base :validate t :if-does-not-exist :ignore))))

(define-test (buffers support-keys-read-and-select)
  ;; every support buffer moves, selects and copies as NORMAL state does, its own keys first
  (flet ((in-help (keys)
           (multiple-value-bind (f editor) (run-keys (format nil "C-h f u n d o RET C-x o ~a" keys) :text "" :width 80 :height 24)
             (declare (ignore f))
             editor)))
    (let* ((editor (in-help "g e"))
           (help (session-text editor))
           (end (session-point editor)))
      ;; *Help* is one buffer for every session: each point is read before the next session
      (is string= "*Help*" (current-name editor))
      (is = (length (tuatara.text:string-octets help)) end)
      (is = 0 (session-point (in-help "g e g g")))
      (is = end (session-point (in-help "g e")))
      (is = (position #\Newline help) (session-point (in-help "g l")))
      ;; v w y copies what was selected, from the start of the text
      (let ((kill (first (tuatara.command:editor-kill-ring (in-help "v w y")))))
        (true (stringp kill))
        (true (plusp (length kill)))
        (is eql 0 (search kill help)))
      ;; the mode's own q still quits; keys that would edit do nothing
      (is string= "*scratch*" (current-name (in-help "q")))
      (is string= help (session-text (in-help "i x p D")))))
  ;; in the buffer list, g begins the motions, r refreshes, d still marks
  (let ((l (tuatara.buffer:find-buffer "*Buffers*"))) (when l (tuatara.buffer:kill-buffer l)))
  (multiple-value-bind (f editor) (run-keys "C-x C-b g e" :text "abc" :width 100 :height 16)
    (declare (ignore f))
    (is = (length (tuatara.text:string-octets (session-text editor))) (session-point editor)))
  (let ((l (tuatara.buffer:find-buffer "*Buffers*"))) (when l (tuatara.buffer:kill-buffer l)))
  (multiple-value-bind (f editor) (run-keys "C-x C-b r" :text "abc" :width 100 :height 16)
    (declare (ignore f))
    (is string= "*Buffers*" (current-name editor))
    ;; still on the first buffer, under its section's heading
    (is = 3 (tuatara.buffer:buffer-line-number (session-buffer editor) (session-point editor))))
  (let ((l (tuatara.buffer:find-buffer "*Buffers*"))) (when l (tuatara.buffer:kill-buffer l))))

(define-test (buffers kept-current)
  ;; a buffer killed while *Buffers* is shown leaves the list at once, and a
  ;; buffer killed is never shown again
  (let ((other (or (tuatara.buffer:find-buffer "kept-current-other")
                   (tuatara.buffer:make-buffer "kept-current-other" :text "x"))))
    (let ((l (tuatara.buffer:find-buffer "*Buffers*"))) (when l (tuatara.buffer:kill-buffer l)))
    (multiple-value-bind (f editor)
        (run-keys (format nil "C-x C-b C-x 2 C-x o C-x b ~a RET C-x k" (key-string "kept-current-other"))
                  :text "abc" :width 100 :height 24)
      (declare (ignore f))
      (let ((list (tuatara.buffer:find-buffer "*Buffers*")))
        (true list)
        (false (tuatara.command:live-buffer-p other))
        (false (search "kept-current-other" (tuatara.buffer:buffer-string list)))
        (false (find other (tuatara.buffer:buffer-local list 'tuatara.defaults::list-entries))))
      (let ((tuatara.command:*editor* editor))
        (fail (tuatara.command:show-buffer other))))))

(define-test (buffers last-one-killed)
  ;; with no other buffer left, the one made to show is *lisp-scratch* as at
  ;; startup, in its mode with its first text: *lisp-scratch* killed last too
  (dolist (keep '("*scratch*" "*lisp-scratch*"))
    (multiple-value-bind (f editor) (run-keys (format nil "C-x b ~a RET" (key-string keep)) :text "abc" :width 100 :height 12)
      (declare (ignore f))
      (let ((tuatara.command:*editor* editor))
        (dolist (b (remove (tuatara.buffer:find-buffer keep) (copy-list tuatara.buffer:*buffers*)))
          (tuatara.buffer:kill-buffer b))
        (tuatara.buffer:kill-buffer (tuatara.buffer:find-buffer keep))
        (let ((b (tuatara.display:window-buffer (tuatara.command:editor-window editor))))
          (true (tuatara.command:live-buffer-p b))
          (is string= "*lisp-scratch*" (tuatara.buffer:buffer-name b))
          (true (typep (tuatara.modes:buffer-major-mode b) 'tuatara.defaults::lisp-interaction-mode))
          (true (search ";; This buffer holds unsaved text" (tuatara.buffer:buffer-string b)))
          (is = 1 (count "*lisp-scratch*" tuatara.buffer:*buffers* :key #'tuatara.buffer:buffer-name :test #'string=)))))))

(define-test (buffers reading-again-what-has-no-source)
  ;; r in *Messages*: said plainly, nothing changed
  (multiple-value-bind (f editor) (run-keys "C-h e r" :text "x")
    (declare (ignore editor))
    (true (tuatara.frontend.fake:fake-frame-showing f "*Messages* shows what was said or made then: nothing to read again"))))

(define-test (buffers names-on-one-line)
  ;; a name with a newline in it shows on one line, the newline escaped: d on
  ;; that line marks that buffer, and x kills it, *Messages* left alone
  (dolist (b (copy-list tuatara.buffer:*buffers*))
    (unless (string= (tuatara.buffer:buffer-name b) "*scratch*") (tuatara.buffer:kill-buffer b)))
  (tuatara.command:messages-buffer)
  (let ((odd (tuatara.buffer:make-buffer (format nil "nl~%name.txt") :text "x")))
    (unwind-protect
         (progn
           (true (search "nl^Jname.txt" (session-text (nth-value 1 (fresh-list-session "C-x C-b" :width 100 :height 16)))))
           (fresh-list-session "C-x C-b C-s n l RET d x" :width 100 :height 16)
           (false (tuatara.buffer:live-buffer-p odd))
           (true (tuatara.buffer:find-buffer "*Messages*")))
      (when (tuatara.buffer:live-buffer-p odd) (tuatara.buffer:kill-buffer odd))
      (let ((l (tuatara.buffer:find-buffer "*Buffers*"))) (when l (tuatara.buffer:kill-buffer l)))))
  (is string= "a^Ib^Mc" (tuatara.command:escape-controls (format nil "a~cb~cc" #\Tab #\Return)))
  (is string= "rtl<202E>gnp.exe<2066>\\x85" (tuatara.command:escape-controls (format nil "rtl~cgnp.exe~c~c" (code-char #x202E) (code-char #x2066) (code-char #x85))))
  (is string= "zw<200B>sp<FEFF>" (tuatara.command:escape-controls (format nil "zw~csp~c" (code-char #x200B) (code-char #xFEFF))))
  ;; joiners stay, for the emoji and scripts they build
  (let ((family (format nil "~c~c~c" (code-char #x1F468) (code-char #x200D) (code-char #x1F469))))
    (is eq family (tuatara.command:escape-controls family)))
  (multiple-value-bind (text spans) (tuatara.command:escape-controls (format nil "a~cbc" #\Tab) '((0 1 :x) (2 4 :y)))
    (is string= "a^Ibc" text)
    (is equal '((0 1 :x) (3 5 :y)) spans))
  (let ((plain "plain")) (is eq plain (tuatara.command:escape-controls plain))))

(define-test (buffers marks-take-a-count)
  ;; C-u 2 m marks two buffers, C-u 2 u unmarks them again
  (dolist (b (copy-list tuatara.buffer:*buffers*))
    (unless (string= (tuatara.buffer:buffer-name b) "*scratch*") (tuatara.buffer:kill-buffer b)))
  (let ((made (loop for name in '("one" "two" "three") collect (tuatara.buffer:make-buffer name :text "1"))))
    (unwind-protect
         (flet ((marked (editor)
                  (count-if (lambda (l) (uiop:string-prefix-p ">" l)) (uiop:split-string (session-text editor) :separator '(#\Newline)))))
           (is = 2 (marked (nth-value 1 (fresh-list-session "C-x C-b C-u 2 m" :width 100 :height 16))))
           (is = 0 (marked (nth-value 1 (fresh-list-session "C-x C-b C-u 2 m C-p C-p C-u 2 u" :width 100 :height 16)))))
      (dolist (b made) (when (tuatara.buffer:live-buffer-p b) (tuatara.buffer:kill-buffer b)))
      (let ((l (tuatara.buffer:find-buffer "*Buffers*"))) (when l (tuatara.buffer:kill-buffer l))))))

(define-test (buffers marks-toggled)
  ;; t marks every entry not marked and unmarks those marked, as in rimu; one marked for deletion stays so
  (dolist (b (copy-list tuatara.buffer:*buffers*))
    (unless (string= (tuatara.buffer:buffer-name b) "*scratch*") (tuatara.buffer:kill-buffer b)))
  (let ((made (loop for name in '("one" "two" "three") collect (tuatara.buffer:make-buffer name :text "1"))))
    (unwind-protect
         (flet ((marked (keys sign)
                  (count-if (lambda (l) (uiop:string-prefix-p sign l))
                            (uiop:split-string (session-text (nth-value 1 (fresh-list-session keys :width 100 :height 16)))
                                               :separator '(#\Newline)))))
           (let ((all (marked "C-x C-b t" ">")))
             (true (>= all 4))
             (is = 0 (marked "C-x C-b t t" ">"))
             (is = (- all 2) (marked "C-x C-b m d t" ">"))
             (is = 1 (marked "C-x C-b m d t" "D"))))
      (dolist (b made) (when (tuatara.buffer:live-buffer-p b) (tuatara.buffer:kill-buffer b)))
      (let ((l (tuatara.buffer:find-buffer "*Buffers*"))) (when l (tuatara.buffer:kill-buffer l))))))

(define-test (buffers messages-come-back-in-their-mode)
  ;; *Messages* killed from the list comes back as it was, in special mode
  (multiple-value-bind (f editor) (fresh-list-session "C-x C-b C-s M e s s a g RET d x" :width 100 :height 16)
    (declare (ignore f editor))
    (let ((messages (tuatara.buffer:find-buffer "*Messages*")))
      (true messages)
      (true (typep (tuatara.modes:buffer-major-mode messages) 'tuatara.defaults::special-mode))
      (true (tuatara.buffer:buffer-local messages :read-only))))
  (let ((l (tuatara.buffer:find-buffer "*Buffers*"))) (when l (tuatara.buffer:kill-buffer l))))

(define-test (buffers sections-with-lines-of-their-own)
  ;; a member may take several lines with entries of their own, a heading may
  ;; count other than the members shown and lines may follow them; every such
  ;; line belongs to its section, and a folded section is a heading alone
  (let ((list (tuatara.buffer:make-buffer "sections with lines of their own" :text "")))
    (labels ((draw (buffer)
               (multiple-value-bind (entries pieces)
                   (tuatara.defaults::sectioned-pieces
                    buffer '((:a "one" "two") (:b "three"))
                    (lambda (section) (string-capitalize section))
                    (lambda (member section)
                      (values (list (format nil "  ~a~%" member) (format nil "    more of ~a~%" member))
                              (list (list :own member section) (list :own member section))))
                    :count (lambda (section members) (if (eq section :a) 5 (length members)))
                    :after (lambda (section members)
                             (declare (ignore members))
                             (when (eq section :a) (values (list (format nil "  ...~%")) (list (list :more section))))))
                 (tuatara.defaults::render-pieces buffer pieces :entries entries :redraw #'draw)))
             (section-of-line (line)
               (setf (tuatara.buffer:buffer-point list) (tuatara.buffer:buffer-goto-line list line))
               (tuatara.defaults::list-section-at list)))
      (unwind-protect
           (progn
             (draw list)
             (is string= (format nil "▾ A (5)~%  one~%    more of one~%  two~%    more of two~%  ...~%~%▾ B (1)~%  three~%    more of three~%")
                 (tuatara.buffer:buffer-string list))
             (is eq :a (section-of-line 4))
             (is eq :a (section-of-line 5))
             (is eq nil (section-of-line 6))
             (is eq :b (section-of-line 9))
             (tuatara.defaults::toggle-list-section list :a)
             (is string= (format nil "▸ A (5)~%▾ B (1)~%  three~%    more of three~%") (tuatara.buffer:buffer-string list)))
        (tuatara.buffer:kill-buffer list)))))

(define-test (buffers project-sections)
  ;; the buffers of each project under its heading, those in none last; TAB folds a section,
  ;; D on its heading kills its buffers, C-x p k those of the current buffer's project
  (with-test-directory (dir)
    (let ((one (merge-pathnames "one/" dir)) (two (merge-pathnames "two/" dir)))
      (ensure-directories-exist (merge-pathnames ".project/" one))
      (ensure-directories-exist (merge-pathnames ".git/" two))
      (dolist (file (list (merge-pathnames "a.txt" one) (merge-pathnames "sub/b.txt" one) (merge-pathnames "c.txt" two)))
        (alexandria:write-string-into-file "x" (ensure-directories-exist file)))
      (flet ((open-files ()
               (dolist (file (list (merge-pathnames "c.txt" two) (merge-pathnames "sub/b.txt" one) (merge-pathnames "a.txt" one)))
                 (tuatara.io:visit-file (tuatara.io:native file))))
             (lines (editor) (uiop:split-string (session-text editor) :separator '(#\Newline)))
             (live (name) (tuatara.buffer:find-buffer name)))
        (open-files)
        (multiple-value-bind (f editor) (fresh-list-session "C-x C-b" :width 120 :height 24)
          (declare (ignore f))
          (let ((lines (lines editor)))
            (true (uiop:string-prefix-p "▾ one (" (third lines)))
            (true (search "(2)" (third lines)))
            (true (find-if (lambda (l) (uiop:string-prefix-p "▾ two (" l)) lines))
            (true (find-if (lambda (l) (uiop:string-prefix-p "▾ No project" l)) lines))
            (true (search "2 projects" (car (last lines 2))))
            ;; the heading has the project's name alone; where it is shows by its files
            (is string= "▾ one (2)" (string-right-trim " " (third lines)))))
        ;; a rimu list of the project: under its heading, its folder in the last column
        (let ((tuatara-user:*rimu-preview* nil))
          (multiple-value-bind (f editor)
              (fresh-list-session (format nil "C-x d C-a C-k ~a C-j C-x C-b" (rimu-typed (namestring (merge-pathnames "sub/" one))))
                                  :width 140 :height 24)
            (declare (ignore f))
            (let ((row (find-if (lambda (l) (search "sub/" l :end2 (min 30 (length l)))) (lines editor))))
              (true row)
              (true (search (tuatara.command::abbreviated-path (tuatara.io:native (merge-pathnames "sub/" one))) row)))
            (true (search "▾ one (3)" (session-text editor))))
          (rimu-forget))
        ;; TAB on a buffer's line folds its section, its buffers hidden
        (multiple-value-bind (f editor) (fresh-list-session "C-x C-b TAB" :width 120 :height 24)
          (declare (ignore f))
          (true (uiop:string-prefix-p "▸ one (" (third (lines editor))))
          (false (search "a.txt" (session-text editor))))
        ;; the cursor goes to the folded section's heading, so a second TAB unfolds it
        (multiple-value-bind (f editor) (fresh-list-session "C-x C-b TAB TAB" :width 120 :height 24)
          (declare (ignore f))
          (true (uiop:string-prefix-p "▾ one (" (third (lines editor))))
          (true (find-if (lambda (l) (uiop:string-prefix-p "▾ two (" l)) (lines editor)))
          (is = 2 (tuatara.buffer:buffer-line-number (tuatara.buffer:find-buffer "*Buffers*") (session-point editor))))
        ;; D on the heading kills the project's buffers after asking
        (fresh-list-session "C-x C-b C-p D y e s RET" :width 120 :height 24)
        (false (live "a.txt"))
        (false (live "b.txt"))
        (true (live "c.txt"))
        ;; C-x p k from a buffer of a project
        (open-files)
        (run-keys "C-x b a . t x t RET C-x p k y e s RET" :text "" :width 120 :height 24)
        (false (live "a.txt"))
        (false (live "b.txt"))
        (true (live "c.txt"))
        (multiple-value-bind (f editor) (run-keys "C-x p k" :text "" :width 120 :height 24)
          (declare (ignore editor))
          (true (tuatara.frontend.fake:fake-frame-showing f "is in no project")))))))

(define-test (buffers project-kept-with-the-buffer)
  ;; a buffer's project is looked for once while it stands in the same folder, again in another
  (with-test-directory (dir)
    (let ((one (namestring (merge-pathnames "one/" dir)))
          (none (namestring (merge-pathnames "none/" dir)))
          (b (tuatara.buffer:make-buffer "project-kept" :text "")))
      (ensure-directories-exist (merge-pathnames ".git/" one))
      (ensure-directories-exist none)
      (unwind-protect
           (progn
             (setf (tuatara.buffer:buffer-local b :directory) one)
             (let ((root (tuatara.defaults::buffer-project b)))
               (true (stringp root))
               (uiop:delete-empty-directory (merge-pathnames ".git/" one))
               (is equal root (tuatara.defaults::buffer-project b)))
             (setf (tuatara.buffer:buffer-local b :directory) none)
             (is eq :none (tuatara.defaults::buffer-project b)))
        (tuatara.buffer:kill-buffer b)))))

(define-test (buffers in-order-of-use)
  ;; the buffers used most recently first, those never shown after them, the list itself left out
  (let* ((a (tuatara.buffer:make-buffer "order-a" :text ""))
         (b (tuatara.buffer:make-buffer "order-b" :text ""))
         (c (tuatara.buffer:make-buffer "order-c" :text ""))
         (tuatara.command:*buffer-order* (list b a)))
    (unwind-protect
         (let ((order (tuatara.defaults::buffers-in-order a)))
           (is eq b (first order))
           (false (member a order))
           (true (member c order))
           (is = (1- (length tuatara.buffer:*buffers*)) (length order)))
      (dolist (x (list a b c)) (tuatara.buffer:kill-buffer x)))))

(define-test (buffers switching-list)
  ;; C-x b names the file a buffer shows, a picture's too, and keeps its columns under a long mode name
  (let ((picture (tuatara.buffer:make-buffer "pic.png" :text ""))
        (lisp (tuatara.buffer:make-buffer "long-mode" :text "")))
    (unwind-protect
         (progn
           (setf (tuatara.buffer:buffer-local picture :viewed-file) "/nowhere/pictures/pic.png"
                 (tuatara.buffer:buffer-local picture :mode-name) "Org"
                 (tuatara.buffer:buffer-local lisp :mode-name) "Lisp Interaction Long")
           (let* ((f (run-keys "C-x b" :text "" :width 140 :height 20))
                  (screen (tuatara.frontend.fake:fake-frame-with f "/nowhere/pictures/pic.png"))
                  (picture-row (and screen (find "/nowhere/pictures/pic.png" screen :test #'search)))
                  (lisp-row (and screen (find "Lisp Interaction Long" screen :test #'search))))
             (true picture-row)
             (true lisp-row)
             (when (and picture-row lisp-row)
               (is = (search "Org" picture-row) (search "Lisp Interaction Long" lisp-row)))))
      (tuatara.buffer:kill-buffer picture)
      (tuatara.buffer:kill-buffer lisp))))

(define-test (buffers section-titles-as-named)
  ;; a project's folder named with *, ? or \ is titled as it is named, not read as a pattern
  (is string= "a*b" (tuatara.defaults::buffer-section-title "/work/a*b/"))
  (is string= "back\\slash [old]" (tuatara.defaults::buffer-section-title "/work/back\\slash [old]/"))
  (is string= "notes" (tuatara.defaults::buffer-section-title "/work/notes/")))

(define-test (buffers marks-named-as-shown)
  ;; the help of the lists says the mark they show: > for marked (LIST-MARK-SIGN), never *
  (is string= ">" (tuatara.defaults::list-mark-sign :marked))
  (dolist (text (list (documentation 'tuatara.defaults::*buffers-columns* 'variable)
                      (documentation 'tuatara.defaults::buffers 'function)
                      (documentation 'tuatara.defaults::*auto-saves-columns* 'variable)
                      (documentation 'tuatara.defaults::bookmark-manager 'function)
                      (documentation 'tuatara.defaults::*trash-columns* 'variable)))
    (true text)
    (false (and text (or (search "(* marked" text) (search "(* or D" text) (search "marks (*" text))) "~a" text)))
