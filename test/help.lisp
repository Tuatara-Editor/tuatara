;;;; test/help.lisp --- Help commands and the read-only *Help* buffer.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.tests)

(define-test help
  :description "describe-key, describe-command, describe-variable, apropos and describe-bindings in a read-only *Help* buffer that q leaves; apropos's list in *Apropos*, each name a link to its description.")

(defun help-text (editor)
  "The text of the *Help* buffer if it is shown, else the current buffer's."
  (session-text editor))

(define-test (help describe)
  (multiple-value-bind (f editor) (run-keys "C-h k C-x C-f" :text "")
    (declare (ignore f))
    (let ((text (help-text editor)))
      (true (search "C-x C-f runs the command find-file" text))
      (true (search "Visit a file" text))
      (true (search "lisp/files/visiting.lisp" text))))
  (multiple-value-bind (f editor) (run-keys "C-h f e n d - o f - b u f f e r RET" :text "")
    (declare (ignore f))
    (let ((text (help-text editor)))
      (true (search "end-of-buffer is a command" text))
      (true (search "M->" text))
      (true (search "Move point to the end of the buffer" text))))
  (multiple-value-bind (f editor) (run-keys "C-h v s e r i a l - w i n d o w RET" :text "")
    (declare (ignore f))
    (true (search "tuatara.syntax::*serial-window*" (help-text editor)))
    (true (search "Value: 4194304" (help-text editor))))
  (multiple-value-bind (f editor) (run-keys "C-h a b u f f e r SPC e n d RET" :text "")
    (declare (ignore f))
    (true (search "end-of-buffer" (help-text editor)))
    (false (search "forward-char" (help-text editor))))
  (multiple-value-bind (f editor) (run-keys "? " :text "")
    (declare (ignore f))
    (true (search "C-x C-f" (help-text editor)))
    (true (search "delete-selection-or-char" (help-text editor)))))

(define-test (help symbol-at-point-offered)
  ;; C-h f in a Lisp buffer: the operator named at point selected first, RET describes it
  (multiple-value-bind (f editor) (run-keys-in-turns '("C-h f RET") :text "mapcar #'1+ list" :mode 'tuatara.defaults::lisp-mode)
    (declare (ignore f))
    (true (search "mapcar is a function" (help-text editor))))
  ;; elsewhere the first one shown
  (multiple-value-bind (f editor) (run-keys-in-turns '("C-h f RET") :text "mapcar #'1+ list")
    (declare (ignore f))
    (false (search "mapcar is a function" (help-text editor)))))

(define-test (help commands-of-every-mode)
  ;; C-h x offers the commands made for another kind of buffer too: rimu-open in a text buffer
  (multiple-value-bind (f editor) (run-keys "C-h x r i m u - o p e n RET" :text "")
    (declare (ignore f))
    (true (search "rimu-open is a command" (help-text editor)))))

(define-test (help apropos-links)
  ;; C-h a lists what it finds in *Apropos*, each name a link: RET on one (TAB
  ;; goes to the next) shows its description in *Help*, the list staying
  (flet ((shown (editor) (tuatara.display:window-buffer (tuatara.command:editor-window editor)))
         (link (buffer text)
           ;; the button over TEXT in BUFFER
           (find-if (lambda (button)
                      (string= text (tuatara.buffer:buffer-string buffer :start (first button) :end (second button))))
                    (tuatara.buffer:buffer-local buffer :buttons))))
    (multiple-value-bind (f editor) (run-keys "C-h a b u f f e r SPC e n d RET" :text "" :width 100 :height 30)
      (declare (ignore f))
      (let ((buffer (shown editor)))
        (is string= "*Apropos*" (tuatara.buffer:buffer-name buffer))
        (true (search "end-of-buffer" (tuatara.buffer:buffer-string buffer)))
        ;; a command's link, and a variable's
        (let ((command (link buffer "end-of-buffer")))
          (true command)
          (when command
            (let ((tuatara.command:*editor* editor)) (funcall (third command)))
            (true (search "end-of-buffer is a command"
                          (tuatara.buffer:buffer-string (tuatara.buffer:find-buffer "*Help*"))))))))
    (multiple-value-bind (f editor) (run-keys "C-h a s e r i a l - w i n d o w RET" :text "" :width 100 :height 30)
      (declare (ignore f))
      (let ((variable (link (shown editor) "tuatara.syntax::*serial-window*")))
        (true variable)
        (when variable
          (let ((tuatara.command:*editor* editor)) (funcall (third variable)))
          (true (search "Value: 4194304" (tuatara.buffer:buffer-string (tuatara.buffer:find-buffer "*Help*")))))))
    ;; the keys: TAB to the first link, RET on it; *Help* shows it beside the
    ;; list, which stays selected for the next
    (multiple-value-bind (f editor) (run-keys "C-h a b u f f e r SPC e n d RET TAB RET" :text "" :width 100 :height 30)
      (declare (ignore f))
      (is string= "*Apropos*" (tuatara.buffer:buffer-name (shown editor)))
      (true (search "is a command" (tuatara.buffer:buffer-string (tuatara.buffer:find-buffer "*Help*")))))))

(define-test (help source-file-a-link)
  ;; where a command, a key's command or a variable is defined is a link: RET on it shows the
  ;; definition in another window, the cursor on it, as M-. does
  (let ((tuatara.defaults::*help-window-select* t))
    (flet ((followed (keys)
             (let* ((editor (nth-value 1 (run-keys (format nil "~a TAB RET" keys) :text "" :width 100 :height 30)))
                    (b (tuatara.display:window-buffer (tuatara.command:editor-window editor)))
                    (point (tuatara.buffer:buffer-point b))
                    (line (tuatara.buffer:buffer-string b :start point :end (tuatara.buffer:buffer-line-end b point))))
               (list (tuatara.buffer:buffer-name b) line))))
      (destructuring-bind (name line) (followed "C-h x d e s c r i b e - c o m m a n d RET")
        (is string= "help.lisp" name)
        (true (uiop:string-prefix-p "(define-command describe-command " line) "~a" line))
      (destructuring-bind (name line) (followed "C-h k C-x C-f")
        (is string= "visiting.lisp" name)
        (true (uiop:string-prefix-p "(define-command find-file " line) "~a" line))
      (destructuring-bind (name line) (followed "C-h v t u a t a r a . d e f a u l t s : : * h e l p - c o l u m n * RET")
        (true (search "(defparameter *help-column* " line) "~a ~a" name line)))))

(define-test (help what-is-at-point)
  ;; C-x =: the character at point, its codes, and where point is
  (multiple-value-bind (f editor) (run-keys "C-x =" :text "héllo" :width 100)
    (declare (ignore editor))
    (true (tuatara.frontend.fake:fake-frame-showing f "Char: h (104, #o150, #x68) point=1 of 5 (0%) octet=0 column=0")))
  (multiple-value-bind (f editor) (run-keys "l C-x =" :text "héllo" :width 100)
    (declare (ignore editor))
    (true (tuatara.frontend.fake:fake-frame-showing f "Char: é (233, #o351, #xe9) point=2 of 5 (20%) octet=1 column=1")))
  (multiple-value-bind (f editor) (run-keys "M-> C-x =" :text "héllo" :width 100)
    (declare (ignore editor))
    (true (tuatara.frontend.fake:fake-frame-showing f "point=6 of 5 (100%) octet=6 column=5")))
  ;; C-u C-x =: all of it in *Help*
  (multiple-value-bind (f editor) (run-keys "l C-u C-x =" :text "héllo" :width 100 :height 30)
    (declare (ignore f editor))
    (let ((text (tuatara.buffer:buffer-string (tuatara.buffer:find-buffer "*Help*"))))
      (true (search "U+00E9, LATIN SMALL LETTER E WITH ACUTE" text))
      (true (search "code         233, #o351, #xe9" text))
      (true (search "kind         a lower-case letter (Ll)" text))
      (true (search "octets       #xc3 #xa9" text))))
  ;; in a buffer parsed with tree-sitter: the syntax node, and the rules colouring it
  (with-test-directory (dir)
    (let ((path (put-file (merge-pathnames "main.rs" dir) (format nil "fn main() {}~%"))))
      (multiple-value-bind (f editor) (run-keys "C-u C-x =" :file (namestring path) :width 100 :height 30)
        (declare (ignore f editor))
        (let ((text (tuatara.buffer:buffer-string (tuatara.buffer:find-buffer "*Help*"))))
          (true (search "syntax node  \"fn\", in function_item, in source_file" text) "~a" text)
          (true (search "rules        @keyword" text) "~a" text)
          (true (search "the colour shown" text))
          (true (search "mode         rust-mode, the language rust" text))))
      (let ((b (tuatara.buffer:find-buffer "main.rs"))) (when b (tuatara.buffer:kill-buffer b))))))

(define-test (help buffer-is-read-only-and-q-returns)
  (multiple-value-bind (f editor) (run-keys "i x C-g C-h f u n d o RET i y C-g" :text "")
    (declare (ignore f))
    ;; typing in *Help* is refused with a message; q brings the previous buffer back
    (is string= "*Help*" (tuatara.buffer:buffer-name (tuatara.display:window-buffer (tuatara.command:editor-window editor))))
    (true (search "undo is a command" (session-text editor)))
    (false (search "y" (subseq (session-text editor) 0 1))))
  (multiple-value-bind (f editor) (run-keys "i x C-g C-h f u n d o RET q" :text "")
    (declare (ignore f))
    (is string= "*scratch*" (tuatara.buffer:buffer-name (tuatara.display:window-buffer (tuatara.command:editor-window editor))))
    (is string= "x" (session-text editor))))

(define-test (help which-tuatara)
  ;; M-x version (tuatara-version): the version, what it was built from and
  ;; when, the SBCL it runs on and where it shows; C-u puts it at point
  (let ((sbcl (subseq (lisp-implementation-version) 0 (or (position #\- (lisp-implementation-version))
                                                          (length (lisp-implementation-version))))))
    (let ((tuatara:*build* nil))
      (is string= (format nil "Tuatara ~a (from its sources) on SBCL ~a" (tuatara:version) sbcl) (tuatara:version-line)))
    (let ((tuatara:*build* '("a1b2c3d" nil "2026-10-02")))
      (is string= (format nil "Tuatara ~a (a1b2c3d, built 2026-10-02) on SBCL ~a, in the terminal (kitty)" (tuatara:version) sbcl)
          (tuatara:version-line "in the terminal (kitty)")))
    (let ((tuatara:*build* '("a1b2c3d" t "2026-10-02")))
      (true (search "(a1b2c3d+, built 2026-10-02)" (tuatara:version-line))))
    (multiple-value-bind (f editor) (run-keys "M-x v e r s i o n RET" :text "x" :width 120)
      (declare (ignore editor))
      (true (tuatara.frontend.fake:fake-frame-showing f (format nil "Tuatara ~a" (tuatara:version)))))
    (multiple-value-bind (f editor) (run-keys "C-u M-x t u a t a r a - v e r s i o n RET" :text "" :width 120)
      (declare (ignore f))
      (true (search (format nil "Tuatara ~a" (tuatara:version)) (session-text editor))))))

(defun readme-credit-names ()
  "The names the README's Credits credit, each the words of an item before its ::."
  (let ((lines (uiop:read-file-lines (asdf:system-relative-pathname :tuatara "README.org")))
        (items '()))
    (loop for line in (rest (member "* Credits" lines :test #'string=))
          until (uiop:string-prefix-p "* " line)
          do (cond ((uiop:string-prefix-p "- " line) (push (subseq line 2) items))
                   ((and items (uiop:string-prefix-p "  " line))
                    (setf (first items) (format nil "~a ~a" (first items) (string-trim " " line))))))
    (mapcar (lambda (item) (subseq item 0 (search " ::" item))) (nreverse items))))

(define-test (help credits-as-in-the-readme)
  ;; M-x credits and the README's Credits name the same projects, in the same order
  (is equal (readme-credit-names) (mapcar #'first tuatara.defaults::*credits*))
  ;; a name too long for its column has its lines below it, never run into them
  (multiple-value-bind (f editor) (run-keys "M-x c r e d i t s RET" :text "x" :width 120 :height 40)
    (declare (ignore f))
    (let ((text (tuatara.buffer:buffer-string
                 (tuatara.display:window-buffer (tuatara.command:editor-window editor)))))
      (true (search (format nil "  dired-du, by Tino Calancha; dired-git-info, by Clemens Radermacher; and dired-duplicates, by Harald Judt~%                        Rimu's sizes") text))
      (true (search (format nil "  SBCL                  the Common Lisp") text)))))

(define-test (help about-and-license)
  ;; C-h C-a: *About Tuatara*, the version, the license and the ways in, each a
  ;; button: RET on one opens what it names, TAB goes to the next
  (flet ((shown (editor) (tuatara.display:window-buffer (tuatara.command:editor-window editor)))
         (octets-to (text string)
           ;; where STRING begins in TEXT, in octets as point counts them
           (length (tuatara.text:string-octets (subseq text 0 (search string text))))))
    (multiple-value-bind (f editor) (run-keys "C-h C-a" :text "x" :width 120 :height 40)
      (declare (ignore f))
      (let* ((buffer (shown editor)) (text (tuatara.buffer:buffer-string buffer)))
        (is string= "*About Tuatara*" (tuatara.buffer:buffer-name buffer))
        (true (search (format nil "Tuatara ~a" (tuatara:version)) text))
        (true (search "GNU General Public License" text))
        (true (search "absolutely no warranty" text))
        (true (search "The tutor" text))
        (true (search "C-h C-c" text))
        ;; point on the first button, the license
        (is = (octets-to text "GNU General Public License") (tuatara.buffer:buffer-point buffer))
        (true (search "Its sources: https://github.com/Tuatara-Editor/tuatara; report a fault there." text))
        (is = 11 (length (tuatara.buffer:buffer-local buffer :buttons)))))
    ;; the second button, TAB from the first, then RET: no warranty's part of the license
    (multiple-value-bind (f editor) (run-keys "C-h C-a TAB RET" :text "x" :width 120 :height 40)
      (declare (ignore f))
      (let ((text (tuatara.buffer:buffer-string (shown editor))))
        (is string= "*No Warranty*" (tuatara.buffer:buffer-name (shown editor)))
        (true (search "15. Disclaimer of Warranty." text))
        (true (search "17. Interpretation of Sections 15 and 16." text))
        (false (search "END OF TERMS AND CONDITIONS" text))
        (false (search "0. Definitions." text))))
    ;; the third, the credits: where the ideas come from
    (multiple-value-bind (f editor) (run-keys "C-h C-a TAB TAB RET" :text "x" :width 120 :height 40)
      (declare (ignore f))
      (let ((text (tuatara.buffer:buffer-string (shown editor))))
        (is string= "*Credits*" (tuatara.buffer:buffer-name (shown editor)))
        (true (search "GNU Emacs" text))
        (true (search "Denote" text))
        (true (search "github.com/YardQuit/donkey" text))))
    ;; the fourth, the sources' address, then reporting a fault
    (multiple-value-bind (f editor) (run-keys "C-h C-a TAB TAB TAB" :text "x" :width 120 :height 40)
      (declare (ignore f))
      (let ((buffer (shown editor)))
        (is = (octets-to (tuatara.buffer:buffer-string buffer) "https://github.com/Tuatara-Editor/tuatara")
            (tuatara.buffer:buffer-point buffer))))
    ;; the sixth, the tutor; S-TAB back round to the last, the health check's
    (multiple-value-bind (f editor) (run-keys "C-h C-a TAB TAB TAB TAB TAB RET" :text "x" :width 120 :height 40)
      (declare (ignore f))
      (is string= "*Tutor*" (tuatara.buffer:buffer-name (shown editor))))
    (multiple-value-bind (f editor) (run-keys "C-h C-a S-TAB" :text "x" :width 120 :height 40)
      (declare (ignore f))
      (let ((buffer (shown editor)))
        (is = (octets-to (tuatara.buffer:buffer-string buffer) "Checking health") (tuatara.buffer:buffer-point buffer))))
    ;; C-h C-c: all of the license
    (multiple-value-bind (f editor) (run-keys "C-h C-c" :text "x" :width 120 :height 40)
      (declare (ignore f))
      (let ((text (tuatara.buffer:buffer-string (shown editor))))
        (is string= "*GNU General Public License*" (tuatara.buffer:buffer-name (shown editor)))
        (true (search "GNU GENERAL PUBLIC LICENSE" text))
        (true (search "END OF TERMS AND CONDITIONS" text))))))


(define-test (help keys-under-a-prefix)
  ;; C-h after a prefix lists every key under it, a prefix's keys under its name
  (run-keys "C-x C-h" :text "" :width 100 :height 30)
  (let ((text (tuatara.buffer:buffer-string (tuatara.buffer:find-buffer "*Help*"))))
    (true (search "Keys under C-x" text))
    (true (cl-ppcre:scan "C-x C-f +find-file" text))
    (true (cl-ppcre:scan "C-x n +\\+notes" text))
    (true (cl-ppcre:scan "    C-x n f +find-file-formatted" text))
    (false (search "C-h k" text)))
  ;; after the leader too
  (run-keys "SPC C-h" :text "" :width 100 :height 30)
  (true (search "Keys under SPC" (tuatara.buffer:buffer-string (tuatara.buffer:find-buffer "*Help*")))))

(define-test (help key-hints-cut-short)
  ;; the key hints take the rows the frame has, as a menu does; more keys than
  ;; rows: the last row says so, and how to see them all
  (flet ((screen (keys rows)
           (let ((tuatara-user:*key-hint-rows* rows))
             (run-keys keys :text "" :width 100 :height 40))))
    (false (tuatara.frontend.fake:fake-frame-showing (screen "C-x" nil) "C-h lists all"))
    (true (tuatara.frontend.fake:fake-frame-showing (screen "C-x" 4) "... C-h lists all"))
    (false (tuatara.frontend.fake:fake-frame-showing (screen "C-x" 4) "M-t"))
    ;; M-t is no key after a prefix
    (true (tuatara.frontend.fake:fake-frame-showing (screen "C-x M-t" nil) "C-x M-t is undefined")))
  ;; taller hints with *key-hint-rows*: the window above them shorter, its mode line higher
  (flet ((mode-line-row (rows)
           (let ((tuatara-user:*key-hint-rows* rows))
             (let ((f (run-keys "C-x" :text "" :width 100 :height 40)))
               (loop for r from 0 below 40
                     when (search "*scratch*" (tuatara.frontend.fake:fake-screen-line f r))
                       return r)))))
    (let ((short (mode-line-row 4)) (tall (mode-line-row 20)))
      (true short)
      (true tall)
      (is = 16 (- short tall)))))

(define-test (help prefix-keys-leave-the-echo)
  ;; C-x C-h shows its keys in *Help*, the echo row no longer saying C-x-
  (multiple-value-bind (f editor) (run-keys "C-x C-h" :text "" :width 100 :height 30)
    (declare (ignore f))
    (false (search "C-x-" (or (tuatara.command:editor-message editor) "")))))

(define-test (help documentation-filled)
  ;; a command's or a variable's documentation is filled to *help-column*, as the
  ;; settings' help is: no line past it, a key written in never split over two lines
  (let ((tuatara.command:*editor* (nth-value 1 (run-keys "" :text ""))))
    (flet ((widest (pieces) (reduce #'max (mapcar #'length (uiop:split-string (tuatara.defaults::pieces-text pieces :keys t) :separator '(#\Newline))))))
      (true (<= (widest (tuatara.defaults::describe-function-pieces 'tuatara.defaults::org-insert-link)) tuatara.defaults::*help-column*))
      (true (<= (widest (tuatara.defaults::describe-variable-pieces 'tuatara-user::*auto-save-directory*)) tuatara.defaults::*help-column*))
      (true (search "C-x a l" (tuatara.defaults::pieces-text (tuatara.defaults::describe-function-pieces 'tuatara.defaults::org-insert-link) :keys t)))))
  ;; a list item's lines filled under its words; a line set in kept as written
  (is string= (format nil "Text one two.~%- an item that~%  goes on~%    | kept |  as is~%")
      (with-output-to-string (out)
        (tuatara.defaults::write-documentation out (format nil "Text~%one two.~%- an item~%  that goes on~%    | kept |  as is") :column 14))))

(define-test (help filled-on-asking)
  ;; show-help with :fill writes a text as documentation is written: paragraphs filled
  ;; to *help-column*, lines set in kept as they are, the keys named written in
  (let ((text (format nil "A report~%~%~{~a ~}\\[find-file] opens a file.~%~%Settings~%  *a-setting*        t~%  *another-setting*  nil~%"
                      (loop repeat 30 collect "word"))))
    (multiple-value-bind (f editor) (run-keys (list (lambda () (tuatara.defaults::show-help text :fill t))) :text "" :width 120)
      (declare (ignore f editor))
      (let* ((help (tuatara.buffer:find-buffer "*Help*"))
             (lines (uiop:split-string (tuatara.buffer:buffer-string help) :separator '(#\Newline))))
        (true (every (lambda (line) (<= (length line) tuatara.defaults::*help-column*)) lines))
        (true (< 1 (count-if (lambda (line) (search "word word" line)) lines)))
        (true (member "  *a-setting*        t" lines :test #'string=))
        (true (search "C-x C-f opens a file." (tuatara.buffer:buffer-string help)))))))

(define-test (help variable-value-cut-short)
  ;; a variable holding a great deal is shown cut short, said so, never printed whole
  (let ((text (tuatara.defaults::variable-value-text (loop repeat 100000 collect (make-string 100 :initial-element #\x)))))
    (true (<= (length text) (+ tuatara.defaults::*variable-value-shown* 20)))
    (true (search "(cut short)" text)))
  (is string= "(1 2 3)" (tuatara.defaults::variable-value-text (list 1 2 3))))

(define-test (help editor-variables-listed)
  ;; the variables of the editor's packages, your settings in TUATARA-USER too
  (let ((variables (tuatara.defaults::tuatara-variables)))
    (true (member 'tuatara-user:*default-directory* variables))
    (true (member 'tuatara.defaults::*org-link-descriptive* variables))
    (false (member '*print-base* variables))
    (true (every (lambda (s) (let ((p (symbol-package s)))
                               (or (eq p (find-package :tuatara-user)) (member p (tuatara.defaults::editor-packages)))))
                 variables))))

(define-test (help source-paths-from-the-tree)
  ;; where a command is defined is named within the source tree, never by the folders the
  ;; program was built in, and the license is found or said missing without the tree known
  (let ((words (tuatara.defaults::pieces-text (tuatara.defaults::source-location-pieces 'tuatara.defaults::search-again :function))))
    (true (search "Defined in lisp/search/incremental.lisp" words) "~a" words)
    (false (search "//" words)))
  (is string= "src/syntax/search.lisp" (tuatara.defaults::source-file-words "/home/ann/work/tuatara/src/syntax/search.lisp"))
  (is string= "notes.lisp" (tuatara.defaults::source-file-words "/tmp/notes.lisp"))
  (let ((tuatara.runtime:*source-tree* nil))
    (finish (tuatara.defaults::license-file))))

(define-test (help columns-by-the-screen)
  ;; what a list of keys and the health report line up after a wide character starts at the
  ;; same column of the screen as after narrow ones: padded by the screen's columns
  (flet ((column-of (line what) (tuatara.defaults::text-columns (subseq line 0 (search what line)))))
    (is = 22 (column-of (with-output-to-string (out) (tuatara.defaults::key-line out "" "漢字" "runs")) "runs"))
    (is = 22 (column-of (with-output-to-string (out) (tuatara.defaults::key-line out "" "C-x" "runs")) "runs"))
    (let ((tuatara.defaults::*health-pieces* '()))
      (tuatara.defaults::health-cell "漢字" 10)
      (tuatara.defaults::health-put "next")
      (is = 11 (column-of (tuatara.defaults::pieces-text (reverse tuatara.defaults::*health-pieces*)) "next")))))
