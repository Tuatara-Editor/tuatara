;;;; test/modes.lisp --- Major modes, auto-mode, CSV colouring.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.tests)

(define-test modes
  :description "The major-mode tree: inheritance of keymaps and modal classes, auto-mode by name,
extension and #! line, the mode name in the mode line, CSV colours and the column indicator.")

(tuatara.modes:define-major-mode test-parent-mode tuatara.defaults::text-mode
  "A mode for the tests: a parent with a key binding."
  :comment ("%" "%%") :extensions ("tpar"))

(tuatara.modes:define-major-mode test-child-mode test-parent-mode
  "A mode for the tests: a child that inherits its parent's keys and becomes a support buffer."
  :lighter "Kid" :modal-class :support :file-names ("KIDFILE") :interpreters ("kidlang"))

(define-test (modes tree-and-auto-mode)
  (is eq 'test-child-mode (tuatara.modes:mode-for-file #p"/tmp/KIDFILE"))
  (is eq 'test-parent-mode (tuatara.modes:mode-for-file #p"/tmp/x.TPAR"))
  (is eq 'test-child-mode (tuatara.modes:mode-for-file #p"/tmp/script" "#!/usr/bin/env kidlang -x"))
  (is eq 'test-child-mode (tuatara.modes:mode-for-file #p"/tmp/script" "#!/opt/bin/kidlang3.12"))
  (is eq 'tuatara.defaults::csv-mode (tuatara.modes:mode-for-file #p"/data/big.csv"))
  (is eq 'tuatara.modes:fundamental-mode (tuatara.modes:mode-for-file #p"/tmp/unknown.zzz"))
  (is string= "python3" (tuatara.modes:shebang-interpreter "#!/usr/bin/env python3"))
  (false (tuatara.modes:shebang-interpreter "plain text"))
  ;; keymaps inherit down the tree
  (tuatara.command:define-key (tuatara.modes:mode-keymap 'test-parent-mode) "C-c z" 'tuatara.defaults::end-of-buffer)
  (is eq 'tuatara.defaults::end-of-buffer (tuatara.command:lookup-key (tuatara.modes:mode-keymap 'test-child-mode)
                                                                       (tuatara.frontend:parse-key-sequence "C-c z")))
  ;; modal classes inherit; a mode can set its own
  (is eq :support (tuatara.modes:mode-modal-class (make-instance 'test-child-mode)))
  (is eq :editing (tuatara.modes:mode-modal-class (make-instance 'test-parent-mode)))
  (is eq :support (tuatara.modes:mode-modal-class (make-instance 'tuatara.defaults::help-mode)))
  ;; so does the way comments are written, given in the definition
  (is equal '("%" "%%") (multiple-value-list (tuatara.modes:mode-comment-syntax (make-instance 'test-child-mode))))
  (is equal '(nil) (multiple-value-list (tuatara.modes:mode-comment-syntax (make-instance 'tuatara.defaults::text-mode))))
  (is equal '(nil) (multiple-value-list (tuatara.modes:mode-block-comment-syntax (make-instance 'test-child-mode)))))

(define-test (modes file-patterns-compiled-once)
  ;; the patterns on a file's whole path are compiled as their modes are defined: a
  ;; lookup only tries them
  (is eq 'tuatara.defaults::blade-mode (tuatara.modes:mode-for-file #p"/srv/site/views/home.blade.php"))
  (is eq 'tuatara.defaults::xml-mode (tuatara.modes:mode-for-file #p"/home/kea/Rust.tmLanguage"))
  (let ((consed (loop repeat 3
                      minimize (let ((before (tuatara.bench::bytes-consed)))
                                 (tuatara.modes:mode-for-file #p"/tmp/unknown.zzz")
                                 (- (tuatara.bench::bytes-consed) before)))))
    (true (< consed (* 64 1024)) "a lookup consed ~:d octets" consed)))

(define-test (modes switching-and-mode-line)
  ;; M-x switches mode; the mode line shows its name; the parent's keys work
  (multiple-value-bind (f editor) (run-keys "M-x t e s t - p a r e n t - m o d e RET C-c z" :text "abc")
    (is = 3 (session-point editor))
    (true (tuatara.frontend.fake:fake-frame-showing f "Test-Parent"))
    (true (typep (tuatara.modes:buffer-major-mode (tuatara.display:window-buffer (tuatara.command:editor-window editor)))
                 'test-parent-mode)))
  ;; a support-class mode: letters are not NORMAL commands, lighter S
  (multiple-value-bind (f editor) (run-keys "M-x t e s t - c h i l d - m o d e RET x" :text "abc")
    (is string= "abc" (session-text editor))
    (true (tuatara.frontend.fake:fake-frame-showing f "Kid" "TUATARA[S]"))))

(define-test (modes command-documentation)
  ;; each mode's command is documented by its mode's whole first sentence (the one
  ;; FIRST-SENTENCE, tested in completion), on one line however many it took, never cut
  (eval '(tuatara.modes:define-major-mode test-sentence-mode tuatara.modes:fundamental-mode
          "A mode that
goes on. More."))
  (true (search "A mode that goes on." (documentation 'test-sentence-mode 'function)))
  (dolist (mode (remove-if-not #'tuatara.command:find-command (tuatara.modes:mode-names)))
    (let ((doc (documentation mode 'function)))
      (true (and doc (char= #\. (char doc (1- (length doc)))))
            "The command ~(~a~) is documented by a sentence cut short: ~s" mode doc))))

(define-test (modes made-by-their-commands)
  ;; the modes of the buffers a command makes (help, the lists, the tutor, IELM)
  ;; are no commands, so no other buffer is switched to one; a file's are
  (dolist (name '("special-mode" "help-mode" "buffers-mode" "jobs-mode" "font-list-mode" "about-mode"
                  "xref-mode" "backtrace-mode" "ielm-mode" "tuatara-tutor-mode"))
    (false (tuatara.command:command-symbol name) "~a is a command" name))
  (dolist (name '("fundamental-mode" "text-mode" "prog-mode" "csv-mode" "org-mode" "lisp-interaction-mode"))
    (true (tuatara.command:command-symbol name) "~a is no command" name))
  ;; M-x jobs-mode leaves the buffer and its text as they were
  (multiple-value-bind (f editor) (run-keys "M-x j o b s - m o d e RET" :text "a,b" :width 80)
    (declare (ignore f))
    (is string= "a,b" (session-text editor))
    (false (typep (tuatara.modes:buffer-major-mode (session-buffer editor)) 'tuatara.defaults::jobs-mode)))
  ;; the commands that make those buffers still give them their modes
  (multiple-value-bind (f editor) (run-keys "M-x l i s t - j o b s RET" :text "x" :width 150 :height 12)
    (declare (ignore f))
    (true (typep (tuatara.modes:buffer-major-mode (tuatara.display:window-buffer (tuatara.command:editor-window editor)))
                 'tuatara.defaults::jobs-mode))))

(defun csv-spans (keys text &key (width 60))
  "Run KEYS in a CSV buffer holding TEXT; return the fake frontend."
  (let* ((path (merge-pathnames (format nil "tuatara-csv-~a.csv" (random 1000000)) (uiop:temporary-directory))))
    (with-open-file (o path :direction :output :if-exists :supersede) (write-string text o))
    (unwind-protect
         (multiple-value-bind (f editor) (run-keys keys :file path :width width)
           (tuatara.buffer:kill-buffer (tuatara.display:window-buffer (tuatara.command:editor-window editor)))
           f)
      (ignore-errors (delete-file path)))))

(define-test (modes csv)
  (let ((f (csv-spans "" (format nil "id,name,price~%1,\"a, b\",3.5~%"))))
    ;; each column has its own face; the quoted field with a comma is one column
    (is equal '((0 2 :csv-column-0) (3 7 :csv-column-1) (8 13 :csv-column-2))
        (aref (tuatara.frontend.fake:fake-spans f) 0))
    (is equal '((0 1 :csv-column-0) (2 8 :csv-column-1) (9 12 :csv-column-2))
        (aref (tuatara.frontend.fake:fake-spans f) 1))
    ;; the mode line names the mode and the column at point with its header
    (true (tuatara.frontend.fake:fake-frame-showing f "CSV" "col 0 id")))
  ;; moving to the third column of the second line shows its header
  (let ((f (csv-spans "j w w w w" (format nil "id,name,price~%1,\"a, b\",3.5~%"))))
    (true (tuatara.frontend.fake:fake-frame-showing f "col 2 price")))
  ;; a quoted field going on over a line break: the line after is still that record
  (let ((f (csv-spans "j j l l l l l l" (format nil "a,b,c~%\"multi~%line\",2,3~%"))))
    (true (tuatara.frontend.fake:fake-frame-showing f "col 1 b"))
    (is equal '((0 5 :csv-column-0) (6 7 :csv-column-1) (8 9 :csv-column-2))
        (aref (tuatara.frontend.fake:fake-spans f) 2)))
  ;; a byte-order mark the file begins with is no part of the first column's name
  (let ((f (csv-spans "j" (format nil "~aid,name~%1,a~%" (code-char #xfeff)))))
    (true (tuatara.frontend.fake:fake-frame-showing f "col 0 id "))
    (false (tuatara.frontend.fake:fake-frame-showing f (format nil "col 0 ~aid" (code-char #xfeff)))))
  ;; the header's names: quotes taken off, a doubled quote one, the byte-order mark no part of the first
  (let ((b (tuatara.buffer:make-buffer "*csv-header*" :text (format nil "~aid, \"a, \"\"b\"\"\" ,c~c~%1,2,3~%" (code-char #xfeff) #\Return))))
    (unwind-protect (is equal '("id" "a, \"b\"" "c") (tuatara.defaults::csv-header-names b))
      (tuatara.buffer:kill-buffer b)))
  ;; columns past the eighth cycle back to the first face
  (let ((f (csv-spans "" (format nil "~{~a~^,~}~%" (loop for i below 10 collect i)))))
    (is eq :csv-column-0 (third (nth 8 (aref (tuatara.frontend.fake:fake-spans f) 0)))))
  ;; the region still shows over the column colours: v l selects the first character
  (let ((f (csv-spans "v l" (format nil "ab,cd~%"))))
    (is equal '((0 1 (:region :csv-column-0)) (1 2 :csv-column-0) (3 5 :csv-column-1))
        (aref (tuatara.frontend.fake:fake-spans f) 0))))

(defun record-starts-read-whole (text)
  "For each line start of TEXT, the start of the CSV record holding it, read
from the start of TEXT: a list of (LINE-START . RECORD-START), in order."
  (let ((quoted nil) (record 0) (pairs (list (cons 0 0))))
    (loop for i from 0 below (length text)
          for c = (char text i)
          do (cond ((char= c #\") (setf quoted (not quoted)))
                   ((char= c #\Newline)
                    (unless quoted (setf record (1+ i)))
                    (when (< (1+ i) (length text))
                      (push (cons (1+ i) record) pairs)))))
    (nreverse pairs)))

(define-test (modes csv-record-starts)
  ;; where the record holding a line begins, quoted fields going on over line
  ;; breaks: the same as read from the start of the text, asked down the text,
  ;; up it, in no order, and again once the text has changed
  (let ((text (with-output-to-string (o)
                (dotimes (i 3000)
                  (if (zerop (mod i 7))
                      (format o "~d,\"two~%lines, \"\"quoted\"\"\",~d~%" i i)
                      (format o "~d,plain,~d~%" i i))))))
    (with-test-buffer (b "*records*" :text text)
      (flet ((agree (pairs what)
               (let ((wrong (loop for (line . record) in pairs
                                  unless (= record (tuatara.defaults::csv-record-start b line))
                                    collect line)))
                 (false wrong "~a: wrong at ~{~d~^, ~}" what (subseq wrong 0 (min 5 (length wrong)))))))
        (let ((pairs (record-starts-read-whole text)))
          (agree pairs "down")
          (agree (reverse pairs) "up")
          (agree (loop with n = (length pairs) for i below n collect (nth (mod (* i 7919) n) pairs)) "in no order"))
        ;; a quote opened at the top: every record after begins elsewhere
        (tuatara.buffer:buffer-insert b 0 (format nil "\"opened~%"))
        (agree (record-starts-read-whole (tuatara.buffer:buffer-string b)) "changed"))
      ;; moving a line at a time reads a line or so, wherever in the text: what one more line makes
      (flet ((made (lines)
               (let ((starts (loop for n from 2000 repeat (1+ lines) collect (tuatara.buffer:buffer-goto-line b n))))
                 (tuatara.defaults::csv-record-start b (pop starts))
                 (nth-value 2 (measuring (dolist (start starts) (tuatara.defaults::csv-record-start b start)))))))
        ;; tens of kilobytes a line before, read again from the start of the text
        (let ((each (/ (- (made 400) (made 100)) 300)))
          (true (< each 2000) "~:d octets a line" (round each)))))))

(define-test (modes csv-colouring)
  (let ((text (format nil "# a comment~%id,\"a, b\",c#5~%")))
    (unwind-protect
         (progn
           ;; sparse, as Emacs's csv-mode: the separators, the quoted fields and
           ;; the lines that start with #; a # further in is text
           (tuatara-user:set-colouring 'csv-mode :sparse)
           (let ((f (csv-spans "" text)))
             (is equal '((0 11 :comment)) (aref (tuatara.frontend.fake:fake-spans f) 0))
             (is equal '((2 3 :csv-separator) (3 9 :string) (9 10 :csv-separator))
                 (aref (tuatara.frontend.fake:fake-spans f) 1)))
           ;; a quoted field left open runs on to the end of what is shown
           (is equal '((1 2 :csv-separator) (2 5 :string))
               (aref (tuatara.frontend.fake:fake-spans (csv-spans "" (format nil "a,\"bc~%"))) 0))
           ;; none: plain
           (tuatara-user:set-colouring 'csv :none)
           (is equal '() (aref (tuatara.frontend.fake:fake-spans (csv-spans "" text)) 1))
           ;; M-x rotate-colouring in a CSV buffer goes on from none to rich
           (let ((f (csv-spans "M-x r o t a t e - c o l o u r i n g RET" text)))
             (true (tuatara.frontend.fake:fake-frame-showing f "CSV and TSV colouring: rich"))
             (is eq :rich tuatara.defaults::*csv-colouring*)))
      (setf tuatara.defaults::*csv-colouring* nil))
    ;; rich, the default: a colour per column, the line starting with # too
    (let ((f (csv-spans "" text)))
      (is equal '((0 11 :csv-column-0)) (aref (tuatara.frontend.fake:fake-spans f) 0))
      (is eq :csv-column-1 (third (second (aref (tuatara.frontend.fake:fake-spans f) 1))))))
  ;; TSV goes with CSV; a major mode's name stands for its language
  (unwind-protect
       (progn (tuatara-user:set-colouring 'tsv-mode :sparse)
              (is eq :sparse tuatara.defaults::*csv-colouring*)
              (tuatara-user:set-colouring 'rust-mode :sparse)
              (is eq :sparse (tuatara.defaults::language-style (tuatara.defaults::find-language 'rust))))
    (setf tuatara.defaults::*csv-colouring* nil)
    (tuatara-user:set-colouring 'rust :rich))
  ;; CSV has no levels; a name that is neither a language nor a mode is refused
  (fail (tuatara-user:set-colouring 'csv-mode :sparse :level 2))
  (fail (tuatara-user:set-colouring 'no-such-mode :sparse))
  (false tuatara.defaults::*csv-colouring*))

(defun csv-run (items text &key (width 60) (height 8) (type "csv"))
  "Run ITEMS in a buffer visiting a new file of TYPE holding TEXT, a string of
ITEMS being keys as RUN-KEYS takes them, a list (BASE COLUMN ROW) a mouse key
at that cell of the screen; return the fake frontend and the buffer's point,
the buffer killed."
  (let ((path (merge-pathnames (format nil "tuatara-csv-~a.~a" (random 1000000) type) (uiop:temporary-directory)))
        (f (tuatara.frontend.fake:make-fake-frontend :width width :height height)))
    (with-open-file (o path :direction :output :if-exists :supersede) (write-string text o))
    (unwind-protect
         (progn
           (dolist (item items)
             (if (stringp item)
                 (tuatara.frontend.fake:fake-queue-keys f item)
                 (destructuring-bind (base column row) item
                   (tuatara.frontend.fake:fake-queue-event
                    f (tuatara.frontend:make-event :key :key (tuatara.frontend:make-key base :column column :row row))))))
           (let* ((editor (tuatara.command:run-editor f :files (list (namestring path))
                                                        :setup #'tuatara.defaults:install-defaults))
                  (buffer (tuatara.display:window-buffer (tuatara.command:editor-window editor)))
                  (point (tuatara.buffer:buffer-point buffer)))
             (tuatara.buffer:kill-buffer buffer)
             (values f point)))
      (ignore-errors (delete-file path)))))

(define-test (modes csv-separator)
  (let ((text (format nil "a;b;c~%1;2;3~%"))
        (one '((0 5 :csv-column-0)))
        (three '((0 1 :csv-column-0) (2 3 :csv-column-1) (4 5 :csv-column-2))))
    (flet ((first-row (f) (aref (tuatara.frontend.fake:fake-spans f) 0)))
      ;; unless set, what the first line has most of: here semicolons
      (is equal three (first-row (csv-run '("") text)))
      ;; set, taken always: a comma, and the semicolons are text of one column
      (let ((tuatara-user:*csv-separator* #\,))
        (is equal one (first-row (csv-run '("") text))))
      ;; set as a character, a string of one or a name, or what the first line has most of
      (dolist (separator (list #\; ";" "semicolon" :detect))
        (let ((tuatara-user:*csv-separator* separator))
          (is equal three (first-row (csv-run '("") text)) "~s" separator)))
      ;; an empty file, or one made empty, takes it from what is typed in its first line
      (let ((two '((0 1 :csv-column-0) (2 3 :csv-column-1))))
        (is equal two (first-row (csv-run '("i a ; b ESC") "")))
        (is equal two (first-row (csv-run '("% d i a ; b ESC") (format nil "x,y~%"))))
        ;; set, it stays
        (is equal '((0 3 :csv-column-0)) (first-row (csv-run '("C-c C-s c o m m a RET i a ; b ESC") ""))))
      ;; what cannot separate fields: a comma
      (let ((tuatara-user:*csv-separator* "ab"))
        (is equal one (first-row (csv-run '("") text))))
      ;; C-c C-s asks, opening on the separator in use: by name, as typed, or detected
      (true (tuatara.frontend.fake:fake-frame-showing (csv-run '("C-c C-s") text) "Separator (default semicolon):"))
      (dolist (keys '("C-c C-s s e m i RET" "C-c C-s ; RET" "C-c C-s d e t e c t RET"))
        (let ((f (csv-run (list keys) text)))
          (is equal three (first-row f) "~a" keys)
          (true (tuatara.frontend.fake:fake-frame-showing f "Fields separated by semicolon"))))
      ;; M-RET takes a letter as typed, where RET would take the name it matches
      (is equal '((0 1 :csv-column-0) (2 3 :csv-column-1))
          (first-row (csv-run '("C-c C-s t M-RET") (format nil "atb~%"))))
      ;; what cannot separate fields is refused, the separator kept
      (let ((f (csv-run '("C-c C-s x y RET") text)))
        (true (tuatara.frontend.fake:fake-frame-showing f "Not a separator: xy"))
        (is equal three (first-row f)))
      ;; a file of tab-separated values keeps to tabs
      (let ((tuatara-user:*csv-separator* #\;))
        (is equal '((0 3 :csv-column-0) (4 5 :csv-column-1))
            (first-row (csv-run '("") (format nil "a;b~cc~%" #\Tab) :type "tsv"))))
      ;; made for CSV buffers alone: M-x does not offer it elsewhere
      (false (tuatara.frontend.fake:fake-frame-showing (run-keys "M-x c s v - d e f i n e RET C-g" :text "a;b") "Separator")))))

(define-test (modes csv-header-lock)
  ;; ten rows: the header line, 7 of text, the mode line, the echo area
  (let ((text (format nil "id,name~%~{~d,n~d~%~}" (loop for i from 1 to 40 append (list i i)))))
    (flet ((row (f r) (tuatara.frontend.fake:fake-screen-line f r)))
      ;; C-x / L keeps the header in the top row, coloured by column, as the rest scrolls under it
      (let ((f (csv-run '("C-x / L M->") text :height 10)))
        (is string= "id,name" (row f 0))
        (is eq :header-line (aref (tuatara.frontend.fake:fake-row-faces f) 0))
        (is equal '((0 2 :csv-column-0) (3 7 :csv-column-1)) (aref (tuatara.frontend.fake:fake-spans f) 0))
        (is string= "35,n35" (row f 1))
        (is string= "40,n40" (row f 6))
        ;; point, on the empty last line, in the last row of text, the mode line under it
        (is = 7 (first (tuatara.frontend.fake:fake-cursor f)))
        (true (search "CSV" (row f 8)))
        (true (tuatara.frontend.fake:fake-frame-showing f "Header kept at the top")))
      ;; without it, and after it again, the header scrolls away: a row more of text
      (dolist (keys '("M->" "C-x / L C-x / L M->"))
        (let ((f (csv-run (list keys) text :height 10)))
          (is string= "34,n34" (row f 0) "~a" keys)))
      ;; *list-header-lock* keeps it in every CSV buffer; another mode lets it go
      (let ((tuatara-user:*list-header-lock* t))
        (is string= "id,name" (row (csv-run '("M->") text :height 10) 0))
        (is string= "34,n34" (row (csv-run '("M-x t e x t - m o d e RET M->") text :height 10) 0)))
      ;; the header follows the text sideways
      (let ((f (csv-run '("C-x / L C-e") (format nil "~a,b~%x,y~%" (make-string 70 :initial-element #\a)) :height 10)))
        (is string= (tuatara.frontend.fake:fake-screen-line f 1) (row f 0)))
      ;; the mouse goes by what each row shows: row 2 shows the line after the header
      (multiple-value-bind (f point) (csv-run '("C-x / L" (:down-mouse-1 2 2)) text :height 10)
        (declare (ignore f))
        (is = (+ (length "id,name") 1 2) point))
      ;; a click on the header line leaves point where it was
      (multiple-value-bind (f point) (csv-run '("C-x / L" (:down-mouse-1 2 0)) text :height 10)
        (declare (ignore f))
        (is = 0 point)))))

(define-test (modes bind-key-for-a-mode)
  (let ((file (merge-pathnames (format nil "tuatara-init-~a.lisp" (random 1000000)) (uiop:temporary-directory))))
    (unwind-protect
         (progn
           (with-open-file (o file :direction :output)
             (format o "(bind-key \"C-c e\" 'end-of-buffer :map 'csv-mode)~%"))
           (let ((tuatara.defaults:*config-file* file))
             ;; bound in CSV buffers ...
             (let ((f (csv-spans "C-c e" (format nil "a,b~%c,d~%"))))
               (true (tuatara.frontend.fake:fake-frame-showing f "L3 C0")))
             ;; ... not elsewhere
             (multiple-value-bind (f editor) (run-keys "C-c e" :text "abc")
               (is = 0 (session-point editor))
               (true (tuatara.frontend.fake:fake-frame-showing f "C-c e is undefined")))))
      (ignore-errors (delete-file file))
      (tuatara-user:unbind-key "C-c e" :map 'tuatara.defaults::csv-mode))))

(define-test (modes methods-documented)
  ;; every method the modes and the editing commands define for a kind of its own says what it does
  (let ((root (namestring (asdf:system-relative-pathname :tuatara "lisp/")))
        (missing '()))
    (do-symbols (s :tuatara.defaults)
      (when (and (fboundp s) (typep (fdefinition s) 'generic-function))
        (dolist (method (sb-mop:generic-function-methods (fdefinition s)))
          (let* ((source (ignore-errors (sb-introspect:find-definition-source method)))
                 (file (and source (sb-introspect:definition-source-pathname source)))
                 (path (and file (namestring file))))
            (when (and path (search root path)
                       ;; a slot's reader, and a generic function's own default, are said by what defines them
                       (not (typep method 'sb-mop:standard-accessor-method))
                       (notany (lambda (class) (subtypep class 'condition)) (sb-mop:method-specializers method))
                       (notevery (lambda (class) (eq class (find-class t))) (sb-mop:method-specializers method))
                       (let ((inside (subseq path (+ (search root path) (length root)))))
                         (or (not (find #\/ inside))
                             (some (lambda (folder) (eql 0 (search folder inside))) '("modes/" "modal/" "editing/"))))
                       (null (documentation method t)))
              (pushnew (format nil "~(~a~) in ~a" s (file-namestring path)) missing :test #'string=))))))
    (is equal '() missing "undocumented: ~{~a~^; ~}" missing)))

(define-test (modes csv-fields-counted-in-place)
  ;; the column point is in, on a long record, is counted where the text is kept, as the
  ;; scan that colours the fields counts them; a one-column file looks for its separator in
  ;; its first line only, and only when that line changed
  (let ((rope (tuatara.text:string->rope (format nil "a,\"b,c\",d~%\"x~%y\",z,"))))
    (loop for end from 0 to (tuatara.text:rope-bytes rope)
          do (is equal (multiple-value-list (tuatara.defaults::csv-scan rope 0 end 44 0 nil))
                 (cons (first (multiple-value-list (tuatara.defaults::csv-scan rope 0 end 44 0 nil)))
                       (multiple-value-list (tuatara.defaults::csv-count-fields rope 0 end 44 0 nil))))))
  (flet ((consed (length)
           (let ((b (tuatara.buffer:make-buffer "*csv count*"
                                                :text (with-output-to-string (o) (dotimes (i (floor length 4)) (write-string "abc," o))))))
             (unwind-protect
                  (progn
                    (tuatara.modes:set-major-mode b 'tuatara.defaults::csv-mode)
                    (tuatara.defaults::csv-column-words b (tuatara.buffer:buffer-size b))
                    (loop repeat 3 minimize (nth-value 2 (tuatara.bench::measuring
                                                           (tuatara.defaults::csv-column-words b (tuatara.buffer:buffer-size b))))))
               (tuatara.buffer:kill-buffer b)))))
    (let ((per-octet (/ (- (consed 800000) (consed 100000)) 700000.0)))
      (true (< per-octet 0.05) "~,3f octets consed for each octet of the record" per-octet)))
  (let ((b (tuatara.buffer:make-buffer "*csv one column*"
                                       :text (format nil "name~%~a" (make-string 100000 :initial-element #\x)))))
    (unwind-protect
         (progn
           (tuatara.modes:set-major-mode b 'tuatara.defaults::csv-mode)
           (is eq :open (tuatara.buffer:buffer-local b :csv-detecting))
           (tuatara.buffer:buffer-insert b (tuatara.buffer:buffer-size b) "y")
           (let ((consed (loop repeat 3 minimize (nth-value 2 (tuatara.bench::measuring
                                                                (tuatara.buffer:buffer-insert b (tuatara.buffer:buffer-size b) "y"))))))
             (true (< consed 16384) "~:d octets consed by an edit below the first line" consed))
           (tuatara.buffer:buffer-insert b 4 ";x")
           (is = 59 (tuatara.defaults::csv-delimiter b))
           (is eq :done (tuatara.buffer:buffer-local b :csv-detecting)))
      (tuatara.buffer:kill-buffer b))))

(defparameter *records-over-lines*
  (format nil "id,name,place,amount~%1,Aroha,\"~atorohanga,~%Waikato\",12.50~%2,\"Tama \"\"T\"\" Rotorua\",Auckland,7.25~%3,Mere,auckland,19.99~%4,Eve,\"Wellington, NZ\",~%"
          (code-char #x14C))
  "Records of a CSV, one of them a quoted field holding a line break.")

(define-test (modes csv-records-kept-whole)
  ;; keeping or deleting records keeps or deletes a record over two lines whole, tried on
  ;; its whole text, and counts it once
  (let ((aroha (format nil "1,Aroha,\"~atorohanga,~%Waikato\",12.50~%" (code-char #x14C))))
    (dolist (on-disk '(nil t))
      (dolist (crlf '(nil t))
        (flet ((crlf (text) (if crlf (cl-ppcre:regex-replace-all (string #\Newline) text (format nil "~c~c" #\Return #\Newline)) text)))
          (is string= (crlf (format nil "id,name,place,amount~%~a" aroha))
              (list-records-after (format nil "C-x / k n a m e RET ~a RET" (typed-keys "^Aroha")) (crlf *records-over-lines*) :on-disk on-disk :crlf crlf)
              "kept by name~:[~;, on disk~]~:[~;, CRLF~]" on-disk crlf)
          (is string= (crlf (format nil "id,name,place,amount~%~a" aroha))
              (list-records-after (format nil "C-x / k a m o u n t RET ~a RET" (typed-keys "^12")) (crlf *records-over-lines*) :on-disk on-disk :crlf crlf)
              "kept by the column after the line break~:[~;, on disk~]~:[~;, CRLF~]" on-disk crlf)
          (is string= (crlf (format nil "id,name,place,amount~%2,\"Tama \"\"T\"\" Rotorua\",Auckland,7.25~%3,Mere,auckland,19.99~%4,Eve,\"Wellington, NZ\",~%"))
              (list-records-after (format nil "C-x / d p l a c e RET ~a RET" (typed-keys "Waikato")) (crlf *records-over-lines*) :on-disk on-disk :crlf crlf)
              "deleted whole~:[~;, on disk~]~:[~;, CRLF~]" on-disk crlf)))))
  (let ((f (run-keys-in-turns (list (format nil "M-x c s v - m o d e RET C-x / k n a m e RET ~a RET" (typed-keys "^Aroha")) "")
                              :text *records-over-lines* :width 100)))
    (true (tuatara.frontend.fake:fake-frame-showing f "Deleted 3 non-matching records"))))

(define-test (modes csv-records-filtered-whole)
  ;; the column filters and their question read a record over two lines whole, and count it once
  (let ((first-line (format nil "1,Aroha,\"~atorohanga," (code-char #x14C))))
    (dolist (keys '("C-x / f a m o u n t RET ^ 1 2 RET" "C-x / f p l a c e RET W a i k a t o RET"))
      (with-records (f buffer path) ((list keys :tick) :text *records-over-lines*)
        (is equal (list "id,name,place,amount" first-line "Waikato\",12.50") (records-shown buffer) "~a" keys)
        (true (screen-showing f "1 of 4 records") "~a" keys)))
    ;; the first record's value beside each column asked for is its whole value
    (let ((b (tuatara.buffer:make-buffer "*records over lines*" :text *records-over-lines*)))
      (unwind-protect
           (progn
             (tuatara.modes:set-major-mode b 'tuatara.defaults::csv-mode)
             (let ((offered (tuatara.defaults::offered-fields b (tuatara.defaults::mode-record-fields (tuatara.modes:buffer-major-mode b) b))))
               (is equal "12.50" (third (find "amount" offered :key #'first :test #'equal)))))
        (tuatara.buffer:kill-buffer b)))))

(tuatara.modes:define-major-mode test-small-files-mode test-parent-mode
  "A mode for the tests: one that opens only small files."
  :extensions ("tsmall") :largest-file 10)

(define-test (modes too-large-for-its-mode)
  ;; a file larger than its mode opens opens in the nearest parent mode that takes it
  (flet ((mode-of (text)
           (let ((buffer (tuatara.buffer:make-buffer "*too-large*" :text text :file #p"/tmp/notes.tsmall")))
             (unwind-protect
                  (progn (tuatara.modes:set-auto-mode buffer)
                         (type-of (tuatara.modes:buffer-major-mode buffer)))
               (tuatara.buffer:kill-buffer buffer)))))
    (is eq 'test-small-files-mode (mode-of "small"))
    (is eq 'test-parent-mode (mode-of "more than ten octets"))
    (is eq 'test-parent-mode (tuatara.modes:mode-for-size 'test-small-files-mode 11))
    (is eq 'test-small-files-mode (tuatara.modes:mode-for-size 'test-small-files-mode 10))))

(define-test (modes every-mode-with-fields-is-of-records)
  ;; a mode that cuts its records into fields is a file of records, filtered by hiding lines
  (let ((classes (loop for method in (sb-mop:generic-function-methods #'tuatara.defaults::mode-record-fields)
                       for class = (first (sb-mop:method-specializers method))
                       unless (eq class (find-class t))
                         collect class)))
    (true (>= (length classes) 8))
    (dolist (class classes)
      (true (tuatara.defaults::filters-hide-lines-p (make-instance class) nil) "~a" (class-name class)))))
