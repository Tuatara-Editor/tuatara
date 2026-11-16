;;;; test/editing-regressions.lisp --- Editing, modes and buffers held to what was put right.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.tests)

(define-test editing-regressions
  :description "Faults put right in editing, the modes and the buffers stay put right: a pair of
escape keys of the wrong length, a negative count to DEL, sorting a read-only buffer, numbers of
many digits, the marks of list items, the Lisp family's hyphens, the gutter switched off and on,
a link followed from a file opened safely, a file of yours copied over a link, the buffer shown
when none is left, a word list where there are no comments, rules still being made ready, and
work done once instead of again on each call.")

(defun garbage-of (function &optional (times 100))
  "The octets FUNCTION, of no arguments, makes in TIMES calls, the least of
three runs."
  (loop repeat 3
        minimize (let ((before (sb-ext:get-bytes-consed)))
                   (dotimes (i times) (funcall function))
                   (- (sb-ext:get-bytes-consed) before))))

(define-test (editing-regressions negative-count-deletes-forward)
  ;; DEL with a negative count deletes after point, as C-d does
  (let ((tuatara.defaults::*modal-editing* nil))
    (multiple-value-bind (f editor) (run-keys "C-u - 2 DEL" :text "abcd")
      (declare (ignore f))
      (is string= "cd" (session-text editor))
      (is = 0 (session-point editor)))))

(define-test (editing-regressions sorting-refused-read-only)
  ;; a read-only buffer is refused before anything is asked or sorted
  (let ((tuatara.defaults::*modal-editing* nil))
    (dolist (command '("s o r t - l i n e s" "s o r t - r e g e x p - f i e l d s"))
      (multiple-value-bind (f editor)
          (run-keys (format nil "C-x C-q C-x h M-x ~a RET" command) :text (format nil "b~%a~%") :width 80)
        (is string= (format nil "b~%a~%") (session-text editor))
        (true (tuatara.frontend.fake:fake-frame-showing f "Buffer is read-only") "~a" command)))))

(define-test (editing-regressions long-numbers-read-quickly)
  (flet ((number-of (text)
           (let ((octets (to-octets text)))
             (tuatara.defaults::field-number octets 0 (length octets)))))
    ;; as many digits as a double holds, read as before
    (is = (coerce 123456789012345678901234567890 'double-float) (number-of "123456789012345678901234567890"))
    (is = (coerce 12345678901234567890123/100000000000000000000000 'double-float)
        (number-of "0.12345678901234567890123"))
    (is = 2.5d3 (number-of "2.5e3"))
    (is = -1.5d-3 (number-of "-1.5e-3"))
    (is = most-positive-double-float (number-of (format nil "1~a" (make-string 400 :initial-element #\0))))
    ;; tens of thousands of digits cost no more than a few
    (let* ((long (format nil "0.~a" (make-string 40000 :initial-element #\7)))
           (began (get-internal-run-time)))
      (is = 0.7777777777777778d0 (number-of long))
      (true (< (- (get-internal-run-time) began) (* 1/10 internal-time-units-per-second))))))

(define-test (editing-regressions item-marks-taken-alike)
  ;; the signs that mark an item when filled begin one, and a tab may follow them
  (let ((b (tuatara.buffer:make-buffer "*item marks*" :text "")))
    (unwind-protect
         (progn
           (tuatara.defaults::set-major-mode b 'tuatara.defaults::text-mode)
           (dolist (line (list "- word" "· word" "– word" "• word" "1. word" "a) word" "  * word"
                               (format nil "-~cword" #\Tab)))
             (true (tuatara.defaults::list-item-start-p b line) "~s" line))
           (dolist (line (list "-tword" "* heading" "word - word"))
             (false (tuatara.defaults::list-item-start-p b line) "~s" line)))
      (tuatara.buffer:kill-buffer b))))

(define-test (editing-regressions fill-keeps-a-lone-mark-on-its-line)
  ;; a spaced dash in prose never begins a wrapped line, so filling again changes nothing
  (let* ((text (format nil "~{~a ~}bbbb – cc dd ee ff gg hh~%" (loop repeat 13 collect "aaaa")))
         (tuatara.defaults::*modal-editing* nil)
         (tuatara-user:*fill-ask-column* nil))
    (multiple-value-bind (f once) (run-keys "C-u 7 0 M-q" :text text :width 100)
      (declare (ignore f))
      (let ((filled (session-text once)))
        (false (search (format nil "~%– ") filled) "~s" filled)
        (multiple-value-bind (f twice) (run-keys "C-u 7 0 M-q M-q" :text text :width 100)
          (declare (ignore f))
          (is string= filled (session-text twice)))))))

(define-modal-test (editing-regressions insert-escape-keys-of-another-length)
  ;; one character, or none, is no pair: typing goes on as it would without one
  (dolist (keys '("j" ""))
    (let ((tuatara-user:*insert-escape-keys* keys))
      (multiple-value-bind (f editor) (run-keys "i j b" :text "" :width 60 :height 8)
        (declare (ignore f))
        (is string= "jb" (session-text editor) "with ~s" keys)
        (is eq :insert (tuatara.defaults:modal-state (session-buffer editor))))))
  ;; nor does a key typed after the first fail, as the hook of typing it
  (let ((b (tuatara.buffer:make-buffer "*escape keys*" :text ""))
        (tuatara.defaults::*insert-escape-first* (vector nil 0 nil nil 0)))
    (unwind-protect
         (progn
           (setf (tuatara.buffer:buffer-local b 'tuatara.defaults::modal-state) :insert)
           (dolist (keys '("j" ""))
             (let ((tuatara-user:*insert-escape-keys* keys))
               (setf (svref tuatara.defaults::*insert-escape-first* 0) b)
               (false (tuatara.defaults::insert-escape-key-typed b #\b) "with ~s" keys))))
      (tuatara.buffer:kill-buffer b))))

(define-test (editing-regressions insert-escape-keys-of-another-length-refused)
  ;; set by the editor, a value that is no pair is refused, naming the setting, which keeps its value
  (dolist (keys '("j" "" "jkl"))
    (let ((words (handler-case (progn (tuatara.runtime:set-setting 'tuatara-user:*insert-escape-keys* keys) nil)
                   (tuatara.runtime:refusal (e) (princ-to-string e)))))
      (true (search "*insert-escape-keys* is " words) "~s" words)
      (true (search "text of exactly two characters" words) "~s" words)
      (false tuatara-user:*insert-escape-keys*))))

(define-test (editing-regressions lisp-family-alike)
  ;; every mode of the Lisp family joins names with hyphens, as it takes Lisp headings
  (dolist (mode '(tuatara.defaults::lisp-mode tuatara.defaults::racket-mode tuatara.defaults::fennel-mode
                  tuatara.defaults::janet-mode tuatara.defaults::hy-mode tuatara.defaults::clojure-mode))
    (true (tuatara.defaults::mode-hyphen-in-words-p (make-instance mode)) "~a" mode))
  (true (tuatara.defaults::mode-hyphen-in-words-p (make-instance 'tuatara.defaults::css-mode)))
  (false (tuatara.defaults::mode-hyphen-in-words-p (make-instance 'tuatara.defaults::python-mode))))

(define-test (editing-regressions vc-gutter-back-on-for-every-buffer)
  ;; the gutter off and on again marks a buffer shown afterwards, not only the one current then
  (with-test-directory (dir)
    (let ((a (merge-pathnames "a.txt" dir)) (b (merge-pathnames "b.txt" dir)))
      (git dir "init" "-q")
      (git dir "config" "user.email" "t@example.com") (git dir "config" "user.name" "t")
      (git dir "config" "commit.gpgsign" "false")
      (put-file a (format nil "one~%")) (put-file b (format nil "two~%"))
      (git dir "add" "a.txt" "b.txt") (git dir "commit" "-q" "-m" "base")
      (put-file a (format nil "onex~%")) (put-file b (format nil "twox~%"))
      (flet ((tracked (name)
               (lambda ()
                 (let ((buffer (tuatara.buffer:find-buffer name)))
                   (and buffer (tuatara.buffer:buffer-local buffer 'tuatara.defaults::vc-tracked)
                        (null (tuatara.buffer:buffer-local buffer 'tuatara.defaults::vc-refresh)))))))
        (let ((tuatara.defaults::*modal-editing* nil) (back nil))
          (unwind-protect
               (auto-save-session
                (list (format nil "C-x C-f C-a C-k ~a RET" (rimu-typed (namestring a)))
                      (list :until (tracked "a.txt"))
                      (format nil "C-x C-f C-a C-k ~a RET" (rimu-typed (namestring b)))
                      (list :until (tracked "b.txt"))
                      "M-x v c - g u t t e r - m o d e RET M-x v c - g u t t e r - m o d e RET"
                      (list :until (tracked "b.txt"))
                      "C-x b a . t x t RET"
                      (list :until (lambda () (setf back (funcall (tracked "a.txt")))) 20)))
            (setf tuatara.defaults::*vc-gutter-enabled* t))
          (true back))))))

(define-test (editing-regressions pattern-question-with-searching)
  ;; the question for a pattern lives with the searching it serves, the number questions with editing
  (flet ((file-of (name)
           (let ((source (first (sb-introspect:find-definition-sources-by-name name :function))))
             (enough-namestring (sb-introspect:definition-source-pathname source)
                                (asdf:system-source-directory :tuatara)))))
    (is string= "lisp/search/questions.lisp" (file-of 'tuatara.defaults::read-pattern))
    (is string= "lisp/editing/questions.lisp" (file-of 'tuatara.defaults::read-whole-number))))

(define-test (editing-regressions blanks-one-set)
  ;; what a blank between words is, written once: the commands of the editor ask the text's own test,
  ;; a form feed a blank as a space is
  (is eq 'tuatara.text:blank-char-p (find-symbol "BLANK-CHAR-P" :tuatara.defaults))
  (true (tuatara.text:blank-char-p #\Page))
  (is string= "x" (string-trim tuatara.text:*blank-characters* (format nil " ~cx~c " #\Page #\Return))))

(define-test (editing-regressions read-only-refused-in-one-way)
  ;; an edit to a read-only buffer and a command that checks first are refused in the same words
  (let ((b (tuatara.buffer:make-buffer "*read-only refusal*" :text "abc")))
    (unwind-protect
         (flet ((said (function)
                  (handler-case (progn (funcall function) nil)
                    (tuatara.runtime:refusal (e) (princ-to-string e)))))
           (setf (tuatara.buffer:buffer-read-only-p b) t)
           (is string= "Buffer is read-only: *read-only refusal*"
               (said (lambda () (tuatara.buffer:buffer-insert b 0 "x"))))
           (is string= (said (lambda () (tuatara.buffer:buffer-insert b 0 "x")))
               (said (lambda () (tuatara.defaults::check-writable b)))))
      (tuatara.buffer:kill-buffer b))))

(define-test (editing-regressions vc-gutter-a-setting)
  ;; whether the gutter shows git's marks is a setting of the init file, said with the values it takes
  (is eq :external (nth-value 1 (find-symbol "*VC-GUTTER-ENABLED*" :tuatara-user)))
  (is string= "t, nil" (tuatara.runtime:setting-values-words 'tuatara-user:*vc-gutter-enabled*)))

(define-test (editing-regressions markdown-link-opens-safely-from-a-safe-file)
  ;; a link followed in a Markdown file opened safely opens its target safely too
  (with-test-directory (dir)
    (let ((a (namestring (merge-pathnames "a.md" dir))) (b (namestring (merge-pathnames "b.md" dir)))
          (safe :unseen))
      (put-file a (format nil "[next](b.md)~%")) (put-file b (format nil "# B~%"))
      (auto-save-session
       (list (list :until (lambda ()
                            (let ((buffer (tuatara.defaults::visit-path a :safe t)))
                              (setf (tuatara.buffer:buffer-point buffer) 1)
                              (tuatara.defaults::markdown-follow-link buffer))
                            t))
             (list :until (lambda ()
                            (let ((target (tuatara.buffer:find-buffer "b.md")))
                              (when target
                                (setf safe (tuatara.io:safe-visit-p target))
                                t)))
                   10)))
      (is eq t safe))))

(define-test (editing-regressions own-file-never-written-through-a-link)
  ;; a link where a file of yours would go, even one leading nowhere, is left as it is
  (with-test-directory (dir)
    (let* ((place (namestring dir))
           (path (concatenate 'string place "kept.snippet"))
           (target (concatenate 'string place "nowhere")))
      (sb-posix:symlink target path)
      (auto-save-session
       (list (list :until (lambda ()
                            (tuatara.defaults::copy-own-file (list place) nil "kept.snippet" "text" "snippet")
                            t))))
      (is string= target (sb-posix:readlink path))
      (false (probe-file target)))))

(define-test (editing-regressions startup-buffer-of-your-own)
  ;; a buffer of your own named as the one to start with is made, empty, when none is left
  (let ((tuatara-user:*auto-scratch-buffers* '("*notes*"))
        (tuatara-user:*startup-buffer* "*notes*")
        (made nil))
    (unwind-protect
         (progn
           (setf made (tuatara.command:host-new-buffer tuatara.runtime:*host* nil))
           (true made)
           (is string= "*notes*" (tuatara.buffer:buffer-name made))
           (is = 0 (tuatara.buffer:buffer-size made)))
      (when made (tuatara.buffer:kill-buffer made)))))

(define-test (editing-regressions word-list-refused-calmly)
  ;; where there are no comments to keep a word list in, it is refused, not failed
  (let ((b (tuatara.buffer:make-buffer "*word list*" :text (format nil "a,b~%"))))
    (unwind-protect
         (progn
           (tuatara.defaults::set-major-mode b 'tuatara.defaults::csv-mode)
           (fail (tuatara.defaults::add-local-word b "Vrbltz") 'tuatara.runtime::refusal))
      (tuatara.buffer:kill-buffer b))))

(define-test (editing-regressions rules-on-their-way-said)
  ;; a key needing rules still being compiled says so while it waits, no longer than the bound
  (let* ((language (tuatara.defaults::find-language 'rust))
         (kept (tuatara.defaults::language-indent-query language)))
    (tuatara.syntax:kept-helper :rules)
    (setf (tuatara.defaults::language-indent-query language) nil)
    (sb-int:encapsulate 'tuatara.syntax:compile-query 'slow
                        (lambda (function grammar source &rest more)
                          (when (equal (first more) "indentation") (sleep 3))
                          (apply function grammar source more)))
    (unwind-protect
         (with-modal-layer
           ;; the wait is the one of a command's work (WAIT-INTERRUPTIBLY, which C-g ends), bounded
           (let ((tuatara.defaults::*rules-editor-wait* 1)
                 (began (get-internal-real-time)))
             (with-file-session ((f editor) "C-e i RET" (format nil "fn f() {~%}~%") "rs")
               (true (tuatara.frontend.fake:fake-frame-showing f "rules of rust are being made ready"))
               (true (< (tuatara.runtime:seconds-since began) 2.5)))))
      (sb-int:unencapsulate 'tuatara.syntax:compile-query 'slow)
      (loop repeat 100 until (tuatara.syntax:query-p (tuatara.defaults::language-indent-query language))
            do (sleep 0.1))
      (unless (tuatara.syntax:query-p (tuatara.defaults::language-indent-query language))
        (setf (tuatara.defaults::language-indent-query language) kept)))))

(define-test (editing-regressions conflicts-found-by-halves)
  ;; going to the next conflict looks at a few of them, not at every one
  (with-test-directory (dir)
    (let ((text (with-output-to-string (out)
                  (dotimes (i 256) (format out "line ~d~%<<<<<<< a~%x~%=======~%y~%>>>>>>> b~%" i))))
          (calls nil))
      (conflict-session dir text
                        (list (list :until (lambda ()
                                             (setf calls (nth-value 1 (counting-calls
                                                                          (tuatara.defaults::conflict-start-at)
                                                                        (tuatara.defaults::conflict-go 1))))
                                             t))))
      (true (and calls (< calls 20)) "~a calls" calls))))

(define-test (editing-regressions patterns-compiled-once)
  ;; the patterns asked word by word, and on each link followed, are not compiled again each time
  (true (< (garbage-of (lambda () (cl-ppcre:scan tuatara.defaults::*laid-out-node-scanner* "paragraph"))) 20000))
  (true (< (garbage-of (lambda () (tuatara.defaults::markdown-link-at "See [a](b.md) here" 6))) 100000))
  (true (< (garbage-of (lambda () (cl-ppcre:scan tuatara.defaults::*item-head-re* "  - [ ] "))) 100000)))

(define-test (editing-regressions terminal-colours-kept)
  ;; the sixteen colours of a terminal are worked out once until the faces change
  (tuatara.defaults::theme-terminal-colour 1)
  (is = 0 (nth-value 1 (counting-calls (tuatara.display:find-theme)
                         (dotimes (i 100) (tuatara.defaults::theme-terminal-colour (mod i 16)))))))

(define-test (editing-regressions colouring-asked-without-a-mode)
  ;; how a language is coloured, asked with no mode at hand, makes no mode
  (let ((language (tuatara.defaults::find-language 'python)))
    (true (< (garbage-of (lambda () (tuatara.defaults::language-colouring language)) 1000) 8000))))

(define-test (editing-regressions line-number-from-the-index)
  ;; the number of a line is read from the text's index, not by counting newlines
  (let ((b (tuatara.buffer:make-buffer "*line numbers*" :text (format nil "a~%b~%c~%d~%"))))
    (unwind-protect
         (let ((tuatara.command::*current-buffer* b))
           (tuatara.buffer:ensure-indexed b)
           (multiple-value-bind (number calls)
               (counting-calls (tuatara.text:rope-count-newlines) (tuatara-user:line-number-at-pos 5))
             (is = 3 number)
             (is = 0 calls)))
      (tuatara.buffer:kill-buffer b))))

(define-test (editing-regressions top-level-form-found-without-a-copy)
  (let ((b (tuatara.buffer:make-buffer "*forms*" :text (format nil "(a) ;; (no)~%#| (no) |# (b)~%(c)~%"))))
    (unwind-protect
         (progn
           (is = 0 (tuatara.defaults::top-level-form-start b 0))
           (is = (search "(b)" (tuatara.buffer:buffer-string b)) (tuatara.defaults::top-level-form-start b 1))
           (is = (search "(c)" (tuatara.buffer:buffer-string b)) (tuatara.defaults::top-level-form-start b 2))
           (false (tuatara.defaults::top-level-form-start b 3)))
      (tuatara.buffer:kill-buffer b)))
  ;; the first form of a large file is found without reading the rest
  (let ((b (tuatara.buffer:make-buffer "*many forms*"
                                       :text (with-output-to-string (out) (dotimes (i 200000) (write-string "(x) " out))))))
    (unwind-protect
         (true (< (garbage-of (lambda () (tuatara.defaults::top-level-form-start b 0)) 3) 300000))
      (tuatara.buffer:kill-buffer b))))

(define-test (editing-regressions latex-preamble-kept-while-typing-below)
  ;; typing in the body leaves the preamble as it was read
  (let ((b (tuatara.buffer:make-buffer "*latex*" :text (format nil "\\documentclass{article}~%\\usepackage{x}~%~
                                                                  \\begin{document}~%Text.~%\\end{document}~%"))))
    (unwind-protect
         (multiple-value-bind (body preamble) (tuatara.defaults::latex-preamble b)
           (tuatara.buffer:buffer-insert b (- (tuatara.buffer:buffer-size b) 15) "More. ")
           (multiple-value-bind (values calls)
               (counting-calls (tuatara.buffer:buffer-string)
                 (multiple-value-list (tuatara.defaults::latex-preamble b)))
             (is equal (list body preamble) values)
             (is = 0 calls))
           ;; a change in the preamble reads it again
           (tuatara.buffer:buffer-insert b 0 (format nil "% note~%"))
           (is equal (list (+ body 7) preamble) (multiple-value-list (tuatara.defaults::latex-preamble b))))
      (tuatara.buffer:kill-buffer b))))
