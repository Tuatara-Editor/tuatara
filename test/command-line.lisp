;;;; test/command-line.lisp --- Files opened at a line, and two files compared, from the command line.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.tests)

(define-test command-line
  :description "The command line's +LINE[:COLUMN] before a file and --diff OLD NEW, read by the program's own option reader and carried out in a session: the line a file opens at, a line past the end, a file named +5 after --, a missing file to compare, and a file in an evidence folder opened at a line staying a safe visit.")

(defun session-from-arguments (arguments &key (directory (uiop:getcwd)) (until (constantly t)) (seconds 20) keys input)
  "Run an editor session started with the command-line ARGUMENTS, as the program
starts one (READ-OPTIONS, then STARTUP-FROM, reading INPUT, a descriptor, for -)
from DIRECTORY, going on until UNTIL, a function of no arguments, is true or
SECONDS pass, and then typing KEYS (QUEUE-TEST-KEYS). Returns the fake
frontend, the editor, and the options read."
  (let ((options (tuatara.terminal::read-options arguments :directory (namestring directory)))
        (f (tuatara.frontend.fake:make-fake-frontend :width 100 :height 12))
        (began (get-internal-real-time)))
    (when (getf options :error) (return-from session-from-arguments (values nil nil options)))
    (let ((scratch (tuatara.buffer:find-buffer "*scratch*")))
      (when scratch (tuatara.buffer:kill-buffer scratch)))
    (tuatara.buffer:make-buffer "*scratch*" :text "")
    (labels ((check ()
               (cond ((or (funcall until)
                          (> (- (get-internal-real-time) began) (* seconds internal-time-units-per-second)))
                      (when keys (queue-test-keys f keys)))
                     (t (sleep 0.02)
                        (tuatara.frontend.fake:fake-queue-action f #'check)))))
      (tuatara.frontend.fake:fake-queue-action f #'check))
    (uiop:with-current-directory (directory)
      (multiple-value-bind (files start) (if input
                                             (tuatara.terminal::startup-from options :input input)
                                             (tuatara.terminal::startup-from options))
        (values f
                (tuatara.command:run-editor f :files files :start start :setup #'tuatara.defaults:install-defaults
                                              :buffer (tuatara.buffer:find-buffer "*scratch*"))
                options)))))

(defun shown-buffer (editor)
  "The buffer in the selected window of EDITOR."
  (tuatara.display:window-buffer (tuatara.command:editor-window editor)))

(defun point-in (path)
  "The line (from 1) and the column (from 0) point is at in the buffer visiting PATH."
  (let ((buffer (tuatara.defaults::buffer-visiting path)))
    (when buffer
      (let ((point (tuatara.buffer:buffer-point buffer)))
        (list (1+ (tuatara.buffer:buffer-line-number buffer point)) (tuatara.buffer:buffer-column buffer point))))))

(defun lines-saying-their-number (count)
  "COUNT lines of text, each saying its number."
  (format nil "~{~a~%~}" (loop for n from 1 to count collect (format nil "line number ~d" n))))

(define-test (command-line opens-at-a-line)
  (with-test-directory (dir)
    (let ((a (namestring (put-file (merge-pathnames "a.txt" dir) (lines-saying-their-number 20))))
          (b (namestring (put-file (merge-pathnames "b.txt" dir) (lines-saying-their-number 20)))))
      (session-from-arguments (list "+7" a) :directory dir)
      (is equal '(7 0) (point-in a))
      (kill-buffers-visiting dir)
      ;; a column, counted in characters from 1
      (session-from-arguments (list "+7:6" a) :directory dir)
      (is equal '(7 5) (point-in a))
      (kill-buffers-visiting dir)
      ;; each file has its own, and the first is the one shown
      (multiple-value-bind (f editor) (session-from-arguments (list "+3" a "+12:2" b) :directory dir)
        (is equal '(3 0) (point-in a))
        (is equal '(12 1) (point-in b))
        (is string= a (tuatara.io:native (tuatara.buffer:buffer-file (tuatara.display:window-buffer (tuatara.command:editor-window editor)))))
        (true (tuatara.frontend.fake:fake-frame-showing f "line number 3")))
      (kill-buffers-visiting dir)
      ;; a file with none is opened at its start
      (session-from-arguments (list a "+5" b) :directory dir)
      (is equal '(1 0) (point-in a))
      (is equal '(5 0) (point-in b)))))

(define-test (command-line line-past-the-end)
  (with-test-directory (dir)
    (let ((a (namestring (put-file (merge-pathnames "a.txt" dir) (lines-saying-their-number 20)))))
      (let ((f (session-from-arguments (list "+500" a) :directory dir)))
        (is equal '(21 0) (point-in a))
        (true (tuatara.frontend.fake:fake-frame-showing f "has no line 500: at its last line")))
      (kill-buffers-visiting dir)
      ;; a column past the line's end stays at its end
      (session-from-arguments (list "+2:99" a) :directory dir)
      (is equal '(2 13) (point-in a)))))

(define-test (command-line wrongly-given)
  (with-test-directory (dir)
    (let ((a (namestring (put-file (merge-pathnames "a.txt" dir) "x"))))
      (flet ((error-of (&rest arguments)
               (getf (tuatara.terminal::read-options arguments :directory (namestring dir)) :error)))
        (true (search "needs the file it is for" (error-of "+5")))
        (true (search "needs the file it is for" (error-of a "+5")))
        (true (search "no file between" (error-of "+5" "+6" a)))
        (false (error-of "+" a) "a lone + is a file's name")
        (false (error-of "+x" "+5x"))
        (true (search "--diff needs two files" (error-of "--diff" a)))
        (true (search "--diff takes the two files" (error-of "--diff" a a a))))
      ;; said plainly by the program, which then does not start
      (let ((said (make-string-output-stream)))
        (let ((*error-output* said))
          (is = 2 (tuatara.terminal:main (list "+5"))))
        (true (search "tuatara: +5 needs the file it is for after it" (get-output-stream-string said)))))))

(define-test (command-line plus-after-two-dashes-is-a-file)
  (with-test-directory (dir)
    (let ((plus (namestring (put-file (merge-pathnames "+5" dir) (lines-saying-their-number 3)))))
      (declare (ignorable plus))
      (let ((options (tuatara.terminal::read-options (list "--" "+5") :directory (namestring dir))))
        (is equal '("+5") (getf options :files)))
      (uiop:with-current-directory (dir)
        ;; with the file there, +5 alone is not guessed at
        (true (search "+5 is a file here too" (getf (tuatara.terminal::read-options (list "+5" "+5") :directory (namestring dir)) :error)))
        ;; opened as the file it is
        (session-from-arguments (list "--" "+5") :directory dir)
        (is equal '(1 0) (point-in plus))
        (kill-buffers-visiting dir)
        (multiple-value-bind (f editor) (session-from-arguments (list "+2" "./+5") :directory dir)
          (declare (ignore f))
          (let ((buffer (tuatara.display:window-buffer (tuatara.command:editor-window editor))))
            (is = 1 (tuatara.buffer:buffer-line-number buffer (tuatara.buffer:buffer-point buffer)))))))))

(define-test (command-line files-compared)
  (with-test-directory (dir)
    (let ((old (namestring (put-file (merge-pathnames "old.txt" dir) (format nil "one~%two~%"))))
          (new (namestring (put-file (merge-pathnames "new.txt" dir) (format nil "one~%three~%")))))
      (flet ((diff-text ()
               (let ((buffer (tuatara.buffer:find-buffer "*Diff: old.txt and new.txt*")))
                 (and buffer (tuatara.buffer:buffer-string buffer)))))
        (multiple-value-bind (f editor)
            (session-from-arguments (list "--diff" old new) :directory dir
                                    :until (lambda () (let ((text (diff-text))) (and text (search "+three" text)))))
          (let ((text (diff-text)))
            (true text)
            (true (search "-two" text))
            (true (search "+three" text)))
          ;; the comparison fills the screen, in the one window
          (is = 1 (length (tuatara.command:editor-windows editor)))
          (is string= "*Diff: old.txt and new.txt*"
              (tuatara.buffer:buffer-name (tuatara.display:window-buffer (tuatara.command:editor-window editor))))
          (true (tuatara.frontend.fake:fake-frame-showing f "+three")))
        (tuatara.buffer:kill-buffer (tuatara.buffer:find-buffer "*Diff: old.txt and new.txt*"))))))

(define-test (command-line missing-file-to-compare)
  (with-test-directory (dir)
    (let ((old (namestring (put-file (merge-pathnames "old.txt" dir) "one")))
          (gone (namestring (merge-pathnames "gone.txt" dir))))
      (dolist (arguments (list (list "--diff" old gone) (list "--diff" gone old)))
        (let ((said (make-string-output-stream)))
          (let ((*error-output* said))
            (is = 2 (tuatara.terminal:main arguments)))
          (true (search (format nil "tuatara: ~a is not there: nothing to compare" gone) (get-output-stream-string said))))))))

(define-test (command-line evidence-file-at-a-line-stays-a-safe-visit)
  (with-case-folder (root evidence work)
    (with-fresh-recent-files
      (with-evidence-folders (`(,(concatenate 'string root "cases/*/evidence/")))
        (let ((path (concatenate 'string evidence "lines.txt")))
          (put-file path (lines-saying-their-number 20))
          (multiple-value-bind (f editor) (session-from-arguments (list "+9:3" path) :directory root)
            (declare (ignore f))
            (let ((buffer (tuatara.display:window-buffer (tuatara.command:editor-window editor))))
              (true (tuatara.io:safe-visit-p buffer))
              (true (tuatara.buffer:buffer-local buffer :read-only))
              (is equal '(9 2) (point-in path))
              (is string= (lines-saying-their-number 20) (file-text path))
              (false (tuatara.buffer:buffer-modified-p buffer)))))))))

(define-test (command-line missing-side-of-a-difftool-is-empty)
  ;; git hands /dev/null for a file added or deleted
  (with-test-directory (dir)
    (let ((new (namestring (put-file (merge-pathnames "new.txt" dir) (format nil "one~%")))))
      (session-from-arguments (list "--diff" "/dev/null" new) :directory dir
                              :until (lambda () (let ((b (tuatara.buffer:find-buffer "*Diff: null and new.txt*")))
                                                  (and b (search "+one" (tuatara.buffer:buffer-string b))))))
      (let ((b (tuatara.buffer:find-buffer "*Diff: null and new.txt*")))
        (true b)
        (when b
          (true (search "+one" (tuatara.buffer:buffer-string b)))
          (tuatara.buffer:kill-buffer b))))))

(define-test (command-line safe-visit-asked-for)
  (with-test-directory (dir)
    (let ((a (namestring (put-file (merge-pathnames "a.txt" dir) (lines-saying-their-number 5)))))
      (dolist (flag '("--safe" "-R"))
        (multiple-value-bind (f editor) (session-from-arguments (list flag "+3" a) :directory dir)
          (declare (ignore f))
          (let ((buffer (shown-buffer editor)))
            (true (tuatara.io:safe-visit-p buffer))
            (true (tuatara.buffer:buffer-local buffer :read-only))
            (is equal '(3 0) (point-in a))))
        (kill-buffers-visiting dir))
      ;; without it the same file is an ordinary one
      (multiple-value-bind (f editor) (session-from-arguments (list a) :directory dir)
        (declare (ignore f))
        (false (tuatara.io:safe-visit-p (shown-buffer editor))))
      (kill-buffers-visiting dir)
      (is equal (lines-saying-their-number 5) (file-text a)))))

(define-test (command-line standard-input-opened-safely)
  (with-test-directory (dir)
    (multiple-value-bind (reader writer) (sb-posix:pipe)
      (let ((text (format nil "(delete-file \"x\")~%second line~%")))
        (with-open-file (out (format nil "/proc/self/fd/~d" writer) :direction :output :if-exists :append
                                                                    :external-format :utf-8)
          (write-string text out))
        (sb-posix:close writer)
        (multiple-value-bind (f editor)
            (session-from-arguments (list "-") :directory dir :input reader
                                    :until (lambda () (let ((b (tuatara.buffer:find-buffer "*stdin*")))
                                                        (and b (search "second line" (tuatara.buffer:buffer-string b))))))
          (let ((buffer (shown-buffer editor)))
            (is string= "*stdin*" (tuatara.buffer:buffer-name buffer))
            (is string= text (tuatara.buffer:buffer-string buffer))
            (true (tuatara.io:safe-visit-p buffer))
            (true (tuatara.buffer:buffer-local buffer :read-only))
            (false (tuatara.buffer:buffer-file buffer))
            (false (tuatara.buffer:buffer-modified-p buffer))
            (true (tuatara.frontend.fake:fake-frame-showing f "second line"))))
        (sb-posix:close reader))
      (ignore-errors (tuatara.buffer:kill-buffer (tuatara.buffer:find-buffer "*stdin*"))))))

(define-test (command-line standard-input-asked-for)
  (with-test-directory (dir)
    (is equal '((:standard-input)) (getf (tuatara.terminal::read-options (list "-") :directory (namestring dir)) :files))
    ;; a file named - is not guessed at, and after -- it is a file
    (put-file (merge-pathnames "-" dir) "x")
    (true (search "- is a file here too" (getf (tuatara.terminal::read-options (list "-") :directory (namestring dir)) :error)))
    (true (search "- is a file here too" (getf (tuatara.terminal::read-options (list "a" "-") :directory (namestring dir)) :error)))
    (is equal '("-") (getf (tuatara.terminal::read-options (list "--" "-") :directory (namestring dir)) :files))
    ;; the terminal itself as the input is refused, plainly
    (when (tuatara.terminal::terminal-p 0)
      (let ((said (make-string-output-stream)))
        (let ((*error-output* said)) (is = 2 (tuatara.terminal:main (list "-"))))
        (true (search "reads standard input" (get-output-stream-string said)))))))

(define-test (command-line hex-and-tutor)
  (with-test-directory (dir)
    (let ((a (namestring (put-file (merge-pathnames "a.txt" dir) (lines-saying-their-number 5)))))
      (multiple-value-bind (f editor) (session-from-arguments (list "--hex" a) :directory dir)
        (declare (ignore f))
        (true (tuatara.display:hex-view-p (shown-buffer editor))))
      (kill-buffers-visiting dir)
      (multiple-value-bind (f editor) (session-from-arguments (list "--tutor") :directory dir)
        (declare (ignore f))
        (is string= "*Tutor*" (tuatara.buffer:buffer-name (shown-buffer editor))))
      (let ((options (tuatara.terminal::read-options (list "--tutor" a) :directory (namestring dir))))
        (true (search "--tutor takes no files" (getf options :error)))))))

(define-test (command-line started-bare)
  ;; -Q: no init file, a state folder of its own that goes with the session, the real one untouched
  (with-test-directory (dir)
    (let* ((real (ensure-directories-exist (merge-pathnames "state/" dir)))
           (a (namestring (put-file (merge-pathnames "a.txt" dir) "x")))
           (was-config tuatara.defaults::*config-file*)
           (was-state tuatara-user:*state-directory*)
           (tuatara.command:*exit-functions* tuatara.command:*exit-functions*)
           (inside nil))
      (setf tuatara-user:*state-directory* real)
      (unwind-protect
           (progn
             (put-file (merge-pathnames "keep.txt" real) "kept")
             (let ((before (mapcar #'namestring (directory (merge-pathnames "*.*" real)))))
               (dotimes (i 15)
                 (session-from-arguments (list "-Q" a) :directory dir
                                         :until (lambda ()
                                                  (setf inside tuatara-user:*state-directory*)
                                                  t))
                 (false (probe-file inside) "its own state folder is gone with the session: ~s"
                        (and (probe-file inside) (directory (merge-pathnames "**/*.*" inside)))))
               (is equal before (mapcar #'namestring (directory (merge-pathnames "*.*" real)))
                   "the real state folder is as it was")
               (is string= "kept" (file-text (merge-pathnames "keep.txt" real))))
             (is string= "no-init-file.lisp" (file-namestring tuatara.defaults::*config-file*))
             (is eq :none tuatara.defaults::*init-file-outcome*)
             (true inside)
             (false (equal (namestring inside) (namestring real)))
             (false (probe-file inside) "its own state folder is gone with the session: ~s"
                   (and (probe-file inside) (directory (merge-pathnames "**/*.*" inside)))))
        (setf tuatara.defaults::*config-file* was-config
              tuatara-user:*state-directory* was-state)))))

(define-test (command-line three-files-merged)
  (with-test-directory (dir)
   (let ((tuatara.command:*exit-functions* tuatara.command:*exit-functions*))
    (flet ((file (name text) (namestring (put-file (merge-pathnames name dir) text))))
      (let* ((base (file "base.txt" (format nil "one~%two~%three~%")))
             (local (file "local.txt" (format nil "one~%TWO~%three~%")))
             (remote (file "remote.txt" (format nil "one~%2~%three~%")))
             (left (format nil "one~%<<<<<<< HEAD~%TWO~%=======~%2~%>>>>>>> side~%three~%"))
             (merged (file "merged.txt" left)))
        (flet ((merge-text () (let ((b (tuatara.defaults::buffer-visiting merged))) (and b (tuatara.buffer:buffer-string b)))))
          (flet ((merging (&optional keys)
                   (multiple-value-bind (f editor)
                       (session-from-arguments (list "--merge" local base remote merged) :directory dir :keys keys
                                               :until (lambda ()
                                                        (let ((b (tuatara.defaults::buffer-visiting merged)))
                                                          (and b (search "<<<<<<< local" (tuatara.buffer:buffer-string b))
                                                               (plusp (length (tuatara.defaults::conflict-list b)))))))
                     (declare (ignore f))
                     editor)))
            ;; quit with the conflict there: the code says it is not done, and MERGED is as git left it
            (merging)
            (let ((text (merge-text)))
              (true (search "<<<<<<< local" text))
              (true (search "TWO" text))
              (true (search "||||||| base" text))
              (true (search ">>>>>>> remote" text)))
            (is = 1 tuatara.command:*exit-code*)
            (is string= left (file-text merged))
            ;; the conflict is one, though the file held git's own marked before
            (is = 1 (length (tuatara.defaults::conflict-list (tuatara.defaults::buffer-visiting merged))))
            (kill-buffers-visiting dir)
            ;; resolved and saved: 0, and MERGED holds the merge
            (merging '("C-c ^ n C-c ^ l C-x C-s"))
            (is = 0 tuatara.command:*exit-code*)
            (is string= (format nil "one~%2~%three~%") (file-text merged)))))))))

(define-test (command-line merge-wrongly-given)
  (with-test-directory (dir)
    (let ((a (namestring (put-file (merge-pathnames "a.txt" dir) "x"))))
      (true (search "--merge needs four files" (getf (tuatara.terminal::read-options (list "--merge" a a a) :directory (namestring dir)) :error)))
      (true (search "--merge takes the four files" (getf (tuatara.terminal::read-options (list "--merge" a a a a a) :directory (namestring dir)) :error)))
      (let ((said (make-string-output-stream)))
        (let ((*error-output* said))
          (is = 2 (tuatara.terminal:main (list "--merge" "/nonexistent/l" a a a))))
        (true (search "/nonexistent/l is not there: nothing to merge" (get-output-stream-string said)))))))

(define-test (command-line options-among-the-files)
  (with-test-directory (dir)
    (let ((a (namestring (put-file (merge-pathnames "a.txt" dir) (lines-saying-their-number 5))))
          (b (namestring (put-file (merge-pathnames "b.txt" dir) (lines-saying-their-number 5)))))
      (flet ((read-it (&rest arguments) (tuatara.terminal::read-options arguments :directory (namestring dir))))
        ;; an option after the files is an option; --safe holds for every file, +LINE for the next
        (let ((read (read-it a "--safe" "+3" b)))
          (false (getf read :error))
          (true (getf read :safe))
          (is equal (list a (list b 3 nil)) (getf read :files)))
        (true (getf (read-it a "-R") :safe))
        (true (getf (read-it a b "--hex") :hex))
        (is equal (list a) (getf (read-it a "-Q") :files))
        ;; what starts with a dash and is no option is refused, never taken as a file
        (true (search "-x is not an option" (getf (read-it a "-x") :error)))
        (true (search "--frobnicate is not an option" (getf (read-it "--frobnicate" a) :error)))
        ;; after -- everything is a file
        (is equal (list a "-x" "--safe") (getf (read-it a "--" "-x" "--safe") :files))
        (false (getf (read-it a "--" "--safe") :safe))
        (is equal (list a) (getf (read-it "--" a) :files)))
      ;; end to end: both files open safely though --safe came last
      (multiple-value-bind (f editor) (session-from-arguments (list a b "--safe") :directory dir)
        (declare (ignore f editor))
        (true (tuatara.io:safe-visit-p (tuatara.defaults::buffer-visiting a)))
        (true (tuatara.io:safe-visit-p (tuatara.defaults::buffer-visiting b)))))))

(define-test (command-line one-file-one-buffer)
  ;; a file reached by x, ./x or through a link and .. is one buffer; the file the .. leads to
  ;; lexically, with the link in the way, is another
  (with-test-directory (dir)
    (let* ((root (namestring dir))
           (x (namestring (put-file (merge-pathnames "x" dir) "decoy")))
           (real (namestring (put-file (progn (ensure-directories-exist (merge-pathnames "real/sub/" dir))
                                              (merge-pathnames "real/x" dir))
                                       "real"))))
      (sb-posix:symlink (concatenate 'string root "real/sub") (concatenate 'string root "a"))
      (let ((plain (tuatara.io:visit-file x))
            (dotted (tuatara.io:visit-file (concatenate 'string root "./x")))
            (doubled (tuatara.io:visit-file (concatenate 'string root "/./x")))
            (through (tuatara.io:visit-file (concatenate 'string root "a/../x")))
            (true-real (tuatara.io:visit-file real)))
        (is eq plain dotted)
        (is eq plain doubled)
        (is eq through true-real)
        (false (eq through plain))
        (is string= "real" (tuatara.buffer:buffer-string through))
        (is string= real (tuatara.io:native (tuatara.buffer:buffer-file through)))
        (is string= x (tuatara.io:native (tuatara.buffer:buffer-file dotted)))
        (is eq plain (tuatara.defaults::buffer-visiting (concatenate 'string root "./x"))))
      ;; through the program's own start, one file named twice is one buffer
      (uiop:with-current-directory (dir)
        (multiple-value-bind (f editor) (session-from-arguments (list "./x" "x") :directory dir)
          (declare (ignore f editor))
          (is = 1 (count x tuatara.buffer:*buffers* :key (lambda (b) (let ((file (tuatara.buffer:buffer-file b))) (and file (tuatara.io:native file))))
                                                    :test #'equal)))))))

(define-test (command-line hard-link-never-hands-evidence-over-as-editable)
  ;; a file with two names, one in an evidence folder: whichever is opened first, the evidence is never
  ;; given as an editable buffer, and the other name is shown the safe buffer
  (with-case-folder (root evidence work)
    (with-fresh-recent-files
      (with-evidence-folders (`(,(concatenate 'string root "cases/*/evidence/")))
        (let ((ev (concatenate 'string evidence "y.org")) (wk (concatenate 'string work "x.org")))
          (put-file ev (format nil "* kept as found~%"))
          (sb-posix:link ev wk)
          ;; the work name first
          (session-from-arguments (list wk ev) :directory root)
          (let ((e (tuatara.defaults::buffer-visiting ev)))
            (true (tuatara.io:safe-visit-p e))
            (true (tuatara.buffer:buffer-local e :read-only))
            (is string= (tuatara.io:native ev) (tuatara.io:native (tuatara.buffer:buffer-file e))))
          (kill-buffers-visiting root)
          ;; the evidence name first: the work name finds that buffer, not an editable one
          (multiple-value-bind (f editor) (session-from-arguments (list ev wk) :directory root)
            (let ((e (tuatara.defaults::buffer-visiting ev)) (w (tuatara.defaults::buffer-visiting wk)))
              (true (tuatara.io:safe-visit-p e))
              (is eq e w)
              (true (tuatara.buffer:buffer-local w :read-only))
              (true (tuatara.frontend.fake:fake-frame-showing f "is the same file as"))
              (true (tuatara.io:safe-visit-p (shown-buffer editor)))))
          (is string= (format nil "* kept as found~%") (file-text ev)))))))

(define-test (command-line keys-need-a-terminal)
  (true (tuatara.terminal::keyboard-available-p :input-terminal t :tty "/nonexistent/tty"))
  (false (tuatara.terminal::keyboard-available-p :input-terminal nil :tty "/nonexistent/tty"))
  (true (tuatara.terminal::keyboard-available-p :input-terminal nil :tty "/dev/null"))
  ;; started with input that is no terminal and no terminal to read keys from: refused, plainly
  (unless (or (tuatara.terminal::terminal-p 0) (tuatara.terminal::keyboard-available-p))
    (with-test-directory (dir)
      (let ((a (namestring (put-file (merge-pathnames "a.txt" dir) "x"))) (said (make-string-output-stream)))
        (let ((*error-output* said))
          (is = 2 (tuatara.terminal:main (list a))))
        (true (search "there is none to read keys from" (get-output-stream-string said)))))))

(define-test (command-line merge-with-a-stray-marker)
  ;; a marker line in the text is not resolved; a ======= line alone (a heading underlined) is no marker;
  ;; the text, common to the three files, comes through the merge into the file
  (flet ((merged-and-saved (tail-of-merged)
           (with-test-directory (dir)
             (let ((tuatara.command:*exit-functions* tuatara.command:*exit-functions*))
               (flet ((file (name text) (namestring (put-file (merge-pathnames name dir) text))))
                 (let* ((base (file "base.txt" (format nil "one~%two~%three~%~a" tail-of-merged)))
                        (local (file "local.txt" (format nil "one~%TWO~%three~%~a" tail-of-merged)))
                        (remote (file "remote.txt" (format nil "one~%2~%three~%~a" tail-of-merged)))
                        (merged (file "merged.txt" (format nil "one~%<<<<<<< HEAD~%TWO~%=======~%2~%>>>>>>> side~%three~%"))))
                   (session-from-arguments (list "--merge" local base remote merged) :directory dir
                                           :keys '("C-c ^ n C-c ^ l C-x C-s")
                                           :until (lambda ()
                                                    (let ((b (tuatara.defaults::buffer-visiting merged)))
                                                      (and b (search "<<<<<<< local" (tuatara.buffer:buffer-string b))
                                                           (plusp (length (tuatara.defaults::conflict-list b)))))))
                   tuatara.command:*exit-code*))))))
    ;; resolved and saved, nothing left: 0
    (is = 0 (merged-and-saved ""))
    ;; a heading underlined with =======, in the text git left too: still 0
    (is = 0 (merged-and-saved (format nil "Heading~%=======~%")))
    ;; a marker of the other kinds left by hand: 1
    (is = 1 (merged-and-saved (format nil "<<<<<<< leftover~%")))
    (is = 1 (merged-and-saved (format nil "||||||| leftover~%")))
    (is = 1 (merged-and-saved (format nil ">>>>>>>~%")))
    (is = 1 (merged-and-saved (format nil "=======~%>>>>>>> x~%")))))

(define-test (command-line standard-input-large-and-once)
  ;; more than is read into memory: read to a file with no name and mapped, not held in the heap
  (with-test-directory (dir)
    (multiple-value-bind (reader writer) (sb-posix:pipe)
      (let* ((line (format nil "~a~%" (make-string 99 :initial-element #\x)))
             (count (ceiling (* 70 1024 1024) 100))
             (feeder (sb-thread:make-thread
                      (lambda ()
                        (let ((octets (tuatara.text:string-octets line)))
                          (dotimes (i count) (tuatara.io:write-octets writer octets))
                          (sb-posix:close writer))))))
        (multiple-value-bind (f editor)
            (session-from-arguments (list "-") :directory dir :input reader :seconds 60
                                    :until (lambda () (let ((b (tuatara.buffer:find-buffer "*stdin*")))
                                                        (and b (> (tuatara.buffer:buffer-size b) 1000000)))))
          (declare (ignore f))
          (let ((buffer (shown-buffer editor)))
            (is = (* count 100) (tuatara.buffer:buffer-size buffer))
            (true (tuatara.buffer:buffer-mapping buffer) "mapped, as a large file is")
            (true (tuatara.io:safe-visit-p buffer))))
        (sb-thread:join-thread feeder)
        (ignore-errors (sb-posix:close reader))
        (ignore-errors (tuatara.buffer:kill-buffer (tuatara.buffer:find-buffer "*stdin*")))))))

(define-test (command-line standard-input-once-and-private)
  ;; one -, no more; and the copy of the descriptor is not passed on to programs started
  (with-test-directory (dir)
    (is string= "- is given twice: standard input is read once"
        (getf (tuatara.terminal::read-options (list "-" "-") :directory (namestring dir)) :error))
    (multiple-value-bind (reader writer) (sb-posix:pipe)
      (let* ((options (tuatara.terminal::read-options (list "-") :directory (namestring dir)))
             (files (tuatara.terminal::startup-from options :input reader))
             (fd (second (first files))))
        (true (logtest 1 (sb-posix:fcntl fd 1 0)) "close-on-exec")
        (sb-posix:close fd) (sb-posix:close reader) (sb-posix:close writer)))))

(define-test (command-line places-written-wrongly)
  (with-test-directory (dir)
    (let ((a (namestring (put-file (merge-pathnames "a.txt" dir) "x"))))
      (dolist (bad '("+12345678901" "+5:" "+:3" "+5:6:7" "+1234567890"))
        (true (search "is not +LINE or +LINE:COLUMN" (getf (tuatara.terminal::read-options (list bad a) :directory (namestring dir)) :error))
              "~a refused" bad))
      (false (getf (tuatara.terminal::read-options (list "+123456789:123456789" a) :directory (namestring dir)) :error))
      ;; names in a message have their control characters written out
      (let ((said (make-string-output-stream)))
        (let ((*error-output* said))
          (is = 2 (tuatara.terminal:main (list "--diff" (format nil "x~ay" #\Newline) a))))
        (let ((text (get-output-stream-string said)))
          (true (search "x^Jy is not there" text)))))))

(define-test (command-line old-bare-folders-swept)
  ;; judged by the lock a session holds on its folder and the machine named in it, as the git shadows are
  (with-test-directory (dir)
    (multiple-value-bind (dead dead-lock) (tuatara.defaults::make-owned-folder "bare-state/")
      (multiple-value-bind (live live-lock) (tuatara.defaults::make-owned-folder "bare-state/")
        (multiple-value-bind (elsewhere elsewhere-lock) (tuatara.defaults::make-owned-folder "bare-state/")
          (let ((was-config tuatara.defaults::*config-file*) (was-state tuatara-user:*state-directory*))
            (unwind-protect
                 (progn
                   ;; one of a session that ended (its lock let go), one of another machine
                   (sb-posix:close dead-lock)
                   (sb-posix:close elsewhere-lock)
                   (put-file (concatenate 'string elsewhere "owner.lock") "another-machine.example")
                   (unwind-protect (session-from-arguments (list "-Q") :directory dir)
                     (setf tuatara.defaults::*config-file* was-config
                           tuatara-user:*state-directory* was-state))
                   (false (probe-file dead) "a folder of a session that ended is swept")
                   (true (probe-file live) "one a session holds locked stays, whatever process numbers it sees")
                   (true (probe-file elsewhere) "another machine's stays"))
              (sb-posix:close live-lock)
              (dolist (folder (list dead live elsewhere))
                (ignore-errors (tuatara.io:delete-tree folder))))))))))

(define-test (command-line options-on-the-stable-surface)
  ;; the options the program takes are pinned, +LINE among them
  (multiple-value-bind (version pinned) (read-stable-surface)
    (declare (ignore version))
    (dolist (flag (cons "+LINE" tuatara.terminal:*options*))
      (true (find (list :flag flag) pinned :key #'row-name :test #'equal) "~a is pinned" flag)
      (true (find (list :flag flag) (stable-surface) :key #'row-name :test #'equal) "~a is kept" flag))
    (true (member "--diff" tuatara.terminal:*options* :test #'string=))))
