;;;; test/man-pages.lisp --- The manual pages tuatara(1) and tuatara-init(5) hold what the program does.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.tests)

(define-test man-pages
  :description "The manual pages made from the program: every option, setting and file that is kept is in them,
and man shows them without a warning.")

(defmacro with-man-pages ((directory) &body body)
  "Run BODY with DIRECTORY a directory of the run's own the pages are written into, taken away after."
  `(let ((,directory (ensure-directories-exist
                   (merge-pathnames (format nil "tuatara-man-pages-~d/" (sb-posix:getpid))
                                    (uiop:temporary-directory)))))
     (unwind-protect (progn (tuatara.terminal:write-man-pages ,directory) ,@body)
       (uiop:delete-directory-tree ,directory :validate t :if-does-not-exist :ignore))))

(defun man-pages-text (directory name)
  "The text of the page NAME (tuatara.1 or tuatara-init.5) in DIRECTORY."
  (alexandria:read-file-into-string (merge-pathnames name directory) :external-format :utf-8))

(defun man-pages-run-groff (directory name width)
  "What groff says about the page NAME in DIRECTORY set WIDTH columns wide, with every warning on: a string, empty when
there is none."
  (multiple-value-bind (out error code)
      (uiop:run-program (list "groff" "-ww" "-z" "-Tutf8" (format nil "-rLL=~dn" width) (format nil "-rLT=~dn" width)
                              "-man" (namestring (merge-pathnames name directory)))
                        :output :string :error-output :string :ignore-error-status t)
    (declare (ignore out))
    (format nil "~a~:[ (exit ~d)~;~*~]" error (eql code 0) code)))

(define-test (man-pages every-option)
  ;; each way of writing each option is on the page, and the synopsis shows each way of starting
  (with-man-pages (directory)
   (let ((page (man-pages-text directory "tuatara.1")))
    (dolist (entry tuatara.terminal:*option-help*)
      (dolist (flag (getf entry :flags))
        (true (search (format nil "\\fB~a\\fR" (tuatara.terminal::roff-text (first flag))) page)
              "~a is in tuatara(1)" (first flag))))
    (dolist (option tuatara.terminal:*options*)
      (true (search (tuatara.terminal::roff-text option) page) "~a is in tuatara(1)" option))
    (dolist (form tuatara.terminal:*usage-forms*)
      (true (search (tuatara.terminal::roff-synopsis-form form) page) "~a is in the synopsis" form))
    ;; --help and the page read the one table
    (dolist (entry tuatara.terminal:*option-help*)
      (true (search (tuatara.terminal::option-label (getf entry :flags)) tuatara.terminal::*usage*))))))

(define-test (man-pages environment-and-exits)
  (with-man-pages (directory)
   (let ((page (man-pages-text directory "tuatara.1")))
    (dolist (name '("XDG_CONFIG_HOME" "XDG_STATE_HOME" "XDG_DATA_HOME" "XDG_CACHE_HOME" "NO_COLOR" "TERM"
                    "TUATARA_HEAP_MB" "GIT_EDITOR" "EXIT STATUS" "FIRST KEYS" "EXAMPLES" "SEE ALSO"))
      (true (search name page) "~a is in tuatara(1)" name))
    (true (search "C\\-c C\\-k" page))
    (true (search "mergetool.tuatara.cmd" page)))))

(define-test (man-pages every-setting)
  ;; every setting the starter init file lists is on the page of the init file, with a sentence
  (with-man-pages (directory)
   (let ((page (man-pages-text directory "tuatara-init.5"))
         (listed (apply #'append (mapcar #'rest (tuatara.terminal::starter-settings)))))
    (true (> (length listed) 200))
    (dolist (symbol listed)
      (let ((at (search (format nil ".B ~a~%" (tuatara.terminal::roff-text (string-downcase (symbol-name symbol))))
                        page)))
        (true at "~a is in tuatara-init(5)" symbol)
        (when at
          (let ((next (position #\Newline page :start (+ at 3))))
            (true (and next (< (1+ next) (length page)) (char/= (char page (1+ next)) #\.))
                  "~a has a sentence" symbol)))))
    (do-external-symbols (symbol :tuatara-user)
      (when (and (boundp symbol) (eq :special (sb-int:info :variable :kind symbol)))
        (true (member symbol listed) "~a is in the starter, so on the page" symbol))))))

(define-test (man-pages every-kept-file)
  ;; every file the pin lists is on the page of the init file, in the directory it is in, with what it is
  (with-man-pages (directory)
   (let ((page (man-pages-text directory "tuatara-init.5")))
    (dolist (row (tuatara.terminal::kept-file-rows))
      (multiple-value-bind (directory name) (tuatara.terminal::kept-file-place row)
        (let ((place (format nil "~a~a" (cdr (assoc directory tuatara.terminal::*kept-file-directories*
                                                    :test #'string=))
                             (tuatara.terminal::roff-text name))))
          (true (search (format nil ".B ~a~%" place) page) "~a is in tuatara-init(5)" place)
          (true (assoc name tuatara.terminal::*kept-file-descriptions* :test #'string=)
                "~a has a description" name))))
    ;; the markers of documentation strings are not left in what is quoted from them
    (false (search "\\e[" page))
    (false (search "\\e<" page))
    (dolist (class '("Kept" "Remembered" "Rebuilt"))
      (true (search (format nil ".SS ~a~%" class) page))))))

(defparameter *options-without-a-choice*
  '("+LINE[:COLUMN]" "--diff" "--merge" "--tutor" "--help" "--version" "--" "--daemon-socket[=NAME]"
    "--receive-file" "--remove-desktop")
  "The first flag of each option of *OPTION-HELP* that has nothing to choose: it stays a line.")

(defparameter *settings-with-a-choice*
  '(tuatara-user:*auto-save-idle* tuatara-user:*kill-ring-max* tuatara-user:*lsp-wait* tuatara-user:*remote-seconds*
    tuatara-user:*split-limit* tuatara-user:*tui-colours*)
  "Settings whose documentation says why to change them and what follows.")

(defun man-pages-flat (text)
  "TEXT with every run of white space, a line break included, a single space."
  (cl-ppcre:regex-replace-all "\\s+" text " "))

(define-test (man-pages choices-explained)
  ;; an option or variable that can be tweaked has its reasons on the page, not only its meaning
  (with-man-pages (directory)
   (let ((page (man-pages-flat (man-pages-text directory "tuatara.1")))
         (init (man-pages-flat (man-pages-text directory "tuatara-init.5"))))
    (dolist (entry tuatara.terminal:*option-help*)
      (let ((flag (first (first (getf entry :flags)))))
        (if (member flag *options-without-a-choice* :test #'string=)
            (false (getf entry :why) "~a has nothing to choose" flag)
            (true (getf entry :why) "~a says why to take it" flag))
        (when (getf entry :why)
          (let ((why (getf entry :why)))
            (true (search (tuatara.terminal::roff-text (format nil "~{~a~^ ~}" (subseq why 0 (position "" why :test #'string=))))
                          page)
                  "~a is explained in tuatara(1)" flag)))))
    (dolist (entry tuatara.terminal:*environment-help*)
      (true (search (tuatara.terminal::roff-text (format nil "~{~a~^ ~}" (getf entry :text))) page)
            "~{~a~} is in tuatara(1)" (getf entry :names)))
    ;; the heap says what raising it overcomes, what lowering it is for, and how it fails
    (true (search "Raise it for work held in memory" page))
    (true (search "Lower it on a small or shared machine" page))
    (true (search "runs out" page))
    (dolist (symbol *settings-with-a-choice*)
      (let ((first-paragraph (tuatara.terminal::documentation-paragraph (documentation symbol 'variable))))
        (true (search (man-pages-flat (tuatara.terminal::roff-fill first-paragraph)) init)
              "~a is in tuatara-init(5) in full" symbol)
        (true (> (count #\. first-paragraph) 1) "~a says more than what it is" symbol))))))

(define-test (man-pages roff-text)
  (is string= "a\\e b\\-c" (tuatara.terminal::roff-text "a\\ b-c"))
  (is string= (format nil "\\&.x~%\\&'y") (tuatara.terminal::roff-text (format nil ".x~%'y")))
  (is string= "a..." (tuatara.terminal::roff-text (format nil "a~c" (code-char #x2026))))
  (true (every (lambda (char) (< (char-code char) 128))
               (tuatara.terminal::roff-text (format nil "~c~c" (code-char #x2192) (code-char #x00e9))))))

(define-test (man-pages shown-by-man)
  ;; groff has nothing to say about the pages, and man, as M-x man runs it, shows what they hold
  (when (and (tuatara.io:find-program "groff") (man-test-ready-p))
    (with-man-pages (directory)
      (dolist (name '("tuatara.1" "tuatara-init.5"))
        (dolist (width '(72 80 100))
          (is string= "" (man-pages-run-groff directory name width) "~a at ~d columns" name width)))
      (flet ((shown (name)
               (tuatara.text:lenient-string
                (tuatara.defaults::man-run (list "--local-file") :file (namestring (merge-pathnames name directory))))))
        (let ((text (shown "tuatara.1")))
          (true (search "SYNOPSIS" text))
          (true (search "--generate-config" text))
          (true (search "EXIT STATUS" text)))
        (let ((text (shown "tuatara-init.5")))
          (true (search "FILES KEPT" text))
          (true (search "recent-files" text)))))))

(defun bash-completion-replies (script &rest words)
  "What the completion in SCRIPT, a file of bash, offers for the command line WORDS, whose last word is the one being
completed: a list of strings."
  (let ((out (uiop:run-program
              (append (list "bash" "-c"
                            "source \"$1\"; shift; COMP_WORDS=(\"$@\"); COMP_CWORD=$(($# - 1)); _tuatara
                             printf '%s\\n' \"${COMPREPLY[@]}\""
                            "bash" (uiop:native-namestring script) "tuatara")
                      words)
              :output :string :error-output nil :ignore-error-status t
              :directory (uiop:pathname-directory-pathname script))))
    (remove "" (tuatara.text:split-words out (list #\Newline)) :test #'string=)))

(define-test (man-pages bash-completion-has-every-option)
  ;; every option of the table is in the script, and the script is valid bash
  (with-test-directory (directory)
    (let* ((script (tuatara.terminal:write-bash-completion directory))
           (text (alexandria:read-file-into-string script :external-format :utf-8)))
      (dolist (option tuatara.terminal:*options*)
        (true (search option text) "~a is in the completion" option))
      (dolist (entry tuatara.terminal:*option-help*)
        (dolist (flag (getf entry :flags))
          (unless (char= (char (first flag) 0) #\+)
            (true (search (tuatara.terminal::bare-flag (first flag)) text) "~a is in the completion" (first flag)))))
      (is = 0 (nth-value 2 (uiop:run-program (list "bash" "-n" script) :ignore-error-status t :error-output nil))))))

(define-test (man-pages bash-completion-completes)
  ;; run in bash with COMP_WORDS set: options by prefix, the choices of --frames, files for a file option, after
  ;; --, and for a plain argument; nothing for a name
  (with-test-directory (directory)
    (put-file (merge-pathnames "notes.org" directory) "")
    (put-file (merge-pathnames "init.lisp" directory) "")
    (let ((script (tuatara.terminal:write-bash-completion directory)))
      (flet ((replies (&rest words) (apply #'bash-completion-replies script words)))
        (true (member "--daemon" (replies "--da") :test #'string=))
        (true (member "--daemon-socket" (replies "--da") :test #'string=))
        (false (member "--gui" (replies "--da") :test #'string=))
        (true (member "--client" (replies "--c") :test #'string=))
        (is equal '("1" "10" "11" "12" "13" "14" "15" "16") (replies "--frames" "1"))
        (is equal '("1" "10" "11" "12" "13" "14" "15" "16") (replies "--frames" "=" "1"))
        (is equal '("init.lisp") (replies "--config" "i"))
        (is equal '("init.lisp") (replies "-c" "in"))
        (is equal '("notes.org") (replies "--diff" "init.lisp" "n"))
        (is equal '("notes.org") (replies "notes.o"))
        (is equal '("notes.org") (replies "--" "--x" "notes.o"))
        (false (replies "--daemon" "=" "w"))
        ;; the values of --merge are all files, so an option is not offered among them
        (is equal '("notes.org") (replies "--merge" "a" "b" "c" "n"))))))

(define-test (man-pages bash-completion-removed-with-desktop)
  ;; tools/install-desktop.sh --remove takes away the completion it put in the data directory of the run's own home
  (with-test-directory (directory)
    (let* ((home (merge-pathnames "home/" directory))
           (data (merge-pathnames ".local/share/" home))
           (completion (merge-pathnames "bash-completion/completions/tuatara" data))
           (other (merge-pathnames "bash-completion/completions/other" data)))
      (ensure-directories-exist completion)
      (put-file completion "complete -F _tuatara tuatara")
      (put-file other "")
      (uiop:run-program (list "bash" (tools-script "install-desktop.sh") "--remove")
                        :output nil :error-output nil
                        :environment (list (format nil "HOME=~a" (uiop:native-namestring home))
                                           (format nil "XDG_DATA_HOME=~a" (uiop:native-namestring data))
                                           "PATH=/usr/bin:/bin"))
      (false (probe-file completion))
      (true (probe-file other)))))

(define-test (man-pages frame-counts-follow-the-limit)
  ;; the frames the options offer and say are those the editor opens, whatever its limit is
  (let ((most tuatara.command:*most-frames*))
    (is equal (loop for n from 1 to most collect (princ-to-string n))
        (cdr (assoc "--frames" tuatara.terminal::*completion-values* :test #'string=)))
    (true (search (format nil "from 1 to ~d" most)
                  (format nil "~{~a~^ ~}"
                          (getf (find "--frames" tuatara.terminal:*option-help*
                                      :key (lambda (entry) (first (first (getf entry :flags))))
                                      :test #'equal)
                                :text))))))

(define-test (man-pages kept-files-pin-read-once)
  ;; the pin of the kept files is read once for the three classes, not once for each
  (let ((reads 0))
    (sb-int:encapsulate 'tuatara.terminal::kept-file-rows 'counted
                        (lambda (function &rest arguments) (incf reads) (apply function arguments)))
    (unwind-protect (tuatara.terminal::kept-files-roff)
      (sb-int:unencapsulate 'tuatara.terminal::kept-file-rows 'counted))
    (is = 1 reads)))
