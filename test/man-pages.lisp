;;;; test/man-pages.lisp --- The manual pages tuatara(1) and tuatara-init(5) hold what the program does.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.tests)

(define-test man-pages
  :description "The manual pages made from the program: every option, setting and file that is kept is in them,
and man shows them without a warning.")

(defmacro with-man-pages ((folder) &body body)
  "Run BODY with FOLDER a folder of the run's own the pages are written into, taken away after."
  `(let ((,folder (ensure-directories-exist
                   (merge-pathnames (format nil "tuatara-man-pages-~d/" (sb-posix:getpid))
                                    (uiop:temporary-directory)))))
     (unwind-protect (progn (tuatara.terminal:write-man-pages ,folder) ,@body)
       (uiop:delete-directory-tree ,folder :validate t :if-does-not-exist :ignore))))

(defun man-pages-text (folder name)
  "The text of the page NAME (tuatara.1 or tuatara-init.5) in FOLDER."
  (alexandria:read-file-into-string (merge-pathnames name folder) :external-format :utf-8))

(defun man-pages-run-groff (folder name width)
  "What groff says about the page NAME in FOLDER set WIDTH columns wide, with every warning on: a string, empty when
there is none."
  (multiple-value-bind (out error code)
      (uiop:run-program (list "groff" "-ww" "-z" "-Tutf8" (format nil "-rLL=~dn" width) (format nil "-rLT=~dn" width)
                              "-man" (namestring (merge-pathnames name folder)))
                        :output :string :error-output :string :ignore-error-status t)
    (declare (ignore out))
    (format nil "~a~:[ (exit ~d)~;~*~]" error (eql code 0) code)))

(define-test (man-pages every-option)
  ;; each way of writing each option is on the page, and the synopsis shows each way of starting
  (with-man-pages (folder)
   (let ((page (man-pages-text folder "tuatara.1")))
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
  (with-man-pages (folder)
   (let ((page (man-pages-text folder "tuatara.1")))
    (dolist (name '("XDG_CONFIG_HOME" "XDG_STATE_HOME" "XDG_DATA_HOME" "XDG_CACHE_HOME" "NO_COLOR" "TERM"
                    "TUATARA_HEAP_MB" "GIT_EDITOR" "EXIT STATUS" "FIRST KEYS" "EXAMPLES" "SEE ALSO"))
      (true (search name page) "~a is in tuatara(1)" name))
    (true (search "C\\-c C\\-k" page))
    (true (search "mergetool.tuatara.cmd" page)))))

(define-test (man-pages every-setting)
  ;; every setting the starter init file lists is on the page of the init file, with a sentence
  (with-man-pages (folder)
   (let ((page (man-pages-text folder "tuatara-init.5"))
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
  ;; every file the pin lists is on the page of the init file, in the folder it is in, with what it is
  (with-man-pages (folder)
   (let ((page (man-pages-text folder "tuatara-init.5")))
    (dolist (row (tuatara.terminal::kept-file-rows))
      (multiple-value-bind (folder name) (tuatara.terminal::kept-file-place row)
        (let ((place (format nil "~a~a" (cdr (assoc folder tuatara.terminal::*kept-file-folders* :test #'string=))
                             (tuatara.terminal::roff-text name))))
          (true (search (format nil ".B ~a~%" place) page) "~a is in tuatara-init(5)" place)
          (true (assoc name tuatara.terminal::*kept-file-descriptions* :test #'string=)
                "~a has a description" name))))
    ;; the markers of documentation strings are not left in what is quoted from them
    (false (search "\\e[" page))
    (false (search "\\e<" page))
    (dolist (class '("Kept" "Remembered" "Rebuilt"))
      (true (search (format nil ".SS ~a~%" class) page))))))

(define-test (man-pages roff-text)
  (is string= "a\\e b\\-c" (tuatara.terminal::roff-text "a\\ b-c"))
  (is string= (format nil "\\&.x~%\\&'y") (tuatara.terminal::roff-text (format nil ".x~%'y")))
  (is string= "a..." (tuatara.terminal::roff-text (format nil "a~c" (code-char #x2026))))
  (true (every (lambda (char) (< (char-code char) 128))
               (tuatara.terminal::roff-text (format nil "~c~c" (code-char #x2192) (code-char #x00e9))))))

(define-test (man-pages shown-by-man)
  ;; groff has nothing to say about the pages, and man, as M-x man runs it, shows what they hold
  (when (and (tuatara.io:find-program "groff") (man-test-ready-p))
    (with-man-pages (folder)
      (dolist (name '("tuatara.1" "tuatara-init.5"))
        (dolist (width '(72 80 100))
          (is string= "" (man-pages-run-groff folder name width) "~a at ~d columns" name width)))
      (flet ((shown (name)
               (tuatara.text:lenient-string
                (tuatara.defaults::man-run (list "--local-file") :file (namestring (merge-pathnames name folder))))))
        (let ((text (shown "tuatara.1")))
          (true (search "SYNOPSIS" text))
          (true (search "--generate-config" text))
          (true (search "EXIT STATUS" text)))
        (let ((text (shown "tuatara-init.5")))
          (true (search "FILES KEPT" text))
          (true (search "recent-files" text)))))))
