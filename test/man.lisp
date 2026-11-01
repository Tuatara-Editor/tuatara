;;;; test/man.lisp --- M-x man: manual pages formatted shut in the sandbox and read in a buffer.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.tests)

(define-test man
  :description "M-x man: a page made at test time formatted by man shut in, its headings, bold and underline in
faces, its references links; reading what man prints; asking for pages.")

(defparameter *man-test-page*
  (format nil ".TH TESTPAGE 1 \"2026\" \"Tuatara\" \"Test\"~%.SH NAME~%testpage \\- a page made for a test~%~
                .SH SYNOPSIS~%.B testpage~%[\\fB\\-v\\fR] \\fIfile\\fR~%.SH DESCRIPTION~%Reads the file.~%~
                .SH \"SEE ALSO\"~%.BR frobnicate (1) ,~%tuatara-nothing(7)~%")
  "A small page in roff, with two references.")

(defun man-test-ready-p ()
  "True when man and the sandbox it runs in are here."
  (and (tuatara.defaults::man-program) (eq :ready (tuatara.io:sandbox-status))))

(defun man-test-wait ()
  "Wait until the pages being formatted are done, and let the editor take them in."
  (loop repeat 500
        while (find-if (lambda (job)
                         (and (tuatara.defaults::man-job-p job) (eq (tuatara.runtime:job-status job) :running)))
                       tuatara.runtime:*jobs*)
        do (sleep 0.02)))

(define-test (man formatting-read)
  ;; escape sequences and struck characters both give bold and underline; the rest is left out
  (multiple-value-bind (text marks)
      (tuatara.defaults::man-formatting
       (substitute #\Escape #\^ (format nil "^[1mNAME^[0m~%a^[4mb^[24mc ^]8;;http://example.org^\\link^]8;;^\\^[31m^[2Jd~%")))
    (is string= (format nil "NAME~%abc linkd~%") text)
    (is equal '(1 1 1 1 0 0 2 0 0) (coerce (subseq marks 0 9) 'list)))
  (multiple-value-bind (text marks) (tuatara.defaults::man-formatting
                                     (format nil "N~cN _~cx o~c+" #\Backspace #\Backspace #\Backspace))
    (is string= "N x +" text)
    (is equal '(1 0 2 0 0) (coerce marks 'list)))
  ;; the control characters a page could carry are dropped
  (is string= (format nil "ab~%")
      (tuatara.defaults::man-formatting (format nil "a~cb~c~c~%" (code-char 7) (code-char 127) (code-char 13)))))

(define-test (man pieces)
  (let* ((followed '())
         (text (substitute #\Escape #\^
                           (format nil "LS(1)  Commands  LS(1)~%^[1mNAME^[0m~%       ls ^[1m-l^[22m  ~
                                        see ^[1mfoo^[22m(1), bar(3p).~%~%Tool 2026  LS(1)~%")))
         (pieces (tuatara.defaults::man-pieces text (lambda (name section) (push (list name section) followed)))))
    ;; a heading is bold from the first column; other bold is strong
    (true (find '("NAME" . :markup.heading) pieces :test #'equal))
    (true (find '("-l" . :strong) pieces :test #'equal))
    ;; a reference is a button across the bold name and the plain section; the page's own title and footer are none
    (let ((buttons (remove-if-not (lambda (piece) (and (consp piece) (listp (cdr piece)) (eq (second piece) :button)))
                                  pieces)))
      (is equal '("foo(1)" "bar(3p)") (mapcar #'first buttons))
      (funcall (third (first buttons)))
      (funcall (third (second buttons)))
      (is equal '(("bar" "3p") ("foo" "1")) followed))))

(define-test (man requests)
  (is equal '("--" "ls") (tuatara.defaults::man-request "ls"))
  (is equal '("--" "1" "ls") (tuatara.defaults::man-request " ls(1) "))
  (is equal '("--" "3p" "printf") (tuatara.defaults::man-request "3p printf"))
  ;; what man would take for an option or a file is refused
  (dolist (asked '("-k" "-l ls" "../ls" "/etc/passwd" "" "a b c" "ls()"))
    (fail (tuatara.defaults::man-request asked) 'error "~s" asked)))

(define-test (man page-names)
  (let ((candidates (tuatara.defaults::man-page-candidates
                     (sb-ext:string-to-octets
                      (format nil "ls (1)  - list directory contents~%gzip, gunzip (1) - compress files~%~
                                   junk line~%printf (3p) - print~%")
                      :external-format :utf-8))))
    (is equal '("ls(1)" "gzip(1)" "gunzip(1)" "printf(3p)") (mapcar #'tuatara.command:candidate-string candidates))
    (is string= "compress files" (tuatara.command:candidate-annotation (second candidates))))
  (true (tuatara.defaults::man-file-p "/x/ls.1"))
  (true (tuatara.defaults::man-file-p "/x/printf.3p"))
  (true (tuatara.defaults::man-file-p "/x/page.man"))
  (false (tuatara.defaults::man-file-p "/x/notes.txt"))
  (false (tuatara.defaults::man-file-p "/x/archive.tar.gz"))
  (false (tuatara.defaults::man-file-p nil)))

(define-test (man outside-program)
  ;; the system without man is told what to install
  (let ((words (tuatara.defaults::outside-program-install-words :man :fedora)))
    (true (search "man-db" words))
    (true (search "groff-base" words))))

(define-test (man file-page)
  (when (man-test-ready-p)
    (let* ((dir (ensure-directories-exist (merge-pathnames "tuatara-man-test/" (uiop:temporary-directory))))
           (path (namestring (merge-pathnames "tuatara-man-test.1" dir))))
      (alexandria:write-string-into-file *man-test-page* path :if-exists :supersede)
      (unwind-protect
           (multiple-value-bind (f editor)
               (run-keys (list "M-x m a n RET RET" #'man-test-wait :tick) :file path :width 70 :height 20)
             (declare (ignore f))
             (let ((buffer (tuatara.buffer:find-buffer "*Man tuatara-man-test.1*")))
               (true buffer)
               (when buffer
                 (let ((text (tuatara.buffer:buffer-string buffer)))
                   (true (search "a page made for a test" text))
                   (true (search "SEE ALSO" text))
                   (false (search (string #\Escape) text)))
                 (true (tuatara.buffer:buffer-local buffer :read-only))
                 (is eq :help (tuatara.buffer:buffer-local buffer :kind))
                 (true (typep (tuatara.modes:buffer-major-mode buffer) 'tuatara.defaults::man-mode))
                 ;; its references are buttons
                 (let* ((text (tuatara.buffer:buffer-string buffer))
                        (at (search "frobnicate(1)" text)))
                   (true at)
                   (when at (true (tuatara.defaults::button-at buffer (1+ at)))))
                 ;; the window shows it
                 (is eq buffer (tuatara.display:window-buffer (tuatara.command:editor-window editor)))
                 (tuatara.buffer:kill-buffer buffer))))
        (uiop:delete-directory-tree dir :validate t :if-does-not-exist :ignore)))))

(define-test (man sections-and-links)
  (when (man-test-ready-p)
    (let* ((dir (ensure-directories-exist (merge-pathnames "tuatara-man-keys/" (uiop:temporary-directory))))
           (path (namestring (merge-pathnames "keys.1" dir)))
           (opened '())
           (original #'tuatara.defaults::man-open))
      (alexandria:write-string-into-file *man-test-page* path :if-exists :supersede)
      (unwind-protect
           (with-functions-replaced ((tuatara.defaults::man-open
                                      (lambda (asked &rest keys)
                                        (push asked opened)
                                        (and (getf keys :file) (apply original asked keys)))))
             ;; n goes to the next section heading, p back; l follows the reference at point
             (let (point-after-n point-after-p)
               (run-keys (list "M-x m a n RET RET" #'man-test-wait :tick "n n n n"
                               (lambda ()
                                 (let ((buffer (tuatara.buffer:find-buffer "*Man keys.1*")))
                                   (setf point-after-n (tuatara.buffer:buffer-point buffer))))
                               "p"
                               (lambda ()
                                 (let ((buffer (tuatara.buffer:find-buffer "*Man keys.1*")))
                                   (setf point-after-p (tuatara.buffer:buffer-point buffer))
                                   (setf (tuatara.buffer:buffer-point buffer)
                                         (+ 2 (search "frobnicate(1)" (tuatara.buffer:buffer-string buffer))))))
                               "l")
                         :file path :width 70 :height 20)
               (let* ((buffer (tuatara.buffer:find-buffer "*Man keys.1*"))
                      (text (tuatara.buffer:buffer-string buffer)))
                 (is = (search "SEE ALSO" text) point-after-n)
                 (is = (search "DESCRIPTION" text) point-after-p)
                 (is equal '("keys.1" "frobnicate(1)") (reverse opened))
                 (tuatara.buffer:kill-buffer buffer))))
        (uiop:delete-directory-tree dir :validate t :if-does-not-exist :ignore)))))

(define-test (man missing-page)
  (when (man-test-ready-p)
    ;; a page the system lacks is said, and no buffer is left
    (multiple-value-bind (f editor)
        (run-keys (list "M-x m a n RET t u a t a r a - n o - s u c h - p a g e C-j" #'man-test-wait :tick) :width 100)
      (declare (ignore editor))
      (false (tuatara.buffer:find-buffer "*Man tuatara-no-such-page*"))
      (true (tuatara.frontend.fake:fake-frame-showing f "No manual entry")))))

(defun man-test-page-file (name text)
  "Write TEXT in the file NAME in a folder of its own for a test, and return its path and the folder."
  (let* ((dir (ensure-directories-exist (merge-pathnames (format nil "tuatara-man-~a/" name) (uiop:temporary-directory))))
         (path (namestring (merge-pathnames (format nil "~a.1" name) dir))))
    (alexandria:write-string-into-file text path :if-exists :supersede)
    (values path dir)))

(defun man-test-plain (path)
  "What the page at PATH shows as text, man run shut in on it and read as the buffer shows it."
  (values (tuatara.defaults::man-formatting (tuatara.text:lenient-string (tuatara.defaults::man-run
                                                                          (list "--local-file") :file path)))))

(define-test (man escape-sequences)
  (let ((e (string #\Escape)))
    ;; where each kind of sequence ends
    (flet ((end (text &rest keys) (apply #'tuatara.text:escape-sequence-end text 0 keys)))
      (is = 5 (end (format nil "~a[31mx" e)))
      (is eql #\m (nth-value 1 (end (format nil "~a[31mx" e))))
      (is = 7 (end (format nil "~a]0;ab~cx" e (code-char 7))))
      (is = 8 (end (format nil "~a]0;ab~a\\x" e e)))
      (is = 8 (end (format nil "~aPdata~a\\x" e e)))
      (is = 2 (end (format nil "~a7x" e)))
      (is = 5 (end (format nil "~a]0;a" e)))
      (is = 3 (end (format nil "~c1mx" (code-char #x9b)) :c1 t))
      (is = 1 (end (format nil "~cx" (code-char #x85)) :c1 t))
      (is = 6 (end (format nil "~cdata~cx" (code-char #x90) (code-char #x9c)) :c1 t)))
    ;; the terminal's text reads them as before: colours and bold kept, the rest left out
    (is equal (list (format nil "ab~%c"))
        (tuatara.defaults::ansi-pieces (format nil "a~a]0;t~cb~a[2K~%~a7c" e (code-char 7) e e)))))

(define-test (man controls-never-reach-the-buffer)
  (let* ((e (string #\Escape))
         (text (format nil "a~cb~a]52;c;SGVsbG8=~cc~aP1;2qDATA~a\\d~c31me~cxyz~cf~%"
                       (code-char #x85) e (code-char 7) e e (code-char #x9b) (code-char #x90) (code-char #x9c)))
         (plain (tuatara.defaults::man-formatting text)))
    ;; C1 controls and a device control string with its payload are left out of the text
    (is string= (format nil "abcdef~%") plain)))

(define-test (man page-cannot-act)
  (when (man-test-ready-p)
    (let* ((canary (man-test-page-file "canary" "CANARYTEXT"))
           (page (man-test-page-file
                  "probe"
                  (format nil ".TH PROBE 1 \"2026\" \"x\" \"y\"~%.SH NAME~%probe \\- probe~%BEGIN~%~
                               .sy echo SYRAN~%.pso echo PSORAN~%.so /etc/passwd~%SO1~%.so ~a~%SO2~%END~%"
                          canary)))
           (plain (man-test-plain page)))
      ;; the page is read, what it asks to be run or read is not
      (true (search "BEGIN" plain))
      (true (search "END" plain))
      (false (search "SYRAN" plain))
      (false (search "PSORAN" plain))
      (false (search "root:" plain))
      (false (search "CANARYTEXT" plain))
      (uiop:delete-directory-tree (uiop:pathname-directory-pathname canary) :validate t :if-does-not-exist :ignore)
      (uiop:delete-directory-tree (uiop:pathname-directory-pathname page) :validate t :if-does-not-exist :ignore))))

(define-test (man raw-controls-in-a-page)
  (when (man-test-ready-p)
    (let* ((e (string #\Escape))
           (page (man-test-page-file
                  "raw"
                  (format nil ".TH PROBE 1 \"2026\" \"x\" \"y\"~%.SH NAME~%probe \\- probe~%~
                               RAW~a]52;c;SGVsbG8=~cA~a[31mB~aP1;2qDATA~a\\C~aDE~aF~%"
                          e (code-char 7) e e e (code-char #x9b) (code-char #x9b))))
           (plain (man-test-plain page)))
      (true (search "RAW" plain))
      (false (find-if (lambda (ch) (or (char= ch #\Escape) (<= 128 (char-code ch) 159))) plain))
      (uiop:delete-directory-tree (uiop:pathname-directory-pathname page) :validate t :if-does-not-exist :ignore))))

(define-test (man own-pages)
  ;; the folder of pages installed for oneself, in the home, is searched, and only that folder of it
  (when (man-test-ready-p)
    (let* ((home (ensure-directories-exist (merge-pathnames "tuatara-man-home/" (uiop:xdg-cache-home))))
           (folder (ensure-directories-exist (merge-pathnames ".local/share/man/man1/" home))))
      (alexandria:write-string-into-file
       (format nil ".TH OWNPAGE 1 \"2026\" \"x\" \"y\"~%.SH NAME~%ownpage \\- a page of mine~%")
       (merge-pathnames "ownpage.1" folder) :if-exists :supersede)
      (unwind-protect
           (with-functions-replaced ((tuatara.io:home-directory (lambda () (namestring home))))
             (is string= (namestring (merge-pathnames ".local/share/man" home)) (tuatara.defaults::own-pages-folder))
             (let ((text (tuatara.text:lenient-string (tuatara.defaults::man-run (list "--" "ownpage")))))
               (true (search "a page of mine" (tuatara.defaults::man-formatting text)))))
        (uiop:delete-directory-tree home :validate t :if-does-not-exist :ignore)))))

(define-test (man timeout-words)
  ;; the seconds said are those man was given
  (with-functions-replaced ((tuatara.defaults::man-program (lambda () "/usr/bin/man"))
                            (tuatara.io:sandbox-run (lambda (&rest arguments)
                                                      (declare (ignore arguments))
                                                      (values (tuatara.text:make-octets 0) :timeout ""))))
    (handler-case (tuatara.defaults::man-run (list "--" "ls"))
      (error (e) (true (search (format nil "~d seconds" tuatara.defaults::*man-seconds*) (princ-to-string e)))))))

(define-test (man page-list-failure-kept)
  ;; man without an index: said once, and not tried at every question
  (let ((saved tuatara.defaults::*man-pages*)
        (runs 0))
    (setf tuatara.defaults::*man-pages* nil)
    (unwind-protect
         (with-functions-replaced ((tuatara.defaults::man-program (lambda () "/usr/bin/man"))
                                   (tuatara.defaults::man-run (lambda (&rest arguments)
                                                                (declare (ignore arguments))
                                                                (incf runs)
                                                                (error "man failed: .: nothing appropriate"))))
           (let ((listing (tuatara.defaults::start-man-page-list)))
             (true listing)
             (tuatara.defaults::wait-for-gathering listing 5)
             (true (search "mandb" (first (tuatara.defaults::take-gathered listing))))
             (false (tuatara.defaults::start-man-page-list))
             (is = 1 runs)))
      (setf tuatara.defaults::*man-pages* saved))))

(define-test (man formatting-twice-stops-the-first)
  (let (first-job)
    (with-functions-replaced ((tuatara.defaults::man-program (lambda () "/usr/bin/man"))
                              (tuatara.defaults::man-run (lambda (arguments &key cancel &allow-other-keys)
                                                           (declare (ignore arguments))
                                                           (loop repeat 250 until (car cancel) do (sleep 0.02))
                                                           (tuatara.text:make-octets 0))))
      (run-keys (list (lambda ()
                        (let ((buffer (tuatara.buffer:make-buffer "*Man twice*" :text "")))
                          (tuatara.defaults::format-man-page buffer)
                          (setf first-job (find-if #'tuatara.defaults::man-job-p tuatara.runtime:*jobs*))
                          (tuatara.defaults::format-man-page buffer)
                          (loop repeat 100 while (eq (tuatara.runtime:job-status first-job) :running) do (sleep 0.02))
                          (is eq :stopped (tuatara.runtime:job-status first-job))
                          (tuatara.buffer:kill-buffer buffer))))
                :width 60 :height 8))))

(define-test (man pieces-garbage)
  ;; a long page costs some twenty octets for each character, not over fifty
  (let* ((line (substitute #\Escape #\^ (format nil "       ^[1mls^[22m lists the ^[4mfiles^[24m named by ~
                                                    foo(1) and the rest of this line, to make it long~%")))
         (text (with-output-to-string (out) (dotimes (i 20000) (write-string line out))))
         (before (sb-ext:get-bytes-consed)))
    (tuatara.defaults::man-pieces text (lambda (name section) (list name section)))
    (true (< (- (sb-ext:get-bytes-consed) before) (* 30 (length text))) "~:d octets for ~:d characters"
          (- (sb-ext:get-bytes-consed) before) (length text))))
