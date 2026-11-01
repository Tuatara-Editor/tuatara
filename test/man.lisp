;;;; test/man.lisp --- M-x man: manual pages formatted shut in the sandbox and read in a buffer.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.tests)

(define-test man
  :description "M-x man: a page made at test time formatted by man shut in, its headings, bold and underline in faces, its references links; reading what man prints; asking for pages.")

(defparameter *man-test-page*
  (format nil ".TH TESTPAGE 1 \"2026\" \"Tuatara\" \"Test\"~%.SH NAME~%testpage \\- a page made for a test~%.SH SYNOPSIS~%.B testpage~%[\\fB\\-v\\fR] \\fIfile\\fR~%.SH DESCRIPTION~%Reads the file.~%.SH \"SEE ALSO\"~%.BR frobnicate (1) ,~%tuatara-nothing(7)~%")
  "A small page in roff, with two references.")

(defun man-test-ready-p ()
  "True when man and the sandbox it runs in are here."
  (and (tuatara.defaults::man-program) (eq :ready (tuatara.io:sandbox-status))))

(defun man-test-wait ()
  "Wait until the pages being formatted are done, and let the editor take them in."
  (loop repeat 500
        while (find-if (lambda (job) (and (tuatara.defaults::man-job-p job) (eq (tuatara.runtime:job-status job) :running)))
                       tuatara.runtime:*jobs*)
        do (sleep 0.02)))

(define-test (man formatting-read)
  ;; escape sequences and struck characters both give bold and underline; the rest is left out
  (multiple-value-bind (text marks) (tuatara.defaults::man-formatting
                                     (format nil "~c[1mNAME~c[0m~%a~c[4mb~c[24mc ~c]8;;http://example.org~c\\link~c]8;;~c\\~c[31m~c[2Jd~%"
                                             #\Escape #\Escape #\Escape #\Escape #\Escape #\Escape #\Escape #\Escape #\Escape #\Escape))
    (is string= (format nil "NAME~%abc linkd~%") text)
    (is equal '(1 1 1 1 0 0 2 0 0) (coerce (subseq marks 0 9) 'list)))
  (multiple-value-bind (text marks) (tuatara.defaults::man-formatting
                                     (format nil "N~cN _~cx o~c+" #\Backspace #\Backspace #\Backspace))
    (is string= "N x +" text)
    (is equal '(1 0 2 0 0) (coerce marks 'list)))
  ;; the control characters a page could carry are dropped
  (is string= (format nil "ab~%") (tuatara.defaults::man-formatting (format nil "a~cb~c~c~%" (code-char 7) (code-char 127) (code-char 13)))))

(define-test (man pieces)
  (let* ((followed '())
         (text (format nil "LS(1)  Commands  LS(1)~%~c[1mNAME~c[0m~%       ls ~c[1m-l~c[22m  see ~c[1mfoo~c[22m(1), bar(3p).~%~%Tool 2026  LS(1)~%" #\Escape #\Escape #\Escape #\Escape #\Escape #\Escape))
         (pieces (tuatara.defaults::man-pieces text (lambda (name section) (push (list name section) followed)))))
    ;; a heading is bold from the first column; other bold is strong
    (true (find '("NAME" . :markup.heading) pieces :test #'equal))
    (true (find '("-l" . :strong) pieces :test #'equal))
    ;; a reference is a button across the bold name and the plain section; the page's own title and footer are none
    (let ((buttons (remove-if-not (lambda (piece) (and (consp piece) (listp (cdr piece)) (eq (second piece) :button))) pieces)))
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
                      (format nil "ls (1)               - list directory contents~%gzip, gunzip (1) - compress files~%junk line~%printf (3p) - print~%") :external-format :utf-8))))
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
