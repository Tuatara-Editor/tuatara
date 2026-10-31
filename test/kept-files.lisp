;;;; test/kept-files.lisp --- The files people keep, read by every later release of a major version.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; Beside the stable surface, which is names, the files Tuatara reads in its
;;;; folders are kept too. test/api/kept-files.sexp pins, at each version
;;;; bump, each list kept between sessions (its file's name, its class and the
;;;; version of its format) and the other files of the folders with their
;;;; class: kept (made by you: never lost to a later release), remembered
;;;; (what you did, read by every later release) or rebuilt (may be deleted at
;;;; any time). test/kept-files/VERSION/ holds a file of each list as that
;;;; release wrote it, and every later release must read them all to the same
;;;; entries. tools/pin-kept-files.sh writes the pin again, and the files of
;;;; the version being pinned, never over those there.

(in-package #:tuatara.tests)

(define-test kept-files
  :description "The lists kept between sessions and the other files of Tuatara's folders keep being read, by every later release of a major version: lines of a later release kept as they are, the files of each release read to the same entries.")

(defparameter *kept-files-file* (asdf:system-relative-pathname :tuatara "test/api/kept-files.sexp")
  "The pinned files Tuatara keeps: the version it was written at, then a row for each file.")

(defparameter *kept-file-samples* (asdf:system-relative-pathname :tuatara "test/kept-files/")
  "The folder of the files each release wrote, a folder for each, named for the version.")

(defparameter *kept-folders*
  '((:file "config" "init.lisp" :kept nil)
    (:file "config" "snippets/" :kept nil)
    (:file "config" "capture-templates/" :kept nil)
    (:file "config" "latex-templates/" :kept nil)
    (:file "config" "docx-templates/" :kept nil)
    (:file "config" "note-templates/" :kept nil)
    (:file "config" "logos/" :kept nil)
    (:file "data" "grammars/" :kept nil)
    (:file "state" "auto-save/" :kept nil)
    (:file "state" "session" :remembered 1)
    (:file "state" "grammar-store/" :rebuilt nil)
    (:file "data" "packages/" :rebuilt nil)
    (:file "cache" "" :rebuilt nil))
  "The files and folders of Tuatara's folders other than the lists kept
between sessions, each (:FILE FOLDER PATH CLASS VERSION): FOLDER config,
state, data or cache; PATH within it, a folder's ending in /; CLASS :KEPT,
:REMEMBERED or :REBUILT; VERSION that of its format where the file says one.")

(defparameter *kept-file-entries*
  `((tuatara.defaults::*recent-files*
     ("/notes/report.org" . nil) ("/evidence/disk 1.img" . t) (,(format nil "/odd/a\\b~cc~%d.txt" #\Tab) . nil))
    (tuatara.defaults::*saved-places*
     ("/notes/report.org" . 120) ("/notes/a b.org" . 0))
    (tuatara.defaults::*pdf-places*
     ("/papers/one.pdf" 12 :width 1.25) ("/papers/two three.pdf" 1 :page 1.0))
    (tuatara.defaults::*chosen-paths*
     "/notes/report.org" "/notes/")
    (tuatara.defaults::*bookmarks*
     ("Findings" "/cases/report.org" 120 "* Findings" ("work" "court") t)
     ("Unmapped" "/mnt/server-not-mapped/evidence/disk.img" 0 "" () t)
     ("Plain" "/notes/plain.txt" 7 ,(format nil "text~%after") () nil))
    (tuatara.defaults::*lsp-trusted*
     "/projects/one/" "/projects/two three/")
    (tuatara.defaults::*package-trust*
     ("https://example.org/signed.git" "SHA256:abcDEF0123456789+/=" "Mere Rotorua" "mere@example.org AAAAC3NzaC1lZDI1NTE5AAAAIExample+/=")
     ("https://example.org/tools.git" "unsigned" nil nil))
    (tuatara.defaults::*fetched-packages*
     ("example https://example.org/tools.git release" "0123456789abcdef0123456789abcdef01234567" "v1.2.0" t
      "89abcdef0123456789abcdef0123456789abcdef" "v1.1.0" "SHA256:abcDEF0123456789+/=")
     ("mine ~/packages/ latest" "-" nil t nil nil nil)
     ("third https://example.org/third.git latest" "abcdef0" nil nil "1234567" nil nil))
    (tuatara.defaults::*grammar-answers*
     ("yaml" . t) ("nix" . nil))
    (tuatara.defaults::*user-grammar-sums*
     ("mylang" . "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef")))
  "For each list kept between sessions (its variable), the entries its sample
files hold, the latest first: the first using every field the list has, the
others leaving out what may be left out.")

(defun kept-file-state-rows ()
  "A row for each list kept between sessions: (:STATE NAME CLASS VERSION)."
  (sort (mapcar (lambda (list)
                  (list :state (tuatara.defaults::state-list-name list) (tuatara.defaults::state-list-class list)
                        (tuatara.defaults::state-list-version list)))
                tuatara.defaults::*state-lists*)
        #'string< :key #'second))

(defun kept-files ()
  "The files Tuatara keeps as they are now, a row for each: the lists kept
between sessions, then the other files of its folders (*KEPT-FOLDERS*)."
  (append (kept-file-state-rows) *kept-folders*))

(defun kept-file-row-name (row)
  "What names the file ROW of the pin is about: the list's name, or the folder and path."
  (subseq row 0 (if (eq (first row) :state) 2 3)))

(defun kept-class-rank (class)
  "How much is promised of a file of CLASS: more for :KEPT than :REMEMBERED, more for that than :REBUILT."
  (or (position class '(:rebuilt :remembered :kept)) -1))

(defun kept-file-breaks (pinned now)
  "The rows of PINNED, files kept, that NOW no longer keeps: a file gone,
promised less (a kept list become remembered), or its format's version
lowered."
  (loop for row in pinned
        for found = (find (kept-file-row-name row) now :key #'kept-file-row-name :test #'equal)
        for class = (car (last row 2))
        for version = (car (last row))
        unless (and found
                    (>= (kept-class-rank (car (last found 2))) (kept-class-rank class))
                    (or (null version) (and (car (last found)) (>= (car (last found)) version))))
          collect row))

(defun read-kept-files (&optional (file *kept-files-file*))
  "The version FILE was pinned at, and its rows, as two values."
  (with-open-file (in file)
    (let ((head (read in)))
      (values (getf head :version)
              (loop for row = (read in nil in) until (eq row in) collect row)))))

(defun kept-file-entries (list)
  "The entries the sample files of LIST, a list kept between sessions, hold (*KEPT-FILE-ENTRIES*)."
  (rest (find list *kept-file-entries* :key (lambda (row) (symbol-value (first row))))))

(defun write-kept-file-samples (folder)
  "Write into FOLDER the file of each list kept between sessions as this
release writes it, holding the entries of *KEPT-FILE-ENTRIES*: those not
there yet, none over a file there. The names of those written."
  (ensure-directories-exist folder)
  (let ((written '()))
    (with-fresh-state
      (loop for (variable . entries) in *kept-file-entries*
            for list = (symbol-value variable)
            for to = (merge-pathnames (tuatara.defaults::state-list-name list) folder)
            unless (probe-file to)
              do (tuatara.defaults::state-list-put list entries)
                 (tuatara.defaults::finish-state-writes)
                 (alexandria:write-string-into-file (file-text (tuatara.defaults::state-list-path list)) to)
                 (push (tuatara.defaults::state-list-name list) written)))
    (nreverse written)))

(defun write-kept-files (&optional (file *kept-files-file*))
  "Pin the files Tuatara keeps as they are now in FILE, with the version it is
pinned at, and write the sample files of that version (WRITE-KEPT-FILE-SAMPLES)."
  (let ((version (asdf:component-version (asdf:find-system :tuatara))))
    (ensure-directories-exist file)
    (with-open-file (out file :direction :output :if-exists :supersede)
      (let ((*print-case* :downcase) (*print-right-margin* 100000))
        (format out ";; The files Tuatara keeps, pinned by tools/pin-kept-files.sh at each version bump: (:state NAME CLASS VERSION),~%~
                     ;; a list kept between sessions; (:file FOLDER PATH CLASS VERSION), the other files of its folders. CLASS~%~
                     ;; :kept, :remembered or :rebuilt; VERSION that of the format, NIL where the file says none.~%")
        (prin1 (list :version version) out)
        (terpri out)
        (dolist (row (kept-files)) (prin1 row out) (terpri out))))
    (write-kept-file-samples (merge-pathnames (format nil "~a/" version) *kept-file-samples*))))

(defun kept-sample-folders ()
  "The folders of sample files, one for each release that wrote them."
  (directory (merge-pathnames "*/" *kept-file-samples*)))

(defun file-lines-but-comments (text)
  "The lines of TEXT that are not empty and do not begin with #."
  (remove-if (lambda (line) (or (zerop (length line)) (char= (char line 0) #\#)))
             (uiop:split-string text :separator '(#\Newline))))

(defun put-state-file (list text)
  "Make TEXT the file of LIST, a list kept between sessions, in the state folder now in force."
  (ensure-directories-exist (tuatara.io:state-directory))
  (tuatara.io:write-private-file (tuatara.defaults::state-list-path list) text))

(define-test (kept-files samples-read)
  ;; each release's files read by this one to the same entries, and written back with their
  ;; lines as they were, under the line saying the version
  (let ((folders (kept-sample-folders)))
    (true folders "there are sample files")
    (dolist (folder folders)
      (dolist (list tuatara.defaults::*state-lists*)
        (let ((sample (merge-pathnames (tuatara.defaults::state-list-name list) folder)))
          (when (probe-file sample)
            (with-fresh-state
              (let ((text (file-text sample))
                    (path (tuatara.defaults::state-list-path list))
                    (where (format nil "~a of ~a" (tuatara.defaults::state-list-name list) (car (last (pathname-directory folder))))))
                (put-state-file list text)
                (is equal (kept-file-entries list) (tuatara.defaults::read-state-file list path) "~a read" where)
                (is equal (kept-file-entries list) (tuatara.defaults::state-list-entries list) "~a listed" where)
                ;; a change to nothing in it: written back, its lines as they were
                (tuatara.defaults::state-list-drop list '("/nowhere/kept-files"))
                (tuatara.defaults::finish-state-writes)
                (let ((written (file-text path)))
                  (true (uiop:string-prefix-p (format nil "# tuatara ~a " (tuatara.defaults::state-list-name list)) written)
                        "~a begins with its version" where)
                  (is equal (file-lines-but-comments text) (file-lines-but-comments written) "~a written back" where))
                (is equal (kept-file-entries list) (tuatara.defaults::read-state-file list path) "~a read again" where)))))))
    ;; the session
    (dolist (folder folders)
      (let ((sample (merge-pathnames "session" folder)))
        (when (probe-file sample)
          (with-fresh-state
            (ensure-directories-exist (tuatara.io:state-directory))
            (tuatara.io:write-private-file (tuatara.defaults::session-path) (file-text sample))
            (multiple-value-bind (buffers layout) (tuatara.defaults::read-session)
              (is equal `((:file 12 ,(format nil "/notes/b~cc.org" #\Tab)) (:safe 0 "/evidence/disk.img")
                          (:directory 0 "/notes/") (:scratch 5 "*scratch*"))
                  buffers)
              (is equal '(:split :side-by-side (0.5d0 0.5d0)
                          (:window t 0 (:file 34 "/notes/a b.org"))
                          (:window nil 0 nil))
                  layout))))))))

(define-test (kept-files every-list-pinned)
  ;; no list is kept between sessions without its class decided, pinned, and a sample of its file
  (multiple-value-bind (version pinned) (read-kept-files)
    (true version "the pin says the version it was written at")
    (dolist (list tuatara.defaults::*state-lists*)
      (let ((name (tuatara.defaults::state-list-name list)))
        (true (member (tuatara.defaults::state-list-class list) '(:kept :remembered)) "~a has a class" name)
        (true (find (list :state name) pinned :key #'kept-file-row-name :test #'equal) "~a is pinned" name)
        (true (kept-file-entries list) "~a has sample entries" name)
        (true (some (lambda (folder) (probe-file (merge-pathnames name folder))) (kept-sample-folders))
              "~a has a sample file" name)))
    ;; bookmarks are made by hand: kept
    (is eq :kept (tuatara.defaults::state-list-class tuatara.defaults::*bookmarks*))
    (is eq :kept (third (find '(:state "bookmarks") pinned :key #'kept-file-row-name :test #'equal)))))

(define-test (kept-files files-kept-within-a-major-version)
  ;; a file gone, promised less or its version lowered breaks the pin; one added does not
  (let ((pinned '((:state "bookmarks" :kept 1) (:state "places" :remembered 2) (:state "gone" :remembered 1)
                  (:file "config" "snippets/" :kept nil) (:file "state" "session" :remembered 1)))
        (now '((:state "bookmarks" :remembered 1) (:state "places" :remembered 1) (:state "added" :remembered 1)
               (:file "config" "snippets/" :kept nil) (:file "state" "session" :kept 2))))
    (is equal '(("bookmarks") ("places") ("gone"))
        (mapcar (lambda (row) (rest (kept-file-row-name row))) (kept-file-breaks pinned now))))
  ;; from 1.0.0 on, what the last release pinned is kept until the major version goes up
  (multiple-value-bind (version pinned) (read-kept-files)
    (let ((now-major (version-major (asdf:component-version (asdf:find-system :tuatara)))))
      (when (and (>= now-major 1) (= now-major (version-major version)))
        (let ((breaks (kept-file-breaks pinned (kept-files))))
          (is equal '() breaks "Files no longer kept within ~d.x: ~s" now-major breaks))))))

(define-test (kept-files later-lines-kept)
  ;; a file a later release wrote: its fields added at the ends of lines passed over, its lines
  ;; of a new kind kept as they are, and its version line kept, when this release writes it
  (dolist (row *kept-file-entries*)
    (let* ((list (symbol-value (first row)))
           (name (tuatara.defaults::state-list-name list))
           (print (tuatara.defaults::state-list-print list))
           (first-entry (second row))
           (second-entry (third row))
           (longer (format nil "~a~clater field" (funcall print first-entry) #\Tab))
           (unknown "zz a kind of line a later release writes"))
      (with-fresh-state
        (put-state-file list (format nil "# tuatara ~a 2~%# a comment of the later release~%~a~%~a~%" name longer unknown))
        (is equal (list first-entry) (tuatara.defaults::state-list-entries list) "~a read" name)
        (tuatara.defaults::state-list-put list (list second-entry))
        (tuatara.defaults::finish-state-writes)
        (is string= (format nil "# tuatara ~a 2~%~@[~a~]~a~%~a~%~a~%"
                            name (tuatara.defaults::state-list-header list) (funcall print second-entry) longer unknown)
            (file-text (tuatara.defaults::state-list-path list))
            "~a written back" name)
        (is equal (list second-entry first-entry) (tuatara.defaults::state-list-entries list) "~a read again" name)))))

(define-test (kept-files unreadable-not-written)
  ;; a file that cannot be read just then is never written over with only what was changed
  (with-fresh-state
    (let* ((list tuatara.defaults::*bookmarks*)
           (path (tuatara.defaults::state-list-path list))
           (entries (kept-file-entries list)))
      (tuatara.defaults::state-list-put list entries)
      (tuatara.defaults::finish-state-writes)
      (let ((before (file-text path)))
        (sb-posix:chmod path 0)
        (unwind-protect
             (progn
               (tuatara.defaults::state-list-put list (list (list "New" "/notes/new.txt" 0 "" () nil)))
               (tuatara.defaults::finish-state-writes))
          (sb-posix:chmod path #o600))
        (is string= before (file-text path))
        ;; written once it can be read again
        (tuatara.defaults::state-list-drop list '("/nowhere/kept-files"))
        (tuatara.defaults::finish-state-writes)
        (is equal (cons (list "New" "/notes/new.txt" 0 "" () nil) entries) (tuatara.defaults::read-state-file list path))))))

(define-test (kept-files bookmark-to-a-path-not-there-kept)
  ;; a bookmark to a server's folder not mapped just now: gone to, said so, and kept
  (with-fresh-state
    (let ((tuatara.defaults::*modal-editing* nil)
          (list tuatara.defaults::*bookmarks*))
      (tuatara.defaults::state-list-put list (kept-file-entries list))
      (tuatara.defaults::finish-state-writes)
      (let ((f (run-keys "C-x m b U n m a p p e d RET" :width 100)))
        (true (tuatara.frontend.fake:fake-frame-showing f "no longer there")))
      (tuatara.defaults::finish-state-writes)
      (true (find "Unmapped" (tuatara.defaults::read-state-file list (tuatara.defaults::state-list-path list))
                  :key #'first :test #'string=)))))

(define-test (kept-files bookmarks-never-dropped-for-their-number)
  ;; past *bookmarks-kept* a bookmark set is kept and says so; none is dropped, here, on disk, from
  ;; another session or among the lines a later release wrote; the remembered lists are still cut
  (with-test-directory (dir)
    (with-fresh-state
      (let* ((tuatara.defaults::*modal-editing* nil)
             (tuatara.defaults::*bookmark-set-asks-categories* nil)
             (tuatara-user:*bookmarks-kept* 3)
             (tuatara-user:*recent-files-max* 3)
             (list tuatara.defaults::*bookmarks*)
             (path (tuatara.defaults::state-list-path list))
             (file (namestring (put-file (merge-pathnames "notes.txt" dir) (format nil "one~%two~%")))))
        (flet ((mark (name) (list name "/notes/plain.txt" 0 "" () nil))
               (names () (mapcar #'first (tuatara.defaults::read-state-file list path))))
          (put-state-file list (format nil "zz a later release's line one~%zz two~%zz three~%zz four~%"))
          (tuatara.defaults::state-list-put list (mapcar #'mark '("bm-e" "bm-d" "bm-c" "bm-b" "bm-a")))
          (tuatara.defaults::finish-state-writes)
          (is equal '("bm-e" "bm-d" "bm-c" "bm-b" "bm-a") (names))
          (is = 5 (length (tuatara.defaults::state-list-entries list)))
          (is = 4 (count-if (lambda (line) (uiop:string-prefix-p "zz" line))
                            (uiop:split-string (file-text path) :separator '(#\Newline))))
          ;; another session's put, past the limit too
          (let ((other (another-session list)))
            (tuatara.defaults::state-list-put other (list (mark "bm-f")))
            (tuatara.defaults::finish-state-writes)
            (tuatara.defaults::write-state-list other))
          (is equal '("bm-f" "bm-e" "bm-d" "bm-c" "bm-b" "bm-a") (names))
          ;; set by C-x m m: kept, and the echo area says how many there are
          (let ((f (run-keys "C-x m m RET" :file file :width 120)))
            (true (tuatara.frontend.fake:fake-frame-showing-across-rows f "7 bookmarks, more than *bookmarks-kept* (3)")))
          (tuatara.defaults::finish-state-writes)
          (is equal '("notes.txt" "bm-f" "bm-e" "bm-d" "bm-c" "bm-b" "bm-a") (names))
          ;; the oldest still listed and gone to
          (let ((f (run-keys "C-x m l" :width 120 :height 20)))
            (true (tuatara.frontend.fake:fake-frame-showing f "notes.txt"))
            (true (tuatara.frontend.fake:fake-frame-showing f "bm-a")))
          (true (find "bm-a" (tuatara.defaults::bookmark-names) :test #'string=))
          (forget-file-buffers)
          ;; a remembered list keeps no more than its limit
          (tuatara.defaults::state-list-put tuatara.defaults::*recent-files*
                                            (mapcar (lambda (n) (cons (format nil "/notes/~a.txt" n) nil)) '(1 2 3 4 5)))
          (tuatara.defaults::finish-state-writes)
          (is = 3 (length (tuatara.defaults::read-recent-files))))))))
