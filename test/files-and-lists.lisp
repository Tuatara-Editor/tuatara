;;;; test/files-and-lists.lisp --- Files, archives, rimu lists, servers and packages kept right.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.tests)

(define-test files-and-lists
  :description "Work on files and the lists of them done as said: archives told and refused by what the
programs say, auto-saved copies listed in the background, bookmarks kept current cheaply, .gitignore
files capped, AppImages unpacked once at a time, sessions opened on a character's start, rimu lists
naming the right entry and the right duplicates, questions given up with the program asking them,
and packages' refusals read only when there is one.")

(defun thread-of-call (name thunk &key (seconds 10))
  "The thread the function NAME is first called on while THUNK runs, waited
for up to SECONDS; NIL when it is not called."
  (let ((thread nil))
    (sb-int:encapsulate name 'thread-seen
                        (lambda (function &rest arguments)
                          (unless thread (setf thread sb-thread:*current-thread*))
                          (apply function arguments)))
    (unwind-protect
         (progn (funcall thunk)
                (tuatara.runtime:wait-until (lambda () thread) seconds))
      (sb-int:unencapsulate name 'thread-seen))))

;;; Archives

(define-test (files-and-lists archive-failure-carried-as-a-condition)
  ;; what a program reading an archive did is kept in the condition, not read back from its words
  (let ((kind (assoc :zip tuatara.defaults::*archive-kinds*)))
    (with-functions-replaced ((tuatara.io:sandbox-run (lambda (&rest arguments)
                                                        (declare (ignore arguments))
                                                        (values nil :too-much nil))))
      (let ((condition (handler-case (tuatara.defaults::run-archive-program :bsdtar kind '("-t"))
                         (error (e) e))))
        (true (typep condition 'tuatara.defaults::archive-program-failed))
        (is eq :too-much (and (typep condition 'tuatara.defaults::archive-program-failed)
                              (tuatara.defaults::archive-program-outcome condition)))))
    (with-functions-replaced ((tuatara.io:sandbox-run (lambda (&rest arguments)
                                                        (declare (ignore arguments))
                                                        (values nil :failed "Wrong password"))))
      (let ((condition (handler-case (tuatara.defaults::run-archive-program :7zz kind '("l"))
                         (error (e) e))))
        (true (and (typep condition 'tuatara.defaults::archive-program-failed)
                   (tuatara.defaults::archive-program-locked-p condition)))))))

(define-test (files-and-lists seven-zip-not-its-own-asked-once)
  ;; a program that answers but is not 7-Zip's own is kept as such, not asked again at each use
  (let ((asked 0)
        (kept tuatara.defaults::**seven-zip**))
    (setf tuatara.defaults::**seven-zip** nil)
    (unwind-protect
         (with-functions-replaced ((tuatara.io:sandbox-find-program (lambda (name) (and (equal name "7zz") "/x/7zz")))
                                   (tuatara.io:sandbox-run (lambda (&rest arguments)
                                                             (declare (ignore arguments))
                                                             (incf asked)
                                                             (values (tuatara.text:string-octets "7-Zip 16.02 p7zip")
                                                                     :done nil))))
           (false (tuatara.defaults::seven-zip))
           (false (tuatara.defaults::seven-zip))
           (is = 1 asked))
      (setf tuatara.defaults::**seven-zip** kept))))

(define-test (files-and-lists archive-kind-from-the-copy)
  ;; the kind is told from the sealed copy the programs read, not from the file read again
  (with-test-directory (dir)
    (let ((path (namestring (merge-pathnames "box.zip" dir))))
      (alexandria:write-byte-vector-into-file
       (coerce #(80 75 5 6 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) '(vector (unsigned-byte 8))) path)
      (tuatara.defaults::call-with-archive-copy
       path
       (lambda (copy)
         ;; the file changed after the copy was made
         (alexandria:write-string-into-file "plain text now" path :if-exists :supersede)
         (is eq :zip (first (tuatara.defaults::archive-kind path copy))))))))

;;; Auto-saves, bookmarks, recent files

(define-test (files-and-lists auto-saved-path-files-listed)
  ;; the copy of a file named *.path is a copy, not the file holding a long name
  (with-test-directory (dir)
    (let* ((copies (namestring (merge-pathnames "copies/" dir)))
           (tuatara-user:*auto-save-directory* copies)
           (file (namestring (merge-pathnames "notes.path" dir))))
      (alexandria:write-string-into-file "old" file)
      (ensure-directories-exist copies)
      (alexandria:write-string-into-file "new" (tuatara.defaults::auto-save-path file))
      (true (find file (tuatara.defaults::auto-save-copies)
                  :key (lambda (copy) (getf copy :file)) :test #'string=)))))

(define-test (files-and-lists auto-saves-listed-away-from-the-editor)
  ;; the copies are looked at on a thread of their own, so a drive not answering holds nothing up
  (with-test-directory (dir)
    (let ((tuatara-user:*auto-save-directory* (namestring (merge-pathnames "copies/" dir)))
          (buffer (tuatara.defaults::buffer-in-mode "*Auto-saves*" 'tuatara.defaults::auto-saves-mode)))
      (unwind-protect
           (let ((thread (thread-of-call 'tuatara.defaults::auto-save-copies
                                         (lambda () (tuatara.defaults::auto-saves-show buffer)))))
             (true thread)
             (false (eq thread sb-thread:*current-thread*)))
        (tuatara.buffer:kill-buffer buffer)))))

(define-test (files-and-lists auto-saves-latest-listing-wins)
  ;; of two listings of the copies that overlap, the one begun first and ended last is let go
  (let ((buffer (tuatara.defaults::buffer-in-mode "*Auto-saves*" 'tuatara.defaults::auto-saves-mode)))
    (unwind-protect
         (progn
           (setf (tuatara.buffer:buffer-local buffer 'tuatara.defaults::auto-saves-listing) 2)
           (tuatara.defaults::auto-saves-take buffer 2 '() 2)
           (let ((filled (tuatara.buffer:buffer-string buffer)))
             (tuatara.defaults::auto-saves-take
              buffer 1 (list (list :copy "/x/old" :file "/x/stale.txt" :time 0 :size 1 :state :older)) 2)
             (is string= filled (tuatara.buffer:buffer-string buffer))))
      (tuatara.buffer:kill-buffer buffer))))

(define-test (files-and-lists bookmark-manager-key-cheap)
  ;; the key the shown manager is kept current by reads the bookmarks once a second at most
  (tuatara.defaults::bookmarks-key nil)
  (true (<= (calls-of 'tuatara.defaults::state-list-entries
                      (lambda () (dotimes (i 20) (tuatara.defaults::bookmarks-key nil))))
            1)))

(define-test (files-and-lists office-documents-held-back)
  ;; an Office document may hold macros: not opened outside unless its kind is added
  (false (intersection '(:docx :xlsx :pptx :odt :ods :odp) tuatara-user:*open-externally-kinds*)))

(define-test (files-and-lists recent-files-examined-away-from-the-editor)
  ;; the files of the list of recent files are examined on a thread of their own
  (let ((thread (thread-of-call 'tuatara.io:path-stamp
                                (lambda ()
                                  (let ((gathering (tuatara.defaults::start-recent-file-stamps
                                                    (list (cons "/nonexistent-recent" nil)))))
                                    (tuatara.defaults::take-recent-file-stamps gathering
                                                                               (make-hash-table :test 'equal)))))))
    (true thread)
    (false (eq thread sb-thread:*current-thread*))))

;;; Files and directories

(define-test (files-and-lists gitignore-capped)
  ;; a .gitignore larger than the cap has no rules; a line too long is passed over
  (with-test-directory (dir)
    (let ((large (namestring (merge-pathnames "large/.gitignore" dir)))
          (long (namestring (merge-pathnames "long/.gitignore" dir))))
      (ensure-directories-exist large)
      (ensure-directories-exist long)
      (let ((tuatara.defaults::*gitignore-most-octets* 64))
        (alexandria:write-string-into-file (format nil "~{~a~%~}" (loop repeat 20 collect "*.log")) large)
        (false (tuatara.defaults::gitignore-rules large (directory-namestring large))))
      (alexandria:write-string-into-file (format nil "~a~%*.tmp~%" (make-string 5000 :initial-element #\a)) long)
      (is = 1 (length (tuatara.defaults::gitignore-rules long (directory-namestring long))))))
  ;; a file whose size the system gives as 0 but which gives more is held to the cap as it is read
  (is eq :too-large (nth-value 1 (tuatara.defaults::read-text-file "/proc/self/smaps" :most 10000))))

(defun slow-unpacker (dir name seconds)
  "A program called NAME in DIR that unpacks as an AppImage does, slowly: its
AppRun written first, then, SECONDS later, the file late; its path."
  (let ((program (namestring (merge-pathnames name dir))))
    (alexandria:write-string-into-file
     (format nil "#!/bin/sh~%mkdir -p squashfs-root && : > squashfs-root/AppRun~%sleep ~a~%: > squashfs-root/late~%"
             seconds)
     program)
    (sb-posix:chmod program #o700)
    program))

(defun unpack-in-thread (program cancel)
  "A thread unpacking PROGRAM as an AppImage (UNPACKED-APPIMAGE) with CANCEL,
which gives the directory, or :FAILED when it is refused."
  (sb-thread:make-thread (lambda ()
                           (handler-case (tuatara.defaults::unpacked-appimage program cancel)
                             (error () :failed)))))

(defun forget-unpacked (name)
  "Delete what the AppImage NAME was unpacked into in the cache."
  (ignore-errors (tuatara.io:delete-tree (tuatara.defaults::private-cache-directory
                                          (format nil "appimages/~a/" name)))))

(define-test (files-and-lists appimage-unpacked-once-at-a-time)
  ;; two unpackings of one program at once: the second waits for all of the first, AppRun written
  ;; early or not, and nothing is deleted under it
  (with-test-directory (dir)
    (let ((program (slow-unpacker dir "slow-unpacker" 0.6)))
      (unwind-protect
           (let ((first (unpack-in-thread program (list nil))))
             (sleep 0.2)
             (let* ((second (unpack-in-thread program (list nil)))
                    (b (sb-thread:join-thread second :default :failed :timeout 30))
                    (late (and (stringp b) (probe-file (concatenate 'string b "late"))))
                    (a (sb-thread:join-thread first :default :failed :timeout 30)))
               (true (stringp a))
               (is equal a b)
               (true late)))
        (forget-unpacked "slow-unpacker")))))

(define-test (files-and-lists appimage-wait-stopped)
  ;; a wait for another unpacking to end is stopped when the job is
  (with-test-directory (dir)
    (let ((program (slow-unpacker dir "stopped-unpacker" 3)))
      (unwind-protect
           (let ((first (unpack-in-thread program (list nil))))
             (sleep 0.2)
             (let* ((start (get-internal-real-time))
                    (second (unpack-in-thread program (list t)))
                    (b (sb-thread:join-thread second :default :timeout :timeout 30)))
               (is eq :failed b)
               (true (< (- (get-internal-real-time) start) (* 2 internal-time-units-per-second))))
             (sb-thread:join-thread first :default nil :timeout 30))
        (forget-unpacked "stopped-unpacker")))))

(define-test (files-and-lists session-point-on-a-character)
  ;; a point kept inside a character, the file changed since, opens on that character's start
  (with-test-directory (dir)
    (let ((path (namestring (merge-pathnames "accent.txt" dir))))
      (alexandria:write-string-into-file "é" path :external-format :utf-8)
      (let ((buffer (handler-case (tuatara.defaults::session-open (list :file 1 path))
                      (error () nil))))
        (true buffer)
        (when buffer
          (is = 0 (tuatara.buffer:buffer-point buffer))
          (tuatara.buffer:kill-buffer buffer))))))

(define-test (files-and-lists sweep-of-old-copies-in-the-background)
  ;; making a private directory never waits on the sweep of old ones
  (with-test-directory (dir)
    (let ((directory (tuatara.io:slash-ended (tuatara.io:native dir))))
      (remhash directory tuatara.defaults::*old-copies-swept*)
      (let ((thread (thread-of-call 'tuatara.defaults::forget-old-copies
                                    (lambda ()
                                      (tuatara.defaults::delete-private-directory
                                       (tuatara.defaults::new-private-directory directory))))))
        (true thread)
        (false (eq thread sb-thread:*current-thread*))))))

(define-test (files-and-lists writing-candidates-kept)
  ;; a question whose answer is written to makes a directory's entries once while it does not change
  (with-test-directory (dir)
    (let ((tuatara.defaults::*file-candidates-kept* nil)
          (chosen (list (namestring (merge-pathnames "a.txt" dir)))))
      (alexandria:write-string-into-file "a" (merge-pathnames "a.txt" dir))
      (is = 1 (calls-of 'tuatara.defaults::file-candidates-of
                        (lambda ()
                          (dotimes (i 3)
                            (tuatara.defaults::directory-candidates (pathname dir) chosen :writing t))))))))

(define-test (files-and-lists rename-asks-for-a-path)
  ;; renaming the file visited asks as for any path, with its directories completed
  (with-test-directory (dir)
    (let ((file (namestring (merge-pathnames "old.txt" dir)))
          (asked nil))
      (alexandria:write-string-into-file "text" file)
      (sb-int:encapsulate 'tuatara.defaults::read-path 'seen
                          (lambda (function &rest arguments) (setf asked t) (apply function arguments)))
      (unwind-protect
           (run-keys (list (format nil "C-x C-f C-a C-k ~a RET" (rimu-typed file))
                           ;; the question of C-x C-f is not the one looked for
                           (lambda () (setf asked nil))
                           (format nil "M-x ~a RET C-g" (rimu-typed "rename-visited-file"))))
        (sb-int:unencapsulate 'tuatara.defaults::read-path 'seen)
        (kill-buffers-visiting dir))
      (true asked))))

(define-test (files-and-lists rename-takes-the-name-typed)
  ;; a new name that is part of another file's is taken as typed, not as that file
  (with-test-directory (dir)
    (let ((file (namestring (merge-pathnames "old.txt" dir))))
      (alexandria:write-string-into-file "text" file)
      (alexandria:write-string-into-file "other" (merge-pathnames "report.org" dir))
      (unwind-protect
           (run-keys (list (format nil "C-x C-f C-a C-k ~a RET" (rimu-typed file))
                           (format nil "M-x ~a RET C-a C-k ~a RET" (rimu-typed "rename-visited-file")
                                   (rimu-typed (namestring (merge-pathnames "port.org" dir))))))
        (kill-buffers-visiting dir))
      (true (probe-file (merge-pathnames "port.org" dir)))
      (is string= "other" (alexandria:read-file-into-string (merge-pathnames "report.org" dir))))))

(define-test (files-and-lists path-from-a-safe-file-opened-safely)
  ;; a path written in a file opened safely opens its target safely, no C-u needed
  (with-test-directory (dir)
    (let ((target (namestring (merge-pathnames "target.txt" dir)))
          (note (namestring (merge-pathnames "note.txt" dir))))
      (alexandria:write-string-into-file "target" target)
      (alexandria:write-string-into-file target note)
      (unwind-protect
           (progn
             (run-keys (format nil "C-u C-x C-f C-a C-k ~a RET C-x C-p" (rimu-typed note)))
             (let ((buffer (tuatara.defaults::buffer-visiting target)))
               (true buffer)
               (true (and buffer (tuatara.io:safe-visit-p buffer)))))
        (kill-buffers-visiting dir)))))

(define-test (files-and-lists shrunk-file-holds-less)
  ;; a file cut short while it is read does not hold what it had to
  (with-test-directory (dir)
    (let ((path (namestring (merge-pathnames "short.bin" dir)))
          (real #'tuatara.io:fd-stamp))
      (alexandria:write-string-into-file "abc" path)
      (with-functions-replaced ((tuatara.io:fd-stamp
                                 (lambda (&rest arguments)
                                   ;; the size as it was when asked, before the file was cut
                                   (let ((s (apply real arguments)))
                                     (tuatara.io:make-stamp (tuatara.io:stamp-dev s) (tuatara.io:stamp-ino s) 6
                                                             (tuatara.io:stamp-mtime s) (tuatara.io:stamp-nlink s)
                                                             (tuatara.io:stamp-mode s) (tuatara.io:stamp-uid s)
                                                             (tuatara.io:stamp-gid s))))))
        (false (tuatara.defaults::file-holds-p path (tuatara.text:string-octets "abcdef")))))))

(define-test (files-and-lists wiping-sizes-asked-in-the-job)
  ;; the sizes of the files to wipe are asked by the job, not on the editor's thread
  (with-test-directory (dir)
    (let ((path (namestring (merge-pathnames "gone.txt" dir))))
      (alexandria:write-string-into-file "secret" path)
      (let ((thread (thread-of-call 'tuatara.io:path-size (lambda () (tuatara.defaults::start-wiping (list path))))))
        (true thread)
        (false (eq thread sb-thread:*current-thread*)))
      (tuatara.runtime:wait-until (lambda () (not (probe-file path))) 10))))

(define-test (files-and-lists note-link-patterns-refused)
  ;; a pattern from a note that could take for ever is not run
  (false (tuatara.defaults::formatted-files-scanner "(.*)*X" :untrusted t))
  (true (tuatara.defaults::formatted-files-scanner "(.*)*X"))
  (let ((scanner (tuatara.defaults::denote-files-scanner "(a+)+b" :untrusted t)))
    ;; matched as the text it is
    (true (cl-ppcre:scan scanner "x(a+)+b"))
    (false (cl-ppcre:scan scanner "aab"))))

(define-test (files-and-lists trust-forgotten-told)
  ;; forgetting a project's trust drops it from both lists and tells those who keep answers
  (with-test-directory (dir)
    (let ((root (tuatara.io:slash-ended (tuatara.io:native dir)))
          (told nil))
      (tuatara.defaults::state-list-put tuatara.defaults::*trusted* (list root))
      (let ((tuatara.defaults::*trust-changed-functions* (list (lambda (r) (setf told r)))))
        (tuatara.defaults::forget-project-trust root))
      (false (member root (tuatara.defaults::state-list-entries tuatara.defaults::*trusted*) :test #'equal))
      (is equal root told))))

;;; Rimu

(define-test (files-and-lists one-letter-name-is-the-file)
  ;; a file called a is the file, not the directory
  (with-test-directory (dir)
    (alexandria:write-string-into-file "x" (merge-pathnames "a" dir))
    (let ((f (rimu-session dir (list "g f a RET" (format nil "M-x ~a RET" (rimu-typed "show-file-name"))))))
      (true (tuatara.frontend.fake:fake-frame-showing f (namestring (merge-pathnames "a" dir)))))
    (rimu-forget)))

(define-test (files-and-lists duplicates-leave-out-links-and-empty-files)
  ;; another name of one file is no copy of it, and empty files hold nothing to free
  (with-found-tree (dir)
    (let ((box (merge-pathnames "box/" dir)))
      (sb-posix:link (namestring (merge-pathnames "copy.txt" box)) (namestring (merge-pathnames "link.txt" box)))
      (alexandria:write-string-into-file "" (merge-pathnames "empty-1.txt" box))
      (alexandria:write-string-into-file "" (merge-pathnames "empty-2.txt" box))
      (multiple-value-bind (f editor) (found-session "rimu-duplicates" box nil)
        (declare (ignore f))
        (let ((lines (rimu-lines editor)))
          (true (line-with lines "2 files the same as another, in 1 group"))
          (false (line-with lines "empty-1.txt")))))))

(define-test (files-and-lists trash-question-names-escaped)
  ;; a name from the trash's own records is shown with its control characters written out
  (with-operations-tree (dir)
    (let ((name (format nil "e~cx.txt" (code-char 27)))
          (asked '()))
      (alexandria:write-string-into-file "x" (here dir name))
      (ops-session dir (format nil "g f e RET D y"))
      (sb-int:encapsulate 'tuatara.defaults::yes-or-no-p* 'seen
                          (lambda (function prompt &rest arguments)
                            (push prompt asked)
                            (apply function prompt arguments)))
      (unwind-protect (ops-session dir "g t d x n RET" :width 200)
        (sb-int:unencapsulate 'tuatara.defaults::yes-or-no-p* 'seen))
      (true asked)
      (false (find-if (lambda (prompt) (find (code-char 27) prompt)) asked)))))

;;; Servers and packages

(define-test (files-and-lists ssh-question-given-up-with-its-program)
  ;; a question still open when the program that ran ssh ends is given up, not waited on
  (let* ((given-up (list nil))
         (thread (sb-thread:make-thread
                  (lambda ()
                    (let ((tuatara.command::*running-editor* (cons nil (sb-thread:main-thread)))
                          (tuatara.defaults::*questions-given-up* given-up))
                      (tuatara.defaults::ask-on-editor (lambda () "answer")))))))
    (sleep 0.2)
    (setf (car given-up) t)
    (let ((result (sb-thread:join-thread thread :default :still-waiting :timeout 5)))
      (when (eq result :still-waiting) (sb-thread:terminate-thread thread))
      (false result))))

(define-test (files-and-lists ssh-question-closed-with-its-program)
  ;; the question put in the minibuffer ends once the program that asked it has
  (let ((update (tuatara.defaults::quit-when-given-up (list nil))))
    (is = 1/4 (funcall update)))
  (fail (funcall (tuatara.defaults::quit-when-given-up (list t))) tuatara.runtime:quit))

(define-test (files-and-lists hidden-question-closed-with-its-program)
  ;; a passphrase asked for a program closes as the other questions do once the program has ended
  (let ((answer :none))
    (run-keys (list (lambda ()
                      (setf answer (handler-case
                                       (tuatara.command:read-password
                                        "Passphrase: " :update (tuatara.defaults::quit-when-given-up (list t)))
                                     (tuatara.runtime:quit () :given-up))))))
    (is eq :given-up answer)))

(define-test (files-and-lists server-listing-bounded)
  ;; a listing of a directory on a server holds 8 M characters of names at most
  (is = (* 8 1024 1024) tuatara.defaults::*sftp-most-listed-characters*))

(define-test (files-and-lists refusal-read-only-when-kept)
  ;; while a package is compiled apart there is no kept refusal, and nothing is read for one
  (let ((tuatara.packages::*package-compiling* :apart)
        (asked '()))
    (sb-int:encapsulate 'tuatara.packages::read-kept-list 'seen
                        (lambda (function file) (push file asked) (funcall function file)))
    (unwind-protect
         (ignore-errors (tuatara.packages::refuse-as-before (tuatara.packages::make-package-check "none" nil)))
      (sb-int:unencapsulate 'tuatara.packages::read-kept-list 'seen))
    (false (member nil asked))))
