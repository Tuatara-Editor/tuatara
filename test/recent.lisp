;;;; test/recent.lisp --- The files visited lately: C-x C-r.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.tests)

(define-test recent-files
  :description "The list of recent files: kept on disk, private, the latest first and each once; a file visited safely opened safely again; C-x C-r offers them.")

(defmacro with-fresh-recent-files (&body body)
  "Run BODY with an empty list of recent files of its own, in a new state
directory removed afterwards."
  (let ((old (gensym)) (state (gensym)))
    `(with-test-directory (,state)
       (let ((,old (sb-posix:getenv "XDG_STATE_HOME")))
         (sb-posix:setenv "XDG_STATE_HOME" (namestring ,state) 1)
         (unwind-protect (progn ,@body)
           ;; what is still being written goes to this directory, before it goes
           (tuatara.defaults::finish-state-writes)
           (if ,old (sb-posix:setenv "XDG_STATE_HOME" ,old 1) (sb-posix:unsetenv "XDG_STATE_HOME")))))))

(define-test (recent-files kept-on-disk)
  (with-fresh-recent-files
    ;; the latest first, each once, at most *recent-files-max*
    (let ((tuatara-user:*recent-files-max* 3))
      (dolist (path '("/a" "/b" "/c" "/b" "/d")) (tuatara.defaults::remember-file path))
      (is equal '(("/d") ("/b") ("/c")) (tuatara.defaults::read-recent-files)))
    ;; written in the background, readable by its owner alone
    (tuatara.defaults::finish-state-writes)
    (is = #o600 (logand #o777 (sb-posix:stat-mode (sb-posix:stat (namestring (tuatara.defaults::state-list-path tuatara.defaults::*recent-files*))))))
    ;; any name comes back as it went: a newline, a backslash; safe is kept
    (let ((odd (format nil "/x~%y\\z")))
      (tuatara.defaults::remember-file odd :safe t)
      (is equal (cons odd t) (first (tuatara.defaults::read-recent-files))))
    ;; lines that make no sense are passed over
    (tuatara.defaults::finish-state-writes)
    (tuatara.io:write-private-file (tuatara.defaults::state-list-path tuatara.defaults::*recent-files*)
                                   (format nil "junk~%file /a~%safe~%file ~%# a note~%safe /b~%file /a~%"))
    (is equal '(("/a") ("/b" . t)) (tuatara.defaults::read-recent-files))
    ;; NIL remembers nothing
    (let ((tuatara-user:*remember-recent-files* nil))
      (tuatara.defaults::remember-file "/never")
      (false (assoc "/never" (tuatara.defaults::read-recent-files) :test #'string=)))))

(define-test (recent-files visiting-again)
  (with-fresh-recent-files
    (with-test-directory (dir)
      (let ((a (namestring (put-file (merge-pathnames "a.txt" dir) (format nil "A~%"))))
            (b (namestring (put-file (merge-pathnames "b.txt" dir) (format nil "B~%"))))
            (c (namestring (merge-pathnames "c.txt" dir))))
        (unwind-protect
             (progn
               ;; nothing yet
               (multiple-value-bind (f editor) (run-keys "C-x C-r" :width 100 :height 12)
                 (declare (ignore editor))
                 (true (tuatara.frontend.fake:fake-frame-showing f "No recent files")))
               ;; visited in turn: C-x C-r RET goes back to the one before, the current one left out
               (multiple-value-bind (f editor)
                   (run-keys (format nil "C-x C-f C-a C-k ~a RET C-x C-f C-a C-k ~a RET C-x C-r" (key-string a) (key-string b))
                             :width 100 :height 12)
                 (declare (ignore editor))
                 (multiple-value-bind (screen) (tuatara.frontend.fake:fake-frame-with f "Open recent file:")
                   (true (find-if (lambda (row) (search "a.txt" row)) screen))
                   (false (find-if (lambda (row) (and (search "b.txt" row) (search "-rw" row))) screen))
                   ;; its permissions, size and when it changed beside it
                   (true (find-if (lambda (row) (and (search "a.txt" row) (search "-rw" row) (search "2 B" row))) screen))))
               (kill-buffers-visiting dir)
               (multiple-value-bind (f editor) (run-keys (format nil "C-x C-f C-a C-k ~a RET C-x C-r RET" (key-string a)) :width 100 :height 12)
                 (declare (ignore f))
                 (is string= "b.txt" (tuatara.buffer:buffer-name (tuatara.display:window-buffer (tuatara.command:editor-window editor)))))
               (is equal (list b a) (mapcar #'car (subseq (tuatara.defaults::read-recent-files) 0 2)))
               (kill-buffers-visiting dir)
               ;; a file visited safely comes back safely, marked safe in the list
               (run-keys (format nil "C-u C-x C-f C-a C-k ~a RET" (key-string a)) :width 100 :height 12)
               (is equal (cons a t) (first (tuatara.defaults::read-recent-files)))
               (kill-buffers-visiting dir)
               ;; wide enough for the safe mark after a long test folder's path
               (multiple-value-bind (f editor) (run-keys "C-x C-r" :width 200 :height 12)
                 (declare (ignore editor))
                 (true (tuatara.frontend.fake:fake-frame-showing f "a.txt" "safe")))
               (multiple-value-bind (f editor) (run-keys "C-x C-r RET" :width 100 :height 12)
                 (declare (ignore f))
                 (true (tuatara.io:safe-visit-p (tuatara.display:window-buffer (tuatara.command:editor-window editor)))))
               (kill-buffers-visiting dir)
               ;; a new file joins the list once saved; one no longer there says so
               (run-keys (format nil "C-x C-f C-a C-k ~a RET i x C-g C-x C-s" (key-string c)) :width 100 :height 12)
               (is string= c (car (first (tuatara.defaults::read-recent-files))))
               (kill-buffers-visiting dir)
               (delete-file b)
               (multiple-value-bind (f editor) (run-keys "C-x C-r" :width 100 :height 12)
                 (declare (ignore editor))
                 (true (tuatara.frontend.fake:fake-frame-showing f "b.txt" "not found"))))
          (kill-buffers-visiting dir))))))

(define-test (recent-files ending-a-safe-visit)
  (with-fresh-recent-files
    (with-test-directory (dir)
      (let ((csv (namestring (put-file (merge-pathnames "t.csv" dir) (format nil "id,name~%1,dog~%")))))
        (flet ((open-safely (then &key (width 100))
                 (run-keys (format nil "C-u C-x C-f C-a C-k ~a RET ~a" (key-string csv) then) :width width :height 12)))
          (unwind-protect
               (progn
                 ;; M-x end-safe-visit: as usual from now on, where it is
                 (multiple-value-bind (f editor) (open-safely "j l M-x e n d - s a f e - v i s i t RET")
                   (let ((b (tuatara.display:window-buffer (tuatara.command:editor-window editor))))
                     (false (tuatara.io:safe-visit-p b))
                     (false (tuatara.buffer:buffer-local b :read-only))
                     (is eq 'tuatara.defaults::csv-mode (type-of (tuatara.modes:buffer-major-mode b)))
                     (is = 9 (tuatara.buffer:buffer-point b))
                     (true (tuatara.frontend.fake:fake-frame-showing f "t.csv is open as usual now"))
                     (true (search "CSV" (tuatara.frontend.fake:fake-screen-line f 10)))
                     (false (search "Safe" (tuatara.frontend.fake:fake-screen-line f 10)))))
                 ;; the files visited lately open it as usual from then on
                 (is equal (cons csv nil) (first (tuatara.defaults::read-recent-files)))
                 (kill-buffers-visiting dir)
                 ;; a mode chosen while it was open safely is kept
                 (multiple-value-bind (f editor) (open-safely "M-x t e x t - m o d e RET M-x e n d - s a f e - v i s i t RET")
                   (declare (ignore f))
                   (is eq 'tuatara.defaults::text-mode
                       (type-of (tuatara.modes:buffer-major-mode (tuatara.display:window-buffer (tuatara.command:editor-window editor))))))
                 (kill-buffers-visiting dir)
                 ;; C-x C-f of a file open safely says how to open it as usual
                 (true (tuatara.frontend.fake:fake-frame-showing
                        (open-safely (format nil "C-x C-f C-a C-k ~a RET" (key-string csv)) :width 120)
                        "t.csv is open safely; M-x end-safe-visit opens it as usual"))
                 (kill-buffers-visiting dir)
                 ;; a buffer not open safely says so
                 (true (tuatara.frontend.fake:fake-frame-showing
                        (run-keys "M-x e n d - s a f e - v i s i t RET" :text "x" :width 100)
                        "*scratch* is not open safely")))
            (kill-buffers-visiting dir)))))))

(define-test (recent-files opening-safely-by-itself)
  (with-fresh-recent-files
    (with-test-directory (dir)
      (let* ((binary (namestring (put-file (merge-pathnames "blob.dat" dir) (format nil "MZ~c~cpayload" (code-char 0) (code-char 1)))))
             (evidence (ensure-directories-exist (merge-pathnames "evidence/" dir)))
             (kept (namestring (put-file (merge-pathnames "note.txt" evidence) "hello")))
             (plain (namestring (put-file (merge-pathnames "plain.txt" dir) "hello"))))
        (flet ((open-it (path &rest more)
                 (multiple-value-bind (f editor)
                     (run-keys (format nil "C-x C-f C-a C-k ~a RET~{ ~a~}" (key-string path) more) :width 150 :height 12)
                   (values f (tuatara.display:window-buffer (tuatara.command:editor-window editor))))))
          (unwind-protect
               (progn
                 ;; a file that looks binary opens safely, and says why
                 (multiple-value-bind (f b) (open-it binary)
                   (true (tuatara.io:safe-visit-p b))
                   (true (tuatara.frontend.fake:fake-frame-showing
                          f "blob.dat opened safely: it looks binary (*open-binary-files-safely*); M-x end-safe-visit opens it as usual"))
                   ;; and shows in the hex view (*hex-view-binary-files*), offsets and octets
                   (true (tuatara.display:hex-view-p b))
                   (true (tuatara.frontend.fake:fake-frame-showing f "4d 5a 00 01")))

                 ;; remembered so among the files visited lately
                 (is equal (cons binary t) (first (tuatara.defaults::read-recent-files)))
                 (kill-buffers-visiting dir)
                 ;; M-x hex-mode shows it as text; NIL keeps such files as text from the start; text never is
                 (false (tuatara.display:hex-view-p (nth-value 1 (open-it binary "M-x h e x - m o d e RET"))))
                 (kill-buffers-visiting dir)
                 (let ((tuatara-user:*hex-view-binary-files* nil))
                   (false (tuatara.display:hex-view-p (nth-value 1 (open-it binary)))))
                 (kill-buffers-visiting dir)
                 (false (tuatara.display:hex-view-p (nth-value 1 (open-it plain))))
                 (kill-buffers-visiting dir)
                 (let ((tuatara-user:*open-binary-files-safely* nil))
                   (false (tuatara.io:safe-visit-p (nth-value 1 (open-it binary)))))
                 (kill-buffers-visiting dir)
                 ;; a file in one of *safe-directories*, and one out of them
                 (let ((tuatara-user:*safe-directories* (list (namestring evidence))))
                   (multiple-value-bind (f b) (open-it kept)
                     (true (tuatara.io:safe-visit-p b))
                     (true (tuatara.frontend.fake:fake-frame-showing f "note.txt opened safely: it is in"))
                     (true (tuatara.frontend.fake:fake-frame-showing f "(*safe-directories*)")))
                   (kill-buffers-visiting dir)
                   (false (tuatara.io:safe-visit-p (nth-value 1 (open-it plain))))
                   (kill-buffers-visiting dir)
                   ;; a link elsewhere leading into it, whatever its name: in it
                   (dolist (name '("plain-link" "st*r" "q?" "br[1]"))
                     (let ((link (concatenate 'string (namestring dir) name)))
                       (sb-posix:symlink kept link)
                       (true (tuatara.defaults::safe-directory-of link) "~a" name)
                       (sb-posix:unlink link)))
                   ;; named on the command line too
                   (multiple-value-bind (f editor) (run-keys "" :file kept :width 150 :height 12)
                     (true (tuatara.io:safe-visit-p (tuatara.display:window-buffer (tuatara.command:editor-window editor))))
                     (true (tuatara.frontend.fake:fake-frame-showing f "note.txt opened safely: it is in")))))
            (kill-buffers-visiting dir)))))))

(define-test (recent-files safe-directories-as-written)
  ;; a directory alone, or a list with a dotted tail, still opens its files safely
  (with-fresh-recent-files
    (with-test-directory (dir)
      (let* ((evidence (ensure-directories-exist (merge-pathnames "evidence/" dir)))
             (kept (namestring (put-file (merge-pathnames "note.txt" evidence) "hello")))
             (plain (namestring (put-file (merge-pathnames "plain.txt" dir) "hello"))))
        (unwind-protect
             (progn
               (dolist (setting (list (namestring evidence)
                                      (cons "/nowhere/at/all/" (namestring evidence))
                                      (list* "/nowhere/at/all/" "/nor/here/" (namestring evidence))))
                 (let ((tuatara-user:*safe-directories* setting))
                   (true (tuatara.defaults::visit-safely-reason kept) "~s" setting)
                   (false (tuatara.defaults::visit-safely-reason plain) "~s" setting)))
               ;; opened by C-x C-f too
               (let ((tuatara-user:*safe-directories* (namestring evidence)))
                 (multiple-value-bind (f editor)
                     (run-keys (format nil "C-x C-f C-a C-k ~a RET" (key-string kept)) :width 150 :height 12)
                   (true (tuatara.io:safe-visit-p (tuatara.display:window-buffer (tuatara.command:editor-window editor))))
                   (true (tuatara.frontend.fake:fake-frame-showing f "note.txt opened safely: it is in")))))
          (kill-buffers-visiting dir))))))

(define-test (recent-files both-ways)
  ;; a file opened both ways is listed twice, the latest first, the safe one marked
  (with-fresh-recent-files
    (with-test-directory (dir)
      (let ((a (namestring (put-file (merge-pathnames "a.txt" dir) "a"))))
        (unwind-protect
             (progn
               (run-keys (format nil "C-x C-f C-a C-k ~a RET" (key-string a)) :width 100 :height 12)
               (kill-buffers-visiting dir)
               (run-keys (format nil "C-u C-x C-f C-a C-k ~a RET" (key-string a)) :width 100 :height 12)
               (kill-buffers-visiting dir)
               (is equal (list (cons a t) (cons a nil)) (subseq (tuatara.defaults::read-recent-files) 0 2))
               ;; wide enough for the safe mark after a long test folder's path
               (multiple-value-bind (f editor) (run-keys "C-x C-r" :width 200 :height 12)
                 (declare (ignore editor))
                 (true (tuatara.frontend.fake:fake-frame-showing f "1/2" "Open recent file:"))
                 (true (tuatara.frontend.fake:fake-frame-showing f "a.txt safe")))
               ;; RET takes the latest, the safe one; C-n the other
               (true (tuatara.io:safe-visit-p (tuatara.display:window-buffer
                                               (tuatara.command:editor-window (nth-value 1 (run-keys "C-x C-r RET" :width 100 :height 12))))))
               (kill-buffers-visiting dir)
               (false (tuatara.io:safe-visit-p (tuatara.display:window-buffer
                                                (tuatara.command:editor-window (nth-value 1 (run-keys "C-x C-r C-n RET" :width 100 :height 12))))))
               ;; still listed once each way
               (is = 2 (count a (tuatara.defaults::read-recent-files) :key #'car :test #'string=)))
          (kill-buffers-visiting dir))))))

(define-test (recent-files cleaned-at-start)
  (with-fresh-recent-files
    (with-test-directory (dir)
      (let* ((here (namestring (put-file (merge-pathnames "here.txt" dir) "x")))
             (gone (namestring (merge-pathnames "gone.txt" dir)))
             (locked (ensure-directories-exist (merge-pathnames "locked/" dir)))
             (hidden (namestring (put-file (merge-pathnames "hidden.txt" locked) "x"))))
        (sb-posix:chmod (namestring locked) 0)
        (unwind-protect
             (progn
               (dolist (path (list hidden gone here)) (tuatara.defaults::remember-file path))
               ;; the editor starting takes out the file no longer there; one it cannot look at stays
               (let ((tuatara.defaults::*recent-files-cleaned* nil))
                 (run-keys "C-g" :width 100 :height 12))
               (is equal (list here hidden) (mapcar #'car (tuatara.defaults::read-recent-files)))
               ;; and says so in *Messages*
               (true (search "Cleaning up the list of recent files...done (1 removed)"
                             (tuatara.buffer:buffer-string (tuatara.buffer:find-buffer "*Messages*"))))
               ;; once a session
               (tuatara.defaults::remember-file gone)
               (run-keys "C-g" :width 100 :height 12)
               (true (assoc gone (tuatara.defaults::read-recent-files) :test #'string=))
               ;; and not at all with *recent-files-auto-cleanup* NIL
               (let ((tuatara.defaults::*recent-files-cleaned* nil)
                     (tuatara-user:*recent-files-auto-cleanup* nil))
                 (run-keys "C-g" :width 100 :height 12))
               (true (assoc gone (tuatara.defaults::read-recent-files) :test #'string=)))
          (sb-posix:chmod (namestring locked) #o700))))))

(define-test (recent-files state-files-locked)
  ;; the files sessions keep together are read, changed and written holding a lock beside
  ;; them, in the background: held by another session, a visit goes on at once, the file
  ;; shown in the list at once and written once the lock is let go
  (with-fresh-recent-files
    (let ((lock (concatenate 'string (tuatara.io:native (tuatara.defaults::state-list-path tuatara.defaults::*recent-files*)) ".lock"))
          (path (tuatara.defaults::state-list-path tuatara.defaults::*recent-files*)))
      (tuatara.defaults::remember-file "/b")
      (tuatara.defaults::finish-state-writes)
      (let ((fd (sb-posix:open lock (logior sb-posix:o-rdwr sb-posix:o-creat) #o600)))
        (unwind-protect
             (progn
               (true (tuatara.runtime:lock-fd fd))
               (let ((start (get-internal-real-time)))
                 (tuatara.defaults::remember-file "/a")
                 (false (tuatara.io:call-with-state-lock path (lambda () t)))
                 (true (< (- (get-internal-real-time) start) (* 1/2 internal-time-units-per-second))))
               (is equal '(("/a") ("/b")) (tuatara.defaults::read-recent-files))
               (false (search "/a" (file-text path))))
          (sb-posix:close fd)))
      ;; let go: written, what was there kept
      (tuatara.defaults::finish-state-writes)
      (is eq :done (tuatara.io:call-with-state-lock path (lambda () :done)))
      (true (search (format nil "file /a~%file /b~%") (file-text path)))
      (is = #o600 (logand #o777 (sb-posix:stat-mode (sb-posix:stat lock)))))))

(define-test (recent-files chosen-paths-merged)
  ;; the paths chosen lately: what another session wrote meanwhile is kept, not overwritten,
  ;; and read again here once it changes
  (with-fresh-recent-files
    (let ((file (tuatara.defaults::chosen-paths-file)))
      (tuatara.io:write-private-file file (format nil "/other/session~%"))
      (is equal '("/other/session") (tuatara.defaults::chosen-paths))
      ;; another session chooses something meanwhile
      (tuatara.io:write-private-file file (format nil "/other/later~%/other/session~%"))
      (tuatara.defaults::note-chosen-path "/mine/file")
      (is equal '("/mine/file" "/other/later" "/other/session") (tuatara.defaults::chosen-paths))
      (tuatara.defaults::finish-state-writes)
      (is string= (format nil "/mine/file~%/other/later~%/other/session~%") (file-text file))
      ;; written again by another session: read again here
      (tuatara.io:write-private-file file (format nil "/third/one~%/mine/file~%"))
      (is equal '("/third/one" "/mine/file") (tuatara.defaults::chosen-paths)))))
