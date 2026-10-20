;;;; test/external.lisp --- A file opened in the desktop's own program, on asking.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.tests)

(define-test external
  :description "O in a list or a PDF's view, and M-x open-externally, hand a picture, a PDF, a sound or
a video to the desktop's program (xdg-open) after asking: only kinds told by the file's octets, with a
name that agrees; a program, a text, or a PDF named as a picture are refused, and why is said. A
program of your own runs shut in on private copies of files named by kind and number, their names
written back as the files' own; an AppImage is told by its header.")

(defmacro with-stand-in-opener ((record directory) &body body)
  "Run BODY with *OPEN-EXTERNALLY-PROGRAM* a script in DIRECTORY that writes
the names it is given to the file RECORD, as xdg-open would open them."
  (let ((script (gensym "SCRIPT")))
    `(let* ((,record (merge-pathnames "opened.txt" ,directory))
            (,script (merge-pathnames "stand-in-xdg-open" ,directory)))
       (alexandria:write-string-into-file (format nil "#!/bin/sh~%printf '%s\\n' \"$@\" >> '~a'~%" (namestring ,record))
                                          ,script :if-exists :supersede)
       (sb-posix:chmod (namestring ,script) #o700)
       (let ((tuatara-user:*open-externally-program* (namestring ,script)))
         ,@body))))

(defun opened-names (record)
  "The names the stand-in opener was given so far, waiting a moment for it."
  (loop repeat 100
        for text = (and (probe-file record) (alexandria:read-file-into-string record))
        when (and text (plusp (length text))) return (uiop:split-string (string-right-trim '(#\Newline) text) :separator '(#\Newline))
        do (sleep 0.02)))

(define-test (external opened-on-asking)
  (with-rimu-tree (dir)
    (with-stand-in-opener (record dir)
      (alexandria:write-byte-vector-into-file *tiny-png* (merge-pathnames "photo.png" dir) :if-exists :supersede)
      ;; asked, and yes: a copy of it opened, yours alone and read only, the file itself never given
      (let ((f (rimu-session dir "g f p h o t o . p n g RET O y RET" :width 200)))
        (true (tuatara.frontend.fake:fake-frame-showing f "Open photo.png (PNG image, 2 × 2"))
        (true (tuatara.frontend.fake:fake-frame-showing f "outside the sandbox? (y or n)"))
        (true (tuatara.frontend.fake:fake-frame-showing f "Opening photo.png in the desktop's program..."))
        (let ((opened (first (opened-names record))))
          (true (uiop:string-prefix-p (tuatara.defaults::opened-copies-directory) opened))
          (true (uiop:string-suffix-p opened "/photo.png"))
          (is equalp *tiny-png* (alexandria:read-file-into-byte-vector opened))
          (is = #o400 (logand #o777 (sb-posix:stat-mode (sb-posix:stat opened))))
          (is = #o700 (logand #o777 (sb-posix:stat-mode (sb-posix:stat (tuatara.io::parent-path opened))))))
        (loop repeat 100 while tuatara.defaults::*openings* do (sleep 0.02)))
      ;; a file changed between being looked at and being copied: not opened
      (true (search "changed after it was looked at"
                    (let ((tuatara.defaults::*openings* '()))
                      (tuatara.defaults::open-copy-externally (namestring (merge-pathnames "photo.png" dir)) "photo.png" :jpeg
                                                              "JPEG image" tuatara-user:*open-externally-program*))))
      ;; no: not opened
      (delete-file record)
      (let ((f (rimu-session dir "g f p h o t o . p n g RET O n RET")))
        (true (tuatara.frontend.fake:fake-frame-showing f "Not opened"))
        (false (probe-file record)))
      ;; not asked, with *open-externally-ask* NIL
      (let ((tuatara-user:*open-externally-ask* nil))
        (rimu-session dir "g f p h o t o . p n g RET O")
        (true (opened-names record)))
      (delete-file (merge-pathnames "photo.png" dir)))))

(define-test (external refused)
  (with-rimu-tree (dir)
    (with-stand-in-opener (record dir)
      (let ((tuatara-user:*open-externally-ask* nil))
        (flet ((refused (name octets words)
                 (alexandria:write-byte-vector-into-file octets (merge-pathnames name dir) :if-exists :supersede)
                 (let ((f (rimu-session dir (format nil "g f ~a RET O" (rimu-typed name)) :width 200)))
                   (true (tuatara.frontend.fake:fake-frame-showing f words))
                   (false (probe-file record)))
                 (delete-file (merge-pathnames name dir))))
          ;; a program named as a picture
          (refused "tool.png" (kind-octets (list #x7F 69 76 70 2 1 1 0 0 0 0 0 0 0 0 0 2 0 62 0))
                   "tool.png is ELF 64-bit executable, x86-64 by its contents: not opened outside Tuatara")
          ;; a PDF named as a picture: a kind it may open, but not what its name says
          (refused "photo.png" (pdf-octets '("Hello")) "not what its name says: not opened outside Tuatara")
          ;; text
          (refused "notes.png" (kind-octets "hello, world") "notes.png is not a picture, a PDF, a sound or a video")
          ;; an SVG image: it can hold a program a web browser would run
          (refused "drawing.svg" (kind-octets "<svg xmlns=\"http://www.w3.org/2000/svg\"><script>alert(1)</script></svg>")
                   "by its contents: not opened outside Tuatara (*open-externally-kinds*)"))
        ;; no program to open with
        (alexandria:write-byte-vector-into-file *tiny-png* (merge-pathnames "photo.png" dir) :if-exists :supersede)
        (let ((tuatara-user:*open-externally-program* "no-such-xdg-open"))
          (let ((f (rimu-session dir "g f p h o t o . p n g RET O" :width 200)))
            (true (tuatara.frontend.fake:fake-frame-showing f "no-such-xdg-open was not found"))))
        (delete-file (merge-pathnames "photo.png" dir))))))

(define-test (external from-a-pdf-view)
  (with-test-directory (dir)
    (with-stand-in-opener (record dir)
      (let ((file (merge-pathnames "report.pdf" dir))
            (tuatara-user:*open-externally-ask* nil))
        (alexandria:write-byte-vector-into-file (pdf-octets '("Hello")) file)
        (pdf-forget)
        ;; O in a PDF's view opens the PDF; no cursor over its page, the cursor back in its text
        (let ((f (pdf-session file nil "O" :cell-pixels '(10 20))))
          (is equalp (alexandria:read-file-into-byte-vector file)
              (alexandria:read-file-into-byte-vector (first (opened-names record))))
          (true (uiop:string-suffix-p (first (opened-names record)) "/report.pdf"))
          (is eq :hidden (tuatara.frontend.fake:fake-cursor-shape f)))
        (pdf-forget)
        (let ((f (pdf-session file nil "T" :cell-pixels '(10 20))))
          (false (eq :hidden (tuatara.frontend.fake:fake-cursor-shape f))))
        (pdf-forget)))))

(define-test (external chosen-programs)
  ;; a file's MIME type, told by its contents, refined by its name where the kind holds sound or video alike
  (is string= "image/png" (tuatara.defaults::file-mime-type :png "photo.png"))
  (is string= "audio/ogg" (tuatara.defaults::file-mime-type :ogg "song.ogg"))
  (is string= "video/ogg" (tuatara.defaults::file-mime-type :ogg "film.OGV"))
  (is string= "video/webm" (tuatara.defaults::file-mime-type :matroska "clip.webm"))
  (false (tuatara.defaults::file-mime-type :zip "a.zip"))
  ;; M-x check-health names the types sent elsewhere
  (let ((tuatara-user:*open-externally-programs* '(("application/pdf" . "zathura") ("video/*" "mpv"))))
    (true (search "; application/pdf, video/* to programs of your choosing"
                  (tuatara.defaults::external-program-words))))
  ;; what runs a thing, said one way in M-x check-health
  (is string= "mailboxes by /usr/bin/readpst, shut in" (tuatara.defaults::program-health-words "mailboxes" "/usr/bin/readpst" "install readpst"))
  (is string= "mailboxes by none: install readpst" (tuatara.defaults::program-health-words "mailboxes" nil "install readpst"))
  (is string= "outside Tuatara by xdg-open, on asking" (tuatara.defaults::program-health-words "outside Tuatara" "xdg-open" "" :how "on asking"))
  ;; a likelihood shown in a face by how likely
  (is eq :info (tuatara.defaults::likelihood-face 97.2))
  (is eq :info (tuatara.defaults::likelihood-face 80))
  (is eq :warning (tuatara.defaults::likelihood-face 50))
  (is eq :error (tuatara.defaults::likelihood-face 3.1))
  ;; *open-externally-programs*: a program of your choosing for a type, its arguments before the name,
  ;; the exact type before its group; the desktop's program for a type with none
  (with-rimu-tree (dir)
    (with-stand-in-opener (record dir)
      (let ((chooser tuatara-user:*open-externally-program*)
            (tuatara-user:*open-externally-program* "no-such-xdg-open")
            (tuatara-user:*open-externally-ask* nil))
        (alexandria:write-byte-vector-into-file *tiny-png* (merge-pathnames "photo.png" dir) :if-exists :supersede)
        (flet ((open-with (programs)
                 (when (probe-file record) (delete-file record))
                 (let ((tuatara-user:*open-externally-programs* programs))
                   (rimu-session dir "g f p h o t o . p n g RET O" :width 200))))
          (let ((f (open-with `(("image/*" ,chooser "--group") ("image/png" ,chooser "--fit")))))
            (true (tuatara.frontend.fake:fake-frame-showing f "Opening photo.png in stand-in-xdg-open..."))
            (let ((names (opened-names record)))
              (is string= "--fit" (first names))
              (true (uiop:string-suffix-p (second names) "/photo.png"))))
          (loop repeat 100 while tuatara.defaults::*openings* do (sleep 0.02))
          (open-with `(("IMAGE/*" ,chooser "--group")))
          (is string= "--group" (first (opened-names record)))
          (loop repeat 100 while tuatara.defaults::*openings* do (sleep 0.02))
          (open-with `(("image/png" . ,chooser)))
          (is = 1 (length (opened-names record)))
          (loop repeat 100 while tuatara.defaults::*openings* do (sleep 0.02))
          ;; asked, the question names the program
          (let ((tuatara-user:*open-externally-ask* t)
                (tuatara-user:*open-externally-programs* `(("image/png" . ,chooser))))
            (true (tuatara.frontend.fake:fake-frame-showing
                   (rimu-session dir "g f p h o t o . p n g RET O n RET" :width 200)
                   "in stand-in-xdg-open, outside the sandbox?")))
          ;; not found, none for the type, and an entry that is no program: said, nothing opened
          (true (tuatara.frontend.fake:fake-frame-showing
                 (open-with '(("image/png" . "no-such-viewer")))
                 "no-such-viewer, the program for image/png in *open-externally-programs*, was not found"))
          (true (tuatara.frontend.fake:fake-frame-showing
                 (open-with `(("application/pdf" . ,chooser)))
                 "no-such-xdg-open was not found"))
          (true (tuatara.frontend.fake:fake-frame-showing
                 (open-with '(("image/png" 42)))
                 "the program for image/png is not a name"))
          (false (probe-file record)))
        (delete-file (merge-pathnames "photo.png" dir))))))

(define-test (external links-copied-not-followed)
  ;; a link opened: its file's content copied into a file of its own; the file it leads to untouched, mode and all
  (with-rimu-tree (dir)
    (with-stand-in-opener (record dir)
      (let ((tuatara-user:*open-externally-ask* nil)
            (evidence (merge-pathnames "evidence.png" dir)))
        (alexandria:write-byte-vector-into-file *tiny-png* evidence :if-exists :supersede)
        (sb-posix:chmod (namestring evidence) #o644)
        (sb-posix:symlink (namestring evidence) (namestring (merge-pathnames "link.png" dir)))
        (rimu-session dir "g f l i n k . p n g RET O" :width 200)
        (let ((opened (first (opened-names record))))
          (true opened)
          (is eq :regular (tuatara.io:stamp-kind (tuatara.io:path-stamp opened :follow nil)))
          (is equalp *tiny-png* (alexandria:read-file-into-byte-vector opened))
          (is = #o400 (logand #o777 (sb-posix:stat-mode (sb-posix:stat opened)))))
        (is = #o644 (logand #o777 (sb-posix:stat-mode (sb-posix:stat (namestring evidence)))))
        (loop repeat 100 while tuatara.defaults::*openings* do (sleep 0.02))
        (delete-file (merge-pathnames "link.png" dir))
        (delete-file evidence)))))

(define-test (external odd-names-said)
  ;; a name with a backslash in it is said as it is
  (with-test-directory (dir)
    (let ((file (concatenate 'string (tuatara.io:native dir) "a\\b.png")))
      (put-file (sb-ext:parse-native-namestring file) "hello, world")
      (true (search "a\\b.png is not a picture" (tuatara.defaults::open-externally-refusal file))))))

(define-test (external run-on-copies)
  ;; a copy's name: its kind, its number and a plain ending, nothing else of the file's name
  (is string= "candidate-2.jpg" (tuatara.defaults::copy-file-name "candidate" 2 "/x/Crowd [1]*.JPG"))
  (is string= "master-1" (tuatara.defaults::copy-file-name "master" 1 "/x/name.a b"))
  ;; copies' names written as their files' in one pass, an unknown one left as it is
  (let ((names (make-hash-table :test 'equal)))
    (setf (gethash "/tmp/files/photo-1.jpg" names) "/case/alice.jpg"
          (gethash "/tmp/files/photo-2" names) "/case/no-ending")
    (is string= "/case/alice.jpg: no face; /case/no-ending.: none; /tmp/files/photo-3.png"
        (tuatara.defaults::copies-original-names "/tmp/files/photo-1.jpg: no face; /tmp/files/photo-2.: none; /tmp/files/photo-3.png"
                                                 names))
    (is string= "/case/no-ending.x" (tuatara.defaults::copies-original-names "/tmp/files/photo-2.x" names))
    (is string= "/case/alice.jpg was stopped"
        (tuatara.defaults::run-on-copies-failure "It" :failed (format nil "/tmp/files/photo-1.jpg was stopped~%") names
                                                 'tuatara-user:*sandbox-seconds*))
    (is string= "It ended without a report"
        (tuatara.defaults::run-on-copies-failure "It" :failed "" names 'tuatara-user:*sandbox-seconds*)))
  ;; the report read from its first {, a line before it left out
  (is = 1 (tuatara.text:json-integer (tuatara.defaults::copies-output-json (tuatara.text:string-octets (format nil "a line~%{\"k\":1}")))
                                     "k"))
  (is eq nil (tuatara.defaults::copies-output-json (tuatara.text:string-octets "no report")))
  ;; a program of yours run shut in on copies: it sees them by their own names, never the folder of the files
  (with-test-directory (dir)
    (unless (eq :ready (tuatara.io:sandbox-status))
      (skip "no sandbox here"))
    (let* ((given (concatenate 'string (tuatara.io:native dir) "case/"))
           (files (mapcar (lambda (name)
                            (let ((path (concatenate 'string given name)))
                              (tuatara.io:ensure-directories path)
                              (tuatara.io:replace-file path name)
                              path))
                          '("alice.jpg" "crowd *.png")))
           (program (concatenate 'string (tuatara.io:native dir) "bin/reader")))
      (tuatara.io:ensure-directories program)
      (tuatara.io:replace-file program (format nil "#!/bin/sh
[ -e '~a' ] && { echo 'the folder is seen' >&2; exit 1; }
cat \"$2\" && echo \" $*\" && echo \"$4 is gone\" >&2 && exit 1
" (string-right-trim "/" given)))
      (sb-posix:chmod program #o700)
      (is equal program (tuatara.defaults::user-program-path program))
      (is eq nil (tuatara.defaults::user-program-path (concatenate 'string program "-gone")))
      (multiple-value-bind (output outcome said names)
          (tuatara.defaults::run-on-copies program (list (cons "master" (list (first files))) (cons "photo" (rest files)))
                                           (lambda (masters photos) (append (list "--master") masters (list "--photo") photos))
                                           :seconds 30)
        (is eq :failed outcome)
        (is string= "alice.jpg --master /tmp/files/master-1.jpg --photo /tmp/files/photo-1.png"
            (string-trim '(#\Newline) (sb-ext:octets-to-string output)))
        (is string= (format nil "~a is gone" (tuatara.command:abbreviated-path (second files)))
            (tuatara.defaults::run-on-copies-failure "It" outcome said names 'tuatara-user:*sandbox-seconds*)))
      ;; its report read, or why there is none, the program named by a setting
      (let ((tuatara-user:*sandbox-seconds* 30))
        (multiple-value-bind (report names failure)
            (tuatara.defaults::run-on-copies-for-report
             program (list (cons "master" (list (first files))) (cons "photo" (rest files)))
             (lambda (masters photos) (append (list "--master") masters (list "--photo") photos))
             :label "It" :missing "It is not there" :seconds-setting 'tuatara-user:*sandbox-seconds*)
          (is eq nil report)
          (is string= (second files) (gethash "/tmp/files/photo-1.png" names))
          (is string= (format nil "~a is gone" (tuatara.command:abbreviated-path (second files))) failure))
        (is string= "It is not there"
            (handler-case (tuatara.defaults::run-on-copies-for-report
                           (concatenate 'string program "-gone") '() (constantly '())
                           :label "It" :missing "It is not there" :seconds-setting 'tuatara-user:*sandbox-seconds*)
              (error (e) (princ-to-string e)))))
      ;; the copies gone, the files as they were
      (is equal nil (tuatara.io:directory-entries (tuatara.defaults::private-cache-directory "sandbox-copies/")))
      (is string= "alice.jpg" (tuatara.io:file-text (first files))))))

(define-test (external appimage-told)
  ;; an AppImage told apart by its header, not its name
  (with-test-directory (dir)
    (let ((appimage (namestring (merge-pathnames "engine.bin" dir)))
          (plain (namestring (merge-pathnames "engine.AppImage" dir))))
      (with-open-file (out appimage :direction :output :element-type '(unsigned-byte 8))
        (write-sequence (coerce #(#x7f 69 76 70 2 1 1 0 65 73 2 0 0 0 0 0) '(vector (unsigned-byte 8))) out))
      (with-open-file (out plain :direction :output :element-type '(unsigned-byte 8))
        (write-sequence (coerce #(#x7f 69 76 70 2 1 1 0 0 0 0 0 0 0 0 0) '(vector (unsigned-byte 8))) out))
      (true (tuatara.defaults::appimage-p appimage))
      (false (tuatara.defaults::appimage-p plain)))))

(define-test (external new-file-private-from-the-start)
  ;; a new file made readable by its owner alone from the start, never over a file there, nor through a link
  (with-test-directory (dir)
    (let ((path (tuatara.io:path-join dir "made.txt"))
          (link (tuatara.io:path-join dir "link"))
          (part (tuatara.io:path-join dir "part")))
      (is string= path (tuatara.io:write-new-file path "kia ora"))
      (is = #o400 (logand #o777 (tuatara.io:stamp-mode (tuatara.io:path-stamp path))))
      (is string= "kia ora" (tuatara.io:file-text path))
      (fail (tuatara.io:write-new-file path "again"))
      (is string= "kia ora" (tuatara.io:file-text path))
      (sb-posix:symlink (tuatara.io:path-join dir "elsewhere") link)
      (fail (tuatara.io:write-new-file link "through a link"))
      (false (tuatara.io:path-kind (tuatara.io:path-join dir "elsewhere") :follow nil))
      ;; octets, the first END of them, with a mode of its own
      (tuatara.io:write-new-file part (tuatara.text:string-octets "abcdef") :end 3 :mode #o600)
      (is = #o600 (logand #o777 (tuatara.io:stamp-mode (tuatara.io:path-stamp part))))
      (is string= "abc" (tuatara.io:file-text part))
      ;; a private copy is one such file
      (let ((copy (tuatara.defaults::write-private-copy "copy.bin" (tuatara.text:string-octets "copied"))))
        (unwind-protect
             (progn (is = #o400 (logand #o777 (tuatara.io:stamp-mode (tuatara.io:path-stamp copy))))
                    (is string= "copied" (tuatara.io:file-text copy)))
          (tuatara.io:delete-tree (tuatara.io:parent-path copy)))))))

(define-test (external private-folders-in-use-kept)
  ;; a folder still in use is never swept as old, however long ago it was last written to;
  ;; one made to go a day later goes, and one deleted is held no longer
  (with-test-directory (dir)
    (let* ((directory (tuatara.io:slash-ended (tuatara.io:native dir)))
           (held (tuatara.defaults::new-private-folder directory))
           (two-days-ago (- (sb-posix:time) (* 2 86400))))
      (flet ((age (folder) (sb-posix:utimes (string-right-trim "/" folder) two-days-ago two-days-ago)))
        (unwind-protect
             (progn
               (age held)
               (tuatara.defaults::delete-private-folder (tuatara.defaults::new-private-folder directory))
               (is eq :directory (tuatara.io:path-kind held :follow nil))
               (let ((loose (tuatara.defaults::new-private-folder directory :hold nil)))
                 (age loose)
                 (let ((another (tuatara.defaults::new-private-folder directory)))
                   (false (tuatara.io:path-kind loose :follow nil))
                   (tuatara.defaults::delete-private-folder another)
                   (false (tuatara.io:path-kind another :follow nil))
                   (false (gethash another tuatara.defaults::*private-folders-held*))))
               ;; kept fresh for other sessions' sweeps
               (age held)
               (let ((tuatara.defaults::*private-folders-touched* 0))
                 (tuatara.defaults::keep-private-folders-fresh nil))
               (true (> (tuatara.defaults::mtime-universal-time (tuatara.io:stamp-mtime (tuatara.io:path-stamp held)))
                        (- (get-universal-time) 3600))))
          (ignore-errors (tuatara.io:delete-tree held))
          (remhash held tuatara.defaults::*private-folders-held*))))))

(defvar *private-folder-readings* '()
  "The buffers the private folder test reads into.")

(define-test (external private-folder-deleted-after-its-job)
  ;; a buffer READING-BUFFER gives a folder of its own: made for you alone, named while the buffer lives,
  ;; deleted when it is killed once its job has ended, and not before
  (let* ((tuatara.command:*editor* (nth-value 1 (run-keys "" :text "")))
         (tuatara.runtime:*stop-wait* 0.05)
         (gate (list nil))
         (job nil)
         (buffer (tuatara:reading-buffer '*private-folder-readings* (lambda (b) (tuatara.buffer:buffer-local b 'private-folder-reading))
                                         "/nonexistent/read.bin" "*private folder test*" (format nil "Reading...~%") 'tuatara:special-mode
                                         (lambda (b)
                                           (setf (tuatara.buffer:buffer-local b 'private-folder-reading) "/nonexistent/read.bin"
                                                 job (tuatara.defaults::%make-count-job :buffer b :what "Buffer" :total 10))
                                           (tuatara.runtime:start-job-thread
                                            job (lambda (job) (declare (ignore job)) (loop repeat 1000 until (car gate) do (sleep 0.01)))
                                            :name "tuatara test job"))
                                         :private-folder "opened/"))
         (folder (tuatara:buffer-private-folder buffer)))
    (unwind-protect
         (progn
           (true (uiop:string-prefix-p (tuatara.defaults::opened-copies-directory) folder))
           (is = #o700 (logand #o777 (tuatara.io:stamp-mode (tuatara.io:path-stamp folder))))
           (tuatara.io:write-new-file (tuatara.io:path-join folder "made") "made")
           (tuatara.buffer:kill-buffer buffer)
           ;; its job still running: the folder kept
           (is eq :directory (tuatara.io:path-kind folder :follow nil))
           (setf (car gate) t)
           (loop repeat 500 while (tuatara.io:path-kind folder :follow nil) do (sleep 0.01))
           (false (tuatara.io:path-kind folder :follow nil)))
      (setf (car gate) t)
      (when job (tuatara.runtime:forget-job job))
      (when (tuatara.io:path-kind folder :follow nil) (tuatara.io:delete-tree folder)))))
