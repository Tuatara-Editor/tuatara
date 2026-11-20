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
the names it is given to the file RECORD, as xdg-open would open them, and
DIRECTORY untrusted (*UNTRUSTED-DIRECTORIES*)."
  (let ((script (gensym "SCRIPT")))
    `(let* ((,record (merge-pathnames "opened.txt" ,directory))
            (,script (merge-pathnames "stand-in-xdg-open" ,directory)))
       (put-program ,script (format nil "#!/bin/sh~%printf '%s\\n' \"$@\" >> '~a'~%" (namestring ,record)))
       (let ((tuatara-user:*open-externally-program* (namestring ,script))
             (tuatara-user:*untrusted-directories* (list (namestring ,directory))))
         ,@body))))

(defun opened-names (record)
  "The names the stand-in opener was given so far, waiting a moment for it."
  (loop repeat 100
        for text = (and (probe-file record) (alexandria:read-file-into-string record))
        when (and text
                  (plusp (length text))) return (uiop:split-string (string-right-trim '(#\Newline) text)
                                                                   :separator '(#\Newline))
        do (sleep 0.02)))

(define-test (external opened-on-asking)
  (with-rimu-tree (dir)
    (with-stand-in-opener (record dir)
      (alexandria:write-byte-vector-into-file *tiny-png* (merge-pathnames "photo.png" dir) :if-exists :supersede)
      ;; asked, and yes: a copy of it opened, yours alone and read only, the file itself never given
      (let ((f (rimu-session dir "g f p h o t o . p n g RET O y RET" :width 200)))
        (true (tuatara.frontend.fake:fake-frame-showing f "Open photo.png (PNG image, 2 × 2"))
        (true (tuatara.frontend.fake:fake-frame-showing f "outside the sandbox? Untrusted: y a copy, e the file"))
        (true (tuatara.frontend.fake:fake-frame-showing f "which may run what it holds (y, n or e, then RET)"))
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

;; Untrusted files: those in an evidence directory or in *untrusted-directories*
;; (the download directory as standard), links followed; every other file is trusted

(defmacro with-untrusted-places ((dir record downloads) &body body)
  "Run BODY in a new directory DIR with the stand-in opener writing to RECORD
\(WITH-STAND-IN-OPENER), *UNTRUSTED-DIRECTORIES* as standard,
DOWNLOADS, the directory downloads/ in DIR, the download directory
\(XDG_DOWNLOAD_DIR), and no evidence directories."
  `(with-rimu-tree (,dir)
     (with-stand-in-opener (,record ,dir)
       (let ((,downloads (merge-pathnames "downloads/" ,dir))
             (tuatara-user:*untrusted-directories* '(:downloads))
             (evidence tuatara-user:*evidence-directories*))
         (ensure-directories-exist ,downloads)
         (with-environment-variable ("XDG_DOWNLOAD_DIR" (namestring ,downloads))
           (unwind-protect (progn (setf tuatara-user:*evidence-directories* '()) ,@body)
             (setf tuatara-user:*evidence-directories* evidence)
             (loop repeat 100 while tuatara.defaults::*openings* do (sleep 0.02))))))))

(defun test-page (file)
  "Write a web page to FILE, a kind of file only a trusted file is opened as."
  (put-file file "<!DOCTYPE html><html><head><link rel=\"stylesheet\" href=\"page.css\"></head></html>"))

(defun copy-opened-p (record)
  "True when the stand-in opener was given a private copy (OPENED-COPIES-DIRECTORY)."
  (let ((opened (first (opened-names record))))
    (and opened (uiop:string-prefix-p (tuatara.defaults::opened-copies-directory) opened))))

(define-test (external untrusted-asked-and-copied)
  (with-untrusted-places (dir record downloads)
    (alexandria:write-byte-vector-into-file *tiny-png* (merge-pathnames "photo.png" downloads))
    ;; in the download directory: asked, then a copy opened
    (let ((f (rimu-session downloads "g f p h o t o . p n g RET O y RET" :width 240)))
      (true (tuatara.frontend.fake:fake-frame-showing f "(y, n or e, then RET)"))
      (true (copy-opened-p record)))
    (loop repeat 100 while tuatara.defaults::*openings* do (sleep 0.02))
    (delete-file record)
    ;; in an evidence directory: asked, then a copy opened
    (let ((case (merge-pathnames "case/" dir)))
      (ensure-directories-exist case)
      (alexandria:write-byte-vector-into-file *tiny-png* (merge-pathnames "exhibit.png" case))
      (setf tuatara-user:*evidence-directories* (list (namestring case)))
      (let ((f (rimu-session case "g f e x h i b i t . p n g RET O y RET" :width 240)))
        (true (tuatara.frontend.fake:fake-frame-showing f "(y, n or e, then RET)"))
        (true (copy-opened-p record))))))

(define-test (external untrusted-through-a-link)
  ;; a link elsewhere leading into the download directory is untrusted: asked, a copy opened
  (with-untrusted-places (dir record downloads)
    (alexandria:write-byte-vector-into-file *tiny-png* (merge-pathnames "photo.png" downloads))
    (sb-posix:symlink (namestring (merge-pathnames "photo.png" downloads))
                      (namestring (merge-pathnames "link.png" dir)))
    (let ((f (rimu-session dir "g f l i n k . p n g RET O y RET" :width 240)))
      (true (tuatara.frontend.fake:fake-frame-showing f "(y, n or e, then RET)"))
      (true (copy-opened-p record)))))

(define-test (external trusted-opened-in-place)
  (with-untrusted-places (dir record downloads)
    (let ((page (merge-pathnames "page.html" dir)))
      (test-page page)
      ;; elsewhere: the file itself, a web page too, without asking
      (let ((f (rimu-session dir "g f p a g e . h t m l RET O" :width 240)))
        (false (tuatara.frontend.fake:fake-frame-showing f "then RET)"))
        (is equal (list (tuatara.io:real-path (namestring page))) (opened-names record))))
    (loop repeat 100 while tuatara.defaults::*openings* do (sleep 0.02))
    (delete-file record)
    ;; nil: the download directory trusted too
    (let ((page (merge-pathnames "page.html" downloads))
          (tuatara-user:*untrusted-directories* '()))
      (test-page page)
      (let ((f (rimu-session downloads "g f p a g e . h t m l RET O" :width 240)))
        (false (tuatara.frontend.fake:fake-frame-showing f "then RET)"))
        (is equal (list (tuatara.io:real-path (namestring page))) (opened-names record))))
    (loop repeat 100 while tuatara.defaults::*openings* do (sleep 0.02))
    (delete-file record)
    ;; "/": every file untrusted, as O took them all before
    (alexandria:write-byte-vector-into-file *tiny-png* (merge-pathnames "photo.png" dir) :if-exists :supersede)
    (let ((tuatara-user:*untrusted-directories* '("/")))
      (let ((f (rimu-session dir "g f p h o t o . p n g RET O y RET" :width 240)))
        (true (tuatara.frontend.fake:fake-frame-showing f "(y, n or e, then RET)"))
        (true (copy-opened-p record))))))

(define-test (external untrusted-executed-on-e)
  (with-untrusted-places (dir record downloads)
    (let* ((page (merge-pathnames "page.html" downloads))
           (real (progn (test-page page) (tuatara.io:real-path (namestring page))))
           (sum (tuatara.defaults::file-sha256 real)))
      ;; not a kind a copy is opened of: n or e asked, and n opens nothing
      (let ((f (rimu-session downloads "g f p a g e . h t m l RET O n RET" :width 240)))
        (true (tuatara.frontend.fake:fake-frame-showing f "may run what it holds (n or e, then RET)"))
        (true (tuatara.frontend.fake:fake-frame-showing f "Not opened"))
        (false (probe-file record)))
      ;; e: the file itself
      (rimu-session downloads "g f p a g e . h t m l RET O e RET" :width 240)
      (is equal (list real) (opened-names record))
      (loop repeat 100 while tuatara.defaults::*openings* do (sleep 0.02))
      ;; its path and SHA-256 in the debug log
      (loop while (sb-concurrency:dequeue tuatara.runtime:*debug-reports*))
      (let ((tuatara.runtime:*debug-on* t))
        (true (search "trusted (SHA-256"
                      (tuatara.defaults::open-in-place-externally (namestring page) "page.html" nil "HTML document"
                                                                  tuatara-user:*open-externally-program* :execute t))))
      (let ((texts (loop for report = (sb-concurrency:dequeue tuatara.runtime:*debug-reports*)
                         while report collect (princ-to-string (fourth report)))))
        (true (find-if (lambda (text) (and (search "trusted this once" text) (search real text) (search sum text)))
                       texts))))))

;; What the security review of the one rule found, each pinned

(defun mark-downloaded (file)
  "Give FILE the mark a web browser leaves on what it downloads (user.xdg.origin.url)."
  (is = 0 (sb-alien:alien-funcall
           (sb-alien:extern-alien "setxattr" (function sb-alien:int sb-alien:c-string sb-alien:c-string
                                                       sb-alien:c-string sb-alien:unsigned-long sb-alien:int))
           (namestring file) "user.xdg.origin.url" "https://example.invalid/x" 25 0)))

(define-test (external untrusted-by-the-standard-places)
  ;; the cache's copies of outside files, removable media, the temporary directories and files marked
  ;; as downloaded are untrusted; a file of your own is not
  ;; (yours outside the temporary directories, as a home is; the cache in the tests' home, as the path to
  ;; its copy is typed)
  (with-untrusted-places (dir record downloads)
    (with-test-directory (own :in-cache t)
      (let* ((tuatara-user:*untrusted-directories* (tuatara.runtime:setting-standard-value
                                                    'tuatara-user:*untrusted-directories*))
             (tuatara-user:*cache-directory* (namestring (ensure-directories-exist (merge-pathnames "cache/" dir))))
             (copy (tuatara.io:path-join (tuatara.defaults::opened-copies-directory) "tuatara-test-copy/sheet.xlsm"))
             (mine (merge-pathnames "mine.txt" own)))
        (put-file mine "words")
        (ensure-directories-exist (sb-ext:parse-native-namestring copy))
        (put-file (sb-ext:parse-native-namestring copy) "macros")
        (unwind-protect
             (progn
               (let ((tuatara-user:*untrusted-directories* '()))
                 (true (tuatara.defaults::untrusted-path-p copy)))
               (true (tuatara.defaults::untrusted-path-p "/tmp/tuatara-test-nowhere/x.txt"))
               (true (tuatara.defaults::untrusted-path-p "/var/tmp/x.txt"))
               (true (tuatara.defaults::untrusted-path-p "/run/media/someone/STICK/repo/a.rs"))
               (true (tuatara.defaults::untrusted-path-p "/mnt/image/a.rs"))
               (false (tuatara.defaults::untrusted-path-p (namestring mine)))
               (mark-downloaded mine)
               (true (tuatara.defaults::untrusted-path-p (namestring mine)))
               ;; a copy in the cache asked about, not opened in place
               (let ((f (rimu-session (tuatara.io:parent-path copy) "g f s h e e t . x l s m RET O n RET" :width 240)))
                 (true (tuatara.frontend.fake:fake-frame-showing f "(n or e, then RET)"))
                 (false (probe-file record))))
          (tuatara.io:delete-tree (tuatara.io:parent-path copy)))))))

(define-test (external evidence-executed-as-a-copy)
  ;; e on evidence opens a private copy whatever its kind, never the file beside the evidence
  (with-untrusted-places (dir record downloads)
    (let ((case (merge-pathnames "case/" dir)))
      (ensure-directories-exist case)
      (test-page (merge-pathnames "exhibit.html" case))
      (setf tuatara-user:*evidence-directories* (list (namestring case)))
      (let ((f (rimu-session case "g f e x h i b i t . h t m l RET O e RET" :width 240)))
        (true (tuatara.frontend.fake:fake-frame-showing f "a copy of this evidence whatever its kind"))
        (true (copy-opened-p record))))))

(define-test (external opened-in-place-only-as-looked-at)
  ;; a file changed, become untrusted, or changed while summed after it was looked at is not opened
  (with-untrusted-places (dir record downloads)
    (let* ((page (merge-pathnames "page.html" dir))
           (native (progn (test-page page) (namestring page)))
           (stamp (tuatara.io:path-stamp native))
           (program tuatara-user:*open-externally-program*))
      (flet ((opened (&rest keys)
               (apply #'tuatara.defaults::open-in-place-externally
                      native "page.html" nil "HTML document" program keys)))
        (put-file page "<html>changed meanwhile</html>")
        (true (search "changed after it was looked at" (opened :stamp stamp)))
        (let ((tuatara-user:*untrusted-directories* (list (namestring dir))))
          (true (search "changed after it was looked at" (opened))))
        (sb-int:encapsulate 'tuatara.defaults::file-sha256 'change-while-summed
                            (lambda (f path &rest keys)
                              (prog1 (apply f path keys) (put-file page "<html>changed while summed!</html>"))))
        (unwind-protect (true (search "changed while it was summed" (opened :execute t)))
          (sb-int:unencapsulate 'tuatara.defaults::file-sha256 'change-while-summed))
        (false (probe-file record))))))

(define-test (external programs-told-by-name-and-launchers)
  ;; launchers the desktop starts, by their name or by a desktop entry after comments, are programs
  (with-test-directory (dir)
    (flet ((program-p (name text)
             (let ((file (merge-pathnames name dir)))
               (put-file file text)
               (tuatara.defaults::program-file-p (namestring file)))))
      (true (program-p "run.bat" "echo hello"))
      (true (program-p "install.msi" "not really"))
      (true (program-p "app.flatpakref" "[Flatpak Ref]"))
      (true (program-p "notes.txt" (format nil "# a comment~%~%[Desktop Entry]~%Exec=true~%")))
      (true (program-p "bom.txt" (format nil "~a[Desktop Entry]~%Exec=true~%" (code-char #xFEFF))))
      (false (program-p "plain.txt" "just words")))))

(define-test (external evidence-links-never-read-for-untrusted-places)
  ;; a path whose link leads to a link in an evidence directory is not followed past the evidence
  (with-untrusted-places (dir record downloads)
    (let ((case (merge-pathnames "case/" dir)))
      (ensure-directories-exist case)
      (put-file (merge-pathnames "target.txt" downloads) "words")
      (sb-posix:symlink (namestring (merge-pathnames "target.txt" downloads))
                        (namestring (merge-pathnames "b" case)))
      (sb-posix:symlink (namestring (merge-pathnames "b" case)) (namestring (merge-pathnames "a" dir)))
      (setf tuatara-user:*evidence-directories* (list (namestring case)))
      (false (tuatara.defaults::untrusted-directory-of (namestring (merge-pathnames "a" dir))))
      (true (tuatara.defaults::untrusted-path-p (namestring (merge-pathnames "a" dir)))))))

(define-test (external download-directory-read-as-the-shell-does)
  ;; the last XDG_DOWNLOAD_DIR line, ${HOME} and spaces before it taken; the home itself is none;
  ;; read again when the file changes
  (with-test-directory (dir)
    (let ((file (merge-pathnames "user-dirs.dirs" dir))
          (home (string-right-trim "/" (sb-ext:native-namestring (user-homedir-pathname)))))
      (with-environment-variable ("XDG_CONFIG_HOME" (string-right-trim "/" (namestring dir)))
        (with-environment-variable ("XDG_DOWNLOAD_DIR" "/nonexistent/tuatara-test-downloads")
          (put-file file (format nil "XDG_DOWNLOAD_DIR=\"/first/\"~%  XDG_DOWNLOAD_DIR=\"${HOME}/Fetched\"~%"))
          (is string= (format nil "~a/Fetched/" home) (tuatara.io:download-directory))
          (sleep 0.01)
          (put-file file (format nil "XDG_DOWNLOAD_DIR=\"$HOME/\"~%"))
          (is string= "/nonexistent/tuatara-test-downloads/" (tuatara.io:download-directory)))))))

(define-test (external untrusted-open-safely)
  ;; a file in the download directory, the standard untrusted directory, opens safely however it is opened;
  ;; one elsewhere opens as usual
  (with-untrusted-places (dir record downloads)
    (let ((tuatara.defaults::*modal-editing* nil))
      (flet ((opened-safely-p (file)
               (put-file file "plain words")
               (auto-save-session (list (format nil "C-x C-f C-a C-k ~a RET" (rimu-typed (namestring file)))))
               (let ((buffer (tuatara.buffer:find-buffer (file-namestring file))))
                 (prog1 (and buffer (tuatara.io:safe-visit-p buffer))
                   (when buffer (tuatara.buffer:kill-buffer buffer))))))
        (true (opened-safely-p (merge-pathnames "fetched.txt" downloads)))
        (false (opened-safely-p (merge-pathnames "mine.txt" dir)))))))

;; A file from outside shown by a view that reads it in the sandbox, as a mailbox by readpst

(defun stand-in-mailbox-view (path show)
  "Show the file at PATH, when its name ends in .pst and the sandbox may be
given it, as \"its folders\" with SHOW, as the view of a mailbox read in the
sandbox would; NIL for any other."
  (let ((native (tuatara.defaults::file-view-path path)))
    (when (and native (uiop:string-suffix-p native ".pst")
               (tuatara.defaults::file-view-can-draw-p native t "readpst"))
      (tuatara.defaults::file-view-show
       native '() #'identity
       (lambda ()
         (tuatara.defaults::make-file-view-buffer native 'tuatara.defaults::special-mode "Folders" "its folders"))
       show))))

(defmacro with-stand-in-mailbox-view ((&key (sandboxed t)) &body body)
  "Run BODY with STAND-IN-MAILBOX-VIEW first among the file views, and among
those that read in the sandbox too when SANDBOXED."
  `(let ((tuatara.defaults::*file-views* (cons 'stand-in-mailbox-view tuatara.defaults::*file-views*))
         (tuatara.defaults::*sandboxed-file-views*
           (if ,sandboxed
               (cons 'stand-in-mailbox-view tuatara.defaults::*sandboxed-file-views*)
               tuatara.defaults::*sandboxed-file-views*))
         (tuatara.defaults::*modal-editing* nil))
     ,@body))

(defun visited-buffer (file &rest keys)
  "The buffer showing FILE once C-x C-f, KEYS before it (C-u), opened it, and the frontend."
  (multiple-value-bind (f editor)
      (auto-save-session (list (format nil "~{~a ~}C-x C-f C-a C-k ~a RET" keys (rimu-typed (namestring file))))
                         :width 200)
    (values (tuatara.display:window-buffer (tuatara.command:editor-window editor)) f)))

(defun mailbox-file (file)
  "Write FILE as a stand-in mailbox: octets with a null among them; FILE."
  (put-file file (format nil "!BDN~c~cfolders" (code-char 0) (code-char 1))))

(define-test (external untrusted-shown-by-a-sandboxed-view)
  ;; a mailbox in the download directory is shown by its view read in the sandbox, not as octets;
  ;; a view not declared so leaves it to its octets, opened safely
  (with-untrusted-places (dir record downloads)
    (let ((file (mailbox-file (merge-pathnames "mail.pst" downloads))))
      (with-stand-in-mailbox-view ()
        (let ((b (visited-buffer file)))
          (is equal "its folders" (tuatara.buffer:buffer-local b :viewed-as))
          (false (tuatara.io:safe-visit-p b))
          (true (tuatara.buffer:buffer-local b :viewed-untrusted))
          (tuatara.buffer:kill-buffer b)))
      (with-stand-in-mailbox-view (:sandboxed nil)
        (let ((b (visited-buffer file)))
          (false (tuatara.buffer:buffer-local b :viewed-as))
          (true (tuatara.io:safe-visit-p b))
          (tuatara.buffer:kill-buffer b))))))

(define-test (external evidence-shown-by-no-view)
  ;; in an evidence directory, or through a link from the download directory into one, the same mailbox
  ;; opens safely as its octets: no view reads evidence in place
  (with-untrusted-places (dir record downloads)
    (let* ((case (ensure-directories-exist (merge-pathnames "case/" dir)))
           (file (mailbox-file (merge-pathnames "exhibit.pst" case)))
           (link (merge-pathnames "exhibit-link.pst" downloads)))
      (sb-posix:symlink (namestring file) (namestring link))
      (setf tuatara-user:*evidence-directories* (list (namestring case)))
      (with-stand-in-mailbox-view ()
        (dolist (name (list file link))
          (let ((b (visited-buffer name)))
            (false (tuatara.buffer:buffer-local b :viewed-as))
            (true (tuatara.io:safe-visit-p b))
            (true (tuatara.display:hex-view-p b))
            (tuatara.buffer:kill-buffer b)))))))

(define-test (external ending-a-safe-visit-shows-the-view)
  ;; a mailbox opened safely on asking, then M-x end-safe-visit: its view in the buffer's place
  (with-untrusted-places (dir record downloads)
    (let ((file (mailbox-file (merge-pathnames "mail.pst" downloads))))
      (with-stand-in-mailbox-view ()
        (multiple-value-bind (f editor)
            (auto-save-session (list (format nil "C-u C-x C-f C-a C-k ~a RET" (rimu-typed (namestring file)))
                                     "M-x e n d - s a f e - v i s i t RET")
                               :width 200)
          (let ((b (tuatara.display:window-buffer (tuatara.command:editor-window editor))))
            (is equal "its folders" (tuatara.buffer:buffer-local b :viewed-as))
            (true (tuatara.frontend.fake:fake-frame-showing f "mail.pst is open as usual now, shown as its folders"))
            (is = 1 (count-if (lambda (buffer) (equal "mail.pst" (tuatara.buffer:buffer-name buffer)))
                              tuatara.buffer:*buffers*))
            (tuatara.buffer:kill-buffer b)))))))

(define-test (external ending-a-safe-visit-carries-the-private-copy)
  ;; the view taking the buffer's place holds its private copy, kept, as a file read from another machine has
  (with-untrusted-places (dir record downloads)
    (let ((file (mailbox-file (merge-pathnames "mail.pst" downloads)))
          (copy (tuatara.defaults::new-private-directory (tuatara.defaults::private-cache-directory "remote/"))))
      (unwind-protect
           (with-stand-in-mailbox-view ()
             (multiple-value-bind (f editor)
                 (auto-save-session (list (format nil "C-u C-x C-f C-a C-k ~a RET" (rimu-typed (namestring file)))
                                          (list :until (lambda ()
                                                         (setf (tuatara.buffer:buffer-local
                                                                (tuatara.buffer:find-buffer "mail.pst")
                                                                'tuatara.defaults::private-directory)
                                                               copy)))
                                          "M-x e n d - s a f e - v i s i t RET")
                                    :width 200)
               (declare (ignore f))
               (let ((b (tuatara.display:window-buffer (tuatara.command:editor-window editor))))
                 (is equal "its folders" (tuatara.buffer:buffer-local b :viewed-as))
                 (is equal copy (tuatara.buffer:buffer-local b 'tuatara.defaults::private-directory))
                 (true (tuatara.io:path-stamp copy :failure nil))
                 (setf (tuatara.buffer:buffer-local b 'tuatara.defaults::private-directory) nil)
                 (tuatara.buffer:kill-buffer b))))
        (tuatara.defaults::delete-private-directory copy)))))

(define-test (external view-refuses-a-file-put-in-its-place)
  ;; a view of a file from outside keeps the stamp its visit judged; a file renamed into its place after
  ;; is refused when read, the same file read
  (with-untrusted-places (dir record downloads)
    (let ((file (mailbox-file (merge-pathnames "mail.pst" downloads)))
          (other (mailbox-file (merge-pathnames "other.pst" downloads))))
      (with-stand-in-mailbox-view ()
        (let* ((b (visited-buffer file))
               (stamp (tuatara.buffer:buffer-local b :viewed-stamp))
               (native (tuatara.io:native (namestring file))))
          (true (tuatara.io:same-stamped-file-p stamp (tuatara.io:path-stamp native)))
          (true (tuatara.io:call-with-file native (constantly t) :same-as stamp))
          (rename-file other file)
          (fail (tuatara.io:call-with-file native (constantly t) :same-as stamp) 'tuatara.io:file-system-error)
          (tuatara.buffer:kill-buffer b))))))

(define-test (external too-large-for-a-view-said-first)
  ;; a mailbox larger than a decoder is given opens as octets: what to set to have it read is said first,
  ;; sizes in plain units, and why it opened safely after
  (with-untrusted-places (dir record downloads)
    (let ((file (mailbox-file (merge-pathnames "mail.pst" dir)))
          (tuatara-user:*sandbox-input-limit* 10))
      (with-stand-in-mailbox-view ()
        (let ((b (visited-buffer file))
              (messages (tuatara.buffer:find-buffer "*Messages*")))
          (true (tuatara.io:safe-visit-p b))
          (true (and messages
                     (search (concatenate 'string "mail.pst opens as octets: it is 13 B, more than the 10 B a "
                                          "decoder is given: set *sandbox-input-limit* above its size, or to nil, "
                                          "to have it read. It opened safely: it looks binary")
                             (tuatara.buffer:buffer-string messages)))
                "~a" (and messages (tuatara.buffer:buffer-string messages)))
          (tuatara.buffer:kill-buffer b))))))

(define-test (external downloaded-mark-opens-safely)
  ;; a text file a browser marked as downloaded, outside the untrusted directories, opens safely, saying why
  (with-untrusted-places (dir record downloads)
    (let ((file (put-file (merge-pathnames "notes.txt" dir) "plain words")))
      (mark-downloaded file)
      (multiple-value-bind (b f) (visited-buffer file)
        (true (tuatara.io:safe-visit-p b))
        (true (tuatara.frontend.fake:fake-frame-showing
               f "notes.txt opened safely: it is marked as come from outside (user.xdg.origin.url)"))
        (tuatara.buffer:kill-buffer b)))))

(define-test (external programs-asked-wherever)
  ;; a program, by its contents or its execute bit, is asked about as an untrusted file even where files are trusted
  (with-untrusted-places (dir record downloads)
    (flet ((asked (name keys)
             (let ((f (rimu-session dir (format nil "g f ~a RET O ~a" (rimu-typed name) keys) :width 240)))
               (true (tuatara.frontend.fake:fake-frame-showing f "may run what it holds (n or e, then RET)")))))
      (alexandria:write-byte-vector-into-file (kind-octets (list #x7F 69 76 70 2 1 1 0 0 0 0 0 0 0 0 0 2 0 62 0))
                                              (merge-pathnames "tool" dir))
      (asked "tool" "n RET")
      (put-file (merge-pathnames "start.desktop" dir) (format nil "[Desktop Entry]~%Exec=true~%"))
      (asked "start.desktop" "n RET")
      (put-file (merge-pathnames "notes.txt" dir) "hello")
      (sb-posix:chmod (namestring (merge-pathnames "notes.txt" dir)) #o755)
      (asked "notes.txt" "n RET")
      (false (probe-file record))
      ;; e opens it in place
      (asked "notes.txt" "e RET")
      (is equal (list (tuatara.io:real-path (namestring (merge-pathnames "notes.txt" dir)))) (opened-names record)))))

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
          (refused "drawing.svg"
                   (kind-octets "<svg xmlns=\"http://www.w3.org/2000/svg\"><script>alert(1)</script></svg>")
                   "by its contents: not opened outside Tuatara (*open-externally-kinds*)"))
        ;; no program to open with
        (alexandria:write-byte-vector-into-file *tiny-png* (merge-pathnames "photo.png" dir) :if-exists :supersede)
        (let ((tuatara-user:*open-externally-program* "no-such-xdg-open"))
          (let ((f (rimu-session dir "g f p h o t o . p n g RET O" :width 200)))
            (true (tuatara.frontend.fake:fake-frame-showing f "no-such-xdg-open was not found"))))
        (delete-file (merge-pathnames "photo.png" dir))))))

(define-test (external only-web-and-mail-addresses-handed-on)
  ;; an address that is no web or mail address (file:, smb:, none) is refused
  ;; by the opener itself, never asked about nor handed to the desktop
  (with-test-directory (dir)
    (with-stand-in-opener (record dir)
      (let ((asked '()))
        (sb-int:encapsulate 'tuatara.command:yes-or-no-p* 'record-asking
                            (lambda (function prompt) (declare (ignore function)) (push prompt asked) nil))
        (unwind-protect
             (let ((f (auto-save-session (list (lambda () (tuatara.defaults::open-address "file:///etc/passwd"))
                                               (lambda () (tuatara.defaults::open-address "SMB://host/share"))
                                               (lambda () (tuatara.defaults::open-address "/etc/passwd"))
                                               (lambda () (tuatara.defaults::open-address "HTTPS://example.org/"))
                                               (list :wait 0.1))
                                         :width 200)))
               (true (tuatara.frontend.fake:fake-frame-showing f "Links of the kind file are not opened: file:///etc/passwd"))
               (true (tuatara.frontend.fake:fake-frame-showing f "Links of the kind smb are not opened")))
          (sb-int:unencapsulate 'tuatara.command:yes-or-no-p* 'record-asking))
        (is = 1 (length asked))
        (true (search "HTTPS://example.org/" (first asked)))
        (false (probe-file record))))))

(define-test (external from-a-pdf-view)
  (with-test-directory (dir)
    (with-stand-in-opener (record dir)
      (let ((file (merge-pathnames "report.pdf" dir))
            (tuatara-user:*open-externally-ask* nil)
            (tuatara-user:*untrusted-directories* '()))
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
  (is string= "mailboxes by /usr/bin/readpst, shut in"
      (tuatara.defaults::program-health-words "mailboxes" "/usr/bin/readpst" "install readpst"))
  (is string= "mailboxes by none: install readpst"
      (tuatara.defaults::program-health-words "mailboxes" nil "install readpst"))
  (is string= "outside Tuatara by xdg-open, on asking"
      (tuatara.defaults::program-health-words "outside Tuatara" "xdg-open" "" :how "on asking"))
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
  ;; a program of yours run shut in on copies: it sees them by their own names, never the directory of the files
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
[ -e '~a' ] && { echo 'the directory is seen' >&2; exit 1; }
cat \"$2\" && echo \" $*\" && echo \"$4 is gone\" >&2 && exit 1
" (string-right-trim "/" given)))
      (sb-posix:chmod program #o700)
      (is equal program (tuatara.defaults::user-program-path program))
      (is eq nil (tuatara.defaults::user-program-path (concatenate 'string program "-gone")))
      (multiple-value-bind (output outcome said names)
          (tuatara.defaults::run-on-copies program
                                           (list (cons "master" (list (first files))) (cons "photo" (rest files)))
                                           (lambda (masters photos)
                                             (append (list "--master") masters (list "--photo") photos))
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

(define-test (external private-directories-in-use-kept)
  ;; a directory still in use is never swept as old, however long ago it was last written to;
  ;; one made to go a day later goes, and one deleted is held no longer
  (with-test-directory (dir)
    (let* ((directory (tuatara.io:slash-ended (tuatara.io:native dir)))
           (held (tuatara.defaults::new-private-directory directory))
           (two-days-ago (- (sb-posix:time) (* 2 86400))))
      (flet ((age (directory) (sb-posix:utimes (string-right-trim "/" directory) two-days-ago two-days-ago)))
        (unwind-protect
             (progn
               (age held)
               (tuatara.defaults::delete-private-directory (tuatara.defaults::new-private-directory directory))
               (tuatara.defaults::forget-old-copies directory)
               (is eq :directory (tuatara.io:path-kind held :follow nil))
               (let ((loose (tuatara.defaults::new-private-directory directory :hold nil)))
                 (age loose)
                 (let ((another (tuatara.defaults::new-private-directory directory)))
                   (tuatara.defaults::forget-old-copies directory)
                   (false (tuatara.io:path-kind loose :follow nil))
                   (tuatara.defaults::delete-private-directory another)
                   (false (tuatara.io:path-kind another :follow nil))
                   (false (gethash another tuatara.defaults::*private-directories-held*))))
               ;; kept fresh for other sessions' sweeps
               (age held)
               (let ((tuatara.defaults::*private-directories-touched* 0))
                 (tuatara.defaults::keep-private-directories-fresh nil))
               (true (> (tuatara.defaults::mtime-universal-time (tuatara.io:stamp-mtime (tuatara.io:path-stamp held)))
                        (- (get-universal-time) 3600))))
          (ignore-errors (tuatara.io:delete-tree held))
          (remhash held tuatara.defaults::*private-directories-held*))))))

(defvar *private-directory-readings* '()
  "The buffers the private directory test reads into.")

(define-test (external private-directory-deleted-after-its-job)
  ;; a buffer READING-BUFFER gives a directory of its own: made for you alone, named while the buffer lives,
  ;; deleted when it is killed once its job has ended, and not before
  (let* ((tuatara.command:*editor* (nth-value 1 (run-keys "" :text "")))
         (tuatara.runtime:*stop-wait* 0.05)
         (gate (list nil))
         (job nil)
         (buffer (tuatara:reading-buffer '*private-directory-readings*
                                         (lambda (b) (tuatara.buffer:buffer-local b 'private-directory-reading))
                                         "/nonexistent/read.bin"
                                         "*private directory test*"
                                         (format nil "Reading...~%")
                                         'tuatara:special-mode
                                         (lambda (b)
                                           (setf (tuatara.buffer:buffer-local b 'private-directory-reading)
                                                 "/nonexistent/read.bin"
                                                 job (tuatara.defaults::%make-count-job :buffer b :what "Buffer" :total 10))
                                           (tuatara.runtime:start-job-thread
                                            job (lambda (job)
                                                  (declare (ignore job))
                                                  (loop repeat 1000 until (car gate) do (sleep 0.01)))
                                            :name "tuatara test job"))
                                         :private-directory "opened/"))
         (directory (tuatara:buffer-private-directory buffer)))
    (unwind-protect
         (progn
           (true (uiop:string-prefix-p (tuatara.defaults::opened-copies-directory) directory))
           (is = #o700 (logand #o777 (tuatara.io:stamp-mode (tuatara.io:path-stamp directory))))
           (tuatara.io:write-new-file (tuatara.io:path-join directory "made") "made")
           (tuatara.buffer:kill-buffer buffer)
           ;; its job still running: the directory kept
           (is eq :directory (tuatara.io:path-kind directory :follow nil))
           (setf (car gate) t)
           (loop repeat 500 while (tuatara.io:path-kind directory :follow nil) do (sleep 0.01))
           (false (tuatara.io:path-kind directory :follow nil)))
      (setf (car gate) t)
      (when job (tuatara.runtime:forget-job job))
      (when (tuatara.io:path-kind directory :follow nil) (tuatara.io:delete-tree directory)))))

(define-test (external tiff-unread-told-by-its-head)
  ;; a TIFF whose directory cannot be read is told by its first octets, never by the error
  (let ((path (format nil "~atuatara-tiff-~36r.tif" (uiop:native-namestring (uiop:temporary-directory))
                      (random (expt 36 8) (make-random-state t))))
        (head (coerce #(73 73 42 0 8 0 0 0 0 0 0 0 0 0 0 0) '(vector (unsigned-byte 8)))))
    (alexandria:write-byte-vector-into-file head path :if-exists :supersede)
    (sb-int:encapsulate 'tuatara.defaults::octets-kind-of-file 'test-failing
                        (lambda (function &rest arguments)
                          (declare (ignore function arguments))
                          (error "a directory that cannot be read")))
    (unwind-protect
         (multiple-value-bind (words kind) (tuatara.defaults::file-kind path)
           (true (keywordp kind) "~s" kind)
           (is eq (nth-value 1 (tuatara.io:octets-kind (tuatara.io:file-head path))) kind)
           (true (stringp words)))
      (sb-int:unencapsulate 'tuatara.defaults::octets-kind-of-file 'test-failing)
      (delete-file path))))
