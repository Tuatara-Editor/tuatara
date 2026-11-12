;;;; test/rimu-operations.lisp --- Rimu's marks and operations, the trash, and rename-formatted.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.tests)

(define-test rimu-operations
  :description "In a rimu list, m u U t mark, d marks for deletion and x puts those in the trash. C copies and
R moves the marked or the one on the line to the other list's directory or a name asked for, asking
before replacing (the old to the trash). D puts in the trash (the freedesktop.org one, with its
.trashinfo), C-u D deletes for good; + makes directories. rename-formatted gives files formatted
names, their headers written and saved, visiting buffers following.")

(defmacro with-operations-tree ((directory) &body body)
  "Run BODY with DIRECTORY a new directory holding a.txt, b.txt, sub/ with
c.txt, and link to a.txt, and a trash of its own (XDG_DATA_HOME in it), no
drive's trash seen (no drive mounted, as far as the trash knows), so that
emptying the trash never reaches a real one."
  (let ((old (gensym "OLD")))
    `(with-test-directory (,directory)
       (alexandria:write-string-into-file "alpha" (merge-pathnames "a.txt" ,directory))
       (alexandria:write-string-into-file "beta" (merge-pathnames "b.txt" ,directory))
       (ensure-directories-exist (merge-pathnames "sub/" ,directory))
       (alexandria:write-string-into-file "gamma" (merge-pathnames "sub/c.txt" ,directory))
       (sb-posix:chmod (namestring (merge-pathnames "b.txt" ,directory)) #o640)
       (sb-posix:symlink "a.txt" (namestring (merge-pathnames "link" ,directory)))
       (let ((,old (sb-posix:getenv "XDG_DATA_HOME")))
         (sb-posix:setenv "XDG_DATA_HOME" (namestring (merge-pathnames "data/" ,directory)) 1)
         (unwind-protect (let ((tuatara.defaults::*mounts-file* (namestring (merge-pathnames "no-drives" ,directory))))
                           ,@body)
           (if ,old (sb-posix:setenv "XDG_DATA_HOME" ,old 1) (sb-posix:unsetenv "XDG_DATA_HOME")))))))

(defun ops-session (directory keys &key (width 120) (height 20))
  "A session listing DIRECTORY with the preview off, typing KEYS, and waiting
for rimu's operations to finish. Returns the frontend and the editor."
  (let ((tuatara-user:*rimu-preview* nil))
    (multiple-value-prog1 (rimu-session directory keys :width width :height height)
      (loop repeat 200 while (find-if #'tuatara.defaults::rimu-operation-p tuatara.runtime:*jobs*) do (sleep 0.02)))))

(defun here (directory name)
  "The file NAME in DIRECTORY, a pathname."
  (merge-pathnames name directory))

(define-modal-test (rimu-operations marks)
  (with-operations-tree (dir)
    ;; m marks and goes on, u unmarks, d marks for deletion; the first column says which
    (multiple-value-bind (f editor) (ops-session dir "g f a . t x t RET m m d")
      (declare (ignore f))
      (let ((lines (rimu-lines editor)))
        (true (find-if (lambda (l) (and (uiop:string-prefix-p "> " l) (search "a.txt" l))) lines))
        (true (find-if (lambda (l) (and (uiop:string-prefix-p "> " l) (search "b.txt" l))) lines))
        (true (find-if (lambda (l) (and (uiop:string-prefix-p "D " l) (search "link" l))) lines))))
    ;; the marking keys are those of every list: a count marks as many, a negative one going up
    (is eq 'tuatara.defaults::list-mark-entry
        (tuatara.command:lookup-key tuatara.defaults::*rimu-keymap* (tuatara.frontend:parse-key-sequence "m")))
    (multiple-value-bind (f editor) (ops-session dir "g f a . t x t RET C-u 2 m C-u - 1 u")
      (declare (ignore f))
      (let ((lines (rimu-lines editor)))
        (true (find-if (lambda (l) (and (uiop:string-prefix-p "> " l) (search "a.txt" l))) lines))
        (false (find-if (lambda (l) (and (uiop:string-prefix-p "> " l) (search "b.txt" l))) lines))))
    ;; t turns marks about, U takes them all away; marks stay while the list is read again
    (multiple-value-bind (f editor) (ops-session dir "g f a . t x t RET m t")
      (declare (ignore f))
      (let ((lines (rimu-lines editor)))
        (false (find-if (lambda (l) (and (uiop:string-prefix-p "> " l) (search "a.txt" l))) lines))
        (true (find-if (lambda (l) (and (uiop:string-prefix-p "> " l) (search "b.txt" l))) lines))))
    (multiple-value-bind (f editor) (ops-session dir "g f a . t x t RET m r")
      (declare (ignore f))
      (true (find-if (lambda (l) (and (uiop:string-prefix-p "> " l) (search "a.txt" l))) (rimu-lines editor))))
    ;; marked for deletion, read again (r lays it out anew), then unmarked: no longer in that mark's colour
    (let ((f (ops-session dir "g f a . t x t RET d r k u")))
      (let ((row (loop for row from 0 below 20
                       when (search "a.txt" (tuatara.frontend.fake:fake-screen-line f row)) return row)))
        (true row)
        (false (find :list-deletion (aref (tuatara.frontend.fake:fake-spans f) row)
                     :key (lambda (span) (let ((face (third span))) (if (listp face) (first face) face)))))))
    (multiple-value-bind (f editor) (ops-session dir "g f a . t x t RET m m U")
      (true (tuatara.frontend.fake:fake-frame-showing f "2 marks taken away"))
      (false (find-if (lambda (l) (uiop:string-prefix-p "> " l)) (rimu-lines editor))))
    ;; a mark of another kind a command gives (a package's M): its own colour, passed over by t,
    ;; and the entries chosen by it
    (multiple-value-bind (f editor)
        (ops-session dir (list "g f a . t x t RET" (lambda () (tuatara.defaults::rimu-mark-and-go-on #\M)) "t"))
      (let ((lines (rimu-lines editor))
            (row (loop for row from 0 below 20
                       when (search "a.txt" (tuatara.frontend.fake:fake-screen-line f row))
                       return row)))
        (true (find-if (lambda (l) (and (uiop:string-prefix-p "M " l) (search "a.txt" l))) lines))
        (true (find-if (lambda (l) (and (uiop:string-prefix-p "> " l) (search "b.txt" l))) lines))
        (true (find :list-marked-other (aref (tuatara.frontend.fake:fake-spans f) row)
                    :key (lambda (span) (let ((face (third span))) (if (listp face) (first face) face)))))
        (is equal (list (tuatara.io:native (merge-pathnames "a.txt" dir)))
            (tuatara.defaults::rimu-chosen-paths (session-buffer editor) :mark #\M))))))

(define-test (rimu-operations marks-across-directories)
  (with-operations-tree (dir)
    ;; a mark stays with its file while the list goes elsewhere; the header says how many
    ;; are marked in other directories; the operations take them all, saying where
    (multiple-value-bind (f editor)
        (ops-session dir "g f a . t x t RET m g f s u b RET RET g f c . t x t RET m" :width 140)
      (true (tuatara.frontend.fake:fake-frame-showing f "1 marked in other directories"))
      (is equal
          (list (tuatara.io:native (merge-pathnames "sub/c.txt" dir)) (tuatara.io:native (merge-pathnames "a.txt" dir)))
          (tuatara.defaults::rimu-chosen-paths (session-buffer editor)))
      ;; or only those of the directory shown
      (is equal (list (tuatara.io:native (merge-pathnames "sub/c.txt" dir)))
          (tuatara.defaults::rimu-chosen-paths (session-buffer editor) :here-only t)))
    (multiple-value-bind (f editor)
        (ops-session dir "g f a . t x t RET m g f s u b RET RET g f c . t x t RET m h" :width 140)
      (declare (ignore f))
      (true (find-if (lambda (l) (and (uiop:string-prefix-p "> " l) (search "a.txt" l))) (rimu-lines editor))))
    (let ((f (ops-session dir "g f a . t x t RET m g f s u b RET RET g f c . t x t RET m C" :width 400)))
      (true (tuatara.frontend.fake:fake-frame-showing f "Copy 2 files in 2 directories to:")))
    ;; U takes away every mark, here and in the other directories, and the line saying so goes
    (multiple-value-bind (f editor)
        (ops-session dir "g f a . t x t RET m g f s u b RET RET U" :width 140)
      (true (tuatara.frontend.fake:fake-frame-showing f "1 mark taken away"))
      (false (search "marked in other directories" (session-text editor)))
      (is equal nil (tuatara.defaults::rimu-marked-elsewhere (session-buffer editor))))))

(define-test (rimu-operations marks-elsewhere-said)
  (with-operations-tree (dir)
    (alexandria:write-string-into-file "sub beta" (here dir "sub/b.txt"))
    ;; the line counts only the files marked (>) still there: one marked for deletion is not counted
    (multiple-value-bind (f editor) (ops-session dir "g f a . t x t RET d g f s u b RET RET" :width 140)
      (declare (ignore f))
      (false (search "marked in other directories" (session-text editor))))
    ;; a file marked gone meanwhile: no longer counted, and C says so, never taking the line's entry instead
    (multiple-value-bind (f editor)
        (ops-session dir (list "g f a . t x t RET m g f s u b RET RET"
                               (lambda () (delete-file (here dir "a.txt")))
                               "r C")
                     :width 200)
      (false (search "marked in other directories" (session-text editor)))
      (true (tuatara.frontend.fake:fake-frame-showing f "Nothing to copy: the files marked in other directories are gone")))
    (alexandria:write-string-into-file "alpha" (here dir "a.txt"))
    ;; K is about this list's lines: with marks only elsewhere it takes out the line's entry
    (multiple-value-bind (f editor) (ops-session dir
                                                 "g f a . t x t RET m g f s u b RET RET g f c . t x t RET K"
                                                 :width 140)
      (true (tuatara.frontend.fake:fake-frame-showing f "1 line taken out of the list"))
      (false (find-if (lambda (l) (search "c.txt" l)) (rimu-lines editor)))
      (true (probe-file (here dir "sub/c.txt"))))
    ;; R and g w name where a file marked in another directory is
    (let ((f (ops-session dir "g f b . t x t RET m g f s u b RET RET g f c . t x t RET R C-g" :width 300)))
      (true (tuatara.frontend.fake:fake-frame-showing f
                                                      (format nil "Rename ~ab.txt to:"
                                                              (tuatara.command::abbreviated-path (namestring dir))))))
    (let ((f (ops-session dir "g f b . t x t RET m g f s u b RET RET g f c . t x t RET g w n RET" :width 300)))
      (true (tuatara.frontend.fake:fake-frame-showing f
                                                      (format nil "Wipe ~ab.txt for good?"
                                                              (tuatara.command::abbreviated-path (namestring dir)))))
      (true (probe-file (here dir "b.txt"))))
    (let ((f (ops-session dir "g f b . t x t RET m g f s u b RET RET m g w n RET" :width 300)))
      (true (tuatara.frontend.fake:fake-frame-showing f "Wipe 2 files in 2 directories for good?")))
    ;; files of one name from two directories copied into one: asked first, only the first copied
    (let ((there (namestring (here dir "there/"))))
      (ensure-directories-exist there)
      (let ((f (ops-session dir
                            (format nil
                                    "g f b . t x t RET m g f s u b RET RET g f b . t x t RET m C C-a C-k ~a C-j n RET"
                                    (rimu-typed there))
                            :width 300)))
        (true (tuatara.frontend.fake:fake-frame-showing f "2 files named b.txt: only one can be there; copy the one in"))
        (true (tuatara.frontend.fake:fake-frame-showing f "Nothing copied"))
        (false (probe-file (here there "b.txt"))))
      (let ((f (ops-session dir
                            (format nil
                                    "g f b . t x t RET m g f s u b RET RET g f b . t x t RET m C C-a C-k ~a C-j y RET"
                                    (rimu-typed there))
                            :width 300)))
        (true (tuatara.frontend.fake:fake-frame-showing f "Copied 1 file"))
        (true (tuatara.frontend.fake:fake-frame-showing f "b.txt (another of its name goes there)"))
        (false (tuatara.frontend.fake:fake-frame-showing f ".tuatara-copy"))
        ;; the list's own first
        (is string= "sub beta" (alexandria:read-file-into-string (here there "b.txt")))))))

(define-test (rimu-operations put-back-only-where-it-may)
  (with-operations-tree (dir)
    (let ((trash (here dir "data/Trash/")))
      (ensure-directories-exist (here trash "info/"))
      (ensure-directories-exist (here trash "files/"))
      (flet ((trashed (name path)
               (alexandria:write-string-into-file "x" (here trash (format nil "files/~a" name)))
               (alexandria:write-string-into-file (format nil "[Trash Info]~%Path=~a~%DeletionDate=2026-10-07T10:00:00~%" path)
                                                  (here trash (format nil "info/~a.trashinfo" name)))))
        ;; a path that is not whole, or climbs with .., is refused, said why, the thing left in the trash
        (trashed "rel" "relput/x")
        (let ((f (ops-session dir "g t R" :width 300)))
          (true (tuatara.frontend.fake:fake-frame-showing f "Not put back: x: its .trashinfo gives no whole path"))
          (true (probe-file (here trash "files/rel"))))
        (delete-file (here trash "info/rel.trashinfo"))
        (trashed "up" (format nil "~asub/../../escaped" (tuatara.io:url-path (namestring dir))))
        (let ((f (ops-session dir "g t R" :width 300)))
          (true (tuatara.frontend.fake:fake-frame-showing f "Not put back: escaped: its .trashinfo's path climbs with .."))
          (true (probe-file (here trash "files/up"))))
        (delete-file (here trash "info/up.trashinfo"))
        ;; several: where they go is said
        (trashed "one" (format nil "~aone" (tuatara.io:url-path (namestring dir))))
        (trashed "two" (format nil "~asub/two" (tuatara.io:url-path (namestring dir))))
        (let ((f (ops-session dir "g t m m R n RET" :width 300)))
          (true (tuatara.frontend.fake:fake-frame-showing f "Put back the 2 things to "))
          (true (tuatara.frontend.fake:fake-frame-showing f (format nil "~asub/" (tuatara.command::abbreviated-path (namestring dir)))))))))
  ;; a drive's: within the drive, its directories as they really are
  (with-operations-tree (dir)
    (let* ((top (namestring (ensure-directories-exist (here dir "drive/"))))
           (outside (namestring (ensure-directories-exist (here dir "home/")))))
      (sb-posix:symlink outside (namestring (here dir "drive/link")))
      (flet ((refusal (path)
               (tuatara.defaults::trash-restore-refusal (tuatara.defaults::make-trashed "/t/" "n" top path "" nil))))
        (false (refusal (concatenate 'string top "a/b")))
        (true (refusal (concatenate 'string top "../home/x")))
        (true (refusal (concatenate 'string outside "x")))
        (true (refusal (concatenate 'string top "link/x")))
        (true (refusal (concatenate 'string top "link/new/x")))
        ;; a NUL or another control character, which a call to the system would cut the path at
        (is string= "its .trashinfo's path holds a control character"
            (refusal (format nil "~aa~cb" top (code-char 0))))
        (true (refusal (format nil "~aa~cb" top #\Newline)))))
    (is string= "its .trashinfo gives no whole path"
        (tuatara.defaults::trash-restore-refusal (tuatara.defaults::make-trashed "/t/" "n" nil "a/b" "" nil)))))

(define-test (rimu-operations questions-and-order)
  ;; what is asked names one file, or how many, and where they are when that is not here
  (is string= "Move a to the trash"
      (tuatara.defaults::rimu-question "Move" '("/d/a") :here "/d/" :after "to the trash"))
  (is string= "Move /e/a to the trash"
      (tuatara.defaults::rimu-question "Move" '("/e/a") :here "/d/" :after "to the trash"))
  (is string= "Copy 2 files to:" (tuatara.defaults::rimu-question "Copy" '("/d/a" "/d/b") :here "/d/" :after "to:"))
  (true (search "Copy 2 files in /e" (tuatara.defaults::rimu-question "Copy" '("/e/a" "/e/b") :here "/d/" :after "to:")))
  (is string= "Copy 2 files in 2 directories to:"
      (tuatara.defaults::rimu-question "Copy" '("/d/a" "/e/b") :here "/d/" :after "to:"))
  (is string= "2 files" (tuatara.defaults::rimu-question nil '("/d/a" "/e/b")))
  ;; the work is done in the order the files were chosen
  (is equal '(("/a" . 2) ("/b" . 3) ("/c" . 1))
      (tuatara.defaults::rimu-sort-pairs '(("/c" . 1) ("/a" . 2) ("/b" . 3)) '("/a" "/b" "/c")))
  ;; a mark is of the directory shown when its file is in it, or anywhere under it in a list of files found
  (true (tuatara.defaults::rimu-mark-here-p "/d/a" "/d/"))
  (false (tuatara.defaults::rimu-mark-here-p "/d/e/a" "/d/"))
  (true (tuatara.defaults::rimu-mark-here-p "/d/e/a" "/d/" t))
  (false (tuatara.defaults::rimu-mark-here-p "/dx/a" "/d/"))
  (false (tuatara.defaults::rimu-mark-here-p "/d/" "/d/")))

(define-test (rimu-operations take-out-lines)
  (with-operations-tree (dir)
    ;; K takes the line out of the list, the file left as it is
    (multiple-value-bind (f editor) (ops-session dir "g f a . t x t RET K")
      (let ((lines (rimu-lines editor)))
        (false (find-if (lambda (l) (search "a.txt" l)) lines))
        (true (find-if (lambda (l) (search "b.txt" l)) lines)))
      (true (tuatara.frontend.fake:fake-frame-showing f "1 line taken out of the list"))
      (true (probe-file (merge-pathnames "a.txt" dir))))
    ;; the marked ones, their marks gone with them; reading the list again shows them
    (multiple-value-bind (f editor) (ops-session dir "g f a . t x t RET m m K")
      (declare (ignore f))
      (let ((lines (rimu-lines editor)))
        (false (find-if (lambda (l) (or (search "a.txt" l) (search "b.txt" l))) lines))
        (true (find-if (lambda (l) (search "sub" l)) lines))))
    (multiple-value-bind (f editor) (ops-session dir "g f a . t x t RET m m K r")
      (declare (ignore f))
      (let ((lines (rimu-lines editor)))
        (true (find-if (lambda (l) (and (uiop:string-prefix-p "  " l) (search "a.txt" l))) lines))
        (true (find-if (lambda (l) (and (uiop:string-prefix-p "  " l) (search "b.txt" l))) lines))))))

(define-test (rimu-operations copy-and-move)
  (with-operations-tree (dir)
    (let ((there (namestring (merge-pathnames "there/" dir))))
      (ensure-directories-exist there)
      ;; C: into the directory given, modes and links kept, a directory with all in it
      (let ((f (ops-session dir (format nil "g f b . t x t RET m g f s u b RET m g f l i n k RET m C C-a C-k ~a C-j"
                                        (rimu-typed there)))))
        (true (tuatara.frontend.fake:fake-frame-showing f "Copied 3 files to"))
        (is string= "beta" (alexandria:read-file-into-string (here there "b.txt")))
        (is = #o640 (logand #o777 (sb-posix:stat-mode (sb-posix:stat (namestring (here there "b.txt"))))))
        (is string= "gamma" (alexandria:read-file-into-string (here there "sub/c.txt")))
        (is string= "a.txt" (sb-posix:readlink (namestring (here there "link"))))
        (true (probe-file (here dir "b.txt"))))
      ;; the question completes on directories, as C-x C-f does; the first line takes what is typed
      (let ((f (ops-session dir
                            (format nil "g f b . t x t RET C C-a C-k ~a t h" (rimu-typed (namestring dir)))
                            :height 30)))
        (true (tuatara.frontend.fake:fake-frame-showing f "there/")))
      ;; TAB on the line typed (selected, a directory) leaves it as it is, again and again; C-j then takes there/ (b.txt
      ;; is in it)
      (let ((f (ops-session dir
                            (format nil
                                    "g f b . t x t RET C C-a C-k ~a TAB TAB C-j"
                                    (rimu-typed (namestring (here dir "there/"))))
                            :width 300)))
        (true (tuatara.frontend.fake:fake-frame-showing f "b.txt is there already")))
      ;; again: asked whether to replace; no leaves them; a directory there is not merged
      (let ((f (ops-session dir (format nil "g f b . t x t RET m g f s u b RET m C C-a C-k ~a C-j n RET" (rimu-typed there)))))
        (true (tuatara.frontend.fake:fake-frame-showing f "b.txt is there already: replace, the old to the trash?"))
        (true (tuatara.frontend.fake:fake-frame-showing f "sub (a directory is there)")))
      (alexandria:write-string-into-file "old beta" (here there "b.txt") :if-exists :supersede)
      (let ((f (ops-session dir (format nil "g f b . t x t RET C C-a C-k ~a C-j y RET" (rimu-typed there)))))
        (true (tuatara.frontend.fake:fake-frame-showing f "1 replaced, the old in the trash"))
        (is string= "beta" (alexandria:read-file-into-string (here there "b.txt")))
        (true (probe-file (here dir "data/Trash/files/b.txt"))))
      ;; R: one entry given a new name; a buffer visiting it follows
      (let ((visiting (tuatara.io:visit-file (namestring (here dir "a.txt")))))
        (ops-session dir "g f a . t x t RET R C-a C-k r e n a m e d . t x t RET")
        (false (probe-file (here dir "a.txt")))
        (is string= "alpha" (alexandria:read-file-into-string (here dir "renamed.txt")))
        (is equal (tuatara.io:expand-path (namestring (here dir "renamed.txt"))) (tuatara.buffer:buffer-file visiting))
        (tuatara.buffer:kill-buffer visiting))
      ;; R: a name only; many marked, or a directory given, point elsewhere
      (let ((f (ops-session dir "g f r e n a m e d . t x t RET R C-a C-k s u b / x RET")))
        (true (tuatara.frontend.fake:fake-frame-showing f "sub/x is no name: a name only"))
        (true (probe-file (here dir "renamed.txt"))))
      (let ((f (ops-session dir "g f b . t x t RET m m R")))
        (true (tuatara.frontend.fake:fake-frame-showing f "2 marked: R renames one")))
      ;; M: into a directory, navigated to; what is no directory is refused
      (let ((f (ops-session dir
                            (format nil "g f r e n a m e d . t x t RET M C-a C-k ~a C-j" (rimu-typed there))
                            :width 300)))
        (true (tuatara.frontend.fake:fake-frame-showing f "Moved 1 file to"))
        (true (probe-file (here there "renamed.txt"))))
      (let ((f (ops-session dir
                            (format nil
                                    "g f b . t x t RET M C-a C-k ~a C-j"
                                    (rimu-typed (namestring (here dir "nowhere.txt"))))
                            :width 300)))
        (true (tuatara.frontend.fake:fake-frame-showing f "is no directory: nothing moved"))
        (true (probe-file (here dir "b.txt"))))
      ;; a directory is not moved into itself
      (let ((f (ops-session dir
                            (format nil "g f s u b RET M C-a C-k ~a C-j" (rimu-typed (namestring (here dir "sub/")))))))
        (true (tuatara.frontend.fake:fake-frame-showing f "sub (itself)"))
        (true (probe-file (here dir "sub/c.txt")))))))

(define-test (rimu-operations trash-and-delete)
  (with-operations-tree (dir)
    ;; D: to the trash, asked first, with a .trashinfo saying where it was
    (let ((f (ops-session dir "g f b . t x t RET D y")))
      (true (tuatara.frontend.fake:fake-frame-showing f "Move b.txt to the trash? (y or n)"))
      (true (tuatara.frontend.fake:fake-frame-showing f "Moved to the trash 1 file"))
      (false (probe-file (here dir "b.txt")))
      (is string= "beta" (alexandria:read-file-into-string (here dir "data/Trash/files/b.txt")))
      (let ((info (alexandria:read-file-into-string (here dir "data/Trash/info/b.txt.trashinfo"))))
        (true (search "[Trash Info]" info))
        (true (search (format nil "Path=~a" (tuatara.io:url-path (namestring (here dir "b.txt")))) info))
        (true (search "DeletionDate=" info))))
    ;; the same name again: a name of its own in the trash
    (alexandria:write-string-into-file "beta 2" (here dir "b.txt"))
    (ops-session dir "g f b . t x t RET D y")
    (is string= "beta 2" (alexandria:read-file-into-string (here dir "data/Trash/files/b.txt.2")))
    ;; d and x: those marked for deletion, a directory too
    (let ((f (ops-session dir "g f s u b RET d x y RET")))
      (true (tuatara.frontend.fake:fake-frame-showing f "Move sub to the trash?"))
      (false (probe-file (here dir "sub/")))
      (true (probe-file (here dir "data/Trash/files/sub/c.txt"))))
    ;; n: nothing done
    (ops-session dir "g f a . t x t RET D n RET")
    (true (probe-file (here dir "a.txt")))
    ;; C-u D: for good, asked twice
    (let ((f (ops-session dir "g f a . t x t RET C-u D y RET y RET")))
      (true (tuatara.frontend.fake:fake-frame-showing f "Really delete a.txt for good? It cannot be put back."))
      (true (tuatara.frontend.fake:fake-frame-showing f "Deleted for good 1 file"))
      (false (probe-file (here dir "a.txt")))
      (false (probe-file (here dir "data/Trash/files/a.txt"))))
    ;; the link: the link itself goes, not where it leads
    (alexandria:write-string-into-file "alpha" (here dir "a.txt"))
    (ops-session dir "g f l i n k RET D y")
    (true (probe-file (here dir "a.txt")))
    (true (sb-posix:readlink (namestring (here dir "data/Trash/files/link"))))))

(define-test (rimu-operations trash-info-read-safely)
  ;; a trash on a drive not to be trusted: a .trashinfo that is a named pipe, a link to an endless
  ;; device, or very large, is left out without holding the list up or filling memory; a line end
  ;; in where a thing was is shown as ?, one line for each thing
  (with-operations-tree (dir)
    (let ((info (here dir "data/Trash/info/"))
          (fifo (namestring (here dir "data/Trash/info/pipe.trashinfo"))))
      (ensure-directories-exist info)
      (ensure-directories-exist (here dir "data/Trash/files/"))
      (alexandria:write-string-into-file
       (format nil "[Trash Info]~%Path=~aline%0Abreak.txt~%DeletionDate=2026-10-07T10:00:00~%"
               (tuatara.io:url-path (namestring dir)))
       (here dir "data/Trash/info/good.trashinfo"))
      (sb-posix:mkfifo fifo #o600)
      (sb-posix:symlink "/dev/zero" (namestring (here dir "data/Trash/info/zero.trashinfo")))
      (alexandria:write-string-into-file
       (format nil
               "[Trash Info]~%Path=/big~%DeletionDate=2026-10-07T10:00:00~%~a"
               (make-string 70000 :initial-element #\x))
       (here dir "data/Trash/info/big.trashinfo"))
      (let* ((reader (sb-thread:make-thread (lambda () (tuatara.defaults::trash-contents))
                                            :name "tuatara test trash reader"))
             (things (sb-thread:join-thread reader :default :hung :timeout 20)))
        (when (eq things :hung)
          ;; a reader held on the pipe let go: opened for writing and closed, it reads its end
          (ignore-errors (sb-posix:close (sb-posix:open fifo (logior sb-posix:o-wronly sb-posix:o-nonblock))))
          (sb-thread:join-thread reader :default nil :timeout 5))
        (false (eq things :hung))
        (is = 1 (length things))
        (is string= (format nil "~aline~%break.txt" (namestring dir)) (tuatara.defaults::trashed-path (first things))))
      (ops-session dir "M-x r i m u - t r a s h RET")
      (let ((list (tuatara.buffer:find-buffer "*Trash*")))
        (true list)
        (let ((text (tuatara.buffer:buffer-string list)))
          ;; the titles, the dashes, the section's heading, the one line of the thing, an empty
          ;; line and the count
          (is = 6 (count #\Newline text))
          (true (search "line^Jbreak.txt" text)))
        (tuatara.buffer:kill-buffer list)))))

(define-test (rimu-operations make-directory)
  (with-operations-tree (dir)
    (let ((f (ops-session dir (format nil "+ C-a C-k ~a C-j" (rimu-typed (namestring (here dir "new/deeper/")))))))
      (true (tuatara.frontend.fake:fake-frame-showing f "Made"))
      (true (probe-file (here dir "new/deeper/"))))
    (let ((f (ops-session dir (format nil "+ C-a C-k ~a C-j" (rimu-typed (namestring (here dir "sub/")))))))
      (true (tuatara.frontend.fake:fake-frame-showing f "is there already")))
    ;; a name with a [ and a * in it is made as it is named, those above it too
    (let* ((path (concatenate 'string (tuatara.io:native dir) "case [1]*/deeper/"))
           (f (ops-session dir (format nil "+ C-a C-k ~a C-j" (rimu-typed path)))))
      (true (tuatara.frontend.fake:fake-frame-showing f "Made"))
      (is eq :directory (tuatara.io:path-kind path)))))

(define-modal-test (rimu-operations rename-formatted)
  (with-operations-tree (dir)
    (alexandria:write-string-into-file (format nil "Some notes.~%") (here dir "notes.org"))
    (alexandria:write-string-into-file (format nil "#+title: Old~%#+filetags: :old:~%~%Body.~%")
                                       (here dir "20260101T101010--old__old.org"))
    ;; a file with no formatted name: its identifier from when it was changed, the header put in and saved
    (let ((f (ops-session dir "g f n o t e s . o r g RET SPC d r C-a C-k M y SPC n o t e s RET p r o j e c t RET RET")))
      (true (tuatara.frontend.fake:fake-frame-showing f "Renamed to"))
      (let ((renamed (find-if (lambda (name) (search "--my-notes__project.org" name))
                              (mapcar #'first (tuatara.io:directory-entries dir)))))
        (true renamed)
        (true (tuatara.defaults::identifier-at-start-p renamed))
        (let ((text (alexandria:read-file-into-string (here dir renamed))))
          (true (uiop:string-prefix-p "#+title: My notes" text))
          (true (search "#+filetags: :project:" text))
          (true (search "Some notes." text)))))
    ;; a formatted one: its identifier kept, the header's title and keywords written anew
    (ops-session dir "g f 2 0 2 6 0 1 0 1 RET SPC d r C-a C-k N e w SPC t i t l e RET g a r d e n RET RET")
    (let ((text (alexandria:read-file-into-string (here dir "20260101T101010--new-title__garden.org"))))
      (true (uiop:string-prefix-p (format nil "#+title: New title~%#+filetags: :garden:~%") text))
      (true (search "Body." text)))
    ;; keywords only, for the marked files: the header's title kept as it is written
    (ops-session dir "g f 2 0 2 6 0 1 0 1 RET SPC d k a l p h a RET RET")
    (true (probe-file (here dir "20260101T101010--new-title__alpha.org")))
    (true (uiop:string-prefix-p (format nil "#+title: New title~%#+filetags: :alpha:~%")
                                (alexandria:read-file-into-string (here dir "20260101T101010--new-title__alpha.org"))))
    ;; and offered when it is renamed again
    (ops-session dir "g f 2 0 2 6 0 1 0 1 RET SPC d r C-e SPC t w o RET RET")
    (true (uiop:string-prefix-p "#+title: New title two"
                                (alexandria:read-file-into-string (here dir "20260101T101010--new-title-two__alpha.org"))))))

(define-test (rimu-operations formatted-rename-waits-for-a-save)
  ;; a file whose buffer is being saved keeps its name: what the save writes
  ;; would land at the old one
  (with-test-directory (dir)
    (alexandria:write-string-into-file "notes" (here dir "notes.txt"))
    (let* ((path (tuatara.io:native (here dir "notes.txt")))
           (buffer (tuatara.buffer:make-buffer (tuatara.buffer:unique-buffer-name "notes.txt"))))
      (setf (tuatara.buffer:buffer-file buffer) (tuatara.io:expand-path path))
      (sb-int:encapsulate 'tuatara.io:save-of 'saving-now (lambda (save-of b) (or (eq b buffer) (funcall save-of b))))
      (unwind-protect
           (fail (tuatara.defaults::rename-file-formatted path "My notes" '("work")))
        (sb-int:unencapsulate 'tuatara.io:save-of 'saving-now)
        (tuatara.buffer:kill-buffer buffer))
      (true (probe-file (here dir "notes.txt"))))))

(define-test (rimu-operations copies-whole-or-said)
  (with-operations-tree (dir)
    ;; a directory into itself through a symbolic link to it: left alone, never copied on and on
    (sb-posix:symlink "sub" (namestring (here dir "sublink")))
    (ensure-directories-exist (here dir "sub/inner/"))
    (let ((f (ops-session dir
                          (format nil
                                  "g f s u b RET C C-a C-k ~a C-j"
                                  (rimu-typed (namestring (here dir "sublink/inner/"))))
                          :width 300)))
      (true (tuatara.frontend.fake:fake-frame-showing f "sub (itself)"))
      (false (probe-file (here dir "sub/inner/sub/"))))
    ;; a link itself copied into the directory it leads to: a copy of the link, not itself
    (let ((f (ops-session dir
                          (format nil
                                  "g f s u b l i n k RET C C-a C-k ~a C-j"
                                  (rimu-typed (namestring (here dir "sub/"))))
                          :width 300)))
      (true (tuatara.frontend.fake:fake-frame-showing f "Copied 1 file"))
      (is string= "sub" (sb-posix:readlink (namestring (here dir "sub/sublink")))))
    ;; a named pipe in a directory copied: left out, and said; the copy not counted whole
    (sb-posix:mkfifo (namestring (here dir "sub/pipe")) #o600)
    (false (tuatara.io:copy-path-tree (namestring (here dir "sub/")) (namestring (here dir "copy"))))
    (true (probe-file (here dir "copy/c.txt")))
    (let ((f (ops-session dir
                          (format nil "g f s u b RET C C-a C-k ~a C-j" (rimu-typed (namestring (here dir "there/"))))
                          :width 300)))
      (true (tuatara.frontend.fake:fake-frame-showing f "left out: pipe (a named pipe)")))
    ;; a move that fails: the buffer visiting the file stays with it, where it still is
    (ensure-directories-exist (here dir "shut/"))
    (sb-posix:chmod (namestring (here dir "shut/")) #o500)
    (let ((visiting (tuatara.io:visit-file (namestring (here dir "a.txt")))))
      (unwind-protect
           (let ((f (ops-session dir
                                 (format nil
                                         "g f a . t x t RET M C-a C-k ~a C-j"
                                         (rimu-typed (namestring (here dir "shut/"))))
                                 :width 300)))
             (true (tuatara.frontend.fake:fake-frame-showing f "not done: a.txt"))
             (true (probe-file (here dir "a.txt")))
             (is equal (tuatara.io:expand-path (namestring (here dir "a.txt"))) (tuatara.buffer:buffer-file visiting)))
        (sb-posix:chmod (namestring (here dir "shut/")) #o700)
        (tuatara.buffer:kill-buffer visiting)))
    ;; a copy stopped part of the way: not counted whole
    (false (tuatara.io:copy-path-tree (namestring (here dir "a.txt")) (namestring (here dir "a2.txt")) :cancel (list t)))
    ;; stopped half-way (on tmpfs, where a copy goes piece by piece rather than shared at once):
    ;; nothing under its name, and no hidden part left beside it
    (when (probe-file "/dev/shm/")
      (let ((shm (format nil "/dev/shm/tuatara-test-copy-~d/" (random 1000000))) (cancel (list nil)))
        (ensure-directories-exist shm)
        (unwind-protect
             (progn
               (alexandria:write-byte-vector-into-file (make-array (* 3 1024 1024)
                                                                   :element-type '(unsigned-byte 8)
                                                                   :initial-element 1)
                                                       (concatenate 'string shm "half.bin"))
               (false (tuatara.io:copy-path-tree (concatenate 'string shm "half.bin")
                                                 (concatenate 'string shm "half-copy.bin")
                                                 :cancel cancel
                                                 :progress (lambda (n) (declare (ignore n)) (setf (car cancel) t))))
               (is equal
                   (list (concatenate 'string shm "half.bin"))
                   (mapcar #'namestring (directory (concatenate 'string shm "*.*")))))
          (ignore-errors (tuatara.io:delete-tree shm)))))
    ;; a move across file systems that cannot be whole: nothing of it left there, the file where it was
    (when (and (probe-file "/dev/shm/") (not (eql (tuatara.io:stamp-dev (tuatara.io:path-stamp "/dev/shm/"))
                                                  (tuatara.io:stamp-dev (tuatara.io:path-stamp (namestring dir))))))
      (let ((there (format nil "/dev/shm/tuatara-test-move-~d/" (random 1000000))))
        (ensure-directories-exist (here dir "withpipe/"))
        (alexandria:write-string-into-file "x" (here dir "withpipe/f.txt"))
        (sb-posix:mkfifo (namestring (here dir "withpipe/pipe")) #o600)
        (ensure-directories-exist there)
        (unwind-protect
             (progn
               (fail (tuatara.io:move-path (namestring (here dir "withpipe/")) (concatenate 'string there "withpipe")))
               (true (probe-file (here dir "withpipe/f.txt")))
               (false (tuatara.io:path-stamp (concatenate 'string there "withpipe") :follow nil))
               (is equal '() (directory (concatenate 'string there "*.*"))))
          (ignore-errors (tuatara.io:delete-tree there)))))
    ;; and in a list, taken away, never left holding part of the file
    (let ((from (namestring (here dir "big.bin"))) (to (namestring (here dir "big-copy.bin"))))
      (alexandria:write-byte-vector-into-file (make-array (* 4 1024 1024)
                                                          :element-type '(unsigned-byte 8)
                                                          :initial-element 7) from)
      (true (search "stopped" (handler-case (progn (tuatara.defaults::rimu-work :copy (cons from to) (list t)) "")
                                (error (e) (princ-to-string e)))))
      (false (probe-file to))))
  ;; the operations under way are asked about before exiting, and stopped only once the exit is certain
  (let ((tuatara.runtime:*jobs*
          (list (tuatara.defaults::make-rimu-operation :copy (list (cons "/a" "/b")) nil nil))))
    (sb-int:encapsulate 'tuatara.command:y-or-n-p* 'test
                        (lambda (f prompt &rest how) (declare (ignore f prompt how)) t))
    (unwind-protect
         (let ((answer (tuatara.defaults::jobs-quit-query)))
           (true (functionp answer))
           (false (car (tuatara.defaults::rimu-operation-cancel (first tuatara.runtime:*jobs*)))))
      (sb-int:unencapsulate 'tuatara.command:y-or-n-p* 'test))))

(define-test (rimu-operations trash-on-other-drives)
  ;; Path= on another drive is counted from its top, read back from it
  (is string= "a/b c" (tuatara.io::trash-info-path "/run/media/u/USB/a/b c" "/run/media/u/USB/"))
  (is string= "srv/x" (tuatara.io::trash-info-path "/srv/x" "/"))
  (is string= "/home/u/x" (tuatara.io::trash-info-path "/home/u/x" nil))
  ;; a mount point with a space, a tab or a backslash in its name, as /proc/self/mounts writes it
  (is string=
      (format nil "/run/media/u/My Disk~c\\x" #\Tab)
      (tuatara.defaults::mount-field "/run/media/u/My\\040Disk\\011\\134x"))
  ;; a drive's trash is used only when it is made of directories themselves, yours: a link in it could lead
  ;; deleting or putting back anywhere; a shared .Trash needs the sticky bit
  (with-operations-tree (dir)
    (let* ((top (namestring (here dir "drive/")))
           (uid (sb-posix:getuid))
           (trash (format nil "~a.Trash-~d/" top uid))
           (decoy (namestring (ensure-directories-exist (here dir "home/Documents/")))))
      (ensure-directories-exist (format nil "~afiles/" trash))
      (ensure-directories-exist (format nil "~ainfo/" trash))
      (true (tuatara.io:drive-trash-sound-p trash top))
      (sb-posix:rmdir (format nil "~afiles" trash))
      (sb-posix:symlink decoy (format nil "~afiles" trash))
      (false (tuatara.io:drive-trash-sound-p trash top))
      (let ((shared (format nil "~a.Trash/~d/" top uid)))
        (ensure-directories-exist (format nil "~afiles/" shared))
        (ensure-directories-exist (format nil "~ainfo/" shared))
        (false (tuatara.io:drive-trash-sound-p shared top))
        (sb-posix:chmod (format nil "~a.Trash" top) #o1777)
        (true (tuatara.io:drive-trash-sound-p shared top)))))
  ;; where a drive's thing was: within the drive, its . and .. taken as written
  (true (tuatara.io:path-within-p "/run/media/u/USB/a/b" "/run/media/u/USB/"))
  (false (tuatara.io:path-within-p "/run/media/u/USB/../../../home/u/.bashrc" "/run/media/u/USB/"))
  (false (tuatara.io:path-within-p "/home/u/.bashrc" "/run/media/u/USB/"))
  ;; a name that starts with the drive's, another drive
  (false (tuatara.io:path-within-p "/run/media/u/USBX/a" "/run/media/u/USB"))
  (true (tuatara.io:path-within-p "/run/media/u/USB" "/run/media/u/USB/")))

(define-modal-test (rimu-operations formatted-headers-written-anew)
  (flet ((anew (style text)
           (tuatara.defaults::text-with-header text (assoc style tuatara.defaults::*formatted-file-types*)
                                               "New" '("a" "b") "20260101T101010" 0)))
    ;; Org's keywords in capitals, kept in capitals
    (is string= (format nil "#+TITLE: New~%#+FILETAGS: :a:b:~%~%Body.~%")
        (anew :org (format nil "#+TITLE: Old~%#+FILETAGS: :old:~%~%Body.~%")))
    ;; lines ending in CR LF keep those ends, the new line too
    (is string= (format nil "#+title: New~c~%#+filetags: :a:b:~c~%~c~%Body.~c~%" #\Return #\Return #\Return #\Return)
        (anew :org (format nil "#+title: Old~c~%~c~%Body.~c~%" #\Return #\Return #\Return)))
    ;; lines ending both ways: left alone
    (false (anew :org (format nil "#+title: Old~c~%~%Body.~%" #\Return)))
    ;; a text file's header ends where its fields do: a tags: line in the body is the body's
    (is string= (format nil "title: New~%tags: a  b~%Body.~%tags: mine~%")
        (anew :text (format nil "title: Old~%Body.~%tags: mine~%")))
    ;; Markdown's keywords a line each, replaced with the new ones
    (is string= (format nil "---~%title: \"New\"~%tags: [\"a\", \"b\"]~%date: x~%---~%Body.~%")
        (anew :markdown (format nil "---~%title: \"Old\"~%tags:~%  - old~%  - older~%date: x~%---~%Body.~%"))))
  ;; a file that is not all text: renamed, its octets untouched, and said
  (with-operations-tree (dir)
    (let ((octets (concatenate '(vector (unsigned-byte 8)) (tuatara.text:string-octets (format nil "#+title: Old~%"))
                               #(233 116 233 10))))
      (alexandria:write-byte-vector-into-file octets (here dir "latin.org"))
      (let ((f (ops-session dir "g f l a t i n RET SPC d r C-a C-k L a t i n RET RET" :width 300)))
        (true (tuatara.frontend.fake:fake-frame-showing f "is not all text"))
        (let ((renamed (find-if (lambda (name) (search "--latin.org" name))
                                (mapcar #'first (tuatara.io:directory-entries dir)))))
          (true renamed)
          (is equalp octets (alexandria:read-file-into-byte-vector (here dir renamed))))))))

(define-test (rimu-operations home-and-back)
  (with-operations-tree (dir)
    ;; g h: the home directory; g l: back to the one before, point on the entry it was on; again, forth
    (multiple-value-bind (f editor) (ops-session dir "g f b . t x t RET g h")
      (declare (ignore f))
      (is string= (tuatara.io:directory-name (tuatara.io:native (tuatara.io:home-directory)) :clean t)
          (tuatara.buffer:buffer-local (tuatara.display:window-buffer (tuatara.command:editor-window editor))
                                       'tuatara.defaults::rimu-directory)))
    (multiple-value-bind (f editor) (ops-session dir "g f b . t x t RET g h g l")
      (declare (ignore f))
      (let ((buffer (tuatara.display:window-buffer (tuatara.command:editor-window editor))))
        (is string= (namestring dir) (tuatara.buffer:buffer-local buffer 'tuatara.defaults::rimu-directory))
        (is string= "b.txt"
            (tuatara.io:listed-name (tuatara.defaults::rimu-entry-at buffer (tuatara.buffer:buffer-point buffer))))))
    (multiple-value-bind (f editor) (ops-session dir "g h g l g l")
      (declare (ignore f))
      (is string= (tuatara.io:directory-name (tuatara.io:native (tuatara.io:home-directory)) :clean t)
          (tuatara.buffer:buffer-local (tuatara.display:window-buffer (tuatara.command:editor-window editor))
                                       'tuatara.defaults::rimu-directory)))
    ;; nothing before: said
    (let ((f (ops-session dir "g l")))
      (true (tuatara.frontend.fake:fake-frame-showing f "No directory listed before in this list")))
    ;; marks stay with their files through the directories visited
    (multiple-value-bind (f editor) (ops-session dir "g f b . t x t RET m g h g l")
      (declare (ignore f))
      (true (find-if (lambda (l) (and (uiop:string-prefix-p "> " l) (search "b.txt" l))) (rimu-lines editor))))))

(defun disk-change-session (directory change shown &key (follow t) before)
  "List DIRECTORY, type the keys BEFORE when given, make CHANGE (a function) to
it as another program would, and wait up to 10 seconds, the editor going on,
for the list to hold the text SHOWN; the frontend and the editor, and whether
it did."
  (rimu-forget)
  (let ((tuatara-user:*rimu-preview* nil)
        (tuatara-user:*rimu-follow-changes* follow)
        (f (tuatara.frontend.fake:make-fake-frontend :width 120 :height 20))
        (start nil) (seen nil))
    (tuatara.frontend.fake:fake-queue-keys f
                                           (format nil
                                                   "C-x d C-a C-k ~a C-j~@[ ~a~]"
                                                   (rimu-typed (namestring directory)) before))
    (labels ((check ()
               (let ((list (find-if (lambda (b) (and (tuatara.defaults::rimu-list-p b)
                                                     (equal (tuatara.buffer:buffer-local b 'tuatara.defaults::rimu-directory)
                                                            (namestring directory))))
                                    tuatara.buffer:*buffers*)))
                 (cond ((null start)
                        (setf start (get-internal-real-time))
                        (sleep 0.3)                      ; the watch in place
                        (funcall change)
                        (tuatara.frontend.fake:fake-queue-action f #'check))
                       ((and list (search shown (tuatara.buffer:buffer-string list)))
                        (setf seen t))
                       ((< (- (get-internal-real-time) start) (* 10 internal-time-units-per-second))
                        (sleep 0.05)
                        (tuatara.frontend.fake:fake-queue-action f #'check))))))
      (tuatara.frontend.fake:fake-queue-action f #'check))
    (let ((editor (tuatara.command:run-editor f :setup #'tuatara.defaults:install-defaults)))
      (values f editor seen))))

(define-test (rimu-operations follows-the-disk)
  (with-operations-tree (dir)
    ;; a file made, one deleted, one renamed by another program: the list shows it by itself
    (true (nth-value 2
                     (disk-change-session dir
                                          (lambda ()
                                            (alexandria:write-string-into-file "x" (here dir "made-elsewhere.txt")))
                                          "made-elsewhere.txt")))
    (false (nth-value 2 (disk-change-session dir (lambda () (delete-file (here dir "b.txt"))) "no-such-name")))
    (multiple-value-bind (f editor) (disk-change-session dir
                                                         (lambda ()
                                                           (rename-file (here dir "a.txt")
                                                                        (here dir "renamed-elsewhere.txt")))
                                                         "renamed-elsewhere.txt")
      (declare (ignore f))
      (let ((text (tuatara.buffer:buffer-string (tuatara.display:window-buffer (tuatara.command:editor-window editor)))))
        (true (search "renamed-elsewhere.txt" text))
        (false (search "a.txt" text))
        (false (search "b.txt" text))))
    ;; with *rimu-follow-changes* NIL, the list stays as it was read
    (false (nth-value 2
                      (disk-change-session dir
                                           (lambda () (alexandria:write-string-into-file "x" (here dir "quiet.txt")))
                                           "quiet.txt" :follow nil)))))

(define-test (rimu-operations symbolic-and-hard-links)
  (with-operations-tree (dir)
    (let ((there (namestring (merge-pathnames "there/" dir))))
      (ensure-directories-exist there)
      ;; S: links to the marked entries in the directory given, each to its entry's whole name
      (let ((f (ops-session dir
                            (format nil "g f b . t x t RET m g f s u b RET m Y C-a C-k ~a C-j" (rimu-typed there))
                            :width 300)))
        (true (tuatara.frontend.fake:fake-frame-showing f "Symlink 2 files to:"))
        (true (tuatara.frontend.fake:fake-frame-showing f "Symlinked 2 files to"))
        (is string= (namestring (here dir "b.txt")) (sb-posix:readlink (namestring (here there "b.txt"))))
        (is string= (namestring (here dir "sub")) (sb-posix:readlink (namestring (here there "sub"))))
        (is string= "gamma" (alexandria:read-file-into-string (here there "sub/c.txt"))))
      ;; one link given a new name
      (ops-session dir
                   (format nil "g f a . t x t RET Y C-a C-k ~a C-j" (rimu-typed (namestring (here there "alias.txt")))))
      (is string= "alpha" (alexandria:read-file-into-string (here there "alias.txt")))
      ;; L: a second name of one file; writing to one is seen by the other
      (let ((f (ops-session dir
                            (format nil
                                    "g f a . t x t RET L C-a C-k ~a C-j"
                                    (rimu-typed (namestring (here there "second.txt"))))
                            :width 300)))
        (true (tuatara.frontend.fake:fake-frame-showing f "Hard linked 1 file to")))
      (is = (sb-posix:stat-ino (sb-posix:stat (namestring (here dir "a.txt"))))
          (sb-posix:stat-ino (sb-posix:stat (namestring (here there "second.txt")))))
      ;; a directory has no second name: said, nothing made
      (let ((f (ops-session dir
                            (format nil "g f s u b RET L C-a C-k ~a C-j" (rimu-typed (namestring (here there "sub2"))))
                            :width 300)))
        (true (tuatara.frontend.fake:fake-frame-showing f "not done: sub"))
        (false (tuatara.io:path-stamp (namestring (here there "sub2")) :follow nil)))
      ;; a name there: asked whether to replace, as a copy asks
      (let ((f (ops-session dir (format nil "g f b . t x t RET Y C-a C-k ~a C-j n RET" (rimu-typed there)) :width 300)))
        (true (tuatara.frontend.fake:fake-frame-showing f "b.txt is there already: replace, the old to the trash?"))))))

(define-test (rimu-operations archives-made-and-extracted)
  (with-operations-tree (dir)
    (let ((there (namestring (merge-pathnames "there/" dir))))
      (ensure-directories-exist there)
      ;; c: entries marked in two directories into one archive, kept below the directory both are in
      (let ((f (ops-session dir (format nil "g f b . t x t RET m g f s u b RET l g f c . t x t RET m c C-a C-k ~a C-j"
                                        (rimu-typed (namestring (here there "both.tar.gz"))))
                            :width 300)))
        (true (tuatara.frontend.fake:fake-frame-showing f "Archive 2 files in 2 directories as:"))
        (true (tuatara.frontend.fake:fake-frame-showing f "Archived 2 files to")))
      (is eq :gzip (nth-value 1 (tuatara.defaults::file-kind (namestring (here there "both.tar.gz")))))
      (false (remove-if-not (lambda (p) (search ".part" (namestring p))) (uiop:directory-files there)))
      ;; Z: extracted into a directory of its name, offered beside it
      (let ((f (ops-session there "g f b o t h . t a r . g z RET Z C-j" :width 300)))
        (true (tuatara.frontend.fake:fake-frame-showing f "Extracted 1 archive to")))
      (is string= "beta" (alexandria:read-file-into-string (here there "both/b.txt")))
      (is string= "gamma" (alexandria:read-file-into-string (here there "both/sub/c.txt")))
      ;; C-c e: what it holds, a line each, nothing unpacked
      (let ((listing (tuatara.defaults::archive-listing (namestring (here there "both.tar.gz")))))
        (true (search " b.txt" listing))
        (true (search " sub/c.txt" listing))
        (true (search "-rw" listing)))
      (ops-session there "g f b o t h . t a r . g z RET C-c e")
      (let ((shown (tuatara.buffer:find-buffer "*Archive: both.tar.gz*")))
        (true shown)
        (when shown
          (loop repeat 200 until (search "sub/c.txt" (tuatara.buffer:buffer-string shown)) do (sleep 0.02))
          (true (search "sub/c.txt" (tuatara.buffer:buffer-string shown)))
          (true (tuatara.buffer:buffer-local shown :read-only))
          (tuatara.buffer:kill-buffer shown)))
      (false (probe-file (here there "b.txt")))
      ;; the name says how it is compressed; the contents say how it is extracted
      (ops-session dir
                   (format nil
                           "g f a . t x t RET c C-a C-k ~a C-j"
                           (rimu-typed (namestring (here there "one.tar.bz2")))))
      (is eq :bzip2 (nth-value 1 (tuatara.defaults::file-kind (namestring (here there "one.tar.bz2")))))
      (rename-file (here there "one.tar.bz2") (here there "renamed.tar"))
      (ops-session there
                   (format nil
                           "g f r e n a m e d . t a r RET Z C-a C-k ~a C-j"
                           (rimu-typed (namestring (here there "x/")))))
      (is string= "alpha" (alexandria:read-file-into-string (here there "x/a.txt")))
      ;; a name of no archive: said, nothing made
      (let ((f (ops-session dir
                            (format nil
                                    "g f a . t x t RET c C-a C-k ~a C-j"
                                    (rimu-typed (namestring (here there "a.rar"))))
                            :width 300)))
        (true (tuatara.frontend.fake:fake-frame-showing f "Name the archive .tar, .tar.gz"))
        (true (tuatara.frontend.fake:fake-frame-showing f ".tzst, .zip or .7z: nothing archived"))
        (false (probe-file (here there "a.rar"))))
      ;; an archive naming a place above where it is extracted writes nothing there
      (ensure-directories-exist (here dir "evil/inner/"))
      (alexandria:write-string-into-file "evil" (here dir "evil/victim.txt"))
      (uiop:run-program (list "tar" "-C" (namestring (here dir "evil/inner/")) "-cPf" (namestring (here dir "evil.tar"))
                              "../victim.txt"))
      (delete-file (here dir "evil/victim.txt"))
      (ops-session dir
                   (format nil
                           "g f e v i l . t a r RET Z C-a C-k ~a C-j"
                           (rimu-typed (namestring (here dir "evil/inner/")))))
      (false (probe-file (here dir "evil/victim.txt"))))))

(define-test (rimu-operations differences)
  (with-operations-tree (dir)
    (flet ((shown (name text)
             ;; the buffer NAME once it shows TEXT, or NIL; killed after
             (let ((buffer (tuatara.buffer:find-buffer name)))
               (when buffer
                 (loop repeat 200 until (search text (tuatara.buffer:buffer-string buffer)) do (sleep 0.02))
                 (prog1 (search text (tuatara.buffer:buffer-string buffer))
                   (tuatara.buffer:kill-buffer buffer))))))
      ;; =: the two marked, the first shown the old
      (ops-session dir "g f a . t x t RET m m =")
      (true (shown "*Diff: a.txt and b.txt*" (format nil "-alpha~%")))
      ;; one marked compared with another asked for: the same says so
      (alexandria:write-string-into-file "alpha" (here dir "sub/same.txt"))
      (ops-session dir (format nil "g f a . t x t RET = C-a C-k ~a C-j" (rimu-typed (namestring (here dir "sub/same.txt")))))
      (true (shown "*Diff: a.txt and same.txt*" "No differences:"))
      ;; M-x diff-files: the entry on the line with another asked for
      (ops-session dir (format nil "g f b . t x t RET M-x d i f f - f i l e s RET C-a C-k ~a C-j"
                               (rimu-typed (namestring (here dir "sub/same.txt")))))
      (true (shown "*Diff: b.txt and same.txt*" "+alpha"))
      ;; two directories with all in them, a file only one holds against none; other/ is shown first
      (ensure-directories-exist (here dir "other/"))
      (alexandria:write-string-into-file "delta" (here dir "other/c.txt"))
      (alexandria:write-string-into-file "new" (here dir "other/only.txt"))
      (ops-session dir "g f s u b RET m g f o t h e r RET m =")
      (let ((buffer (tuatara.buffer:find-buffer "*Diff: other and sub*")))
        (true buffer)
        (when buffer
          (loop repeat 200 until (search "only.txt" (tuatara.buffer:buffer-string buffer)) do (sleep 0.02))
          (let ((text (tuatara.buffer:buffer-string buffer)))
            (true (search "-delta" text))
            (true (search "+gamma" text))
            (true (search "-new" text)))))
      ;; a file against a directory: with the file of its name there, none made up when there is none
      (ops-session dir "g f a . t x t RET m g f o t h e r RET m =")
      (true (or (shown "*Diff: a.txt and other*" "could not be compared")
                (shown "*Diff: other and a.txt*" "could not be compared")))
      ;; two comparisons of files of the same names: the second leaves the first be
      (dolist (directory '("p/" "q/" "r/" "s/"))
        (ensure-directories-exist (here dir directory))
        (alexandria:write-string-into-file directory (here dir (concatenate 'string directory "x.txt"))))
      (ops-session (here dir "p/")
                   (format nil "g f x . t x t RET = C-a C-k ~a C-j" (rimu-typed (namestring (here dir "q/x.txt")))))
      (ops-session (here dir "r/")
                   (format nil "g f x . t x t RET = C-a C-k ~a C-j" (rimu-typed (namestring (here dir "s/x.txt")))))
      (let ((both (remove-if-not (lambda (b) (search "*Diff: x.txt and x.txt*" (tuatara.buffer:buffer-name b)))
                                 tuatara.buffer:*buffers*)))
        (is = 2 (length both))
        (mapc #'tuatara.buffer:kill-buffer both))
      ;; three marked: said, nothing compared
      (let ((f (ops-session dir "g f a . t x t RET m m m =" :width 300)))
        (true (tuatara.frontend.fake:fake-frame-showing f "3 entries are marked: mark two"))))))

(define-test (rimu-operations link-onto-what-it-leads-to)
  ;; a link copied, or linked to, onto the very file it leads to is refused:
  ;; replacing that file would leave a link to nothing, its contents in the trash
  (with-operations-tree (dir)
    (sb-posix:symlink (namestring (here dir "b.txt")) (namestring (here dir "sub/b.txt")))
    (dolist (key '("C" "Y"))
      (let ((f (ops-session (here dir "sub/")
                            (format nil "g f b . t x t RET ~a C-a C-k ~a C-j y RET" key (rimu-typed (namestring dir)))
                            :width 300)))
        (true (tuatara.frontend.fake:fake-frame-showing f "the very same file") "~a" key))
      (is string= "beta" (alexandria:read-file-into-string (here dir "b.txt")))
      (is eq :regular (tuatara.io:path-kind (namestring (here dir "b.txt")) :follow nil)))))

(define-test (rimu-operations archives-held-in)
  ;; an archive holding far more than it takes up stops before the drive is
  ;; full, said so; nothing is left of a directory made for one that fails; a file
  ;; in a directory also packed goes in once; xz is held to the memory there is,
  ;; and times are written in your zone
  (with-operations-tree (dir)
    (ensure-directories-exist (here dir "zeros/"))
    (with-open-file (out (here dir "zeros/zero") :direction :output :element-type '(unsigned-byte 8))
      (let ((block (make-array (* 1024 1024) :element-type '(unsigned-byte 8) :initial-element 0)))
        (loop repeat 24 do (write-sequence block out))))
    (uiop:run-program (list "tar" "-C"
                            (namestring (here dir "zeros/")) "-czf"
                            (namestring (here dir "zeros.tar.gz")) "zero"))
    (delete-file (here dir "zeros/zero"))
    (let* ((free (tuatara.io:free-octets (namestring dir)))
           (tuatara.defaults::*archive-free-reserve* (- free (* 4 1024 1024)))
           (said (handler-case (progn (tuatara.defaults::extract-archive (namestring (here dir "zeros.tar.gz"))
                                                                         (namestring (here dir "out/")))
                                      nil)
                   (error (e) (princ-to-string e)))))
      (true (search "*archive-free-reserve*" (or said "")) "~a" said)
      (let ((size (ignore-errors (tuatara.io:stamp-size (tuatara.io:path-stamp (namestring (here dir "out/zero")))))))
        (true (or (null size) (< size (* 24 1024 1024))))))
    ;; a broken archive: the directory made for it taken away again
    (alexandria:write-string-into-file "not an archive at all" (here dir "broken.tar"))
    (false (ignore-errors
             (tuatara.defaults::extract-archive (namestring (here dir "broken.tar")) (namestring (here dir "never/")))))
    (false (tuatara.io:path-stamp (namestring (here dir "never/"))))
    ;; sub/ and sub/c.txt marked: c.txt packed once
    (tuatara.defaults::write-archive (list (namestring (here dir "sub/")) (namestring (here dir "sub/c.txt")))
                                     (namestring (here dir "once.tar")))
    (let ((listing (tuatara.defaults::archive-listing (namestring (here dir "once.tar")))))
      (is = 1 (count-matches "sub/c.txt" listing)))
    (let ((environment (tuatara.defaults::archive-environment)))
      (true (assoc "XZ_DEFAULTS" environment :test #'string=)))
    (is string= "a: b" (tuatara.defaults::archive-said-words
                        (format nil "tar: Removing leading `../' from member names~%a: b~%tar: Exiting with failure status due to previous errors")))))

;;; Archives of the kinds bsdtar reads

(defun bsdtar-make (archive format &rest arguments)
  "Make ARCHIVE in bsdtar's FORMAT (zip, 7zip, iso9660, newc, pax) with
ARGUMENTS as bsdtar -c takes them; what it said, as text, an error or not."
  (nth-value 1 (uiop:run-program (list* "bsdtar" "-c" "-f" (namestring archive) "--format" format arguments)
                                 :error-output :string :ignore-error-status t)))

(defun rar5-octets (entries)
  "A RAR 5 archive holding ENTRIES, each (NAME . TEXT), stored as they are, laid
out as RARLAB's description of the RAR 5.0 format has it (a signature, then
headers each with its CRC-32 and size), since no free program makes one."
  (labels ((vint (n)
             (loop for octet = (logand n 127)
                   do (setf n (ash n -7))
                   collect (if (plusp n) (logior octet 128) octet)
                   while (plusp n)))
           (u32 (n) (loop for k below 4 collect (ldb (byte 8 (* 8 k)) n)))
           (octets (list) (make-array (length list) :element-type '(unsigned-byte 8) :initial-contents list))
           (header (type flags body &optional data)
             (let* ((fields (append (vint type) (vint flags) (and data (vint (length data))) body))
                    (size (vint (length fields))))
               (append (u32 (tuatara.io:crc32 (octets (append size fields)))) size fields (coerce data 'list)))))
    (octets (append (list #x52 #x61 #x72 #x21 #x1A #x07 #x01 #x00)
                    (header 1 0 (vint 0))
                    (loop for (name . text) in entries
                          for data = (tuatara.text:string-octets text)
                          for name-octets = (coerce (tuatara.text:string-octets name) 'list)
                          ;; a file: its CRC-32 given, its size, Unix mode, stored, made on Unix, its name
                          append (header 2 2 (append (vint 4) (vint (length data)) (vint #o100644)
                                                     (u32 (tuatara.io:crc32 data)) (vint 0) (vint 1)
                                                     (vint (length name-octets)) name-octets)
                                         data))
                    (header 5 0 (vint 0))))))

(defun extract-failure (archive directory &rest keys)
  "Extract ARCHIVE into DIRECTORY (KEYS as EXTRACT-ARCHIVE takes them): NIL
when it went well, else what the error said."
  (handler-case (progn (apply #'tuatara.defaults::extract-archive (namestring archive) (namestring directory) keys) nil)
    (error (e) (princ-to-string e))))

(defmacro with-each-archive-program ((program) &body body)
  "Run BODY twice, PROGRAM bound to :7ZZ with 7-Zip as installed, then to
:BSDTAR with 7-Zip hidden; the first is skipped, said so, where 7-Zip is not
installed. Work in the background sees 7-Zip as installed either way."
  `(dolist (,program '(:7zz :bsdtar))
     (let ((tuatara.defaults::*seven-zip-names* (if (eq ,program :7zz) tuatara.defaults::*seven-zip-names* '())))
       (if (and (eq ,program :7zz) (null (tuatara.defaults::seven-zip)))
           (format t "~&7-Zip is not installed: the part of the test for it is skipped~%")
           (progn ,@body)))))

(define-test (rimu-operations archives-of-every-kind)
  ;; zip, 7z, ISO 9660, cpio and rar archives listed and extracted as tar ones
  ;; are, told by what they hold; a file that is none is refused, said so
  (with-operations-tree (dir)
    (loop for (name format) in '(("z.zip" "zip") ("s.7z" "7zip") ("i.iso" "iso9660") ("c.cpio" "newc"))
          do (bsdtar-make (here dir name) format "-C" (namestring dir) "a.txt" "sub"))
    (alexandria:write-byte-vector-into-file (rar5-octets '(("a.txt" . "alpha") ("sub/c.txt" . "gamma")))
                                            (here dir "r.rar"))
    (with-each-archive-program (program)
      (dolist (name '("z.zip" "s.7z" "i.iso" "c.cpio" "r.rar"))
        (let ((listing (tuatara.defaults::archive-listing (namestring (here dir name)))))
          (true (search "sub/c.txt" listing) "~a ~a: ~a" program name listing)
          (true (search "a.txt" listing) "~a ~a: ~a" program name listing))
        ;; every file there, each of three times (no worker of the program's left behind to lose one)
        (dotimes (i 3)
          (let ((out (here dir (format nil "out-~(~a~)-~a-~d/" program name i))))
            (false (extract-failure (here dir name) out) "~a ~a" program name)
            (is string= "alpha" (alexandria:read-file-into-string (here out "a.txt")) "~a ~a" program name)
            (is string= "gamma" (alexandria:read-file-into-string (here out "sub/c.txt")) "~a ~a" program name)))))
    ;; 7-Zip lists times to the second
    (when (tuatara.defaults::seven-zip)
      (true (cl-ppcre:scan "\\d{4}-\\d\\d-\\d\\d \\d\\d:\\d\\d:\\d\\d sub/c.txt"
                           (tuatara.defaults::archive-listing (namestring (here dir "s.7z"))))))
    ;; named as something else: told by what it holds
    (uiop:copy-file (here dir "s.7z") (here dir "holiday.jpg"))
    (false (extract-failure (here dir "holiday.jpg") (here dir "misnamed/")))
    (is string= "gamma" (alexandria:read-file-into-string (here dir "misnamed/sub/c.txt")))
    (is string= "holiday-extracted" (tuatara.defaults::archive-stem "/x/holiday"))
    (is string= "holiday" (tuatara.defaults::archive-stem "/x/holiday.jpg"))
    (is string= "r" (tuatara.defaults::archive-stem "/x/r.rar"))
    ;; no archive at all, whatever its name: refused, said so, nothing made
    (alexandria:write-string-into-file "only some text" (here dir "fake.zip"))
    (true (search "it is no archive" (or (extract-failure (here dir "fake.zip") (here dir "fake/")) "")))
    (false (tuatara.io:path-stamp (namestring (here dir "fake/"))))
    ;; Z in rimu: into a directory of its name beside it; C-c e lists it
    (let ((f (ops-session dir "g f c . c p i o RET Z C-j" :width 300)))
      (true (tuatara.frontend.fake:fake-frame-showing f "Extracted 1 archive to")))
    (is string= "alpha" (alexandria:read-file-into-string (here dir "c/a.txt")))
    (ops-session dir "g f r . r a r RET C-c e")
    (let ((shown (tuatara.buffer:find-buffer "*Archive: r.rar*")))
      (true shown)
      (when shown
        (loop repeat 200 until (search "sub/c.txt" (tuatara.buffer:buffer-string shown)) do (sleep 0.02))
        (true (search "sub/c.txt" (tuatara.buffer:buffer-string shown)))
        (tuatara.buffer:kill-buffer shown)))
    ;; an archive in an archive is left as it is
    (bsdtar-make (here dir "outer.zip") "zip" "-C" (namestring dir) "z.zip")
    (false (extract-failure (here dir "outer.zip") (here dir "outer/")))
    (true (probe-file (here dir "outer/z.zip")))
    (false (probe-file (here dir "outer/a.txt")))))

(define-test (rimu-operations archives-extracted-writable)
  ;; what is extracted can be moved and deleted, whatever modes the archive
  ;; gives its directories and files (an ISO image's are read only), by tar and
  ;; bsdtar alike; what was there before is left as it is, links unchanged
  (with-operations-tree (dir)
    (let ((content (namestring (here dir "a.txt"))))
      (alexandria:write-string-into-file
       (format nil "#mtree~%./ro type=dir mode=0555~%./ro/f.txt type=file mode=0444 contents=~a~%~
                    ./ro/inner type=dir mode=0500~%./ro/inner/g.txt type=file mode=0400 contents=~a~%~
                    ./ro/link type=link link=f.txt~%"
               content content)
       (here dir "spec.mtree"))
      (dolist (format '("pax" "zip" "newc"))
        (bsdtar-make (here dir (format nil "ro.~a" format)) format
                     (format nil "@~a" (namestring (here dir "spec.mtree")))))
      (bsdtar-make (here dir "ro.iso") "iso9660" "-C" (namestring dir) "a.txt" "sub")
      (dolist (name '("ro.pax" "ro.zip" "ro.newc" "ro.iso"))
        (let ((out (here dir (format nil "out-~a/" name))))
          (ensure-directories-exist out)
          ;; one there before, read only: stays so
          (alexandria:write-string-into-file "mine" (here out "before.txt"))
          (sb-posix:chmod (namestring (here out "before.txt")) #o444)
          (sleep 0.1)
          (false (extract-failure (here dir name) out) "~a" name)
          (is = #o444 (logand #o777 (tuatara.io:stamp-mode
                                     (tuatara.io:path-stamp (namestring (here out "before.txt"))))))
          (dolist (made (if (string= name "ro.iso")
                            '("sub/" "sub/c.txt" "a.txt")
                            '("ro/" "ro/f.txt" "ro/inner/" "ro/inner/g.txt")))
            (let ((stamp (tuatara.io:path-stamp (namestring (here out made)) :follow nil)))
              (true stamp "~a ~a" name made)
              (when stamp
                (true (logtest #o200 (tuatara.io:stamp-mode stamp)) "~a ~a writable" name made))))
          (unless (string= name "ro.iso")
            (is eq :symlink (tuatara.io:path-kind (namestring (here out "ro/link")) :follow nil)))
          ;; deleted as any directory of yours
          (tuatara.io:delete-tree (namestring out))
          (false (probe-file out) "~a" name))))))

(define-test (rimu-operations archives-files-room-kept)
  ;; unpacking stops before the drive has room for too few more files, said so;
  ;; a drive keeping no count of them is not held to one
  (let ((root (format nil "/tmp/tuatara-test-files-~d/" (sb-posix:getpid))))
    (when (tuatara.io::free-inodes "/tmp")
      (unwind-protect
           (progn
             (ensure-directories-exist root)
             (alexandria:write-string-into-file "alpha" (concatenate 'string root "a.txt"))
             (bsdtar-make (concatenate 'string root "few.cpio") "newc" "-C" root "a.txt")
             (let* ((free (tuatara.io::free-inodes root))
                    (tuatara.defaults::*archive-least-files-left* (+ free 1000))
                    (said (extract-failure (concatenate 'string root "few.cpio") (concatenate 'string root "out/"))))
               (true (search "room for too few more files" (or said "")) "~a" said)
               (false (probe-file (concatenate 'string root "out/")))))
        (uiop:delete-directory-tree (pathname root) :validate t :if-does-not-exist :ignore)))
    (with-test-directory (dir)
      (unless (tuatara.io::free-inodes (namestring dir))
        (false (tuatara.defaults::archive-files-left (namestring dir)))))))

(define-test (rimu-operations archives-hostile)
  ;; whatever an archive holds, nothing is written outside the directory it is
  ;; extracted into, nor through a link; no setuid bit is kept, no named pipe
  ;; or device made: zip, 7z, cpio and rar by bsdtar, tar by tar
  (with-operations-tree (dir)
    (let ((outside (namestring (ensure-directories-exist (here dir "outside/"))))
          (content (namestring (here dir "a.txt"))))
      (alexandria:write-string-into-file
       (format nil "#mtree~%./a/../../victim.txt type=file mode=0644 contents=~a~%~
                    ./abs.txt type=file mode=0644 contents=~a~%~
                    ./link type=link link=~a~%./link/through.txt type=file mode=0644 contents=~a~%~
                    ./suid type=file mode=04755 contents=~a~%./fifo type=fifo mode=0644~%~
                    ./dev type=char mode=0644 device=native,1,3~%"
               content content (string-right-trim "/" outside) content content)
       (here dir "spec.mtree"))
      ;; the same names, a hard link to one outside then a file through it
      (alexandria:write-string-into-file "precious" (here dir "kept.txt"))
      (ensure-directories-exist (here dir "pair/"))
      (alexandria:write-string-into-file "first" (here dir "pair/x"))
      (sb-posix:link (namestring (here dir "pair/x")) (namestring (here dir "pair/y")))
      (dolist (format '("zip" "7zip" "newc" "pax"))
        (bsdtar-make (here dir (format nil "hostile.~a" format)) format "-P" "-s" ",^\\./abs,/abs,"
                     (format nil "@~a" (namestring (here dir "spec.mtree")))))
      (dolist (format '("newc" "pax"))
        (bsdtar-make (here dir (format nil "linked.~a" format)) format "-P" "-C" (namestring (here dir "pair/"))
                     "-s" ",^x$,../kept.txt," "x" "y"))
      (alexandria:write-byte-vector-into-file (rar5-octets '(("../victim.txt" . "evil") ("ok.txt" . "fine")))
                                              (here dir "hostile.rar"))
      (with-each-archive-program (program)
        (dolist (name '("hostile.zip" "hostile.7zip" "hostile.newc" "hostile.pax" "hostile.rar"))
          (let* ((out (here dir (format nil "out-~(~a~)-~a/" program name)))
                 (said (extract-failure (here dir name) out)))
            (false (probe-file (here dir "victim.txt")) "~a ~a: ~a" program name said)
            (false (probe-file (here dir "abs.txt")) "~a ~a" program name)
            (false (probe-file (here outside "through.txt")) "~a ~a" program name)
            (let ((suid (tuatara.io:path-stamp (namestring (here out "suid")) :follow nil)))
              (when suid
                (is = 0 (logand #o6000 (tuatara.io:stamp-mode suid)) "~a ~a" program name)))
            (dolist (special '("fifo" "dev"))
              (false (member (tuatara.io:path-kind (namestring (here out special)) :follow nil)
                             '(:fifo :character-device :block-device))
                     "~a ~a ~a" program name special))
            ;; an archive in it is left packed
            (false (remove-if-not (lambda (p) (search ".part" (namestring p))) (uiop:subdirectories out))
                   "~a ~a" program name)))
        (is string= "fine" (alexandria:read-file-into-string
                            (here dir (format nil "out-~(~a~)-hostile.rar/ok.txt" program)))))
      (dolist (format '("newc" "pax"))
        (let ((out (here dir (format nil "out-linked-~a/" format))))
          (extract-failure (here dir (format nil "linked.~a" format)) out)
          (is string= "precious" (alexandria:read-file-into-string (here dir "kept.txt")) "~a" format)
          (is = 1 (sb-posix:stat-nlink (sb-posix:stat (namestring (here dir "kept.txt")))) "~a" format))))))

(defparameter *locked-7z-samples*
  '(("names.7z" "N3q8ryccAAThTxWfgAAAAAAAAAAuAAAAAAAAALGhKW33bMI1NTRGHjjxd1TtPKV/LN4jrQZ6DEKOKYQwKZNKDqulHlC1KcnAmJJoyBdNUEhVzz/IYBw8YZevB4L8WQokcN5+gq8XVNSE5LUJg8Tm1r31qlfaiCBbL25S8MOhGnMg9e0We3Thtbj7kbc/XQPLQDqY/6kmI75oFIoAgwQ16BcGEAEJcAAHCwEAASQG8QcBElMPmdJMi8LmH/ZxSnXanu1QFgxiCgHpC34qAAA=")
    ("files.7z" "N3q8ryccAAR2+v8mEAAAAAAAAABiAAAAAAAAAEAUPyKxDp5OT5xb+vdR2SsFBaE2AQQGAAEJEAAHCwEAAiQG8QcBElMPaQyLHXs7BbgervvOr6/SxSEhAQABAAwKBgAICgEgMDo2AAAFARkBABENAGEALgB0AHgAdAAAABQKAQBoOjOsuWvdARUGAQAggKSBAAA="))
  "Two 7z archives locked with the password secret, each holding a.txt with
hello and a new line: names.7z with its names locked too, files.7z its files
alone (made by 7z a -psecret -mhe=on, and without -mhe=on).")

(defparameter *locked-zip-samples*
  '(("locked-aes256.zip" "UEsDBBQACQBjAFlobF0AAAAAAAAAAAAAAAAFACsAYS50eHR1eAsAAQToAwAABOgDAAABmQcAAgBBRQMIAFVUDQAH66r1auuq9WrrqvVqCxgvXjlVTamZYbubwKQLXsLVCjcFYsBSH1VO3Z+u5HqTjftQSwcIAAAAACMAAAAFAAAAUEsDBBQAAAAAAFlobF0AAAAAAAAAAAAAAAAEACAAc3ViL3V4CwABBOgDAAAE6AMAAFVUDQAH66r1auuq9WrrqvVqUEsDBBQACQBjAFlobF0AAAAAAAAAAAAAAAAJACsAc3ViL2MudHh0dXgLAAEE6AMAAAToAwAAAZkHAAIAQUUDCABVVA0AB+uq9WrrqvVq66r1aghH+trZAXVln4y9F1tuc1iO3ey11goR/KS/RHaLPUcjXoEdUEsHCAAAAAAjAAAABQAAAFBLAQIUAxQACQBjAFlobF0AAAAAIwAAAAUAAAAFACMAAAAAAAAAAACkgQAAAABhLnR4dHV4CwABBOgDAAAE6AMAAAGZBwACAEFFAwgAVVQFAAHrqvVqUEsBAhQDFAAAAAAAWWhsXQAAAAAAAAAAAAAAAAQAGAAAAAAAAAAAAO1BgQAAAHN1Yi91eAsAAQToAwAABOgDAABVVAUAAeuq9WpQSwECFAMUAAkAYwBZaGxdAAAAACMAAAAFAAAACQAjAAAAAAAAAAAApIHDAAAAc3ViL2MudHh0dXgLAAEE6AMAAAToAwAAAZkHAAIAQUUDCABVVAUAAeuq9WpQSwUGAAAAAAMAAwD6AAAASAEAAAAA")
    ("locked-zipcrypt.zip" "UEsDBBQACQAIAFlobF0AAAAAAAAAAAAAAAAFACAAYS50eHR1eAsAAQToAwAABOgDAABVVA0AB+uq9WrrqvVq66r1ailcmpvcneXPfFDV8fdzFoVWPtdQSwcIajng0BMAAAAFAAAAUEsDBBQAAAAAAFlobF0AAAAAAAAAAAAAAAAEACAAc3ViL3V4CwABBOgDAAAE6AMAAFVUDQAH66r1auuq9WrrqvVqUEsDBBQACQAIAFlobF0AAAAAAAAAAAAAAAAJACAAc3ViL2MudHh0dXgLAAEE6AMAAAToAwAAVVQNAAfrqvVq66r1auuq9Wp5pCYFmRHA4dUL6yzl7pGsnsCRUEsHCHHQQ8QTAAAABQAAAFBLAQIUAxQACQAIAFlobF1qOeDQEwAAAAUAAAAFABgAAAAAAAAAAACkgQAAAABhLnR4dHV4CwABBOgDAAAE6AMAAFVUBQAB66r1alBLAQIUAxQAAAAAAFlobF0AAAAAAAAAAAAAAAAEABgAAAAAAAAAAADtQWYAAABzdWIvdXgLAAEE6AMAAAToAwAAVVQFAAHrqvVqUEsBAhQDFAAJAAgAWWhsXXHQQ8QTAAAABQAAAAkAGAAAAAAAAAAAAKSBqAAAAHN1Yi9jLnR4dHV4CwABBOgDAAAE6AMAAFVUBQAB66r1alBLBQYAAAAAAwADAOQAAAASAQAAAAA="))
  "Two zip archives locked with the password secret, each holding a.txt with
alpha and sub/c.txt with gamma, made by bsdtar --options
zip:encryption=aes256 and zip:encryption=zipcrypt. They are kept made rather
than made by each run because the old zip encryption checks a password by one
byte: a wrong one slips past it one time in 256, and which do depends on the
archive. With these, guess is refused at the check and guess16 slips past it
to the locked zipcrypt one.")

(define-test (rimu-operations archives-locked)
  ;; an archive locked with a password, found so in the background, opens with
  ;; the right one; a wrong one is said and extracts nothing; with none it is
  ;; asked for. 7-Zip opens locked zip and 7z archives, their names locked
  ;; too; bsdtar only zip ones, and says 7-Zip opens the others
  (with-operations-tree (dir)
    (bsdtar-make (here dir "open.zip") "zip" "-C" (namestring dir) "a.txt")
    (loop for (name base64) in (append *locked-zip-samples* *locked-7z-samples*)
          do (alexandria:write-byte-vector-into-file (tuatara.io:base64-octets (tuatara.text:string-octets base64))
                                                     (here dir name)))
    (with-each-archive-program (program)
      (flet ((locked-p (name)
               (let* ((path (namestring (here dir name)))
                      (kind (tuatara.defaults::archive-kind path)))
                 (tuatara.defaults::archive-locked-p path kind (tuatara.defaults::archive-program kind))))
             (out (name) (here dir (format nil "~(~a~)-~a/" program name))))
        (true (locked-p "locked-aes256.zip") "~a" program)
        (true (locked-p "locked-zipcrypt.zip") "~a" program)
        (false (locked-p "open.zip") "~a" program)
        (dolist (encryption '("aes256" "zipcrypt"))
          (let ((archive (here dir (format nil "locked-~a.zip" encryption))))
            ;; a wrong one: said, nothing made, no empty file left to stand in a right one's way
            (is string= "the password is not right"
                (extract-failure archive (out (format nil "wrong-~a" encryption)) :password "guess") "~a" program)
            (false (tuatara.io:path-stamp (namestring (out (format nil "wrong-~a" encryption)))))
            ;; a wrong one past the check byte of the old encryption: no worse a message, nothing made
            (when (string= encryption "zipcrypt")
              (true (search "the password is not right"
                            (or (extract-failure archive (out "near-miss") :password "guess16") ""))
                    "~a" program)
              (false (tuatara.io:path-stamp (namestring (out "near-miss")))))
            ;; none: it waits for one
            (true (search "is locked with a password"
                          (or (extract-failure archive (out (format nil "none-~a" encryption))) "")))
            (false (tuatara.io:path-stamp (namestring (out (format nil "none-~a" encryption)))))
            ;; the right one
            (false (extract-failure archive (out (format nil "right-~a" encryption)) :password "secret") "~a" program)
            (is string= "gamma"
                (alexandria:read-file-into-string (here (out (format nil "right-~a" encryption)) "sub/c.txt")))))
        (dolist (name '("names.7z" "files.7z"))
          (if (eq program :7zz)
              (progn (true (locked-p name) "~a" name)
                     (is string= "the password is not right"
                         (extract-failure (here dir name) (out (format nil "wrong-~a" name)) :password "guess")
                         "~a" name)
                     (true (search "is locked with a password"
                                   (or (extract-failure (here dir name) (out (format nil "none-~a" name))) ""))
                           "~a" name)
                     (false (extract-failure (here dir name) (out (format nil "right-~a" name)) :password "secret")
                            "~a" name)
                     (is string= (format nil "hello~%")
                         (alexandria:read-file-into-string (here (out (format nil "right-~a" name)) "a.txt"))))
              (dolist (password '(nil "secret"))
                (true (search "bsdtar opens no 7z archive so locked; 7-Zip does"
                              (or (extract-failure (here dir name) (out "seven") :password password) ""))
                      "~a ~a" name password))))))
    ;; Z: found locked in the background, then asked for, hidden, and extracted
    (let ((f (ops-session dir (list "g f l o c k e d - a e s 2 5 6 . z i p RET Z C-j"
                                    #'wait-for-rimu-operations :tick
                                    "s e c r e t RET")
                          :width 300)))
      (true (tuatara.frontend.fake:fake-frame-showing f "Password for locked-aes256.zip:"))
      (false (tuatara.frontend.fake:fake-frame-showing f "secret"))
      (true (tuatara.frontend.fake:fake-frame-showing f "Extracted 1 archive to")))
    (is string= "alpha" (alexandria:read-file-into-string (here dir "locked-aes256/a.txt")))
    ;; whether one is locked is never looked into on the editor's thread
    (let ((threads '()) (editor sb-thread:*current-thread*))
      (sb-int:encapsulate 'tuatara.defaults::archive-locked-p 'thread-seen
                          (lambda (function &rest arguments)
                            (push sb-thread:*current-thread* threads)
                            (apply function arguments)))
      (unwind-protect (ops-session dir (list "g f o p e n . z i p RET Z C-j" #'wait-for-rimu-operations))
        (sb-int:unencapsulate 'tuatara.defaults::archive-locked-p 'thread-seen))
      (true threads)
      (false (member editor threads)))
    ;; no core dump of the editor while a password is held
    (flet ((dumpable () (sb-alien:alien-funcall (sb-alien:extern-alien "prctl" (function sb-alien:int sb-alien:int
                                                                                          sb-alien:unsigned-long
                                                                                          sb-alien:unsigned-long
                                                                                          sb-alien:unsigned-long
                                                                                          sb-alien:unsigned-long))
                                                3 0 0 0 0)))
      (tuatara.defaults::hold-secret)
      (is = 0 (dumpable))
      (tuatara.defaults::let-go-of-secret)
      (unless tuatara.defaults::*encrypted-buffers*
        (is = 1 (dumpable))))))

(defun wait-for-rimu-operations ()
  "Wait, ten seconds at most, until no rimu operation is running."
  (loop repeat 1000
        while (find-if (lambda (job) (and (tuatara.defaults::rimu-operation-p job) (tuatara.runtime:job-running-p job)))
                       tuatara.runtime:*jobs*)
        do (sleep 0.01)))

(defun zip-aes-256-p (octets)
  "True when the zip archive OCTETS holds an entry locked with AES-256: its
extra field of the AE-x kind (ID #x9901) saying strength 3."
  (loop for at from 0 below (- (length octets) 9)
          thereis (and (= (aref octets at) #x01) (= (aref octets (+ at 1)) #x99)
                       (= (aref octets (+ at 2)) 7) (= (aref octets (+ at 3)) 0)
                       (= (aref octets (+ at 6)) (char-code #\A)) (= (aref octets (+ at 7)) (char-code #\E))
                       (= (aref octets (+ at 8)) 3))))

(define-test (rimu-operations archives-zip-and-7z-made)
  ;; zip and 7z archives made by 7-Zip, else bsdtar, read back by the other;
  ;; locked ones open only with the password, which is never among any
  ;; program's arguments; a locked 7z needs 7-Zip, and with nothing to make
  ;; one that is said, naming what to install, nothing written
  (with-operations-tree (dir)
    (let ((secrets '()) (arguments '()) (names tuatara.defaults::*seven-zip-names*))
      (flet ((make (name paths &optional password)
               (handler-case (progn (tuatara.defaults::write-archive
                                     (mapcar (lambda (path) (namestring (here dir path))) paths)
                                     (namestring (here dir name)) :password (and password (copy-seq password)))
                                    nil)
                 (error (e) (princ-to-string e))))
             (reader (maker)
               ;; the other program reads what one made, 7-Zip hidden for bsdtar
               (if (eq maker :7zz) '() names))
             (left (name)
               (remove-if-not (lambda (p) (search name (namestring p)))
                              (uiop:directory-files dir))))
        (sb-int:encapsulate 'tuatara.io:sandbox-run 'arguments-seen
                            (lambda (function program list &rest keys)
                              (push (cons program list) arguments)
                              (push (getf keys :secret) secrets)
                              (apply function program list keys)))
        (unwind-protect
             (let ((seven (and (tuatara.defaults::seven-zip) t)))
               (with-each-archive-program (program)
                 (dolist (ending '("zip" "7z"))
                   ;; plain: a file, a link and a directory, the link kept a link
                   (let ((name (format nil "plain-~(~a~).~a" program ending)))
                     (false (make name '("a.txt" "link" "sub/")) "~a" name)
                     (is eq (if (string= ending "zip") :zip :7z)
                         (nth-value 1 (tuatara.defaults::file-kind (namestring (here dir name)))))
                     (let ((tuatara.defaults::*seven-zip-names* (reader program))
                           (out (here dir (format nil "out-~a/" name))))
                       (false (extract-failure (here dir name) out) "~a" name)
                       (is string= "alpha" (alexandria:read-file-into-string (here out "a.txt")) "~a" name)
                       (is string= "gamma" (alexandria:read-file-into-string (here out "sub/c.txt")) "~a" name)
                       (is eq :symlink (tuatara.io:path-kind (namestring (here out "link")) :follow nil) "~a" name)))
                   ;; locked: opens only with the password
                   (let ((name (format nil "locked-~(~a~).~a" program ending)))
                     (if (and (eq program :bsdtar) (string= ending "7z"))
                         (progn (true (search "bsdtar locks no 7z archive with a password, 7-Zip does: install 7-Zip"
                                              (or (make name '("a.txt") "kiwi pass 7") ""))
                                      "~a" name)
                                (false (left name)))
                         (progn
                           (false (make name '("a.txt" "sub/") "kiwi pass 7") "~a" name)
                           (let ((octets (alexandria:read-file-into-byte-vector (here dir name))))
                             (if (string= ending "zip")
                                 (true (zip-aes-256-p octets) "~a" name)
                                 ;; the names locked too
                                 (false (or (search (tuatara.text:string-octets "c.txt") octets)
                                            (search (tuatara.text:string-octets (format nil "c~c.~ct~cx~ct"
                                                                                        #\Nul #\Nul #\Nul #\Nul))
                                                    octets))
                                        "~a" name)))
                           (let ((tuatara.defaults::*seven-zip-names*
                                   (if (string= ending "7z") names (reader program))))
                             (true (search "is locked with a password"
                                           (or (extract-failure (here dir name) (here dir "none/")) ""))
                                   "~a" name)
                             (is string= "the password is not right"
                                 (extract-failure (here dir name) (here dir "wrong/") :password "kiwi pass 8")
                                 "~a" name)
                             (false (extract-failure (here dir name) (here dir (format nil "out-~a/" name))
                                                     :password "kiwi pass 7")
                                    "~a" name)
                             (is string= "gamma" (alexandria:read-file-into-string
                                                  (here dir (format nil "out-~a/sub/c.txt" name)))
                                 "~a" name)))))))
               ;; the password went on standard input alone
               (true (member "kiwi pass 7" secrets :test #'equal))
               (false (find-if (lambda (call) (some (lambda (word) (search "kiwi pass" word)) call)) arguments))
               ;; 7-Zip was asked to store links as links, take names as they are, lock names
               (when seven
                 (true (find-if (lambda (call)
                                  (every (lambda (word) (member word call :test #'equal)) '("-snl" "-spd" "-mhe=on")))
                                arguments))))
          (sb-int:unencapsulate 'tuatara.io:sandbox-run 'arguments-seen))
        ;; refusals, nothing written: no password for tar, nor one of other than plain ASCII
        (true (search "a tar archive takes no password" (or (make "t.tar.gz" '("a.txt") "kiwi") "")))
        (true (search "plain ASCII" (or (make "u.zip" '("a.txt") (format nil "kiwi~c" (code-char #x101))) "")))
        ;; nothing to make zip or 7z archives with: what to install is named
        (let ((tuatara.defaults::*seven-zip-names* '()))
          (sb-int:encapsulate 'tuatara.io:sandbox-find-program 'no-bsdtar
                              (lambda (function name)
                                (and (string/= name "bsdtar") (funcall function name))))
          (unwind-protect
               (dolist (name '("n.zip" "n.7z"))
                 (let ((said (or (make name '("a.txt")) "")))
                   (true (search "neither 7-Zip nor bsdtar is installed" said) "~a" said)
                   (true (search "install 7-Zip (" said) "~a" said)
                   (true (search "install bsdtar (" said) "~a" said)))
            (sb-int:unencapsulate 'tuatara.io:sandbox-find-program 'no-bsdtar)))
        (dolist (name '("t.tar.gz" "u.zip" "n.zip" "n.7z"))
          (false (left name) "~a" name))
        (false (left ".part"))
        ;; C-u c in rimu: the password asked twice, hidden, after the name; the archive locked with it
        (let ((f (ops-session dir (format nil "g f a . t x t RET C-u c C-a C-k ~a C-j k i w i RET k i w i RET"
                                          (rimu-typed (namestring (here dir "ui.7z"))))
                              :width 300)))
          (true (tuatara.frontend.fake:fake-frame-showing f "Archive a.txt with a password, as:"))
          (true (tuatara.frontend.fake:fake-frame-showing f "Password for ui.7z:"))
          (false (tuatara.frontend.fake:fake-frame-showing f "kiwi"))
          (if (tuatara.defaults::seven-zip)
              (progn (true (tuatara.frontend.fake:fake-frame-showing f "Archived 1 file to"))
                     (is string= "the password is not right"
                         (extract-failure (here dir "ui.7z") (here dir "ui-wrong/") :password "kiwj"))
                     (false (extract-failure (here dir "ui.7z") (here dir "ui/") :password "kiwi"))
                     (is string= "alpha" (alexandria:read-file-into-string (here dir "ui/a.txt"))))
              (true (tuatara.frontend.fake:fake-frame-showing f "bsdtar locks no 7z archive"))))
        ;; C-u c to a tar name: said, nothing asked
        (let ((f (ops-session dir (format nil "g f a . t x t RET C-u c C-a C-k ~a C-j"
                                          (rimu-typed (namestring (here dir "ui.tar.gz"))))
                              :width 300)))
          (true (tuatara.frontend.fake:fake-frame-showing f "a tar archive takes no password: name it .zip or .7z"))
          (false (tuatara.frontend.fake:fake-frame-showing f "Password for"))
          (false (probe-file (here dir "ui.tar.gz"))))))))

(define-test (rimu-operations archives-hard-link-over-nothing)
  ;; a cpio archive whose second name of a file is written after the first,
  ;; already there, was skipped: the file there is never written through it
  (with-operations-tree (dir)
    (ensure-directories-exist (here dir "pair/"))
    (alexandria:write-string-into-file "from the archive" (here dir "pair/notes.txt"))
    (sb-posix:link (namestring (here dir "pair/notes.txt")) (namestring (here dir "pair/x")))
    (bsdtar-make (here dir "linked.cpio") "newc" "-C" (namestring (here dir "pair/")) "notes.txt" "x")
    (ensure-directories-exist (here dir "target/"))
    (alexandria:write-string-into-file "mine" (here dir "target/notes.txt"))
    (extract-failure (here dir "linked.cpio") (here dir "target/"))
    (is string= "mine" (alexandria:read-file-into-string (here dir "target/notes.txt")))
    ;; what was there is said to be left as it is; nothing of the passing directory stays
    (ensure-directories-exist (here dir "again/"))
    (alexandria:write-string-into-file "mine" (here dir "again/notes.txt"))
    (is equal '("notes.txt (in linked.cpio)")
        (tuatara.defaults::rimu-work :extract (cons (namestring (here dir "linked.cpio"))
                                                    (namestring (here dir "again/")))
                                     (list nil)))
    (is string= "mine" (alexandria:read-file-into-string (here dir "again/notes.txt")))
    (dolist (directory '("target/" "again/"))
      (false (remove-if-not (lambda (p) (search ".part" (namestring p)))
                            (append (uiop:directory-files (here dir directory)) (uiop:subdirectories (here dir directory))))))))

(define-test (rimu-operations archives-format-held)
  ;; an archive is read only as the format its first octets say: a list of
  ;; files to copy (mtree) dressed as a disc image copies nothing
  (with-operations-tree (dir)
    (alexandria:write-string-into-file "secret beside it" (here dir "beside.txt"))
    (with-open-file (out (here dir "fake.iso") :direction :output :if-exists :supersede :external-format :latin-1)
      (format out "#mtree~%./copied type=file mode=0644 contents=~a~%" (namestring (here dir "beside.txt")))
      ;; the disc's mark inside a comment of the list
      (loop repeat (- 32768 (file-position out)) do (write-char #\Newline out))
      (write-string "#CD001" out)
      (loop repeat 2048 do (write-char #\Newline out)))
    (is eq :iso (nth-value 1 (tuatara.defaults::file-kind (namestring (here dir "fake.iso")))))
    (let ((said (extract-failure (here dir "fake.iso") (here dir "out/"))))
      (true said)
      (false (probe-file (here dir "out/copied")) "~a" said))))

(define-test (rimu-operations archives-listed-plainly)
  ;; a name that would turn the direction its line is drawn in is written as its code
  (with-operations-tree (dir)
    (let ((name (format nil "photo~cgpj.exe" (code-char #x202E))))
      (alexandria:write-string-into-file "x" (here dir name))
      (bsdtar-make (here dir "names.zip") "zip" "-C" (namestring dir) name)
      (bsdtar-make (here dir "names.tar") "pax" "-C" (namestring dir) name)
      (with-each-archive-program (program)
        (dolist (archive '("names.zip" "names.tar"))
          (let ((listing (tuatara.defaults::archive-listing (namestring (here dir archive)))))
            (true (or (search "photo<202E>gpj.exe" listing) (search "photo\\u202E" listing :test #'char-equal))
                  "~a ~a: ~a" program archive listing)
            (false (find (code-char #x202E) listing) "~a ~a" program archive)))))))

(define-test (rimu-operations archives-cleaned-up-after-a-failure)
  ;; a step that fails while what was unpacked is put in place leaves nothing
  ;; of the passing directory behind, and is the error said
  (with-operations-tree (dir)
    (bsdtar-make (here dir "sub.zip") "zip" "-C" (namestring dir) "sub")
    (ensure-directories-exist (here dir "target/sub/"))
    (sb-posix:chmod (namestring (here dir "target/sub/")) #o555)
    (unwind-protect
         (let ((said (extract-failure (here dir "sub.zip") (here dir "target/"))))
           (true said)
           (false (remove-if-not (lambda (p) (search ".part" (namestring p)))
                                 (uiop:subdirectories (here dir "target/")))
                  "~a" said))
      (sb-posix:chmod (namestring (here dir "target/sub/")) #o755))))

(define-test (rimu-operations archives-modes-by-umask)
  ;; what is extracted has the modes your umask allows, whatever the archive says, by 7-Zip and bsdtar alike
  (with-operations-tree (dir)
    (alexandria:write-string-into-file
     (format nil "#mtree~%./wide type=file mode=0777 contents=~a~%./shared type=file mode=0666 contents=~a~%"
             (namestring (here dir "a.txt")) (namestring (here dir "a.txt")))
     (here dir "modes.mtree"))
    (bsdtar-make (here dir "modes.zip") "zip" (format nil "@~a" (namestring (here dir "modes.mtree"))))
    (let ((umask (tuatara.io::umask-bits)))
      (with-each-archive-program (program)
        (let ((out (here dir (format nil "modes-~(~a~)/" program))))
          (false (extract-failure (here dir "modes.zip") out) "~a" program)
          (loop for (name mode) in '(("wide" #o777) ("shared" #o666))
                do (is = (logior #o200 (logand mode (lognot umask)))
                       (logand #o7777 (tuatara.io:stamp-mode (tuatara.io:path-stamp (namestring (here out name)))))
                       "~a ~a" program name)))))))

(define-test (rimu-operations archive-password-kept-from-debug-log)
  ;; an archive that turns out no archive once its password is given: the
  ;; failure, with the calls under way, never shows the password
  (with-operations-tree (dir)
    (drain-debug-reports)
    (clrhash tuatara.runtime::*error-report-places*)
    (setf tuatara-user:*debug-mode* t)
    (tuatara.runtime:set-debug t)
    (unwind-protect
         (progn
           (tuatara.defaults::hold-secret)
           (tuatara.defaults::rimu-start-operation
            :extract (list (cons (namestring (here dir "a.txt")) (namestring (here dir "out/")))) nil
            :passwords (list (cons (namestring (here dir "a.txt")) (copy-seq "hunter2-kiwi"))))
           (wait-for-rimu-operations)
           (let ((said (format nil "~{~a~%~}" (mapcar #'fourth (sb-concurrency:list-queue-contents
                                                               tuatara.runtime:*debug-reports*))))
                 (log (if (probe-file *test-debug-log*) (alexandria:read-file-into-string *test-debug-log*) "")))
             (true (search "extract" said) "~a" said)
             (false (search "hunter2-kiwi" said))
             (false (search "hunter2-kiwi" log))))
      (drain-debug-reports))))

(define-test (rimu-operations archive-password-typed-after)
  ;; a password asked unasked takes only keys pressed once it shows, never those typed before
  (let ((f (tuatara.frontend.fake:make-fake-frontend :width 60 :height 8))
        (waiting t) (got nil))
    (setf (tuatara.frontend.fake:fake-pending f) (lambda (f) (and waiting (tuatara.frontend.fake::fake-events f))))
    (queue-test-keys f (list (lambda () (setf got (tuatara.command:read-password "Password for x.zip: " :fresh t)))
                             "w r o n g" (lambda () (setf waiting nil)) "s e c r e t RET"))
    (tuatara.command:run-editor f :setup #'tuatara.defaults:install-defaults)
    (is equal "secret" got))
  ;; and the question rimu asks for an archive found locked is one such
  (let ((fresh nil))
    (sb-int:encapsulate 'tuatara.command:read-password 'fresh-seen
                        (lambda (function prompt &rest keys)
                          (declare (ignore function prompt))
                          (setf fresh (getf keys :fresh))
                          (error 'tuatara.command:quit)))
    (unwind-protect (ignore-errors (handler-case (tuatara.defaults::rimu-ask-passwords (list (cons "/x.zip" "/y/")) nil)
                                     (tuatara.command:quit () nil)))
      (sb-int:unencapsulate 'tuatara.command:read-password 'fresh-seen))
    (true fresh)))

(define-test (rimu-operations archives-seven-zip-said)
  ;; 7-Zip's own program known by what it says of itself, and the formats it reads; p7zip's is not taken
  (let ((said (tuatara.defaults::seven-zip-said
               (format nil "~%7-Zip 26.02 (x64) : Copyright (c) 1999-2026 Igor Pavlov : 2026-06-25~%~%Formats:~%~
                            0 C...F..........c.a.m+.. w...0  7z       7z            7 z BC AF ' 1C~%~
                            0  ......................  Iso      iso img       offset=32769 C D 0 0 1~%~%Codecs:~%"))))
    (is equal '(26 2) (first said))
    (is equal '("7z" "Iso") (second said)))
  (false (tuatara.defaults::seven-zip-said
          (format nil "7-Zip [64] 16.02 : Copyright (c) 1999-2016 Igor Pavlov : 2016-05-21~%p7zip Version 16.02~%")))
  ;; with no 7-Zip, bsdtar reads what 7-Zip would
  (let ((tuatara.defaults::*seven-zip-names* '()))
    (is eq :bsdtar (tuatara.defaults::archive-program (assoc :7z tuatara.defaults::*archive-kinds*))))
  (when (tuatara.defaults::seven-zip)
    (is eq :7zz (tuatara.defaults::archive-program (assoc :7z tuatara.defaults::*archive-kinds*))))
  ;; a technical list in the columns of the others
  (is string=
      (format nil "-rw-r--r-- -/- ~12@a 2026-11-03 21:33:46 a.txt~%drwxr-xr-x -/- ~12@a 2026-11-03 21:33:46 sub~%" 6 0)
      (tuatara.defaults::seven-zip-listing
       (format nil "Listing archive: x.7z~%~%--~%Path = x.7z~%Type = 7z~%~%----------~%Path = a.txt~%Size = 6~%~
                    Modified = 2026-11-03 21:33:46.1675118~%Attributes = A -rw-r--r--~%Encrypted = -~%~%~
                    Path = sub~%Size = 0~%Modified = 2026-11-03 21:33:46.1712538~%Attributes = D drwxr-xr-x~%~%"))))

(define-test (rimu-operations archives-bomb-held)
  ;; a zip archive holding far more than it takes up stops before the drive is full, said so
  (with-operations-tree (dir)
    (ensure-directories-exist (here dir "zeros/"))
    (with-open-file (out (here dir "zeros/zero") :direction :output :element-type '(unsigned-byte 8))
      (let ((block (make-array (* 1024 1024) :element-type '(unsigned-byte 8) :initial-element 0)))
        (loop repeat 24 do (write-sequence block out))))
    (bsdtar-make (here dir "zeros.zip") "zip" "-C" (namestring (here dir "zeros/")) "zero")
    ;; locked too: the password tried on all of it first is held to the same room
    (bsdtar-make (here dir "locked-zeros.zip") "zip" "--options" "zip:encryption=aes256" "--passphrase" "secret"
                 "-C" (namestring (here dir "zeros/")) "zero")
    (delete-file (here dir "zeros/zero"))
    (with-each-archive-program (program)
      (loop for (name password) in '(("zeros.zip" nil) ("locked-zeros.zip" "secret"))
            do (let* ((free (tuatara.io:free-octets (namestring dir)))
                      (tuatara.defaults::*archive-free-reserve* (- free (* 4 1024 1024)))
                      (out (here dir (format nil "out-~(~a~)-~a/" program name)))
                      (said (extract-failure (here dir name) out :password password)))
                 (true (search "*archive-free-reserve*" (or said "")) "~a ~a: ~a" program name said)
                 (let ((size (ignore-errors
                               (tuatara.io:stamp-size (tuatara.io:path-stamp (namestring (here out "zero")))))))
                   (true (or (null size) (< size (* 24 1024 1024))) "~a ~a" program name)))))))

(define-modal-test (rimu-operations keys-listed)
  ;; ? lists the keys a list takes away as doing nothing there, never as their commands; nor do the key hints
  (with-operations-tree (dir)
    (ops-session dir "?")
    (let ((help (tuatara.buffer:buffer-string (tuatara.buffer:find-buffer "*Help*"))))
      (true (search "rimu-copy" help))
      (false (search "undefined" help))
      ;; w, which a list takes away, shows as (none here) under every keymap, the modal ones' too
      (let ((lines (remove-if-not (lambda (line) (uiop:string-prefix-p "  w " line))
                                  (uiop:split-string help :separator '(#\Newline)))))
        (true lines)
        (true (every (lambda (line) (search "(none here)" line)) lines))))
    (let ((f (ops-session dir "g" :height 30)))
      (true (tuatara.frontend.fake:fake-frame-showing f "rimu-home"))
      ;; with the g keys a list's parent mode gives it: g r reads the list again
      (true (tuatara.frontend.fake:fake-frame-showing f "revert-buffer"))
      (true (tuatara.frontend.fake:fake-frame-showing f "rimu-trash"))
      (false (tuatara.frontend.fake:fake-frame-showing f "undefined")))))

(define-modal-test (rimu-operations names-as-text)
  (with-operations-tree (dir)
    ;; C-x C-q: the names as text, a line each (sub, a.txt, b.txt, link), point on a.txt's;
    ;; a.txt and b.txt swapped by editing their lines, in two steps; a buffer on a.txt follows it
    (let ((visiting (tuatara.io:visit-file (namestring (here dir "a.txt")))))
      (multiple-value-bind (f editor)
          (ops-session dir "g f a . t x t RET C-x C-q i C-a C-k b . t x t C-g j i C-a C-k a . t x t C-g C-c C-c")
        (true (tuatara.frontend.fake:fake-frame-showing f "Rename: C-c C-c renames the lines changed"))
        (true (tuatara.frontend.fake:fake-frame-showing f "Renamed 2"))
        (true (tuatara.defaults::rimu-list-p (tuatara.display:window-buffer (tuatara.command:editor-window editor)))))
      (is string= "beta" (alexandria:read-file-into-string (here dir "a.txt")))
      (is string= "alpha" (alexandria:read-file-into-string (here dir "b.txt")))
      (is equal (tuatara.io:expand-path (namestring (here dir "b.txt"))) (tuatara.buffer:buffer-file visiting))
      (tuatara.buffer:kill-buffer visiting))
    ;; refused, nothing renamed: a name twice, a name another file has, a line taken away
    (let ((f (ops-session dir "g f a . t x t RET C-x C-q i C-a C-k s u b C-g C-c C-c")))
      (true (tuatara.frontend.fake:fake-frame-showing f "Nothing renamed: sub is given twice")))
    (alexandria:write-string-into-file "z" (here dir "other.txt"))
    (let ((f (ops-session dir "g f a . t x t RET C-x C-q i C-a C-k C-k C-g C-c C-c" :width 200)))
      (true (tuatara.frontend.fake:fake-frame-showing f "Nothing renamed: a line was added, taken away or joined (4 lines for 5 names)")))
    (true (probe-file (here dir "a.txt")))
    ;; a line taken away and another added: as many lines as names, but none renamed in a cascade
    (let ((f (ops-session dir "g f a . t x t RET C-x C-q i C-a C-k C-k M-> n e w C-g C-c C-c" :width 200)))
      (true (tuatara.frontend.fake:fake-frame-showing f "Nothing renamed: a line was added, taken away or joined")))
    (true (probe-file (here dir "a.txt")))
    (true (probe-file (here dir "other.txt")))
    (false (probe-file (here dir "new")))
    ;; a name with a line break in it is left out, said in the header; the others pair as they are
    (alexandria:write-string-into-file "nl" (here dir (format nil "nl~%name")))
    (let ((f (ops-session dir "g f a . t x t RET C-x C-q i C-a C-k a 2 . t x t C-g C-c C-c" :width 200)))
      (true (tuatara.frontend.fake:fake-frame-showing f "1 name with a control or invisible character left out"))
      (true (tuatara.frontend.fake:fake-frame-showing f "Renamed 1")))
    (true (probe-file (here dir "a2.txt")))
    (true (probe-file (here dir (format nil "nl~%name"))))
    (true (probe-file (here dir "other.txt")))
    (rename-file (here dir "a2.txt") (here dir "a.txt"))
    (delete-file (here dir (format nil "nl~%name")))
    ;; C-c C-k: nothing renamed, back to the list
    (let ((f (ops-session dir "g f a . t x t RET C-x C-q i C-a C-k z z z C-g C-c C-k")))
      (true (tuatara.frontend.fake:fake-frame-showing f "No name changed")))
    (true (probe-file (here dir "a.txt")))
    ;; a name with a slash is refused: renaming never moves
    (let ((f (ops-session dir "g f a . t x t RET C-x C-q i C-a C-k s u b / a . t x t C-g C-c C-c" :width 160)))
      (true (tuatara.frontend.fake:fake-frame-showing f "Nothing renamed: sub/a.txt is no name in this directory")))
    ;; a mark goes with its file renamed: a.txt (beta) marked for deletion, a and b swapped, x trashes it
    (ops-session dir "g f a . t x t RET d g f a . t x t RET C-x C-q i C-a C-k b . t x t C-g j i C-a C-k a . t x t C-g C-c C-c x y RET")
    (is string= "alpha" (alexandria:read-file-into-string (here dir "a.txt")))
    (false (probe-file (here dir "b.txt")))
    ;; the edits refused are left open; they go, so as not to stand in later tests' way
    (dolist (buffer (copy-list tuatara.buffer:*buffers*))
      (when (uiop:string-prefix-p "*Rename: " (tuatara.buffer:buffer-name buffer))
        (tuatara.buffer:kill-buffer buffer)))))

(define-test (rimu-operations names-as-text-checked-in-linear-time)
  ;; checking the names of a directory renamed as text costs the same for each
  ;; name, however many there are: four times the names, at most about four
  ;; times the time
  (with-test-directory (dir)
    (flet ((seconds-each (count)
             (let* ((names (loop for i below count collect (format nil "photo-~d.jpg" i)))
                    (finals (loop for i below count collect (format nil "renamed-~d.jpg" i)))
                    (changes (mapcar #'cons names finals))
                    (start (thread-cpu-time)))
               (loop repeat 3
                     do (false (tuatara.defaults::rimu-names-refusal (tuatara.io:native dir) names finals changes)))
               (/ (- (thread-cpu-time) start) internal-time-units-per-second count))))
      (seconds-each 500)
      (let ((small (seconds-each 2000)) (large (seconds-each 8000)))
        (true (< large (* 2.5 (max small 1/1000000))) "each name: ~,2e s of 2000, ~,2e s of 8000" small large)))
    ;; a name given twice, and one another file has, are still refused
    (alexandria:write-string-into-file "x" (merge-pathnames "kept.jpg" dir))
    (true (search "given twice"
                  (tuatara.defaults::rimu-names-refusal (tuatara.io:native dir)
                                                        '("a" "b")
                                                        '("c" "c")
                                                        '(("a" . "c") ("b" . "c")))))
    (true (search "there already"
                  (tuatara.defaults::rimu-names-refusal (tuatara.io:native dir)
                                                        '("a")
                                                        '("kept.jpg")
                                                        '(("a" . "kept.jpg")))))))

(define-test (rimu-operations new-files)
  (with-operations-tree (dir)
    ;; a: an empty file made there, the list on it
    (multiple-value-bind (f editor) (ops-session dir "a f r e s h . t x t RET" :width 300)
      (true (tuatara.frontend.fake:fake-frame-showing f "Touch in"))
      (true (tuatara.frontend.fake:fake-frame-showing f "Made"))
      (is = 0 (length (alexandria:read-file-into-string (here dir "fresh.txt"))))
      (let ((list (tuatara.display:window-buffer (tuatara.command:editor-window editor))))
        (is string= "fresh.txt"
            (tuatara.io:listed-name (tuatara.defaults::rimu-entry-at list (tuatara.buffer:buffer-point list))))))
    ;; a name there already, a file or a directory, is touched: its times now, its text kept
    (flet ((age (name)
             (let ((path (here dir name)))
               (sb-posix:utimes path 1000000000 1000000000)
               path))
           (modified (path) (sb-posix:stat-mtime (sb-posix:lstat path))))
      (let* ((path (age "a.txt"))
             (text (alexandria:read-file-into-string path))
             (f (ops-session dir "a a . t x t RET" :width 300)))
        (true (tuatara.frontend.fake:fake-frame-showing f "Touched a.txt"))
        (true (> (modified path) 1000000000))
        (is string= text (alexandria:read-file-into-string path)))
      (let ((path (age "sub")))
        (ops-session dir "a s u b RET" :width 300)
        (true (> (modified path) 1000000000)))
      ;; RET alone touches the entry on the line, or those marked
      (let ((path (age "a.txt")))
        (let ((f (ops-session dir "g f a . t x t RET a RET" :width 300)))
          (true (tuatara.frontend.fake:fake-frame-showing f "(RET: a.txt)")))
        (true (> (modified path) 1000000000)))
      (let ((one (age "a.txt")) (two (age "b.txt")))
        (let ((f (ops-session dir "g f a . t x t RET m g f b . t x t RET m a RET" :width 300)))
          (true (tuatara.frontend.fake:fake-frame-showing f "(RET: 2 files)"))
          (true (tuatara.frontend.fake:fake-frame-showing f "Touched 2 entries")))
        (true (> (modified one) 1000000000))
        (true (> (modified two) 1000000000))))
    ;; only a name: a / is refused
    (let ((f (ops-session dir "a s u b / x RET" :width 300)))
      (true (tuatara.frontend.fake:fake-frame-showing f "sub/x is no name of a file in"))
      (false (probe-file (here dir "sub/x"))))
    ;; a plain name is one the directory can hold as it is: no /, no control character, not . nor ..
    (dolist (name '("notes.txt" ".hidden" "a b" "ü"))
      (true (tuatara.defaults::plain-file-name-p name) "~s" name))
    (dolist (name (list "" "." ".." "sub/x" "/" (format nil "a~cb" #\Tab) (format nil "a~c" (code-char 127))))
      (false (tuatara.defaults::plain-file-name-p name) "~s" name))
    ;; A: a formatted file, its header written and saved at once
    (let ((tuatara.defaults::*formatted-file-prompts* '(:title :keywords)))
      (ops-session dir "A M y SPC n o t e RET RET"))
    (let ((made (find-if (lambda (name) (search "--my-note" name)) (mapcar #'first (tuatara.io:directory-entries dir)))))
      (true made)
      (true (search "#+title: My note" (alexandria:read-file-into-string (here dir made)))))))

(define-test (rimu-operations tests-see-no-real-trash)
  ;; the tests' trashes are their own: neither the home one nor a drive's (a USB stick's)
  (with-operations-tree (dir)
    (is equal (list (cons (tuatara.io:home-trash) nil)) (tuatara.defaults::trash-directories))
    (true (search (namestring dir) (tuatara.io:home-trash))))
  ;; and the runner's own: a data home of the tests', no drive mounted
  (false (search "/.local/share/" (tuatara.io:home-trash)))
  (is equal (list (cons (tuatara.io:home-trash) nil)) (tuatara.defaults::trash-directories)))

(define-test (rimu-operations trash-list)
  (with-operations-tree (dir)
    ;; g t lists the trash; R puts the one on the line back where it was, after asking
    (ops-session dir "g f b . t x t RET D y")
    (let ((f (ops-session dir "g t" :width 300)))
      (true (tuatara.frontend.fake:fake-frame-showing f "1 thing in the trash"))
      (true (tuatara.frontend.fake:fake-frame-showing f "b.txt")))
    ;; answered no: left in the trash
    (let ((f (ops-session dir "g t R n" :width 300)))
      (false (tuatara.frontend.fake:fake-frame-showing f "Put back:"))
      (false (probe-file (here dir "b.txt")))
      (true (probe-file (here dir "data/Trash/info/b.txt.trashinfo"))))
    (let ((f (ops-session dir "g t R" :width 300)))
      (true (tuatara.frontend.fake:fake-frame-showing f "Put back the thing"))
      (false (probe-file (here dir "b.txt"))))
    (let ((f (ops-session dir "g t R y" :width 300)))
      (true (tuatara.frontend.fake:fake-frame-showing f "Put back:"))
      (is string= "beta" (alexandria:read-file-into-string (here dir "b.txt")))
      (false (probe-file (here dir "data/Trash/info/b.txt.trashinfo"))))
    ;; taken: another name asked for
    (ops-session dir "g f b . t x t RET D y")
    (alexandria:write-string-into-file "new beta" (here dir "b.txt"))
    (let ((f (ops-session dir (format nil "g t R y C-a C-k ~a C-j" (rimu-typed (namestring (here dir "b-old.txt"))))
                          :width 300)))
      (true (tuatara.frontend.fake:fake-frame-showing f "is taken; put it back as:"))
      (is string= "beta" (alexandria:read-file-into-string (here dir "b-old.txt"))))
    ;; D for good, X empties, asked twice; each thing counted once, its .trashinfo going with it
    (ops-session dir "g f b - o l d . t x t RET D y g f s u b RET D y")
    (ops-session dir "g f a . t x t RET D y")
    (let ((f (ops-session dir "g t D y RET" :width 300)))
      (true (tuatara.frontend.fake:fake-frame-showing f "Deleted for good 1 file"))
      (false (tuatara.frontend.fake:fake-frame-showing f "Deleted for good 2 files")))
    (let ((f (ops-session dir "g t" :width 300)))
      (true (tuatara.frontend.fake:fake-frame-showing f "2 things in the trash")))
    ;; back from the list of the trash, the cursor is on an entry of the list
    (multiple-value-bind (f editor) (ops-session dir "g e g t q")
      (declare (ignore f))
      (true (tuatara.defaults::rimu-entry-at (tuatara.display:window-buffer (tuatara.command:editor-window editor))
                                             (session-point editor))))
    (is = 2 (length (uiop:directory-files (here dir "data/Trash/info/"))))
    (let ((f (ops-session dir "g t X y RET y RET" :width 300)))
      (true (tuatara.frontend.fake:fake-frame-showing f "Deleted for good 2 files"))
      (false (tuatara.frontend.fake:fake-frame-showing f "Deleted for good 4 files")))
    (is eq nil (uiop:directory-files (here dir "data/Trash/info/")))
    (let ((f (ops-session dir "g t" :width 300)))
      (true (tuatara.frontend.fake:fake-frame-showing f "Nothing in the trash")))))

(define-test (rimu-operations trash-drawn-from-what-was-read)
  (with-operations-tree (dir)
    (ops-session dir "g f b . t x t RET D y")
    ;; marks drawn from what was read: a thing put in the trash meanwhile shows only once it is read again
    (let ((f (ops-session dir
                          (list "g t" (lambda () (tuatara.io:trash-path (namestring (here dir "a.txt")))) "m u TAB TAB")
                          :width 200)))
      (true (tuatara.frontend.fake:fake-frame-showing f "1 thing in the trash"))
      (false (tuatara.frontend.fake:fake-frame-showing f "2 things in the trash")))
    ;; rimu having changed the trash, the list shown reads it again by itself
    (let ((f (ops-session dir (list "g t" (lambda ()
                                            (tuatara.io:trash-path (namestring (here dir "sub")))
                                            (tuatara.defaults::trash-changed))
                                    :tick)
                          :width 200)))
      (true (tuatara.frontend.fake:fake-frame-showing f "3 things in the trash")))
    ;; deleted for good in the background, the list read again after
    (let ((f (ops-session dir "g t D y RET" :width 200)))
      (true (tuatara.frontend.fake:fake-frame-showing f "Deleted for good"))
      (true (tuatara.frontend.fake:fake-frame-showing f "2 things in the trash")))))

(define-test (rimu-operations restored-into-odd-names)
  ;; put back as a name in a directory not there, whose name holds [ ] and *: the
  ;; directory made again, characters, not patterns
  (with-operations-tree (dir)
    (let ((target (concatenate 'string (tuatara.io:native dir) "gone [1]*/b.txt")))
      (ops-session dir "g f b . t x t RET D y")
      (alexandria:write-string-into-file "new beta" (here dir "b.txt"))
      (let ((f (ops-session dir (format nil "g t R y C-a C-k ~a C-j" (rimu-typed target)) :width 300)))
        (true (tuatara.frontend.fake:fake-frame-showing f "Put back:")))
      (is string= "beta" (file-text (sb-ext:parse-native-namestring target))))))

(define-test (rimu-operations trash-refresh)
  ;; r and g r list the trash again, what went there since shown; nothing put back
  (with-operations-tree (dir)
    (ops-session dir "g f b . t x t RET D y")
    (dolist (keys '("r" "g r"))
      (let ((name (if (string= keys "r") "a.txt" "sub")))
        (let ((f (ops-session dir (list "g t" (lambda () (tuatara.io:trash-path (namestring (here dir name)))) keys))))
          (true (tuatara.frontend.fake:fake-frame-showing f (if (string= keys "r") "2 things in the trash" "3 things in the trash")))
          (false (tuatara.frontend.fake:fake-frame-showing f "Put back:"))
          (false (probe-file (here dir "b.txt"))))))))

(define-test (rimu-operations typed-line-selected)
  ;; as Vertico does: in C-x d the line typed is selected while it names a directory, RET takes it;
  ;; C-n goes to the first directory, RET into it, RET again takes it
  (with-operations-tree (dir)
    (rimu-forget)
    (let ((tuatara-user:*rimu-preview* nil))
      (multiple-value-bind (f editor) (run-keys (format nil "C-x d C-a C-k ~a RET" (rimu-typed (namestring dir)))
                                                :width 300
                                                :height 20)
        (true (tuatara.frontend.fake:fake-frame-showing f "*/"))
        (is string= (namestring dir)
            (tuatara.buffer:buffer-local (tuatara.display:window-buffer (tuatara.command:editor-window editor))
                                         'tuatara.defaults::rimu-directory)))
      (rimu-forget)
      (multiple-value-bind (f editor) (run-keys (format nil
                                                        "C-x d C-a C-k ~a C-n RET RET"
                                                        (rimu-typed (namestring dir)))
                                                :width 300
                                                :height 20)
        (declare (ignore f))
        (is string= (namestring (here dir "sub/"))
            (tuatara.buffer:buffer-local (tuatara.display:window-buffer (tuatara.command:editor-window editor))
                                         'tuatara.defaults::rimu-directory)))
      ;; + with a name begun that a directory begins with (s, sub/ there): sub/ selected; C-p RET makes s
      (ops-session dir (format nil "+ C-a C-k ~a s C-p RET" (rimu-typed (namestring dir))) :width 300)
      (true (probe-file (here dir "s/")))
      (false (probe-file (here dir "sub/s/"))))
    ;; C-x C-f on a directory, the line typed selected: RET lists it, as Emacs's dired does
    (rimu-forget)
    (let ((tuatara-user:*rimu-preview* nil))
      (multiple-value-bind (f editor) (run-keys (format nil "C-x C-f C-a C-k ~a RET"
                                                        (rimu-typed (namestring (here dir "sub/"))))
                                                :width 300 :height 20)
        (declare (ignore f))
        (is string= (namestring (here dir "sub/"))
            (tuatara.buffer:buffer-local (tuatara.display:window-buffer (tuatara.command:editor-window editor))
                                         'tuatara.defaults::rimu-directory))))
    ;; C-j with nothing typed: this directory, the one the editor is in
    (rimu-forget)
    (let ((tuatara-user:*rimu-preview* nil)
          (*default-pathname-defaults* (pathname (namestring dir))))
      (multiple-value-bind (f editor) (run-keys "C-x d C-a C-k C-j" :width 300 :height 20)
        (declare (ignore f))
        (true (tuatara.buffer:buffer-local (tuatara.display:window-buffer (tuatara.command:editor-window editor))
                                           'tuatara.defaults::rimu-directory))))
    ;; M the same: su, sub/ selected, RET into it, RET moves there
    (ops-session dir (format nil "g f b . t x t RET M C-a C-k ~a s u RET RET" (rimu-typed (namestring dir))) :width 300)
    (is string= "beta" (alexandria:read-file-into-string (here dir "sub/b.txt")))))
(define-test (rimu-operations last-used-first)
  ;; what was chosen lately in a directory, or something in it, comes first there: RET goes that way
  (with-operations-tree (dir)
    (ensure-directories-exist (here dir "zeta/"))
    (tuatara.defaults::note-chosen-path (namestring (here dir "zeta/notes.txt")))
    (let ((tuatara-user:*last-used-first* t))
      (rimu-forget)
      (multiple-value-bind (f editor) (run-keys (format nil
                                                        "C-x d C-a C-k ~a C-n RET RET"
                                                        (rimu-typed (namestring dir)))
                                                :width 300
                                                :height 20)
        (declare (ignore f))
        (is string= (namestring (here dir "zeta/"))
            (tuatara.buffer:buffer-local (tuatara.display:window-buffer (tuatara.command:editor-window editor))
                                         'tuatara.defaults::rimu-directory)))
      ;; and what is chosen is remembered, the latest first
      (is string= (namestring (here dir "zeta/")) (first (tuatara.defaults::chosen-paths))))
    ;; with *last-used-first* NIL: the plain order, sub/ before zeta/
    (tuatara.defaults::note-chosen-path (namestring (here dir "zeta/notes.txt")))
    (let ((tuatara-user:*last-used-first* nil))
      (rimu-forget)
      (multiple-value-bind (f editor) (run-keys (format nil
                                                        "C-x d C-a C-k ~a C-n RET RET"
                                                        (rimu-typed (namestring dir)))
                                                :width 300
                                                :height 20)
        (declare (ignore f))
        (is string= (namestring (here dir "sub/"))
            (tuatara.buffer:buffer-local (tuatara.display:window-buffer (tuatara.command:editor-window editor))
                                         'tuatara.defaults::rimu-directory))))))

(define-test (rimu-operations links-never-followed)
  (with-test-directory (dir)
    (let ((outside (merge-pathnames "outside/" dir))
          (tree (merge-pathnames "tree/" dir)))
      (ensure-directories-exist outside)
      (alexandria:write-string-into-file "keep" (merge-pathnames "precious.txt" outside))
      (flet ((outside-whole () (and (probe-file (merge-pathnames "precious.txt" outside))
                                    (equal '("precious.txt")
                                           (mapcar #'first (tuatara.io:directory-entries (namestring outside))))))
             (fill-tree ()
               (ensure-directories-exist (merge-pathnames "sub/deeper/" tree))
               (dotimes (i 20)
                 (alexandria:write-string-into-file "x"
                                                    (merge-pathnames (format nil "sub/f~d" i) tree)
                                                    :if-exists :supersede)
                 (alexandria:write-string-into-file "y"
                                                    (merge-pathnames (format nil "sub/deeper/g~d" i) tree)
                                                    :if-exists :supersede))))
        ;; a directory swapped for a link to another between being examined and opened: not opened through it
        (sb-posix:symlink (namestring outside) (namestring (merge-pathnames "swapped" dir)))
        (tuatara.io::call-with-parent (namestring (merge-pathnames "swapped" dir))
          (lambda (dirfd name)
            (fail (tuatara.io::open-directory-entry dirfd name "swapped") 'tuatara.io:file-system-error)
            (fail (tuatara.io::copy-file-at dirfd name dirfd (tuatara.text:string-octets "copied") "swapped" "copied")
                'tuatara.io:file-system-error)))
        (false (probe-file (merge-pathnames "copied" dir)))
        ;; deleted and copied while another program swaps a directory in it for a link to OUTSIDE, again and again
        (let* ((sub (namestring (merge-pathnames "tree/sub" dir)))
               (aside (namestring (merge-pathnames "tree/aside" dir)))
               (stop nil)
               ;; the tree filled again only while SUB is a directory, never written through the link
               (lock (sb-thread:make-mutex :name "swap"))
               (swapper (sb-thread:make-thread
                         (lambda ()
                           (loop until stop
                                 do (sb-thread:with-mutex (lock)
                                      (ignore-errors
                                       (when (probe-file sub)
                                         (sb-posix:rename sub aside)
                                         (sb-posix:symlink (namestring outside) sub)
                                         (sb-posix:unlink sub)
                                         (sb-posix:rename aside sub))))))
                         :name "swapper")))
          (unwind-protect
               (dotimes (round 60)
                 (sb-thread:with-mutex (lock) (ignore-errors (fill-tree)))
                 (ignore-errors (tuatara.io:copy-path-tree (namestring tree) (namestring (merge-pathnames "copy" dir))))
                 (ignore-errors (tuatara.io:delete-tree (namestring (merge-pathnames "copy" dir))))
                 (ignore-errors (tuatara.io:delete-tree (namestring tree))))
            (setf stop t)
            (sb-thread:join-thread swapper)))
        (true (outside-whole) "what the links led to is untouched")))))

(define-test (rimu-operations deep-trees-refused)
  ;; a tree nested deeper than *DIRECTORY-TREE-DEPTH-LIMIT*: refused whole, never left half-copied, nothing hidden
  ;; beside
  (with-test-directory (dir)
    (let ((tuatara.io::*directory-tree-depth-limit* 4)
          (deep (merge-pathnames "deep/a/b/c/d/e/f/" dir))
          (shallow (merge-pathnames "shallow/a/b/" dir)))
      (ensure-directories-exist deep)
      (ensure-directories-exist shallow)
      (alexandria:write-string-into-file "x" (merge-pathnames "file" deep))
      (fail (tuatara.io:copy-path-tree (namestring (merge-pathnames "deep" dir))
                                       (namestring (merge-pathnames "copy" dir)))
          'tuatara.io:file-system-error)
      (is equal '("deep" "shallow") (sort (mapcar #'first (tuatara.io:directory-entries (namestring dir))) #'string<))
      (fail (tuatara.io:delete-tree (namestring (merge-pathnames "deep" dir))) 'tuatara.io:file-system-error)
      (true (probe-file (merge-pathnames "file" deep)))
      ;; within the depth, both are done
      (true (tuatara.io:copy-path-tree (namestring (merge-pathnames "shallow" dir))
                                       (namestring (merge-pathnames "copy" dir))))
      (true (tuatara.io:delete-tree (namestring (merge-pathnames "copy" dir))))
      (let ((tuatara.io::*directory-tree-depth-limit* 20))
        (true (tuatara.io:delete-tree (namestring (merge-pathnames "deep" dir))))))))

(define-test (rimu-operations trash-as-a-list)
  ;; the trash as the buffer list is: a section for your trash, marks, two put back at once,
  ;; one marked for deletion deleted for good with x
  (with-operations-tree (dir)
    (ops-session dir "g f a . t x t RET D y g f b . t x t RET D y g f s u b RET D y")
    (let ((f (ops-session dir "g t" :width 200)))
      (true (tuatara.frontend.fake:fake-frame-showing f "▾ Your trash (3)"))
      (true (tuatara.frontend.fake:fake-frame-showing f "sub/"))
      (true (tuatara.frontend.fake:fake-frame-showing f "3 things in the trash")))
    ;; sub, then b, marked (the last to go first), put back after one question
    (let ((f (ops-session dir "g t m m R y" :width 200)))
      (true (tuatara.frontend.fake:fake-frame-showing f
                                                      (format nil
                                                              "Put back the 2 things to ~a?"
                                                              (tuatara.command::abbreviated-path (namestring dir)))))
      (true (probe-file (here dir "b.txt")))
      (true (probe-file (here dir "sub/c.txt")))
      (true (tuatara.frontend.fake:fake-frame-showing f "1 thing in the trash")))
    ;; a.txt marked for deletion and deleted for good
    (let ((f (ops-session dir "g t d x y RET" :width 200)))
      (true (tuatara.frontend.fake:fake-frame-showing f "Delete the thing a.txt for good?"))
      (false (probe-file (here dir "a.txt")))
      (true (tuatara.frontend.fake:fake-frame-showing f "Nothing in the trash")))))

(define-test (rimu-operations trash-filtered-questions)
  ;; in the trash list narrowed by a filter, R and D on the marked say how many the filter hides;
  ;; answered no, nothing is put back or deleted
  (with-operations-tree (dir)
    (ops-session dir "g f a . t x t RET m m D y")
    (unwind-protect
         (loop for (keys question) in '(("R n" "Put back the 2 things (1 hidden by the filter) to ")
                                        ("D n" "Delete the 2 things (1 hidden by the filter) for good?"))
               do (let ((f (ops-session dir (format nil "g t C-x / r M-< C-n C-n C-n m m C-x / s a . t x t RET ~a" keys)
                                        :width (width-for dir 200))))
                    (true (tuatara.frontend.fake:fake-frame-showing f question))))
      (let ((list (tuatara.buffer:find-buffer "*Trash*")))
        (when list (tuatara.buffer:kill-buffer list))))
    (false (probe-file (here dir "a.txt")))
    (false (probe-file (here dir "b.txt")))))

(defun filter-line (editor)
  "The line saying how the current list is narrowed by search filters, or NIL."
  (find-if (lambda (line) (search "iltered by" line)) (rimu-lines editor)))

(define-test (rimu-operations narrowed-by-filters)
  ;; C-x / s narrows a directory's list to the entries whose lines match, as in every list;
  ;; filters stack, and C-x / r takes them away
  (with-operations-tree (dir)
    (multiple-value-bind (f editor) (ops-session dir "C-x / s t x t RET")
      (declare (ignore f))
      (let ((lines (rimu-lines editor)))
        (true (find-if (lambda (l) (search "a.txt" l)) lines))
        (true (find-if (lambda (l) (search "b.txt" l)) lines))
        (false (find-if (lambda (l) (search "sub" l)) lines))
        (false (find-if (lambda (l) (search "link" l)) lines))
        (true (search "Filtered by txt: 2 of 4 entries" (filter-line editor)))
        ;; the directory's name stays above the entries
        (true (find-if (lambda (l) (search (string-right-trim "/" (namestring dir)) l)) lines))))
    (multiple-value-bind (f editor) (ops-session dir "C-x / s t x t RET C-x / s a RET")
      (declare (ignore f))
      (true (search "Filtered by txt, a: 1 of 4 entries" (filter-line editor)))
      (false (find-if (lambda (l) (search "b.txt" l)) (rimu-lines editor))))
    (multiple-value-bind (f editor) (ops-session dir "C-x / s t x t RET C-x / s a RET C-x / r")
      (true (tuatara.frontend.fake:fake-frame-showing f "2 filters taken away"))
      (false (filter-line editor))
      (true (find-if (lambda (l) (search "link" l)) (rimu-lines editor))))
    ;; each line still acts on the entry it shows
    (multiple-value-bind (f editor) (ops-session dir "C-x / s b RET g f b . t x t RET")
      (declare (ignore f))
      (let ((list (session-buffer editor)))
        (is string= "b.txt"
            (tuatara.io:listed-name (tuatara.defaults::rimu-entry-at list (tuatara.buffer:buffer-point list))))
        (is equal (list (tuatara.io:native (here dir "b.txt"))) (tuatara.defaults::rimu-chosen-paths list))))
    ;; read again, or laid out anew, the list is narrowed again
    (dolist (keys '("g r" "I" "s" "H"))
      (multiple-value-bind (f editor) (ops-session dir (format nil "C-x / s t x t RET ~a" keys))
        (declare (ignore f))
        (true (search "Filtered by txt: " (filter-line editor)) "~a" keys)
        (false (find-if (lambda (l) (search "sub" l)) (rimu-lines editor)))))))

(define-test (rimu-operations filters-by-column)
  ;; with the details shown, C-x / f asks for a column, the name among them, and tries that cell
  ;; alone: ^b finds b.txt by its name, which the whole line, starting with its mark and
  ;; details, never begins with; with the names alone there is no column to ask for
  (with-operations-tree (dir)
    (multiple-value-bind (f editor) (ops-session dir "I C-x / f n a m e RET ^ b RET")
      (let ((lines (rimu-lines editor)) (list (session-buffer editor)))
        (true (tuatara.frontend.fake:fake-frame-showing f "Filter by the column:"))
        (true (find-if (lambda (l) (search "b.txt" l)) lines))
        (false (find-if (lambda (l) (search "a.txt" l)) lines))
        (false (find-if (lambda (l) (search "sub" l)) lines))
        (is equal '("inode" "permissions" "links" "owner" "group" "size" "time" "name")
            (mapcar #'first (tuatara.defaults::list-table-columns list)))))
    (multiple-value-bind (f editor) (ops-session dir "")
      (declare (ignore f))
      (false (tuatara.defaults::list-table-columns (session-buffer editor))))))

(define-test (rimu-operations marks-under-filters)
  ;; marks stay on the entries a filter hides, and the operations take them, their questions
  ;; saying how many the filter hides; a file marked in another directory is not one of them
  (with-operations-tree (dir)
    (multiple-value-bind (f editor) (ops-session dir "g f a . t x t RET m m C-x / s b RET")
      (declare (ignore f))
      (let ((list (session-buffer editor)))
        (false (find-if (lambda (l) (search "a.txt" l)) (rimu-lines editor)))
        (true (find-if (lambda (l) (and (uiop:string-prefix-p "> " l) (search "b.txt" l))) (rimu-lines editor)))
        (is equal (list (tuatara.io:native (here dir "a.txt")) (tuatara.io:native (here dir "b.txt")))
            (tuatara.defaults::rimu-chosen-paths list))))
    (let ((f (ops-session dir "g f a . t x t RET m m C-x / s b RET D n" :width 200)))
      (true (tuatara.frontend.fake:fake-frame-showing f "Move 2 files (1 hidden by the filter) to the trash?")))
    (let ((f (ops-session dir "g f a . t x t RET m m C-x / s b RET C" :width 200)))
      (true (tuatara.frontend.fake:fake-frame-showing f "Copy 2 files (1 hidden by the filter) to:")))
    (true (probe-file (here dir "a.txt")))
    ;; marked while narrowed, the mark is still there once the filter is taken away; t turns only
    ;; the entries shown about
    (multiple-value-bind (f editor) (ops-session dir "C-x / s b RET g f b . t x t RET m C-x / r")
      (declare (ignore f))
      (true (find-if (lambda (l) (and (uiop:string-prefix-p "> " l) (search "b.txt" l))) (rimu-lines editor))))
    (multiple-value-bind (f editor) (ops-session dir "C-x / s t x t RET t C-x / r")
      (declare (ignore f))
      (let ((lines (rimu-lines editor)))
        (true (find-if (lambda (l) (and (uiop:string-prefix-p "> " l) (search "a.txt" l))) lines))
        (true (find-if (lambda (l) (and (uiop:string-prefix-p "  " l) (search "sub" l))) lines))))
    ;; one marked in another directory is not hidden by this list's filter
    (let ((f (ops-session dir "g f a . t x t RET m g f s u b RET RET g f c . t x t RET m C-x / s z z RET C" :width 400)))
      (true (tuatara.frontend.fake:fake-frame-showing f "Copy 2 files (1 hidden by the filter) in 2 directories to:")))))

(define-test (rimu-operations filters-and-directories)
  ;; going into a directory lists it whole, the filters dropped; going up too
  (with-operations-tree (dir)
    (multiple-value-bind (f editor) (ops-session dir "C-x / s s u b RET g f s u b RET RET")
      (declare (ignore f))
      (let ((list (session-buffer editor)))
        (false (tuatara:search-filters list))
        (false (filter-line editor))
        (true (find-if (lambda (l) (search "c.txt" l)) (rimu-lines editor)))))
    (multiple-value-bind (f editor) (ops-session dir "g f s u b RET RET C-x / s z z RET h")
      (declare (ignore f))
      (false (filter-line editor))
      (true (find-if (lambda (l) (search "a.txt" l)) (rimu-lines editor))))
    ;; a file made on disk while the list is narrowed shows when it matches, the filter kept
    (multiple-value-bind (f editor seen)
        (disk-change-session dir (lambda ()
                                   (alexandria:write-string-into-file "x" (here dir "made.dat"))
                                   (alexandria:write-string-into-file "x" (here dir "made.txt")))
                             "made.txt" :before "C-x / s t x t RET")
      (declare (ignore f))
      (true seen)
      (let ((text (tuatara.buffer:buffer-string (tuatara.display:window-buffer (tuatara.command:editor-window editor)))))
        (false (search "made.dat" text))
        (true (search "Filtered by txt: 3 of" text))))))

(define-test (rimu-operations filters-beside-the-preview)
  ;; the preview shows the entry on the line of the narrowed list
  (with-operations-tree (dir)
    (rimu-forget)
    (let ((tuatara-user:*rimu-preview* t) (tuatara-user:*rimu-preview-delay* 0))
      (let ((f (run-keys (list (format nil "C-x d C-a C-k ~a C-j C-x / s b . t x t RET" (rimu-typed (namestring dir)))
                               :tick :tick)
                         :width 120 :height 20)))
        (true (tuatara.frontend.fake:fake-frame-showing f "beta"))
        (false (tuatara.frontend.fake:fake-frame-showing f "alpha"))))
    (rimu-forget)))

(define-test (rimu-operations filters-in-a-safe-list)
  ;; a directory opened safely is narrowed as any list is, and still changes nothing
  (with-operations-tree (dir)
    (rimu-forget)
    (multiple-value-bind (f editor)
        (run-keys (format nil "M-x f i n d - f i l e - s a f e l y RET C-a C-k ~a C-j C-x / s a . t x t RET D"
                          (rimu-typed (namestring dir)))
                  :width 120 :height 20)
      (true (tuatara.io:safe-visit-p (session-buffer editor)))
      (true (search "Filtered by a.txt: 1 of 4 entries" (filter-line editor)))
      (false (find-if (lambda (l) (search "b.txt" l)) (rimu-lines editor)))
      (true (tuatara.frontend.fake:fake-frame-showing f "changes files, and"))
      (true (probe-file (here dir "a.txt"))))
    (rimu-forget)))

(define-test (rimu-operations masters-marked-apart)
  ;; C-c f m marks a master (M) and goes on; the masters marked anywhere and the files marked here
  ;; come apart, a master never among those compared with it
  (with-operations-tree (dir)
    (multiple-value-bind (f editor) (ops-session dir "g f a . t x t RET C-c f m g f b . t x t RET m")
      (declare (ignore f))
      (let ((buffer (session-buffer editor)))
        (true (find-if (lambda (l) (and (uiop:string-prefix-p "M " l) (search "a.txt" l))) (rimu-lines editor)))
        (multiple-value-bind (masters chosen) (tuatara.defaults::rimu-paths-marked-apart #\M
                                                                                         :files-only t
                                                                                         :buffer buffer)
          (is equal (list (tuatara.io:native (here dir "a.txt"))) masters)
          (is equal (list (tuatara.io:native (here dir "b.txt"))) chosen)))))
  (true (tuatara.defaults::fraction-p 1))
  (true (tuatara.defaults::fraction-p 0.8))
  (false (tuatara.defaults::fraction-p 0))
  (false (tuatara.defaults::fraction-p 1.5))
  (false (tuatara.defaults::fraction-p "0.5")))

(define-test (rimu-operations formatted-file-asked-alike)
  ;; A in a list asks what new-file-formatted asks but the subdirectory, and makes the same text
  (with-operations-tree (dir)
    (let ((tuatara-user:*formatted-file-prompts* '(:title :subdirectory :file-type))
          (tuatara-user:*formatted-file-type* :org)
          (tuatara-user:*formatted-file-directory* (namestring dir)))
      (let ((f (ops-session dir "A K e a SPC w a l k RET RET" :width 200)))
        (declare (ignore f))
        (let ((made (find-if (lambda (p) (search "--kea-walk" (file-namestring p))) (uiop:directory-files dir))))
          (true made)
          (when made
            (let ((text (alexandria:read-file-into-string made)))
              (true (uiop:string-prefix-p "#+title: Kea walk" text))
              (is string= text
                  (tuatara.defaults::formatted-file-start-text
                   (tuatara.defaults::formatted-file-type-for ".org") "Kea walk" '()
                   (subseq (file-namestring made) 0 15)
                   (tuatara.defaults::identifier-time (subseq (file-namestring made) 0 15))
                   ""))))))))
  (true (search "SKIP" (documentation 'tuatara.defaults::ask-formatted-file-answers 'function))))
