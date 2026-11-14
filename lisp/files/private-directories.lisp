;;;; lisp/files/private-directories.lisp --- Directories for one session alone, swept when abandoned.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; Copies made for programs outside, exports and views live in directories
;;;; of the cache and state directories made for you alone. Those a session
;;;; holds are kept fresh while it runs; those left by sessions long gone are
;;;; deleted.

(in-package #:tuatara.defaults)

(defun private-directory-in (base name)
  "The directory NAME (ending in /, latex/pictures/ for one within another)
in the directory BASE, for you alone, made with those above it when it is
not there (ENSURE-PRIVATE-DIRECTORY); its native name, ending in /."
  (tuatara.io:ensure-private-directory (tuatara.io:path-join (tuatara.io:native base) name)))

(defvar *cache-directories-made* (make-hash-table :test 'equal :synchronized t)
  "The native name of each directory PRIVATE-CACHE-DIRECTORY made or found,
by the NAME it was asked for.")

(defun private-cache-directory (name)
  "The directory NAME (ending in /) in the cache directory, for you alone, made
when it is not there; its native name, ending in /."
  (setf (gethash name *cache-directories-made*) (private-directory-in (tuatara.io:cache-directory) name)))

(defun remote-copy-p (path)
  "True when PATH is in the directory of the copies of files read from another
machine, which last only as long as their buffers: a name not worth remembering.
Told by name alone, the disk not asked."
  (let ((copies (gethash "remote/" *cache-directories-made*)))
    (and copies (uiop:string-prefix-p copies path))))

(defun private-state-directory (name)
  "The directory NAME (ending in /) in the state directory, which keeps what
lasts from one session to the next, for you alone, made when it is not
there; its native name, ending in /."
  (private-directory-in (tuatara.io:state-directory) name))

(defvar *private-directories-held* (make-hash-table :test 'equal :synchronized t)
  "The private directories this session made and still uses (NEW-PRIVATE-DIRECTORY),
by native name ending in /: FORGET-OLD-COPIES leaves them be, however old,
and KEEP-PRIVATE-DIRECTORIES-FRESH keeps them from looking old to other sessions.")

(defvar *private-directories-touched* 0
  "When KEEP-PRIVATE-DIRECTORIES-FRESH last marked the private directories held as
used, in universal time.")

(defun forget-old-copies (directory)
  "Delete what DIRECTORY holds from more than a day ago, but for the private
directories a session still uses (*PRIVATE-DIRECTORIES-HELD*, or the owner.lock of
MAKE-OWNED-DIRECTORY held): copies whose programs have them open still, or are
long done."
  (let ((day-ago (- (get-universal-time) 86400)))
    (dolist (entry (ignoring-errors-reported ("listing ~a" directory) (tuatara.io:directory-entries directory)))
      (let* ((path (tuatara.io:path-join directory (first entry)))
             (stamp (tuatara.io:path-stamp path :follow nil :failure nil)))
        (when (and stamp (< (mtime-universal-time (tuatara.io:stamp-mtime stamp)) day-ago)
                   (not (gethash (tuatara.io:slash-ended path) *private-directories-held*))
                   (not (tuatara.io:lock-file-held-p (tuatara.io:path-join (tuatara.io:slash-ended path)
                                                                           "owner.lock"))))
          (ignoring-errors-reported ("deleting ~a" path) (tuatara.io:delete-tree path)))))))

(defun new-private-directory (directory &key (hold t))
  "A new directory of its own in DIRECTORY, its name random, for you alone,
made; what DIRECTORY holds from more than a day ago deleted first
\(FORGET-OLD-COPIES). With HOLD (as standard), the directory is kept, however
old, until DELETE-PRIVATE-DIRECTORY deletes it or the session ends; with HOLD
NIL, it goes a day after it was made, as the copies opened in other programs
do."
  (forget-old-copies directory)
  (let ((place (tuatara.io:path-join directory (tuatara.io:slash-ended (tuatara.io:random-name)))))
    (tuatara.io:make-directory place #o700)
    (when hold
      (setf (gethash place *private-directories-held*) t))
    place))

(defun make-owned-directory (name &key (base (tuatara.io:cache-directory)))
  "A new directory of its own in NAME (ending in /, in the directory BASE, by
default the cache directory), its file owner.lock locked from the moment it
is made while the session runs, and then naming this machine, so that no
other session, whatever process numbers it sees, takes it for one left
behind (FORGET-ABANDONED-DIRECTORIES). Returns the directory, a native name ending
in /, and the descriptor of the lock, to close when the directory is deleted."
  (let* ((directory (new-private-directory (private-directory-in base name)))
         (lock (tuatara.io:path-join directory "owner.lock"))
         (fd (tuatara.io:open-lock-file lock)))
    (tuatara.io:write-octets fd (tuatara.text:string-octets (machine-instance)) :path lock)
    (values directory fd)))

(defun forget-abandoned-directories (name &key own (base (tuatara.io:cache-directory)) before)
  "Delete the directories in NAME (in the directory BASE, by default the cache
directory) that sessions on this machine no longer running made with
MAKE-OWNED-DIRECTORY and left (they crashed): each whose owner.lock names this
machine and no session holds, BEFORE, a function of the directory, called
first, when given; and one with no owner.lock, made more than an hour ago.
OWN, a directory of this session, stays. Another machine's, sharing the
directory, are its own to judge."
  (let ((all (private-directory-in base name))
        (hour-ago (- (get-universal-time) 3600)))
    (dolist (entry (ignoring-errors-reported ("listing ~a" all) (tuatara.io:directory-entries all)))
      (let* ((directory (tuatara.io:slash-ended (tuatara.io:path-join all (first entry))))
             (lock (tuatara.io:path-join directory "owner.lock")))
        (unless (equal directory own)
          (if (eq (tuatara.io:path-kind lock :follow nil) :regular)
              (let ((host (ignoring-errors-reported ("reading ~a" lock)
                            (tuatara.text:lenient-string (tuatara.io:file-octets lock :limit 1024))))
                    (fd (ignoring-errors-reported ("opening ~a" lock) (sb-posix:open lock sb-posix:o-rdwr))))
                (when (and fd (equal host (machine-instance)) (tuatara.runtime:lock-fd fd))
                  (when before
                    (ignoring-errors-reported ("ending what ~a held" directory) (funcall before directory)))
                  (ignoring-errors-reported ("deleting ~a" directory) (tuatara.io:delete-tree directory)))
                (when fd (sb-posix:close fd)))
              (let ((stamp (tuatara.io:path-stamp directory :follow nil :failure nil)))
                (when (and stamp (< (mtime-universal-time (tuatara.io:stamp-mtime stamp)) hour-ago))
                  (ignoring-errors-reported ("deleting ~a" directory) (tuatara.io:delete-tree directory))))))))))

(defun delete-private-directory (directory)
  "Delete DIRECTORY, a private directory NEW-PRIVATE-DIRECTORY made, and all in it; it
is held no longer."
  (ignoring-errors-reported ("deleting ~a" directory) (tuatara.io:delete-tree directory))
  (remhash directory *private-directories-held*))

(defun keep-private-directories-fresh (editor)
  "Tick function: every six hours, mark the private directories this session
holds (*PRIVATE-DIRECTORIES-HELD*) as used now, so that another session's sweep
of old copies (FORGET-OLD-COPIES) leaves them be. Returns the seconds until
it is next wanted."
  (declare (ignore editor))
  (let ((due (+ *private-directories-touched* (* 6 3600)))
        (now (get-universal-time)))
    (when (>= now due)
      (setf *private-directories-touched* now)
      (maphash (lambda (directory held)
                 (declare (ignore held))
                 (ignoring-errors-reported ("marking ~a as used" directory) (tuatara.io:touch-path directory)))
               *private-directories-held*))
    (max 60 (- (+ *private-directories-touched* (* 6 3600)) now))))
