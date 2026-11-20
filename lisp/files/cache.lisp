;;;; lisp/files/cache.lisp --- What the cache directory keeps to save time: kept, pinned, cleared once unused, listed.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; Copies of files read from other machines, mailboxes read by readpst and
;;;; the other work kept in the cache directory stay there after the buffer
;;;; showing them is closed, to be used again rather than made again. Each is
;;;; an entry: a directory of its own, made for you alone. One made by a kind
;;;; (DEFINE-CACHE-KIND) is found again by its key: a file's name on its
;;;; server, a mailbox's sum. While a session uses an entry it holds a shared
;;;; lock on the file in-use.lock in it, which every session sees; an entry in
;;;; use is never deleted. One neither in use nor pinned is cleared
;;;; *CACHE-KEEP* after its last use, the moment it stopped being in use. The
;;;; entries of the kinds, the pins and the times of last use are kept between
;;;; sessions in the state directory, in the list "cache"; nothing outside the
;;;; cache directory is ever named there or cleared from it. M-x cache-manager
;;;; lists what the cache holds.

(in-package #:tuatara.defaults)

(define-setting *cache-keep* (* 24 60 60)
  "How long after it was last used an entry of the cache is cleared: a copy of a
file read from another machine, a mailbox read by readpst, the index of a
directory kept read, or other work kept in the cache directory
\(\\[cache-manager]). It is last used when it stops being in use, its buffer or
list closed, its search or reading over, or the session ended; while in use it
is never cleared, and one pinned in \\[cache-manager] is never cleared by
itself. A file on another machine whose copy was cleared is read from there
again when next visited.
- a time, \"12h\", \"3d\", or a number of seconds: cleared that long after its
  last use. 1 day (86400) as standard.
- 0: cleared as soon as it is no longer in use, looked at once a minute; the
  index of a directory kept read is then written again after each search
  that finds it cleared, which for a large directory is much writing."
  :values (seconds 0 *))

;;; The entries kept between sessions

;; An entry of the cache, as the list kept between sessions holds it, is a
;; list (PLACE KIND KEY NAME MADE USED PINNED FACTS), compared whole by EQUAL.

(defun make-cache-entry-record (place kind key name made used pinned facts)
  "An entry of the cache as the list kept between sessions holds it. PLACE is
its directory's path within the cache directory, ending in / (remote/x8Fq2/).
KIND is the kind that made it (DEFINE-CACHE-KIND), \"\" for a directory only
pinned. KEY is what finds it again among those of its kind (FIND-CACHE-ENTRY),
or \"\". NAME is what it is, in words. MADE and USED are when it was made and
last used, in universal time, 0 when not known. PINNED is true when it is never
cleared by itself. FACTS is what its kind noted of it (NOTE-CACHE-ENTRY), or
\"\"."
  (list place kind key name made used (and pinned t) facts))

(defun cache-entry-place (entry)
  "The path of ENTRY's directory within the cache directory (MAKE-CACHE-ENTRY-RECORD)."
  (first entry))

(defun cache-entry-kind (entry)
  "The kind that made ENTRY, \"\" for one only pinned (MAKE-CACHE-ENTRY-RECORD)."
  (second entry))

(defun cache-entry-key (entry)
  "What finds ENTRY again among those of its kind, or \"\" (MAKE-CACHE-ENTRY-RECORD)."
  (third entry))

(defun cache-entry-name (entry)
  "What ENTRY is, in words (MAKE-CACHE-ENTRY-RECORD)."
  (fourth entry))

(defun cache-entry-made (entry)
  "When ENTRY was made, in universal time, 0 when not known (MAKE-CACHE-ENTRY-RECORD)."
  (fifth entry))

(defun cache-entry-used (entry)
  "When ENTRY was last used, in universal time, 0 when not known (MAKE-CACHE-ENTRY-RECORD)."
  (sixth entry))

(defun cache-entry-pinned (entry)
  "True when ENTRY is never cleared by itself (MAKE-CACHE-ENTRY-RECORD)."
  (seventh entry))

(defun cache-entry-facts (entry)
  "What ENTRY's kind noted of it, or \"\" (MAKE-CACHE-ENTRY-RECORD)."
  (eighth entry))

(defparameter *cache-entries-kept* 10000
  "The most entries of the cache the list kept between sessions holds, those
last put there first; one past them is still cleared by the age of its
directory.")

(defun cache-place-p (place)
  "True when PLACE is a path within the cache directory as an entry's PLACE is
written: relative, ending in /, with no empty, . or .. part, so that it can
name nothing outside the cache directory."
  (and (stringp place) (> (length place) 1) (char/= #\/ (char place 0)) (uiop:string-suffix-p place "/")
       (every (lambda (part) (and (plusp (length part)) (not (member part '("." "..") :test #'string=))))
              (butlast (uiop:split-string place :separator "/")))))

(defun parse-cache-entry-line (line)
  "The entry of the cache LINE of the list \"cache\" writes (CACHE-ENTRY-LINE), or NIL
for a line that is none: one whose place could name something outside the
cache directory (CACHE-PLACE-P) is none."
  (let ((fields (mapcar #'unescape-path (state-line-fields line))))
    (when (>= (length fields) 8)
      (destructuring-bind (place kind key name made used pinned facts &rest later) fields
        (declare (ignore later))
        (and (cache-place-p place) (digits-word-p made) (digits-word-p used)
             (make-cache-entry-record place kind key name (parse-integer made) (parse-integer used)
                                      (string= pinned "pinned") facts))))))

(defun cache-entry-line (entry)
  "The line of the list \"cache\" writing ENTRY: its place, kind, key, name,
when made and last used, pinned or not and its facts, parted by tabs."
  (format nil (format nil "~~{~~a~~^~c~~}" #\Tab)
          (mapcar #'escape-path
                  (list (cache-entry-place entry) (cache-entry-kind entry) (cache-entry-key entry)
                        (cache-entry-name entry) (princ-to-string (cache-entry-made entry))
                        (princ-to-string (cache-entry-used entry)) (if (cache-entry-pinned entry) "pinned" "")
                        (cache-entry-facts entry)))))

(define-state-list *cache-entries* "cache" 'parse-cache-entry-line 'cache-entry-line '*cache-entries-kept*
  :header (format nil "# What the cache directory keeps to be used again, a line each. A tab ends~%~
                       # each field: its directory there, its kind, key and name, when it was made~%~
                       # and last used, pinned or not, and what its kind noted of it (\\\\, \\n and~%~
                       # \\r stand for \\, a newline and a return).~%")
  :path-of #'cache-entry-place :fields 8 :line-key (field-line-key 8 0))

(defvar *cache-root-seen* nil
  "The cache directory as CACHE-ROOT last worked it out: (KEY . ROOT), KEY
what it was worked out from, so that it is worked out again only when that
changes.")

(defun cache-root ()
  "The cache directory, a native name ending in /; worked out again only when
*CACHE-DIRECTORY*, $XDG_CACHE_HOME, $HOME or $XDG_RUNTIME_DIR changes, or the
directory stops or starts being kept in memory for want of its drive (looked
at as the cache is looked through, CLEAR-OLD-CACHE), so that asking costs next
to nothing."
  (let ((key (list tuatara-user:*cache-directory* (tuatara.io:own-variable "XDG_CACHE_HOME")
                   (tuatara.io:own-variable "HOME") (tuatara.io:own-variable "XDG_RUNTIME_DIR")
                   (tuatara.io:directory-in-memory-p 'tuatara-user:*cache-directory*)))
        (seen *cache-root-seen*))
    (if (equal key (car seen))
        (cdr seen)
        (let ((root (tuatara.io:slash-ended (tuatara.io:native (tuatara.io:cache-directory)))))
          (setf *cache-root-seen* (cons key root))
          root))))

(defun cache-place (path)
  "The place within the cache directory of PATH, a directory there (ending in
/ or not), as an entry writes it (CACHE-PLACE-P), PATH taken as written or,
when the cache directory is reached through a link, as where it really is;
NIL for anything else."
  (flet ((place-in (root directory)
           (and root directory (uiop:string-prefix-p root directory)
                (let ((place (subseq directory (length root))))
                  (and (cache-place-p place) place)))))
    (let ((root (cache-root)) (directory (tuatara.io:slash-ended (tuatara.io:native path))))
      (or (place-in root directory)
          (let ((real-root (tuatara.io:real-path-or-parent root))
                (real (tuatara.io:real-path-or-parent directory)))
            (and real-root real (string/= real-root root)
                 (place-in (tuatara.io:slash-ended real-root) (tuatara.io:slash-ended real))))))))

(defun cache-place-directory (place)
  "The directory PLACE names within the cache directory, a native name ending
in /; NIL when PLACE could name something outside it."
  (and (cache-place-p place) (concatenate 'string (cache-root) place)))

(defun cache-entry-at (directory)
  "The entry of the cache kept for DIRECTORY, a directory in the cache directory, or NIL."
  (let ((place (cache-place directory)))
    (and place (state-list-find *cache-entries* place))))

(defun changed-cache-entry (entry &key (kind (cache-entry-kind entry)) (key (cache-entry-key entry))
                                       (name (cache-entry-name entry)) (used (cache-entry-used entry))
                                       (pinned (cache-entry-pinned entry)) (facts (cache-entry-facts entry)))
  "ENTRY, an entry of the cache, with KIND, KEY, NAME, USED, PINNED or FACTS changed, a new list."
  (make-cache-entry-record (cache-entry-place entry) kind key name (cache-entry-made entry) used pinned facts))

(defun put-cache-entry (entry)
  "Keep ENTRY in the list kept between sessions, in place of the one of its place."
  (state-list-put *cache-entries* (list entry)))

;;; Kinds

(define-record (cache-kind (:constructor make-cache-kind (name directory title open untrusted)) (:copier nil))
  "A kind of entry of the cache (DEFINE-CACHE-KIND): NAME, a string; DIRECTORY,
where in the cache directory its entries are made; TITLE, what \\[cache-manager]
calls them; OPEN, a function of an entry's directory, key and facts opening it,
or NIL; UNTRUSTED, true when its files came from outside."
  (name "" :type string :read-only t :documentation "The name of KIND.")
  (directory "" :type string :read-only t :documentation "Where in the cache directory KIND's entries are made.")
  (title "" :type string :read-only t :documentation "What the list of the cache calls KIND's entries.")
  (open nil :read-only t :documentation "The function opening an entry of KIND, or NIL.")
  (untrusted t :read-only t :documentation "True when the files of KIND's entries came from outside."))

(defvar *cache-kinds* (make-hash-table :test 'equal :synchronized t)
  "The kinds of entry of the cache (DEFINE-CACHE-KIND), by name.")

(defun define-cache-kind (kind directory title &key open (untrusted t))
  "Make KIND, a string (\"mailbox\"), a kind of entry of the cache: its entries
are made in DIRECTORY of the cache directory (\"mailboxes/\", ending in /), are
listed by \\[cache-manager] under TITLE (\"Mailboxes read by readpst\"), and are
opened from there by OPEN, a function of the entry's directory, its key and
the facts noted of it (NOTE-CACHE-ENTRY), when given. With UNTRUSTED, true as
standard, what its entries hold came from outside, read or written by a
program taking an outside file apart, and is untrusted (UNTRUSTED-PATH-P):
it opens safely and its project runs nothing of its own. UNTRUSTED NIL is for
a kind holding only what Tuatara made itself of trusted files. A package
defines its kinds as it loads; defining a kind again replaces it."
  (unless (cache-place-p directory)
    (error "~s is no directory within the cache directory" directory))
  (setf (gethash kind *cache-kinds*) (make-cache-kind kind directory title open (and untrusted t)))
  kind)

(defun untrusted-cache-kind-directories ()
  "The directories within the cache directory (\"mailboxes/\") of the kinds of
entry of the cache defined untrusted (DEFINE-CACHE-KIND)."
  (let ((directories '()))
    (sb-ext:with-locked-hash-table (*cache-kinds*)
      (maphash (lambda (name kind)
                 (declare (ignore name))
                 (when (cache-kind-untrusted kind)
                   (pushnew (cache-kind-directory kind) directories :test #'string=)))
               *cache-kinds*))
    directories))

;;; In use

(defvar *cache-uses* (make-hash-table :test 'equal :synchronized t)
  "The entries of the cache this session uses, by directory: each (FD . USERS),
FD the descriptor holding a shared lock on the entry's in-use.lock, USERS what
uses it, buffers as a rule.")

(defun lock-still-named-p (fd path)
  "True when the file open as file descriptor (FD) is still the one at PATH,
not a link: the same inode on the same device, and not taken away."
  (let ((open (sb-posix:fstat fd))
        (named (handler-case (sb-posix:lstat path) (sb-posix:syscall-error () nil))))
    (and named (plusp (sb-posix:stat-nlink open))
         (= (sb-posix:stat-ino open) (sb-posix:stat-ino named))
         (= (sb-posix:stat-dev open) (sb-posix:stat-dev named)))))

(defun use-cache-directory (directory user)
  "Note that USER, a buffer as a rule, uses DIRECTORY, an entry of the cache:
not cleared, by any session, until USER is done with it
\(STOP-USING-CACHE-DIRECTORY, called when a buffer is killed). True when it
may be used; NIL when it is not there, is no longer kept, or another session is
clearing it (CLEAR-CACHE-PATH): the lock it took is then let go."
  (flet ((take-up (fd)
           ;; USER joins those using DIRECTORY already: :JOINED; else with FD, its lock, the first: :TAKEN
           (sb-ext:with-locked-hash-table (*cache-uses*)
             (let ((use (gethash directory *cache-uses*)))
               (cond (use (pushnew user (cdr use)) :joined)
                     (fd (setf (gethash directory *cache-uses*) (list fd user)) :taken))))))
    (cond ((take-up nil) t)
          ((not (eq :directory (tuatara.io:path-kind directory :follow nil))) nil)
          (t
           ;; the disk and the list of the cache read with the table free, so that no other thread waits on them
           (let* ((lock (tuatara.io:path-join directory "in-use.lock"))
                  (fd (ignoring-errors-reported ("using ~a" directory)
                        (tuatara.io:open-lock-file lock :shared t))))
             (cond ((null fd) nil)
                   ;; a clearer moves the entry aside under its lock before deleting it
                   ((and (lock-still-named-p fd lock) (cache-entry-at directory))
                    ;; another thread took it up meanwhile: its lock serves, this one goes
                    (when (eq (take-up fd) :joined)
                      (tuatara.runtime:close-fd-quietly fd "~a" directory))
                    t)
                   (t (tuatara.runtime:close-fd-quietly fd "~a" directory)
                      nil)))))))

(defun note-cache-use (directory now &optional (quiet 0))
  "Note NOW, a universal time, as when DIRECTORY, an entry of the cache, was
last used; left as it is when the last use kept is less than QUIET seconds
before NOW."
  (let ((entry (cache-entry-at directory)))
    (when (and entry (or (zerop quiet) (>= (- now (cache-entry-used entry)) quiet)))
      (put-cache-entry (changed-cache-entry entry :used now)))))

(defun stop-using-cache-directory (directory user &key (now (get-universal-time)) (quiet 0))
  "USER no longer uses DIRECTORY, an entry of the cache: once nothing of this
session uses it, its lock is let go and NOW, a universal time, noted as its
last use, from which *CACHE-KEEP* counts, unless the last use kept is less
than QUIET seconds before NOW."
  (let ((fd nil))
    (sb-ext:with-locked-hash-table (*cache-uses*)
      (let ((use (gethash directory *cache-uses*)))
        (when (and use (member user (cdr use)))
          (setf (cdr use) (remove user (cdr use)))
          (unless (cdr use)
            (setf fd (car use))
            (remhash directory *cache-uses*)))))
    (when fd
      (tuatara.runtime:close-fd-quietly fd "~a" directory)
      (note-cache-use directory now quiet))))

(defun call-with-cache-directory-used (directory function)
  "Call FUNCTION, from any thread, with or without a buffer, while DIRECTORY,
an entry of the cache, is held in use for it: no session clears it meanwhile
\(CLEAR-CACHE-PATH). FUNCTION is given T then, or NIL when DIRECTORY could not
be held (it is gone, no longer kept, or another session is clearing it), and
must then not read it. Once FUNCTION returns, DIRECTORY is let go when nothing
else of this session uses it, and noted as last used now, unless it was so
noted less than a minute before, so that frequent use does not write the list
of the cache each time. Returns what FUNCTION returns."
  (let ((user (list :thread)) (used nil))
    ;; no interrupt between taking the use and the cleanup that lets it go
    (sb-sys:without-interrupts
      (unwind-protect
           (progn (setf used (use-cache-directory directory user))
                  (sb-sys:with-local-interrupts (funcall function used)))
        (when used
          (stop-using-cache-directory directory user :quiet 60))))))

(defun cache-directories-used-by (user)
  "The entries of the cache USER uses, their directories."
  (let ((directories '()))
    (maphash (lambda (directory use) (when (member user (cdr use)) (push directory directories))) *cache-uses*)
    directories))

(defun cache-entry-heir (user directory)
  "Another live buffer than USER visiting a file in DIRECTORY, an entry of the
cache, however the path to it is written (CACHE-PLACE); NIL when there is none."
  (let ((place (cache-place directory)))
    (and place
         (find-if (lambda (other)
                    (let* ((file (and (not (eq other user)) (tuatara.buffer:buffer-file other)))
                           (within (and file (cache-place (tuatara.io:parent-path (tuatara.io:native file))))))
                      (and within (uiop:string-prefix-p place within))))
                  tuatara.buffer:*buffers*))))

(defun stop-using-cache (user)
  "Kill-buffer function: USER, a buffer killed, uses no entry of the cache any
more, each noted as last used now; one in which another buffer visits a file
\(CACHE-ENTRY-HEIR) is that buffer's from then on."
  (dolist (directory (cache-directories-used-by user))
    (let ((heir (cache-entry-heir user directory)))
      (when heir
        (use-cache-directory directory heir)))
    (stop-using-cache-directory directory user)))

(defun pass-cache-uses (old new)
  "Have NEW use the entries of the cache OLD uses, in its place: a buffer
taking another's place before it is killed."
  (dolist (directory (cache-directories-used-by old))
    (use-cache-directory directory new)
    (stop-using-cache-directory directory old)))

(defun touch-cache-uses ()
  "Note every entry of the cache this session uses as last used now, so that
one a session that ended badly used is cleared *CACHE-KEEP* after it last
used it, not after it began."
  (let ((directories '()))
    (maphash (lambda (directory use) (declare (ignore use)) (push directory directories)) *cache-uses*)
    (let ((now (get-universal-time)))
      (dolist (directory directories)
        (note-cache-use directory now)))))

(defun private-directories-touch-seconds ()
  "How many seconds apart the private directories this session holds are
marked as used (KEEP-PRIVATE-DIRECTORIES-FRESH): a third of *CACHE-KEEP*, at
least a minute and at most six hours."
  (max 60 (min (* 6 3600) (floor *cache-keep* 3))))

(defun keep-private-directories-fresh (editor)
  "Tick function: every PRIVATE-DIRECTORIES-TOUCH-SECONDS, mark the private
directories this session holds (*PRIVATE-DIRECTORIES-HELD*) as used now, so
that another session's sweep of old copies (FORGET-OLD-COPIES) leaves them be,
and the entries of the cache it uses as last used now (TOUCH-CACHE-USES).
Returns the seconds until it is next wanted."
  (declare (ignore editor))
  (let* ((every (private-directories-touch-seconds))
         (due (+ *private-directories-touched* every))
         (now (get-universal-time)))
    (when (>= now due)
      (setf *private-directories-touched* now)
      (touch-cache-uses)
      (maphash (lambda (directory held)
                 (declare (ignore held))
                 (ignoring-errors-reported ("marking ~a as used" directory) (tuatara.io:touch-path directory)))
               *private-directories-held*))
    (max 60 (- (+ *private-directories-touched* every) now))))

(defun cache-at-exit (editor)
  "As the editor stops: every entry of the cache it uses is used no longer, each
noted as last used now."
  (declare (ignore editor))
  (let ((users '()))
    (maphash (lambda (directory use) (declare (ignore directory)) (setf users (union users (cdr use)))) *cache-uses*)
    (dolist (user users) (stop-using-cache user))))

(defun cache-directory-in-use-p (directory)
  "True when DIRECTORY, a directory of private work (ending in /), is in use:
by an entry's user in any session (its in-use.lock held), by this session's
private directories (*PRIVATE-DIRECTORIES-HELD*), by a buffer visiting a file
in it, or by a session holding it as its own (MAKE-OWNED-DIRECTORY)."
  (or (and (gethash directory *cache-uses*) t)
      (and (gethash directory *private-directories-held*) t)
      (some (lambda (buffer)
              (let ((file (tuatara.buffer:buffer-file buffer)))
                (and file (uiop:string-prefix-p directory (tuatara.io:native file)))))
            tuatara.buffer:*buffers*)
      (tuatara.io:lock-file-held-p (tuatara.io:path-join directory "owner.lock"))
      (tuatara.io:lock-file-held-p (tuatara.io:path-join directory "in-use.lock") :shared t)))

;;; Finding, making and noting entries

(defun find-cache-entry (kind key &key user)
  "The directory of the entry of the cache of KIND that KEY finds, kept from
an earlier use in this session or another, and the facts noted of it
\(NOTE-CACHE-ENTRY), two values; NIL when there is none or its directory is
gone. With USER, a buffer as a rule, the entry is used by USER from then on,
not cleared until USER is killed, its last use then; NIL when it cannot be
\(another session is clearing it)."
  (let ((entry (find-if (lambda (entry) (and (string= kind (cache-entry-kind entry))
                                             (string= key (cache-entry-key entry))))
                        (state-list-entries *cache-entries*))))
    (when entry
      (let ((directory (cache-place-directory (cache-entry-place entry)))
            (known (gethash kind *cache-kinds*)))
        (when known
          ;; the kind's directory known by name from now on (REMOTE-COPY-P)
          (private-cache-directory (cache-kind-directory known)))
        (when (and directory (eq :directory (tuatara.io:path-kind directory :follow nil))
                   (or (null user) (use-cache-directory directory user)))
          (values directory (cache-entry-facts entry)))))))

(defun forget-cache-key (kind key)
  "Clear the entries of the cache of KIND that KEY finds: each deleted when not
in use, else left to be cleared by its age, no longer found by KEY."
  (dolist (entry (state-list-entries *cache-entries*))
    (when (and (string= kind (cache-entry-kind entry)) (string= key (cache-entry-key entry)))
      (let ((directory (cache-place-directory (cache-entry-place entry))))
        (unless (and directory (clear-cache-path directory))
          (put-cache-entry (changed-cache-entry entry :key "")))))))

(defun make-cache-entry (kind key name &key user)
  "A new entry of the cache of KIND, found again by KEY (FIND-CACHE-ENTRY) and
called NAME in \\[cache-manager]: its directory, new, empty, for you alone, made
in the directory of KIND (DEFINE-CACHE-KIND), a native name ending in /. An
entry KEY found before is cleared, or, while in use, found by KEY no more.
With USER, a buffer as a rule, the entry is used by USER from then on, not
cleared until USER is killed, its last use then."
  (let* ((known (or (gethash kind *cache-kinds*) (error "~s is no kind of entry of the cache" kind)))
         (directory (new-private-directory (private-cache-directory (cache-kind-directory known)) :hold nil))
         (now (get-universal-time)))
    (forget-cache-key kind key)
    (put-cache-entry (make-cache-entry-record (cache-place directory) kind key name now now nil ""))
    (when user
      (use-cache-directory directory user))
    directory))

(defun keep-cache-directory (directory kind key name)
  "Keep DIRECTORY, a directory of the cache directory made under a name of
its own rather than by MAKE-CACHE-ENTRY, as an entry of KIND found by KEY and
called NAME in \\[cache-manager], when it is not one already: from then on it
can be held in use (USE-CACHE-DIRECTORY) and is cleared by the rule of the
cache, *CACHE-KEEP* after its last use. True when it is an entry; NIL, nothing
kept, for a directory outside the cache directory."
  (let ((place (cache-place directory)))
    (when place
      (unless (state-list-find *cache-entries* place)
        (let ((now (get-universal-time)))
          (put-cache-entry (make-cache-entry-record place kind key name now now nil ""))))
      t)))

(defun note-cache-entry (directory &key facts name)
  "Note of DIRECTORY, an entry of the cache made by MAKE-CACHE-ENTRY, FACTS, a
string its kind reads back from FIND-CACHE-ENTRY (\"listed\", the sizes of what
was read), and NAME, what \\[cache-manager] calls it, those given; kept between
sessions."
  (let ((entry (cache-entry-at directory)))
    (when entry
      (put-cache-entry (changed-cache-entry entry :facts (or facts (cache-entry-facts entry))
                                                  :name (or name (cache-entry-name entry)))))))

(defun pin-cache-directory (directory pinned)
  "Pin DIRECTORY, a directory in the cache directory, so that it is never
cleared by itself, or with PINNED NIL let it be cleared again once unused for
*CACHE-KEEP*; kept between sessions. NIL, nothing done, for a directory not in
the cache directory."
  (let ((place (cache-place directory)))
    (when place
      (let ((entry (state-list-find *cache-entries* place)))
        (cond (entry
               (if (and (not pinned) (string= "" (cache-entry-kind entry)))
                   (state-list-drop *cache-entries* (list place))
                   (put-cache-entry (changed-cache-entry entry :pinned pinned))))
              (pinned
               (put-cache-entry (make-cache-entry-record place "" "" "" 0 (get-universal-time) t ""))))
        t))))

;;; Clearing

(defun cache-clears-at (entry stamp)
  "When the directory whose STAMP is given, kept as ENTRY (or NIL when not
kept), is cleared once it is not in use: *CACHE-KEEP* after its last use, the
later of ENTRY's and its STAMP's time; NIL when ENTRY is pinned."
  (unless (and entry (cache-entry-pinned entry))
    (+ (max (if entry (cache-entry-used entry) 0)
            (if stamp (mtime-universal-time (tuatara.io:stamp-mtime stamp)) 0))
       *cache-keep*)))

(defun clear-cache-path (path)
  "Delete PATH, a directory (ending in /) or a file in the cache directory, and
all in it, and forget the entry it was: true when it is gone, else NIL and why,
in words. Nothing is deleted outside the cache directory, nor through a link: a
link there is taken away itself. A directory in use (CACHE-DIRECTORY-IN-USE-P)
is not deleted, nor one whose in-use.lock another session takes first; a file
a buffer visits is not deleted."
  (let* ((native (tuatara.io:native path))
         (directory-p (uiop:string-suffix-p native "/"))
         (bare (string-right-trim "/" native))
         (name (tuatara.io:native-file-name bare))
         (place (if directory-p
                    (cache-place native)
                    (let ((parent (cache-place (tuatara.io:parent-path bare))))
                      (and parent (not (member name '("" "." "..") :test #'string=))
                           (concatenate 'string parent name)))))
         (kind (and place (tuatara.io:path-kind bare :follow nil))))
    (cond ((null place) (values nil "it is not in the cache directory"))
          ((null kind) (when directory-p (state-list-drop *cache-entries* (list place))) t)
          ((not (tuatara.io:really-within-p (tuatara.io:parent-path bare) (cache-root)))
           (values nil "it leads out of the cache directory"))
          ((not (eq kind :directory))
           (if (find bare tuatara.buffer:*buffers*
                     :key (lambda (buffer) (let ((file (tuatara.buffer:buffer-file buffer)))
                                             (and file (tuatara.io:native file))))
                     :test #'equal)
               (values nil "a buffer visits it")
               (progn (tuatara.io:delete-tree bare) t)))
          ((not directory-p) (values nil "it is a directory"))
          ((or (gethash native *cache-uses*) (gethash native *private-directories-held*)
               (cache-directory-in-use-p native))
           (values nil "it is in use"))
          (t
           (let ((fd (ignoring-errors-reported ("clearing ~a" native)
                       (tuatara.io:open-lock-file (tuatara.io:path-join native "in-use.lock")))))
             (if (null fd)
                 (values nil "it is in use")
                 (unwind-protect
                      ;; moved aside first, so that no session finds it half deleted
                      (let ((gone (concatenate 'string (tuatara.io:parent-path bare) ".gone-"
                                               (tuatara.io:random-name))))
                        (tuatara.io:rename-path bare gone)
                        (state-list-drop *cache-entries* (list place))
                        (tuatara.io:delete-tree gone)
                        t)
                   (tuatara.runtime:close-fd-quietly fd "~a" native))))))))

(defun forget-old-copies (directory &key (now (get-universal-time)))
  "Delete what DIRECTORY holds that was last used more than *CACHE-KEEP*
before NOW: each directory and file in it not pinned (\\[cache-manager]), of the
directories one not in use (CACHE-DIRECTORY-IN-USE-P), its last use the later
of its time and the one kept for it as an entry of the cache. In the cache
directory, as CLEAR-CACHE-PATH clears; a directory elsewhere is deleted when
not in use."
  (let ((in-cache (cache-place directory)))
    (dolist (entry (ignoring-errors-reported ("listing ~a" directory) (tuatara.io:directory-entries directory)))
      (let* ((path (tuatara.io:path-join directory (first entry)))
             (stamp (tuatara.io:path-stamp path :follow nil :failure nil))
             (directory-p (and stamp (eq (tuatara.io:stamp-kind stamp) :directory)))
             (whole (if directory-p (tuatara.io:slash-ended path) path))
             (clears (and stamp (cache-clears-at (and directory-p in-cache (cache-entry-at whole)) stamp))))
        (when (and clears (< clears now))
          (if in-cache
              (ignoring-errors-reported ("clearing ~a" whole) (clear-cache-path whole))
              (unless (and directory-p (cache-directory-in-use-p whole))
                (ignoring-errors-reported ("deleting ~a" path) (tuatara.io:delete-tree path)))))))))

(defvar *old-copies-swept* (make-hash-table :test 'equal :synchronized t)
  "The directories of private directories this session made some in
\(SWEEP-OLD-COPIES), each swept of what is old as the cache is looked through
\(CLEAR-OLD-CACHE), by name; the value, when it was first swept, in universal time.")

(defun sweep-old-copies (directory)
  "Delete what DIRECTORY holds that was last used more than *CACHE-KEEP* ago
\(FORGET-OLD-COPIES) on a thread of its own, the first time it is asked in a
session, so that making a directory in it never waits on the sweep; from then
on whenever the cache is looked through (CLEAR-OLD-CACHE)."
  (unless (gethash directory *old-copies-swept*)
    (setf (gethash directory *old-copies-swept*) (get-universal-time))
    (tuatara.runtime:make-guarded-thread
     (tuatara.runtime:carry-settings '(*cache-keep* *cache-directory*) (lambda () (forget-old-copies directory)))
     :name "tuatara old copies")))

(defun clear-old-cache (&key (now (get-universal-time)))
  "Clear from the cache what was last used more than *CACHE-KEEP* before NOW:
in the directories of private directories this session made some in
\(FORGET-OLD-COPIES), and every entry kept between sessions that is neither
pinned nor in use; an entry whose directory is gone is forgotten. The cache
directory is looked for first, so that a drive lost or found meanwhile is
noticed (CACHE-ROOT)."
  (ignoring-errors-reported ("looking for the cache directory") (tuatara.io:cache-directory))
  (let ((directories '()))
    (maphash (lambda (directory when) (declare (ignore when)) (push directory directories)) *old-copies-swept*)
    (dolist (directory directories)
      (forget-old-copies directory :now now)))
  (dolist (entry (state-list-entries *cache-entries*))
    (let* ((directory (cache-place-directory (cache-entry-place entry)))
           (stamp (and directory (tuatara.io:path-stamp directory :follow nil :failure nil))))
      (cond ((null stamp) (state-list-drop *cache-entries* (list (cache-entry-place entry))))
            ((eq (tuatara.io:stamp-kind stamp) :directory)
             (let ((clears (cache-clears-at entry stamp)))
               (when (and clears (< clears now))
                 (ignoring-errors-reported ("clearing ~a" directory) (clear-cache-path directory)))))))))

(defvar *cache-looked-through* nil
  "When the cache was last looked through for what to clear (CLEAR-OLD-CACHE),
in universal time; NIL before the session's first look.")

(defvar *cache-clearing* nil
  "The thread looking through the cache for what to clear, or NIL.")

(defun cache-look-seconds ()
  "How many seconds apart the cache is looked through for what to clear: a
quarter of *CACHE-KEEP*, at least a minute and at most an hour."
  (max 60 (min 3600 (floor *cache-keep* 4))))

(defun clear-old-cache-tick (editor)
  "Tick function: look through the cache for what to clear (CLEAR-OLD-CACHE)
on a thread of its own every CACHE-LOOK-SECONDS, the first a minute after the
session began. Returns the seconds until it is next wanted."
  (declare (ignore editor))
  (let ((now (get-universal-time)) (every (cache-look-seconds)))
    (unless *cache-looked-through*
      (setf *cache-looked-through* (- (+ now 60) every)))
    (when (and (>= now (+ *cache-looked-through* every))
               (not (and *cache-clearing* (sb-thread:thread-alive-p *cache-clearing*))))
      (setf *cache-looked-through* now
            *cache-clearing* (tuatara.runtime:make-guarded-thread
                              (tuatara.runtime:carry-settings '(*cache-keep* *cache-directory*)
                                                              (lambda () (clear-old-cache)))
                              :name "tuatara cache cleared")))
    (max 1 (- (+ *cache-looked-through* every) now))))

;;; The list of the cache

(defparameter *cache-directory-titles*
  '(("indexes/" . "Search indexes") ("formulas/" . "Pictures of formulas") ("latex/" . "LaTeX exports")
    ("opened/" . "Copies opened in other programs") ("git/" . "Commits shown from git")
    ("git-shadow/" . "Working copies of repositories looked at only")
    ("sandbox-copies/" . "Copies for programs shut in") ("latex-texmf-var/" . "TeX's own fonts and formats")
    ("bare-state/" . "State of sessions started bare") ("mail-pages/" . "Pages of mail drawn"))
  "What the list of the cache calls the directories of the cache directory that
no kind of entry names (DEFINE-CACHE-KIND), by name ending in /; another is
called by its name.")

(defparameter *cache-left-out* '("packages/")
  "The directories of the cache directory the list of the cache leaves out:
the packages compiled, which loading them keeps.")

(defun cache-group-title (top)
  "What the list of the cache calls the directory TOP (ending in /) of the cache directory."
  (or (loop for kind being the hash-values of *cache-kinds*
            when (string= top (cache-kind-directory kind)) return (cache-kind-title kind))
      (cdr (assoc top *cache-directory-titles* :test #'string=))
      (string-right-trim "/" top)))

(defun cache-kind-of-directory (top)
  "The kind of entry of the cache whose entries are made in TOP, or NIL."
  (loop for kind being the hash-values of *cache-kinds*
        when (string= top (cache-kind-directory kind)) return kind))

(defun cache-item (path group entry stamp now &key swept wanted)
  "A line of the list of the cache, a plist, for PATH, a directory (ending in
/) or a file of the directory GROUP's title, kept as ENTRY or not, whose STAMP
is given: its size summed (TUATARA.IO:TREE-SIZE) while WANTED says so, when it
is cleared (SWEPT, true when its directory is looked through for what is old;
:NEVER otherwise, NIL when pinned), and whether it is in use, NOW the time."
  (declare (ignore now))
  (let ((directory-p (uiop:string-suffix-p path "/")))
    (list :path path :group group :entry entry
          :name (if (and entry (plusp (length (cache-entry-name entry))))
                    (cache-entry-name entry)
                    (tuatara.io:native-file-name (string-right-trim "/" path)))
          :size (if directory-p (tuatara.io:tree-size path :wanted wanted) (tuatara.io:stamp-size stamp))
          :made (if (and entry (plusp (cache-entry-made entry))) (cache-entry-made entry) nil)
          :used (max (if entry (cache-entry-used entry) 0) (mtime-universal-time (tuatara.io:stamp-mtime stamp)))
          :clears (cond ((and entry (cache-entry-pinned entry)) nil)
                        ((or swept (and entry (plusp (length (cache-entry-kind entry))))) (cache-clears-at entry stamp))
                        (t :never))
          :in-use (and directory-p (cache-directory-in-use-p path)))))

(defun cache-listing (&key wanted)
  "What the list of the cache shows, in sections, each (TITLE . ITEMS) as
CACHE-ITEM makes them: the entries of each kind (DEFINE-CACHE-KIND) first,
then what each other directory of the cache directory holds but those of
*CACHE-LEFT-OUT*; the files directly in the cache directory (the logs) are
left out. WANTED, a function of none, is asked as sizes are summed whether
they are still wanted. Reads the disk: for a thread of its own."
  (let* ((root (cache-root))
         (now (get-universal-time))
         (kept (make-hash-table :test 'equal))
         (swept (let ((places '()))
                  (maphash (lambda (directory when) (declare (ignore when))
                             (let ((place (cache-place directory))) (when place (push place places))))
                           *old-copies-swept*)
                  places))
         (sections '()))
    (dolist (entry (state-list-entries *cache-entries*))
      (setf (gethash (cache-entry-place entry) kept) entry))
    (dolist (top (ignoring-errors-reported ("listing ~a" root) (tuatara.io:directory-entries root)))
      (let ((name (concatenate 'string (first top) "/")))
        (when (and (eq (fourth top) :directory) (not (member name *cache-left-out* :test #'string=)))
          (let* ((kind (cache-kind-of-directory name))
                 (title (cache-group-title name))
                 (items '()))
            (dolist (child (ignoring-errors-reported ("listing ~a" name)
                             (tuatara.io:directory-entries (concatenate 'string root name) :stamps t)))
              (let* ((stamp (fifth child))
                     (directory-p (eq (fourth child) :directory))
                     (place (concatenate 'string name (first child) (if directory-p "/" "")))
                     (entry (and directory-p (gethash place kept))))
                (when (and stamp (member (fourth child) '(:directory :regular))
                           (not (member (first child) '("in-use.lock" "writing.lock") :test #'string=))
                           (not (uiop:string-prefix-p ".gone-" (first child)))
                           (or (null kind) (and entry (string= (cache-entry-kind entry) (cache-kind-name kind)))))
                  (push (cache-item (concatenate 'string root place) title entry stamp now
                                    :swept (or kind (some (lambda (s) (uiop:string-prefix-p s place)) swept))
                                    :wanted wanted)
                        items))))
            (when items
              (push (list* (if kind 0 1) title (sort items #'> :key (lambda (item) (getf item :used)))) sections))))))
    (mapcar #'cdr (stable-sort (sort sections #'string< :key #'second) #'< :key #'first))))

(define-major-mode cache-list-mode sectioned-list-mode
  "What the cache directory keeps to save time (\\[cache-manager]), in sections by
kind: copies of files read from other machines, mailboxes read by readpst,
search indexes and the rest, a line each: what it is, its size, when it was
made and last used, whether it is pinned or when it is cleared
\(*CACHE-KEEP* after its last use), and whether it is in use.
\\<cache-list-mode>\\[list-visit] opens the entry on the line: a remote file
visited as it would be, its kept copy used when the file there is unchanged; a
mailbox listed again. \\[cache-toggle-pin] pins the marked entries, or the one
on the line, or unpins them. \\[list-mark-for-deletion] marks an entry for
deletion, \\[list-execute] deletes those marked, after asking, never one in
use; \\[list-delete-marked] deletes the marked ones or the one on the line.
\\[revert-buffer] lists it again, \\[quit-window] closes."
  :lighter "Cache" :interactive nil)

(defparameter *cache-columns*
  '((" " :width 1) ("Name" :width 44) ("Size" :width 9 :right t :elide nil) ("Made" :width 16)
    ("Last used" :width 16) ("Kept" :width 22) ("Use"))
  "The columns of the list of the cache, as TABLE-HEADING-PIECES takes them: the
mark, what an entry is, its size, when it was made and last used, pinned or when
it is cleared, and whether it is in use.")

(defun cache-kept-words (item)
  "What the list of the cache says of how long ITEM is kept: pinned, the time
it is cleared once unused, or that it is not cleared by itself."
  (let ((clears (getf item :clears)))
    (cond ((null clears) "pinned")
          ((eq clears :never) "not cleared by itself")
          ((getf item :in-use) "while in use")
          (t (format nil "until ~a" (short-time-words clears))))))

(defun cache-list-show (buffer &optional (point 2))
  "Fill BUFFER, the list of the cache, point on line POINT, or with :keep on
the line it was on, marks kept on the entries still there. What the cache
holds and the sizes are read on a thread of their own (CACHE-LISTING), and
BUFFER filled once they are; an empty list says meanwhile that it is being
read. Of listings that overlap, only the one begun last fills BUFFER, and an
older one stops summing sizes."
  (when (zerop (tuatara.buffer:buffer-size buffer))
    (render-pieces buffer (list (format nil "Reading what the cache holds...~%")) :point :start))
  (let ((listing (setf (tuatara.buffer:buffer-local buffer 'cache-listing)
                       (1+ (or (tuatara.buffer:buffer-local buffer 'cache-listing) 0)))))
    (flet ((wanted ()
             (and (live-buffer-p buffer) (eql listing (tuatara.buffer:buffer-local buffer 'cache-listing)))))
      (tuatara.runtime:make-guarded-thread
       (tuatara.runtime:carry-settings
        '(*cache-keep* *cache-directory*)
        (lambda ()
          (let ((sections (ignoring-errors-reported ("listing the cache") (cache-listing :wanted #'wanted))))
            (defer-action (lambda () (when (wanted) (cache-list-fill buffer sections point)))))))
       :name "tuatara cache listed"))))

(defun cache-list-fill (buffer sections point)
  "Fill BUFFER, the list of the cache, with SECTIONS (CACHE-LISTING), point as
CACHE-LIST-SHOW says."
  (let ((items (make-hash-table :test 'equal)) (total 0))
    (loop for (nil . members) in sections
          do (dolist (item members)
               (setf (gethash (getf item :path) items) item)
               (incf total (or (getf item :size) 0))))
    (setf (tuatara.buffer:buffer-local buffer 'cache-items) items)
    (render-table-list
     buffer *cache-columns* sections
     (lambda (item mark section)
       (declare (ignore section))
       (progn
         (list (list-mark-sign mark)
               (cons (getf item :name) (list-mark-face mark))
               (cons (if (getf item :size) (octets-words (getf item :size)) "") :list-size)
               (cons (or (short-time-words (getf item :made)) "") :list-date)
               (cons (or (short-time-words (getf item :used)) "") :list-date)
               (cons (cache-kept-words item) (and (null (getf item :clears)) :list-marked))
               (cons (if (getf item :in-use) "in use" "") :list-note))))
     :sections t :section-title #'identity
     :marked-by (lambda (item section) (declare (ignore section)) (getf item :path))
     :before (list (cons (format nil "The cache, ~a: ~a~%" (abbreviated-path (cache-root)) (octets-words total))
                         :heading)
                   (format nil "Unused entries are cleared ~a after their last use (*cache-keep*); ~
                                pinned ones are kept.~%~%"
                           (duration-words *cache-keep*)))
     :point point
     :refill (lambda (buffer) (cache-list-show buffer :keep)))))

(define-command cache-manager ()
  "List what the cache directory keeps to save time, by kind: copies of files
read from other machines, mailboxes read by readpst, search indexes, pictures
of formulas and the rest (the logs and the compiled packages left out), with
their sizes, when they were made and last used, whether they are pinned or when
they are cleared (*CACHE-KEEP*), and whether they are in use. There
\\<cache-list-mode>\\[list-visit] opens an entry, \\[cache-toggle-pin] pins or
unpins it, \\[list-mark-for-deletion] and \\[list-execute] delete."
  (let ((buffer (buffer-in-mode "*Cache*" 'cache-list-mode)))
    (cache-list-show buffer)
    (show-buffer buffer)))

(defun cache-items-of (list keys)
  "The items of LIST, the list of the cache, known by KEYS, their paths."
  (let ((items (tuatara.buffer:buffer-local list 'cache-items)))
    (remove nil (mapcar (lambda (key) (and items (gethash key items))) keys))))

(defmethod list-entry-noun ((mode cache-list-mode))
  "An entry of the cache."
  "entry")

(defmethod visit-entry ((mode cache-list-mode) list item other)
  "Open ITEM of the cache as its kind opens it (DEFINE-CACHE-KIND): a copy of a
remote file by visiting that file, a mailbox by listing it again; other
directories in rimu, a file safely; with OTHER in another window."
  (declare (ignore list))
  (let* ((entry (getf item :entry))
         (kind (and entry (gethash (cache-entry-kind entry) *cache-kinds*)))
         (path (getf item :path))
         (show (if other (lambda (buffer) (pop-to-buffer buffer :other t)) (lambda (buffer) (show-buffer buffer)))))
    (cond ((and kind (cache-kind-open kind))
           (keeping-list-selected other (lambda () (funcall (cache-kind-open kind) path (cache-entry-key entry)
                                                            (cache-entry-facts entry)))))
          ((uiop:string-suffix-p path "/") (rimu-show-directory path :show show :safe t))
          (t (visit-path path :safe t :show show)))))

(defmethod delete-entries ((mode cache-list-mode) list keys mark)
  "Delete the entries of the cache KEYS name, after asking, in the background:
those in use are not deleted, and said."
  (declare (ignore mark))
  (let* ((items (cache-items-of list keys))
         (busy (remove-if-not (lambda (item) (getf item :in-use)) items))
         (free (set-difference items busy)))
    (cond ((null free) (message "~:[Nothing to delete~;Not deleted: in use~]" busy))
          ((yes-or-no-p* (format nil "Delete ~:[~a~*~;the ~*~d entries~] from the cache~@[, leaving ~d in use~]?"
                                 (rest free) (getf (first free) :name) (length free) (and busy (length busy))))
           (unmark-list list)
           (tuatara.runtime:make-guarded-thread
            (lambda ()
              (let ((refused 0))
                (dolist (item free)
                  (unless (ignoring-errors-reported ("deleting ~a" (getf item :path))
                            (clear-cache-path (getf item :path)))
                    (incf refused)))
                (defer-action (lambda ()
                                (message "Deleted ~d entr~:@p of the cache~[~:;; ~:*~d in use or not deletable~]"
                                         (- (length free) refused) refused)
                                (when (live-buffer-p list) (refill-list list))))))
            :name "tuatara cache deleted"))
          (t (message "Kept")))))

(define-command cache-toggle-pin ()
  "Pin the marked entries of the list of the cache, or the one on the line, so
that they are never cleared by themselves (*CACHE-KEEP*); entries all pinned
already are unpinned. A pin keeps only a directory in the cache directory."
  (let ((list (current-buffer)))
    (act-on-list list
                 (lambda (list keys)
                   (let* ((items (remove-if-not (lambda (item) (uiop:string-suffix-p (getf item :path) "/"))
                                                (cache-items-of list keys)))
                          (pin (notevery (lambda (item) (null (getf item :clears))) items)))
                     (if (null items)
                         (message "Only directories are pinned")
                         (progn (dolist (item items) (pin-cache-directory (getf item :path) pin))
                                (message "~:[Unpinned~;Pinned~] ~d entr~:@p" pin (length items))
                                (refill-list list)))))
                 "entry")))

(defun install-cache ()
  "Keep the entries of the cache as their buffers are killed and the editor
stops, clear what is old now and then, and give the list of the cache its keys."
  (pushnew 'stop-using-cache tuatara.buffer:*kill-buffer-functions*)
  (pushnew 'cache-at-exit *exit-functions*)
  (pushnew 'clear-old-cache-tick *tick-functions*)
  (entry-list-keys 'cache-list-mode "the list of the cache (\\[cache-manager])")
  (define-key (mode-keymap 'cache-list-mode) "P" 'cache-toggle-pin)
  (commands-for-mode 'cache-list-mode "the list of the cache (\\[cache-manager])" 'cache-toggle-pin))
