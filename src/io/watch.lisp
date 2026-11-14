;;;; src/io/watch.lisp --- Watching files and directories for changes on disk.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; One thread (the watcher) follows the files of buffers and the
;;;; directories asked for with inotify where it can and by looking again
;;;; every *POLL-INTERVAL* seconds where it cannot. What it sees is settled
;;;; for a moment, so that a burst of changes is told once, and handed to
;;;; the editor (TAKE-FILE-LOOKS, TAKE-DIRECTORY-NEWS).

(in-package #:tuatara.io)

(defvar *poll-interval* 2
  "Seconds between looks at a visited file the kernel cannot tell of: one on a
network file system, where another machine's writes raise no inotify event
here, or in a directory no inotify watch could be had for.")

(defvar *settle-time* 1/5
  "Seconds a file being written must be left alone before it is looked at, so
that a burst of writes makes one look.")

(defvar *burst-time* 1/20
  "Seconds the watcher waits after a file is closed after writing, renamed,
made or deleted before looking at it, and within which it looks at other files
together, so that many changed at once (a git checkout) come as one piece of
news and one message.")

(defvar *settle-limit* 1
  "Seconds at most between looks at a file written without a pause, a log, so
that its growth is seen.")

(defvar *watcher* nil
  "The thread that watches the visited files on disk: told by inotify of what
happens in their directories, it looks at the files concerned (and every
*POLL-INTERVAL* seconds at those it cannot be told of), so that no file server
that stops answering holds the editor up. NIL until needed.")

(defvar *watcher-wake* nil
  "The wake pipe the watcher waits on besides inotify, written after each request.")

(defvar *watcher-requests* (sb-concurrency:make-mailbox :name "tuatara watcher requests")
  "Requests for the watcher: (:WATCH ITEMS) to watch the files of ITEMS, lists
of (BUFFER PATH STAMP CLASS), from now on instead of those before;
\(:DIRECTORIES DIRECTORIES) to watch DIRECTORIES whole.")

(defvar *look-results* (sb-concurrency:make-mailbox :name "tuatara file looks done")
  "What the watcher found: (:NEWS . RESULTS), RESULTS being lists of (BUFFER
PATH STAMP NOW), NOW the file's stamp, NIL or :UNKNOWN.")

(defvar *directory-news* (sb-concurrency:make-mailbox :name "tuatara directories changed")
  "Directories the watcher found changed, each a message of their native names,
for TAKE-DIRECTORY-NEWS.")

(defvar *watched-directories* '()
  "The directories WATCH-DIRECTORIES last sent to the watcher.")

(defvar *watched* '()
  "The items WATCH-FILES last sent to the watcher, so that only a change is sent.")

(define-record (watch-item (:constructor make-watch-item (buffer path stamp keys)) (:copier nil))
  "A file the watcher watches for BUFFER: PATH as the buffer names it, STAMP
what the buffer read or saved, REPORTED the stamp last reported (a file is
reported when that changes). KEYS are the (DIRECTORY . NAME) its events come
under, NAME in octets: its own, and the file its symbolic links lead to. DUE
is when to look at it (an internal real time) or NIL; FIRST when a burst of
writes not yet looked at began; POLLED true when it is looked at every
*POLL-INTERVAL* seconds instead."
  (buffer nil :documentation "The buffer WATCH-ITEM watches its file for.")
  (path nil :documentation "The file's path as the buffer names it.")
  (stamp nil :documentation "The stamp of the file as the buffer read or saved it.")
  (reported nil
            :documentation "The stamp last reported for WATCH-ITEM's file; it is reported again only when this
changes.")
  (keys nil
        :documentation "The (DIRECTORY . NAME) pairs under which WATCH-ITEM's events come, NAME in octets: those of its
own name and of the files its symbolic links lead to.")
  (due nil :documentation "When to look at WATCH-ITEM's file, in internal real time, or NIL.")
  (first nil
         :documentation "When a burst of writes to WATCH-ITEM's file that has not been looked at yet began, or NIL.")
  (polled nil :documentation "True when WATCH-ITEM's file is looked at every *POLL-INTERVAL* seconds instead."))

(setf (documentation 'make-watch-item 'function) "Return a WATCH-ITEM for BUFFER's PATH, read or
saved at STAMP, watched under KEYS.")

(define-record (directory-watch (:constructor make-directory-watch (directory stamp)) (:copier nil))
  "A directory the watcher watches whole, for its list: DIRECTORY, its native
name ending in /; STAMP, as last seen (for a directory looked at every
*POLL-INTERVAL* seconds, POLLED); DUE and FIRST as a WATCH-ITEM has them."
  (directory nil :documentation "The native name of the directory DIRECTORY-WATCH watches, ending in /.")
  (stamp nil :documentation "The stamp of DIRECTORY-WATCH's directory as last seen.")
  (due nil :documentation "When to look at DIRECTORY-WATCH's directory, in internal real time, or NIL.")
  (first nil
         :documentation "When a burst of changes to DIRECTORY-WATCH's directory that has not been looked at yet began,
or NIL.")
  (polled nil
          :documentation "True when DIRECTORY-WATCH's directory is looked at every *POLL-INTERVAL* seconds instead."))

(defun watch-key (path)
  "The (DIRECTORY . NAME) inotify reports events on PATH's file under: the
native name of its directory and its own name in octets."
  (cons (parent-path path)
        (tuatara.text:string-octets (native-file-name path))))

(defun watch-keys (path)
  "The keys (WATCH-KEY) of PATH and, when symbolic links lead elsewhere, of the file they lead to."
  (let ((own (watch-key path))
        (real (tuatara.runtime:ignoring-errors-reported ("following the links of ~a" path)
                (watch-key (resolve-links path)))))
    (if (and real (not (and (string= (car real) (car own)) (equalp (cdr real) (cdr own)))))
        (list own real)
        (list own))))

(defun look-at (buffer path stamp)
  "What the watcher reports of the file at PATH for BUFFER, which read or saved it at STAMP."
  (list buffer path stamp (handler-case (tuatara.runtime:reporting-errors ("examining ~a" path) (path-stamp path))
                            (error () :unknown))))

(defun seconds-units (seconds)
  "SECONDS in internal time units."
  (round (* seconds internal-time-units-per-second)))

(defun unique-strings (strings)
  "STRINGS, each once, in their order, the first of those alike kept: in time
growing with their number alone, for lists of tens of thousands."
  (let ((seen (make-hash-table :test 'equal :size (max 16 (length strings)))))
    (loop for string in strings
          unless (gethash string seen)
            collect (setf (gethash string seen) string))))

(defun sync-inotify-watches (inotify directories wds wanted)
  "Make and end the inotify watches of INOTIFY (DIRECTORIES from their native
names to watch descriptors, WDS from each descriptor to the names it stands
for, more than one when they are one directory, through a symbolic link) so
that just WANTED, native names of directories, are watched, as far as
watches can be had. A watch ends when no name wants it any more."
  (when inotify
    (let ((gone '())
          (wanted-set (let ((set (make-hash-table :test 'equal :size (max 16 (length wanted)))))
                        (dolist (directory wanted set) (setf (gethash directory set) t)))))
      (maphash (lambda (directory wd) (unless (gethash directory wanted-set) (push (cons directory wd) gone)))
               directories)
      (loop for (directory . wd) in gone
            do (remhash directory directories)
               (let ((left (remove directory (gethash wd wds) :test #'string=)))
                 (if left
                     (setf (gethash wd wds) left)
                     (progn (inotify-unwatch inotify wd) (remhash wd wds))))))
    (dolist (directory wanted)
      (unless (gethash directory directories)
        (let ((wd (inotify-watch inotify directory)))
          (when wd
            (setf (gethash directory directories) wd)
            (pushnew directory (gethash wd wds) :test #'string=)))))))

(defun rewatch-directories (watches wanted inotify directories wds items)
  "The directory watches for WANTED, native names of directories, keeping what
WATCHES knew of each, with the inotify watches of INOTIFY made and ended to
match those of ITEMS, the files watched, too. A directory no watch can be had
for is looked at every *POLL-INTERVAL* seconds."
  (let* ((now (get-internal-real-time))
         (known (let ((table (make-hash-table :test 'equal :size (max 16 (length watches)))))
                  (dolist (watch watches table) (setf (gethash (directory-watch-directory watch) table) watch))))
         (new (mapcar (lambda (directory)
                        (or (gethash directory known)
                            (make-directory-watch directory (path-stamp directory :failure nil))))
                      wanted)))
    (sync-inotify-watches inotify directories wds
                          (unique-strings (append wanted (loop for item in items
                                                               unless (watch-item-polled item)
                                                                 append (mapcar #'car (watch-item-keys item))))))
    (dolist (watch new)
      ;; polled while no watch can be had, and no longer once one can
      (setf (directory-watch-polled watch) (not (gethash (directory-watch-directory watch) directories)))
      (when (and (directory-watch-polled watch) (null (directory-watch-due watch)))
        (setf (directory-watch-due watch) (+ now (seconds-units *poll-interval*)))))
    new))

(defun rewatch (items requests inotify directories wds &optional also)
  "The watch items for REQUESTS, (BUFFER PATH STAMP CLASS) lists, keeping what
ITEMS knew of each file, with the inotify watches of INOTIFY (DIRECTORIES from
their native names to watch descriptors, WDS the other way) made and ended to
match, and the watches of ALSO, directories watched whole, kept. A file whose
directories cannot all be watched, or on a network file system, is polled. A
file new here, or read or saved again, is looked at once straight away: a
change made before its watch began, or reported against the stamp before, is
not missed."
  (let* ((now (get-internal-real-time))
         (known (let ((table (make-hash-table :test 'equal :size (max 16 (length items)))))
                  (dolist (item items table)
                    (setf (gethash (cons (watch-item-buffer item) (watch-item-path item)) table) item))))
         (new (mapcar (lambda (request)
                        (destructuring-bind (buffer path stamp class) request
                          (let ((old (gethash (cons buffer path) known)))
                            (if old
                                (progn (unless (eq stamp (watch-item-stamp old))
                                         (setf (watch-item-stamp old) stamp (watch-item-reported old) stamp
                                               (watch-item-due old) now))
                                       old)
                                (let ((item (make-watch-item buffer path stamp (watch-keys path))))
                                  (setf (watch-item-reported item) stamp
                                        (watch-item-polled item) (eq class :remote)
                                        (watch-item-due item) now)
                                  item)))))
                      requests))
         (wanted (unique-strings (append also
                                         (loop for item in new
                                               unless (watch-item-polled item)
                                                 append (mapcar #'car (watch-item-keys item)))))))
    (sync-inotify-watches inotify directories wds wanted)
    (dolist (item new)
      (unless (every (lambda (key) (gethash (car key) directories)) (watch-item-keys item))
        (setf (watch-item-polled item) t))
      (when (and (watch-item-polled item) (null (watch-item-due item)))
        (setf (watch-item-due item) (+ now (seconds-units *poll-interval*)))))
    new))

(defun note-watch-event (items event wds directories now &optional inotify)
  "Set when to look at the ITEMS inotify's EVENT (WD MASK NAME) concerns. A
file written to is looked at once its writes settle (*SETTLE-TIME*). After any
other change it is looked at once the burst it is part of is over
\(*BURST-TIME*). Either way it is at most *SETTLE-LIMIT* after the first event.
Everything is looked at once after events were lost. A directory's files are
looked at when it went away, polled from then on (its watch ended in INOTIFY,
which goes on watching a directory moved)."
  (destructuring-bind (wd mask name) event
    (flet ((due-now (item) (setf (watch-item-due item) now)))
      (cond ((= wd -1)
             (when (logtest mask +in-queue-overflow+) (mapc #'due-now items)))
            (t
             (let ((names (gethash wd wds)))
               (when names
                 (if (logtest mask +in-directory-gone+)
                     (progn (remhash wd wds)
                            (when inotify (inotify-unwatch inotify wd))
                            (dolist (directory names) (remhash directory directories))
                            (dolist (item items)
                              (when (find-if (lambda (key) (member (car key) names :test #'string=))
                                             (watch-item-keys item))
                                (setf (watch-item-polled item) t)
                                (due-now item))))
                     (when name
                       (dolist (item items)
                         (when (find-if (lambda (key)
                                          (and (member (car key) names :test #'string=) (equalp (cdr key) name)))
                                        (watch-item-keys item))
                           (let ((first (or (watch-item-first item) now))
                                 (quiet (if (= mask +in-modify+) *settle-time* *burst-time*)))
                             (setf (watch-item-first item) first
                                   (watch-item-due item) (min (+ now (seconds-units quiet))
                                                              (+ first (seconds-units *settle-limit*))))))))))))))))

(defun note-directory-event (watches event wds now)
  "Set when to look at the directories of WATCHES inotify's EVENT (WD MASK
NAME) concerns. It is once the burst of changes it is part of is over, at most
*SETTLE-LIMIT* after the first. A file still being written (IN_MODIFY alone)
is no change to the list. Everything is looked at after events were lost. A
directory gone is looked at every *POLL-INTERVAL* seconds from then on."
  (destructuring-bind (wd mask name) event
    (declare (ignore name))
    (flet ((due (watch)
             (let ((first (or (directory-watch-first watch) now)))
               (setf (directory-watch-first watch) first
                     (directory-watch-due watch) (min (+ now (seconds-units *burst-time*))
                                                      (+ first (seconds-units *settle-limit*)))))))
      (cond ((= wd -1) (when (logtest mask +in-queue-overflow+) (mapc #'due watches)))
            ((= mask +in-modify+))
            (t (let ((names (gethash wd wds)))
                 (dolist (watch watches)
                   (when (member (directory-watch-directory watch) names :test #'string=)
                     (when (logtest mask +in-directory-gone+) (setf (directory-watch-polled watch) t))
                     (due watch)))))))))

(defun look-at-due-directories (watches now)
  "The directories of WATCHES whose time has come that changed: every one
told of by inotify, and a polled one when its stamp is not as last seen."
  (loop for watch in watches
        for due = (directory-watch-due watch)
        when (and due (>= now due))
          nconc (let ((stamp (path-stamp (directory-watch-directory watch) :failure nil))
                      (told (directory-watch-first watch)))
                  (setf (directory-watch-first watch) nil
                        (directory-watch-due watch) (and (directory-watch-polled watch)
                                                         (+ now (seconds-units *poll-interval*))))
                  (when (or told
                            (let ((old (directory-watch-stamp watch)))
                              ;; not there, or not readable, as before: no news
                              (and (or stamp old)
                                   (not (and stamp old
                                             (= (stamp-mtime stamp) (stamp-mtime old))
                                             (= (stamp-ino stamp) (stamp-ino old)))))))
                    (setf (directory-watch-stamp watch) stamp)
                    (list (directory-watch-directory watch))))))

(defun look-at-due (items now)
  "Look at the ITEMS whose time has come, and with them those due within
*BURST-TIME*, so that files changed together are reported together; the
results of those whose file is not as last reported, remembering it as
reported."
  (loop with soon = (and (some (lambda (item) (let ((due (watch-item-due item))) (and due (>= now due)))) items)
                         (+ now (seconds-units *burst-time*)))
        for item in items
        for due = (watch-item-due item)
        when (and soon due (<= due soon))
          nconc (let ((result (look-at (watch-item-buffer item) (watch-item-path item) (watch-item-stamp item))))
                  (setf (watch-item-first item) nil
                        (watch-item-due item) (and (watch-item-polled item) (+ now (seconds-units *poll-interval*))))
                  (let ((found (fourth result)) (reported (watch-item-reported item)))
                    (unless (or (eq found :unknown)
                                (and (null found) (null reported))
                                (and found reported (same-content-p found reported)))
                      (setf (watch-item-reported item) found)
                      (list result))))))

(defun tell-editor ()
  "Wake the editor (HOST-WAKE), on the watcher's thread, to take in what the
watcher found (TAKE-FILE-LOOKS)."
  (tuatara.runtime:ignoring-errors-reported ("taking in what the file watcher found")
    (tuatara.runtime:host-wake tuatara.runtime:*host*)))

(defun soonest-due (items)
  "The soonest time any of ITEMS is due to be looked at, or NIL when none is."
  (let ((soonest nil))
    (dolist (item items soonest)
      (let ((due (watch-item-due item)))
        (when (and due (or (null soonest) (< due soonest)))
          (setf soonest due))))))

(defun watcher-loop ()
  "What the watcher thread does: take requests, read inotify's events, look at
the files whose time has come, report what changed, and wait for the next of
these, which may be a long time; no looking without cause but for polled files."
  (let ((inotify (inotify-open))
        (buffer (make-array 65536 :element-type '(unsigned-byte 8)))
        (directories (make-hash-table :test 'equal))
        (wds (make-hash-table))
        (items '())
        (watches '()))
    (loop
      (drain-wake-pipe *watcher-wake*)
      (dolist (request (sb-concurrency:receive-pending-messages *watcher-requests*))
        (ecase (first request)
          (:watch (setf items (rewatch items (second request) inotify directories wds
                                       (mapcar #'directory-watch-directory watches))))
          (:directories (setf watches (rewatch-directories watches (second request) inotify directories wds items)))))
      (let ((now (get-internal-real-time)))
        (when inotify
          (dolist (event (inotify-events inotify buffer))
            ;; the lists' first: the files' forgets a directory gone
            (note-directory-event watches event wds now)
            (note-watch-event items event wds directories now inotify)))
        (let ((found (look-at-due items now)))
          (when found
            (sb-concurrency:send-message *look-results* (cons :news found))
            (tell-editor)))
        (let ((changed (look-at-due-directories watches now)))
          (when changed
            (sb-concurrency:send-message *directory-news* changed)
            (tell-editor)))
        (let ((soonest (let ((times (remove nil (cons (soonest-due items) (mapcar #'directory-watch-due watches)))))
                         (and times (reduce #'min times)))))
          (wait-for-fds (if inotify (list inotify (car *watcher-wake*)) (list (car *watcher-wake*)))
                        (and soonest (/ (max 0 (- soonest now)) internal-time-units-per-second))))))))

(defun send-watcher (request)
  "Send REQUEST to the watcher, starting it first when needed, and wake it."
  (unless *watcher-wake* (setf *watcher-wake* (make-wake-pipe)))
  (unless (and *watcher* (sb-thread:thread-alive-p *watcher*))
    (setf *watcher* (tuatara.runtime:make-guarded-thread #'watcher-loop :name "tuatara file watcher")))
  (sb-concurrency:send-message *watcher-requests* request)
  (wake *watcher-wake*))

(defun watch-files (buffers)
  "Have the watcher watch the files of BUFFERS on disk from now on, and no
others, telling it only when something changed since the last call: a file
visited, killed, saved or read again. Files of pseudo file systems (/proc) are
not watched: they change as they are read."
  (flet ((watched-p (buffer)
           (and (tuatara.buffer:buffer-file buffer) (not (eq (tuatara.buffer:buffer-local buffer :file-class) :pseudo))
                (not (buffer-arriving-p buffer))))
         (item (buffer)
           (list buffer (tuatara.buffer:buffer-file buffer) (buffer-file-stamp buffer)
                 (tuatara.buffer:buffer-local buffer :file-class))))
    (unless (let ((watched *watched*))
              (and (loop for buffer in buffers
                         always (or (not (watched-p buffer))
                                    (and watched
                                         (destructuring-bind (was-buffer path stamp class) (pop watched)
                                           (and (eq was-buffer buffer) (equal path (tuatara.buffer:buffer-file buffer))
                                                (eq stamp (buffer-file-stamp buffer))
                                                (eq class (tuatara.buffer:buffer-local buffer :file-class)))))))
                   (null watched)))
      (setf *watched* (loop for buffer in buffers when (watched-p buffer) collect (item buffer)))
      (send-watcher (list :watch *watched*)))))

(defun watch-directories (directories)
  "Have the watcher watch DIRECTORIES (native names ending in /) whole from now
on, and no others, for lists of them, telling it only when that changed: what
changes in them comes back through TAKE-DIRECTORY-NEWS. Tens of thousands of
directories are compared in time growing with their number alone."
  (let ((wanted (unique-strings directories)))
    (unless (and (= (length wanted) (length *watched-directories*))
                 (let ((watched (make-hash-table :test 'equal :size (max 16 (length wanted)))))
                   (dolist (d *watched-directories*) (setf (gethash d watched) t))
                   (every (lambda (d) (gethash d watched)) wanted)))
      (setf *watched-directories* wanted)
      (send-watcher (list :directories wanted)))
    wanted))

(defun take-directory-news ()
  "The directories (WATCH-DIRECTORIES) the watcher found changed since this
was last asked, native names ending in /."
  (remove-duplicates (loop for message in (sb-concurrency:receive-pending-messages *directory-news*) append message)
                     :test #'string=))

(defun take-file-looks ()
  "Note what the watcher found in each buffer's FILE-DISK-STATE, as CHECK-FILE
does, skipping buffers saved, reverted or killed since, and those being saved
(the change is the save). Returns the buffers whose state changed, and as a
second value those whose file changed again without the state changing (a
file that goes on growing)."
  (let ((changed '()) (again '()))
    (dolist (message (sb-concurrency:receive-pending-messages *look-results*))
      (dolist (result (cdr message))
        (destructuring-bind (buffer path stamp now) result
          (when (and (tuatara.buffer:live-buffer-p buffer)
                     (equal path (tuatara.buffer:buffer-file buffer))
                     (eq stamp (buffer-file-stamp buffer))
                     (not (eq now :unknown))
                     (not (saving-p buffer)))
            (multiple-value-bind (state new) (note-disk-status buffer (disk-status buffer stamp now))
              (cond (new (pushnew buffer changed))
                    (state (pushnew buffer again))))))))
    (values (nreverse changed) (nreverse (set-difference again changed)))))
