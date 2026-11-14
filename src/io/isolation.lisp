;;;; src/io/isolation.lisp --- Keeping a visited file's text from other programs' changes.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; A large file is read where it is under a read lease, copied only when
;;;; another program is about to write it; else cloned, or copied in the
;;;; background (a snapshot), the mapping moved onto the copy once it is
;;;; made. Copies go beside the file only where its file system shares
;;;; blocks, else to the cache directory. A large text's lines are counted in
;;;; the background meanwhile (START-INDEXING). See files.lisp's header for
;;;; which file gets which.

(in-package #:tuatara.io)

(defvar *lease-large-files* t
  "True to read large local files themselves under a read lease, copying them
only when another program is about to write them. Tests turn it off to take the
other paths.")

(defvar *clone-large-files* t
  "True to clone large files on file systems that share blocks. Tests turn it off
to take the paths other file systems take.")

(defvar *snapshot-large-files* t
  "True to copy a large file that cannot be cloned into a snapshot in the
background, where there is room, so that other programs' writes cannot reach
the buffer.")

(defvar *snapshot-free-margin* (* 1024 1024 1024)
  "Octets of free space a background snapshot must leave on its file system.")

;;; Snapshots made in the background

(define-record (snapshot (:include tuatara.runtime:job) (:constructor %make-snapshot) (:copier nil))
  "A copy of a mapped file being made in the background, a job
\(TUATARA.RUNTIME:JOB) of no buffer: of SOURCE, a descriptor of its own for
the file behind MAPPING, PATH, which was at STAMP; file descriptor (FD) is the
copy. SERVED is the buffer whose text it keeps, or NIL; CHANGED is true when
the file changed while it was copied, the copy then of no use."
  (mapping nil :documentation "The mapping of the file SNAPSHOT copies.")
  (source nil :documentation "The descriptor of its own that SNAPSHOT reads the file behind its mapping through.")
  (path nil :documentation "The path of the file SNAPSHOT copies.")
  (stamp nil
         :documentation "The stamp the file had when SNAPSHOT began, compared with the file's stamp once it is copied.")
  (served nil :documentation "The buffer whose text SNAPSHOT keeps, or NIL.")
  (fd nil :documentation "The descriptor of the copy SNAPSHOT makes.")
  (changed nil :documentation "True when the file changed while SNAPSHOT copied it, the copy then of no use."))

(tuatara.runtime:define-job-words snapshot
  :doing "Copying"
  :description (format nil "Copying ~a, so that other programs' writes do not reach its buffer" (snapshot-path job))
  :verb "copy the file"
  :left-words "the file is read where it is"
  :at-exit :quiet)
(defmethod tuatara.runtime:job-holds-queue-p ((job snapshot)) nil)
(defmethod tuatara.runtime:job-shows-processing-p ((job snapshot)) nil)

(defmethod tuatara.runtime:job-take-in ((job snapshot))
  "A finished copy becomes what its mapping reads; else the buffer it served
reads the file where it is."
  (let ((buffer (snapshot-served job))
        (taken (and (eq (snapshot-status job) :done) (not (snapshot-changed job))
                    (tuatara.runtime:ignoring-errors-reported ("reading ~a from its copy" (snapshot-path job))
                      (tuatara.text:retarget-mapping (snapshot-mapping job) (snapshot-fd job))))))
    (unless taken
      (when (snapshot-fd job)
        (tuatara.runtime:close-fd-quietly (snapshot-fd job) "the copy of ~a" (snapshot-path job))))
    (when (and buffer (eq (file-isolation buffer) :pending))
      (setf (tuatara.buffer:buffer-local buffer :file-isolation) (if taken :snapshot :direct)))))

(defun start-snapshot (mapping fd path stamp directory)
  "Start copying the file PATH, open as file descriptor (FD) and mapped as
MAPPING, into an unnamed file in DIRECTORY in the background, a job, where
its file system has room. The copy reads through a descriptor of its own.
Returns the snapshot, or NIL."
  (let ((free (free-octets directory)))
    (when (and free (> free (+ (stamp-size stamp) *snapshot-free-margin*)))
      (let* ((source (sb-posix:dup fd))
             (snapshot (%make-snapshot :mapping mapping :source source :path path :stamp stamp)))
        (tuatara.runtime:with-fd-closed-on-error (source "~a" path)
          (tuatara.runtime:start-job-thread
           snapshot
           (lambda (snapshot)
             (unwind-protect
                  (let ((copy (open-scratch-file directory)))
                    (setf (snapshot-fd snapshot) copy)
                    (if (copy-fd-contents source copy (stamp-size stamp) :path path :cancel (snapshot-cancel snapshot))
                        ;; a copy is only good if the file did not change while it was made
                        (setf (snapshot-changed snapshot)
                              (not (same-content-p stamp (tuatara.runtime:ignoring-errors-reported ("examining ~a" path)
                                                           (fd-stamp source path)))))
                        (setf (snapshot-status snapshot) :stopped)))
               (tuatara.runtime:close-fd-quietly source "~a" path)))
           :name "tuatara snapshot"))
        snapshot))))

(defun snapshots ()
  "The background snapshots running or ended and not yet taken in."
  (remove-if-not #'snapshot-p tuatara.runtime:*jobs*))

(defun finish-snapshots ()
  "Take in the background snapshots that have ended, on the editor's thread
\(their JOB-TAKE-IN): a finished copy becomes what its mapping reads."
  (tuatara.runtime:take-in-jobs #'snapshot-p))

(defun stop-snapshots-of (buffer)
  "Stop any background snapshot serving BUFFER."
  (dolist (snapshot (snapshots))
    (when (eq (snapshot-served snapshot) buffer)
      (tuatara.runtime:job-stop snapshot))))

;;; Leases: reading the file itself until someone is about to write it

(define-record (lease (:constructor %make-lease) (:copier nil))
  "A read lease on a visited file: file descriptor (FD), open on the file at
PATH as it was at STAMP, is what MAPPING reads while STATUS is :HELD. When
a program is about to write the file, the keeper copies it into PLACE, a
directory (COPIED-BY :CLONE or :SNAPSHOT), moves MAPPING onto the copy and
lets the writer go: :MOVING, then :MOVED. :LOST when the copy could not be
made, or not in time; :RELEASED when given up. BUFFER is the buffer it
serves: NIL once that buffer is killed with its text kept elsewhere,
MAPPING being then a weak pointer to the mapping (LEASE-TARGET), and the
lease given up when nothing reads the mapping any more."
  (mapping nil
           :documentation "The mapping LEASE keeps reading the file through, or a weak pointer to it when LEASE's buffer
is gone (LEASE-TARGET).")
  (fd nil :documentation "The descriptor open on LEASE's file.")
  (path nil :documentation "The path of the file LEASE is on.")
  (stamp nil :documentation "The stamp the file had when LEASE was taken.")
  (buffer nil :documentation "The buffer LEASE serves, or NIL once it is killed with its text kept elsewhere.")
  (place nil
         :documentation "The directory LEASE's copy of the file is put in when a program is about to write the file, or
NIL before.")
  (status :held :documentation "What state LEASE is in: :HELD, :MOVING, :MOVED, :LOST or :RELEASED.")
  (copied-by nil :documentation "How LEASE's copy was made, :CLONE or :SNAPSHOT, or NIL before it is."))

(defun lease-target (lease)
  "The mapping LEASE guards, or NIL once it has been garbage collected."
  (let ((mapping (lease-mapping lease)))
    (if (sb-ext:weak-pointer-p mapping) (sb-ext:weak-pointer-value mapping) mapping)))

(defvar *leases* '()
  "Leases held on visited files, and those that ended and are not yet taken in.")

(defvar *lease-lock* (sb-thread:make-mutex :name "tuatara leases")
  "Held while *LEASES* or a lease's status changes.")

(defvar *lease-keeper* nil
  "The thread that watches the leases, while there are any.")

(defvar *lease-check-interval* 5
  "Seconds the keeper waits at most between looks at the leases. It looks at
once when the system says a program is about to write a leased file, and after
each garbage collection while text kept from a killed buffer holds a lease;
this is for the rare signal that does not come.")

(defvar *lease-grace* 1/2
  "Seconds the keeper waits after moving a mapping before it lets the writer
go, so that reads begun on the file itself have finished.")

(defun copy-for-lease (lease)
  "A descriptor for a copy of what LEASE's mapping reads in its place, and how
it was made: a clone where the file system shares blocks (:CLONE), else a full
copy (:SNAPSHOT). A mapping that reads some of its octets from elsewhere (a
save's journal) has each copied from where it reads it."
  (let* ((fd (lease-fd lease))
         (path (lease-path lease))
         (mapping (lease-target lease))
         (copy (open-scratch-file (lease-place lease))))
    (tuatara.runtime:with-fd-closed-on-error (copy "the copy of ~a" path)
      (cond ((and mapping (rest (tuatara.text:mapping-sources mapping)))
             (loop for (start end source delta) in (tuatara.text:mapping-sources mapping)
                   do (sb-posix:lseek copy start sb-posix:seek-set)
                      ;; the lease is broken: the writer waits for the copy, which waits for no device
                      (copy-fd-contents source copy (- end start) :from (+ start delta) :path path :wait nil))
             (values copy :snapshot))
            ((clone-fd fd copy) (values copy :clone))
            (t (copy-fd-contents fd copy (stamp-size (lease-stamp lease)) :path path :wait nil)
               (values copy :snapshot))))))

(defun move-leased (lease)
  "A program is about to write LEASE's file: copy it, move the mapping onto the
copy, and only then let the writer go. When the copy cannot be made, or the
system broke the lease before it was, the lease is :LOST, and a change to the
file will show the buffer stale. LEASE is :MOVING already (LEASE-KEEPER-LOOP)."
  (let ((copy nil))
    (handler-case
        (tuatara.runtime:reporting-errors ("copying ~a before a program writes it" (lease-path lease))
          (multiple-value-bind (fd how) (copy-for-lease lease)
            (setf copy fd)
            (if (and (lease-target lease)
                     (same-content-p (lease-stamp lease) (fd-stamp (lease-fd lease) (lease-path lease)))
                     (tuatara.text:retarget-mapping (lease-target lease) copy))
                (progn
                  (setf copy nil)
                  (sleep *lease-grace*)
                  (setf (lease-copied-by lease) how
                        (lease-status lease) :moved))
                (setf (lease-status lease) :lost))))
      (error () (setf (lease-status lease) :lost)))
    (tuatara.runtime:close-fd-quietly copy "the copy of ~a" (lease-path lease))
    (release-lease (lease-fd lease))))

(defun lease-keeper-loop ()
  "What the keeper thread does: whenever woken (*LEASE-BREAK-PIPE*), and every
*LEASE-CHECK-INTERVAL* seconds at most, move the leased files a program is
about to write, and give up the leases whose mapping nothing reads any more;
end when no lease is held."
  (loop
    (drain-wake-pipe *lease-break-pipe*)
    (let ((held (sb-thread:with-mutex (*lease-lock*)
                  (or (remove-if-not (lambda (lease) (eq (lease-status lease) :held)) *leases*)
                      (progn (setf *lease-keeper* nil) (return))))))
      (dolist (lease held)
        (cond ((not (eq (lease-status lease) :held)))
              ((null (lease-target lease)) (give-up-leases (lambda (other) (eq other lease))))
              ((and (lease-breaking-p (lease-fd lease))
                    ;; :MOVING only while still :HELD, so that a lease given up meanwhile is not copied
                    (sb-thread:with-mutex (*lease-lock*)
                      (when (eq (lease-status lease) :held)
                        (setf (lease-status lease) :moving))))
               (move-leased lease)))))
    (wait-for-fds (list (car *lease-break-pipe*)) *lease-check-interval*)))

(defun wake-lease-keeper-after-gc ()
  "Wake the lease keeper after a garbage collection when a lease guards text
kept from a killed buffer, which the collection may have freed."
  (when (and *lease-keeper* (find-if #'sb-ext:weak-pointer-p *leases* :key #'lease-mapping))
    (wake *lease-break-pipe*)))

(pushnew 'wake-lease-keeper-after-gc sb-ext:*after-gc-hooks*)

(defun keep-leases-watched ()
  "Start the keeper thread that watches the leases, unless it runs; called
with *LEASE-LOCK* held, after a lease is held."
  (unless *lease-keeper*
    (setf *lease-keeper* (tuatara.runtime:make-guarded-thread
                          (lambda ()
                            (unwind-protect (lease-keeper-loop)
                              ;; ended by trouble: another started when a lease is next held
                              (sb-thread:with-mutex (*lease-lock*)
                                (when (eq *lease-keeper* sb-thread:*current-thread*)
                                  (setf *lease-keeper* nil)))))
                          :name "tuatara lease keeper"))))

(defun hold-lease (mapping fd path stamp place)
  "Keep the lease just taken on file descriptor (FD), which MAPPING reads, and
see that it is watched; a copy, if one is needed, goes into the directory
PLACE. Returns the lease."
  (let ((lease (%make-lease :mapping mapping :fd fd :path path :stamp stamp :place place)))
    (sb-thread:with-mutex (*lease-lock*)
      (push lease *leases*)
      (keep-leases-watched))
    lease))

(defun step-aside-leases (mappings)
  "Have the held leases on what MAPPINGS read stand aside while the editor
writes their file itself: given up for the time being, so that its own
writing does not wait on them, and neither copied nor ended (:SAVING).
Returns them, for TAKE-BACK-LEASES, and true as a second value when a lease
on what MAPPINGS read was not held or was breaking: another program is about
to write their file, or has."
  (sb-thread:with-mutex (*lease-lock*)
    (let ((stood '()) (broken nil))
      (dolist (lease *leases*)
        (when (member (lease-target lease) mappings)
          (case (lease-status lease)
            (:held (when (lease-breaking-p (lease-fd lease)) (setf broken t))
             (setf (lease-status lease) :saving)
             (release-lease (lease-fd lease))
             (push lease stood))
            ((:moving :moved :lost) (setf broken t)))))
      (values (nreverse stood) broken))))

(defun take-back-leases (leases)
  "Take again LEASES, which stood aside while the editor wrote their file
\(STEP-ASIDE-LEASES), for the file as it is now; a lease the system will not
give again is :LOST, and the buffer it served is watched for changes."
  (when leases
    (sb-thread:with-mutex (*lease-lock*)
      (dolist (lease leases)
        (let ((stamp (tuatara.runtime:ignoring-errors-reported ("examining ~a" (lease-path lease))
                       (fd-stamp (lease-fd lease) (lease-path lease)))))
          (setf (lease-stamp lease) (or stamp (lease-stamp lease))
                (lease-status lease) (if (and stamp (take-read-lease (lease-fd lease))) :held :lost))))
      (keep-leases-watched))))

(defun give-up-leases (predicate)
  "Give up the held leases PREDICATE is true of, letting any writer go."
  (sb-thread:with-mutex (*lease-lock*)
    (dolist (lease *leases*)
      (when (and (eq (lease-status lease) :held) (funcall predicate lease))
        (setf (lease-status lease) :released)
        (release-lease (lease-fd lease))))))

(defun finish-leases ()
  "Take in the leases that have ended, on the editor's thread: a buffer whose
file was copied reads a clone or snapshot now; one whose copy failed reads the
file directly, watched for changes like any other."
  (let ((ended (sb-thread:with-mutex (*lease-lock*)
                 (let ((ended (remove-if (lambda (lease) (member (lease-status lease) '(:held :moving :saving)))
                                         *leases*)))
                   (setf *leases* (set-difference *leases* ended))
                   ended))))
    (dolist (lease ended)
      (let ((buffer (lease-buffer lease)))
        (when (and buffer (eq (file-isolation buffer) :leased)
                   (eq (tuatara.buffer:buffer-mapping buffer) (lease-mapping lease)))
          (setf (tuatara.buffer:buffer-local buffer :file-isolation)
                (case (lease-status lease)
                  (:moved (lease-copied-by lease))
                  (:lost :direct)
                  (t :snapshot))))))))

(defun leases-moving-p ()
  "True while the keeper is copying a file a program is about to write."
  (some (lambda (lease) (eq (lease-status lease) :moving)) *leases*))

(defun buffer-lease (buffer)
  "The lease that keeps BUFFER's text, or NIL."
  (tuatara.buffer:buffer-local buffer :file-lease))

(defun attend (guard buffer)
  "Let GUARD, the snapshot or lease OPEN-FILE-TEXT returned (or NIL), know the
buffer it serves, and the buffer its lease."
  (setf (tuatara.buffer:buffer-local buffer :file-lease) (and (lease-p guard) guard))
  (etypecase guard
    (null nil)
    (snapshot (setf (snapshot-served guard) buffer))
    (lease (setf (lease-buffer guard) buffer))))

;;; Counting a large text's lines in the background

(define-record (index-job (:include tuatara.runtime:job) (:constructor %make-index-job (buffer rope donor))
                          (:copier nil))
  "The counting of the lines and characters of ROPE, BUFFER's text, in the
background, a job (TUATARA.RUNTIME:JOB), taking what DONOR (the text counted
before, or NIL) has counted where the text is still the same: PROGRESS is a
cons of the octets read and those to read; RESULT the counted rope once done.
TALLY is what is counted so far, from the start of the text: the line
numbers there are known before the whole text is counted
\(COUNTED-LINE-NUMBER, COUNTED-LINE-START); CARRIED, that tally carried from
the version counted through the edits made since (TUATARA.TEXT:CARRY-TALLY)."
  (rope nil :documentation "The rope INDEX-JOB counts.")
  (donor nil
         :documentation "The earlier counted text, or NIL, whose counts INDEX-JOB takes where the text is still the
same.")
  (progress (cons 0 0) :documentation "A cons of the octets INDEX-JOB has read and those it has to read.")
  (result nil :documentation "The counted rope once INDEX-JOB is done, or NIL.")
  (tally (tuatara.text:make-tally) :documentation "What INDEX-JOB has counted so far, from the start of the text.")
  (carried nil :documentation "INDEX-JOB's tally carried through the edits made since the version counted, or NIL."))

(setf (documentation 'index-job-status 'function)
      "How the counting of an INDEX-JOB stands: :RUNNING, :DONE, :STOPPED or :FAILED.")

(defmethod tuatara.runtime:job-percent ((job index-job)) (min 99 (index-progress job)))
(tuatara.runtime:define-job-words index-job
  :doing "Counting lines"
  :description (format nil "Counting the lines of ~a" (tuatara.runtime:job-buffer-name job))
  :verb "count the lines"
  :left-words "the lines are counted as they are needed"
  :at-exit :quiet)
(defmethod tuatara.runtime:job-holds-queue-p ((job index-job)) nil)
(defmethod tuatara.runtime:job-shows-processing-p ((job index-job)) nil)

(defmethod tuatara.runtime:job-take-in ((job index-job))
  "A counting done gives its buffer the counted text, when the text is still
the one counted; one that failed is noted, so that the same text is not
counted again, and one stopped too, so that it is not started again by itself."
  (let ((buffer (index-job-buffer job)))
    (when (tuatara.buffer:live-buffer-p buffer)
      (case (index-job-status job)
        (:failed
         (setf (tuatara.buffer:buffer-local buffer :index-failed) (index-job-rope job)
               (tuatara.buffer:buffer-local buffer :index-error) (index-job-error job))
         (tuatara.runtime:debug-report :file "the lines of ~a could not be counted: ~a"
                                       (tuatara.buffer:buffer-name buffer) (index-job-error job)))
        (:stopped
         (setf (tuatara.buffer:buffer-local buffer :index-stopped) (index-job-rope job)))
        (:done
         (setf (tuatara.buffer:buffer-local buffer :index-donor) (index-job-result job))
         (when (eq (tuatara.buffer:buffer-text buffer) (index-job-rope job))
           (tuatara.buffer:buffer-adopt-text buffer (index-job-result job))
           (setf (tuatara.buffer:buffer-local buffer :counted-so-far) nil)))))))

(defvar *index-job-hook* nil
  "A function each counting calls before it begins, or NIL. Tests use it to keep
one running long enough to act while it runs.")

(defun index-jobs ()
  "The countings running in the background, or ended and not taken in yet."
  (remove-if-not #'index-job-p tuatara.runtime:*jobs*))

(defun index-job-of (buffer)
  "The counting of BUFFER's lines that runs or has not been taken in, or NIL."
  (find-if (lambda (job) (and (index-job-p job) (eq (index-job-buffer job) buffer))) tuatara.runtime:*jobs*))

(defun index-progress (job)
  "How much of JOB's counting is done, from 0 to 100."
  (tuatara.runtime:part-percent (index-job-progress job)))

(defun counted-so-far (buffer)
  "The count of BUFFER's lines made in the background, running or ended,
carried through the edits made since (TUATARA.TEXT:CARRY-TALLY), while its
text is not counted itself; NIL once it is. One that can no longer be carried
gives way to the count running, when there is one."
  (let ((carried (tuatara.buffer:buffer-local buffer :counted-so-far))
        (job (index-job-of buffer)))
    (cond ((null carried) nil)
          ((tuatara.buffer:buffer-indexed-p buffer) (setf (tuatara.buffer:buffer-local buffer :counted-so-far) nil))
          ((and job (index-job-carried job) (not (eq carried (index-job-carried job)))
                (tuatara.text:carried-lost-p carried (tuatara.buffer:buffer-version buffer)))
           (setf (tuatara.buffer:buffer-local buffer :counted-so-far) (index-job-carried job)))
          (t carried))))

(defun counted-line-number (buffer byte)
  "While BUFFER's lines are being counted: the line, from 0, holding offset
BYTE, when the count has passed it from the start, edits made meanwhile
included; else NIL."
  (let ((carried (counted-so-far buffer)))
    (and carried (tuatara.text:carried-line-number carried (tuatara.buffer:buffer-version buffer) byte))))

(defun buffer-known-line-number (buffer byte)
  "The line number, from 0, of BYTE in BUFFER when it is known without waiting:
always once the lines are counted; while they are, where the count has passed
\(COUNTED-LINE-NUMBER); else NIL."
  (if (tuatara.buffer:buffer-indexed-p buffer)
      (tuatara.buffer:buffer-line-number buffer byte)
      (counted-line-number buffer byte)))

(defun counted-line-start (buffer line)
  "While BUFFER's lines are being counted: where line LINE, from 0, starts,
when the count has passed it from the start, edits made meanwhile included;
else NIL."
  (let ((carried (counted-so-far buffer)))
    (and carried (tuatara.text:carried-line-start carried (tuatara.buffer:buffer-version buffer) line))))

(defun counted-lines (buffer)
  "While BUFFER's lines are being counted: two values, the newlines counted from
the start of its text so far and the offset the count has reached, edits made
meanwhile included; else NIL."
  (let ((carried (counted-so-far buffer)))
    (and carried (tuatara.text:carried-counted-lines carried (tuatara.buffer:buffer-version buffer)))))

(sb-ext:defglobal **arriving-paths** '()
  "The native paths of the files still arriving from elsewhere, which nothing is to open (BUFFER-ARRIVING-P).")

(sb-ext:defglobal **arriving-paths-lock** (sb-thread:make-mutex :name "tuatara arriving files")
  "Held while **ARRIVING-PATHS** is changed.")

(defun refuse-arriving-file (path)
  "Refuse (NOT-ARRIVED) when the file at PATH is still arriving from elsewhere:
what has not come would read as zeros."
  (when (and **arriving-paths** (member (native path) **arriving-paths** :test #'string=))
    (tuatara.text:refuse-not-arrived)))

(defun buffer-arriving-p (buffer)
  "True while BUFFER's file is still being written with what comes from
elsewhere (the rest of a file on another machine): its lines are not counted,
it is not watched, and nothing reads it by its name, until all of it is here."
  (tuatara.buffer:buffer-local buffer :arriving))

(defun (setf buffer-arriving-p) (arriving buffer)
  "Say whether BUFFER's file is still arriving (BUFFER-ARRIVING-P): its path
is refused to whatever opens it (REFUSE-ARRIVING-FILE) while it is."
  (let ((path (native (tuatara.buffer:buffer-file buffer))))
    (sb-thread:with-mutex (**arriving-paths-lock**)
      (setf **arriving-paths** (remove path **arriving-paths** :test #'string=))
      (when arriving (push path **arriving-paths**))))
  (setf (tuatara.buffer:buffer-local buffer :arriving) arriving))

(defun start-indexing (buffer)
  "Start counting the lines of BUFFER's text in the background, a job, unless
they are known or being counted, the file is still arriving
\(BUFFER-ARRIVING-P), or counting this same text failed
\(:INDEX-FAILED): on the worker pool, taking what the text counted last had
where it is the same. Returns the job, or NIL."
  (or (index-job-of buffer)
      (unless (or (tuatara.buffer:buffer-indexed-p buffer) (buffer-arriving-p buffer)
                  (eq (tuatara.buffer:buffer-local buffer :index-failed) (tuatara.buffer:buffer-text buffer)))
        (let ((hook *index-job-hook*)
              (job (%make-index-job buffer (tuatara.buffer:buffer-text buffer)
                                    (tuatara.buffer:buffer-local buffer :index-donor)))
              (kept (counted-so-far buffer))
              (version (tuatara.buffer:buffer-version buffer)))
          (setf (index-job-carried job) (tuatara.text:carry-tally (index-job-tally job) version))
          ;; a whole count carried to the text now serves until this one is done
          (unless (and kept (eql (nth-value 1 (tuatara.text:carried-counted-lines kept version))
                                 (tuatara.buffer:buffer-size buffer)))
            (setf (tuatara.buffer:buffer-local buffer :counted-so-far) (index-job-carried job)))
          (tuatara.runtime:start-job-thread
           job
           (lambda (job)
             (when hook (funcall hook))
             (let ((result (tuatara.text:index-rope (index-job-rope job) :donor (index-job-donor job)
                                                                        :progress (index-job-progress job)
                                                                        :cancel (index-job-cancel job)
                                                                        :tally (index-job-tally job))))
               (setf (index-job-result job) result
                     (index-job-status job) (if result :done :stopped))))
           :name "tuatara counting lines")))))

(defvar *index-again-pause* 1/2
  "Seconds a large text counted before, then edited, rests unchanged before its
lines are counted again: while it is typed in, counting it again at each
change would go through all its pieces each time.")

(defvar *index-again-octets* (* 64 1024 1024)
  "Octets from which a text edited while it was counted waits for
*INDEX-AGAIN-PAUSE* before it is counted again.")

(defun index-again-waits-p (buffer)
  "True when BUFFER, a large text counted before and edited since, was
changed less than *INDEX-AGAIN-PAUSE* ago, as the ticks that look at it saw:
its counting then waits for the edits to pause."
  (and (tuatara.buffer:buffer-local buffer :index-donor)
       (> (tuatara.buffer:buffer-size buffer) *index-again-octets*)
       (let ((seen (tuatara.buffer:buffer-local buffer :index-again-seen))
             (version (tuatara.buffer:buffer-version buffer))
             (now (get-internal-real-time)))
         (if (and seen (eql (car seen) version))
             (< (- now (cdr seen)) (* *index-again-pause* internal-time-units-per-second))
             (progn (setf (tuatara.buffer:buffer-local buffer :index-again-seen) (cons version now))
                    t)))))

(defun finish-indexing ()
  "On the editor's thread: take in the countings that have ended (their
JOB-TAKE-IN). A buffer whose text is still the one counted takes the counted
text, the same octets (TUATARA.BUFFER:BUFFER-ADOPT-TEXT); one edited meanwhile
is counted again, which reads only around the edits, once the edits pause
when it is large (INDEX-AGAIN-WAITS-P). A buffer whose text is not counted
and not being counted has a counting started, unless counting that same text
failed (its :INDEX-FAILED and :INDEX-ERROR say so) or was stopped. True while
countings run, or one waits."
  (tuatara.runtime:take-in-jobs #'index-job-p)
  (let ((waiting nil))
    (dolist (buffer tuatara.buffer:*buffers*)
      (unless (or (tuatara.buffer:buffer-indexed-p buffer) (index-job-of buffer)
                  (eq (tuatara.buffer:buffer-local buffer :index-failed) (tuatara.buffer:buffer-text buffer))
                  (eq (tuatara.buffer:buffer-local buffer :index-stopped) (tuatara.buffer:buffer-text buffer)))
        (if (index-again-waits-p buffer)
            (setf waiting t)
            (start-indexing buffer))))
    (when waiting (return-from finish-indexing t)))
  (some (lambda (job) (eq (index-job-status job) :running)) (index-jobs)))

(defun forget-arriving-file (buffer)
  "Kill-buffer function: BUFFER's file is no longer arriving for anyone."
  (when (buffer-arriving-p buffer) (setf (buffer-arriving-p buffer) nil)))

(defun stop-indexing (buffer)
  "Stop the counting of BUFFER's lines, if one runs, and wait for its workers,
so that none reads the buffer's text once its file is closed; one that does
not stop in time (TUATARA.RUNTIME:*STOP-WAIT*) is left to end on its own, the
file kept mapped for it (BUFFER-LENT-P)."
  (let ((job (index-job-of buffer)))
    (when job
      (unless (tuatara.runtime:stop-and-settle job)
        (setf (tuatara.buffer:buffer-lent-p buffer) t))
      (tuatara.runtime:forget-job job))))

(defun try-clone (fd path)
  "A descriptor for an unnamed clone of the file open as file descriptor (FD)
made in its own directory, sharing its blocks, or NIL where the file system
cannot. One that shares no blocks (SHARES-BLOCKS-P) is not tried, and so
not written to."
  (when *clone-large-files*
    (let ((directory (parent-path path)))
      (when (and (writable-p directory) (shares-blocks-p directory))
        (let ((clone (tuatara.runtime:ignoring-errors-reported ("making a clone of ~a" path)
                       (open-scratch-file directory))))
          (when clone
            (if (clone-fd fd clone)
                clone
                (progn (sb-posix:close clone) nil))))))))

(defvar *block-sharing-devices* (make-hash-table :synchronized t)
  "For each file system tried by SHARES-BLOCKS-P, by its device number, T when
it shares blocks between files and NIL when it does not, kept for the
session.")

(defun shares-blocks-p (directory)
  "True when the file system holding DIRECTORY shares blocks between files.
Only the kinds of file system that can (*BLOCK-SHARING-FILE-SYSTEMS*) are
tried, once for each, by cloning one new unnamed file into another in the
first directory on it that may be written in (*BLOCK-SHARING-DEVICES*): an XFS
made without reflink shares none. Until then, as its kind says."
  (and (member (path-file-system directory) *block-sharing-file-systems*)
       (let ((device (let ((stamp (path-stamp directory :failure nil))) (and stamp (stamp-dev stamp)))))
         (multiple-value-bind (shares known) (if device (gethash device *block-sharing-devices*) (values nil nil))
           (if known
               shares
               (let ((tried (and device (try-sharing-blocks directory))))
                 (if tried
                     (setf (gethash device *block-sharing-devices*) (eq tried :shares))
                     t)))))))

(defun try-sharing-blocks (directory)
  ":SHARES when one new unnamed file in DIRECTORY can be cloned into another,
:APART when it cannot, NIL when it could not be tried: the directory may not be
written in, or its file system has no unnamed files. Nothing is left behind."
  (handler-case
      (call-with-directory
       directory
       (lambda (dirfd)
         (let ((source (open-unnamed-at dirfd #o600)))
           (when source
             (unwind-protect
                  (let ((dest (open-unnamed-at dirfd #o600)))
                    (when dest
                      (unwind-protect (if (clone-fd source dest) :shares :apart)
                        (sb-posix:close dest))))
               (sb-posix:close source)))))
       "try sharing blocks in")
    (file-system-error () nil)))

(defun copies-directory (directory local room)
  "Where copies and scratch files serving a file in DIRECTORY go, ROOM octets
of them or so: DIRECTORY itself when it is LOCAL (on this machine), takes new
files and its file system shares blocks (btrfs, XFS), where a copy is a clone
taking no time and no space, and a save can share scratch files' blocks. Else
the editor's cache directory, when it has ROOM and 1 GB more free: a copy
written back to the same drive, such as a USB stick with exFAT, is slower,
and is gone with the drive when it is unplugged. Else DIRECTORY, when it is
LOCAL and takes new files; else the cache directory. Never /tmp, which may be
memory."
  (cond ((not (and directory local (writable-p directory))) (cache-directory))
        ((shares-blocks-p directory) directory)
        ((let ((free (free-octets (cache-directory))))
           (and free (> free (+ room *snapshot-free-margin*))))
         (cache-directory))
        (t directory)))

(defun scratch-directory (path &key (room 0))
  "Where scratch files serving the file at PATH go (the runs of a sort,
rewritten text), ROOM octets of them or so: see COPIES-DIRECTORY. PATH may be
NIL, for a buffer without a file."
  (let ((directory (and path (directory-pathname (parent-path path)))))
    (copies-directory directory (and directory (eq (directory-class directory) :local)) room)))

(defun buffer-plaintext-p (buffer)
  "True when BUFFER's text, decrypted, is kept from the disk and from other
programs: auto-save, the session, scratch files, the git gutter, the
clipboard, exports and code files leave it alone."
  (tuatara.buffer:buffer-local buffer :plaintext))

(defun (setf buffer-plaintext-p) (value buffer)
  "Keep BUFFER's text from the disk and other programs when VALUE is true
\(BUFFER-PLAINTEXT-P)."
  (setf (tuatara.buffer:buffer-local buffer :plaintext) value))

(defun buffer-scratch-directory (buffer)
  "Where the scratch files of work on BUFFER's text go (the runs of a sort,
rewritten text). It is as SCRATCH-DIRECTORY has it for its file and a text of
its size, except for a file visited safely, whose file system stays as it was
even once editing is allowed. Its scratch files go to the cache directory. It
is NIL for an encrypted file's text, which never goes to a scratch file (its
work is done in memory, or refused)."
  (cond ((buffer-plaintext-p buffer) nil)
        ((safe-visit-p buffer) (cache-directory))
        (t (scratch-directory (tuatara.buffer:buffer-file buffer) :room (tuatara.buffer:buffer-size buffer)))))

(defun snapshot-directory (path class &key (room 0))
  "Where a copy of the file PATH goes, a snapshot or the copy made when another
program is about to write it, ROOM octets, the file being on a file system of
CLASS (FILE-SYSTEM-CLASS): see COPIES-DIRECTORY."
  (copies-directory (directory-pathname (parent-path path)) (eq class :local) room))

(defun copy-to-cache (fd size path)
  "A descriptor for an unnamed copy, made now in the cache directory, of the
first SIZE octets of the file open as file descriptor (FD), which is PATH."
  (let ((copy (open-scratch-file (cache-directory))))
    (tuatara.runtime:with-fd-closed-on-error (copy "the copy of ~a" path)
      (copy-fd-contents fd copy size :path path))
    copy))
