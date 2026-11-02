;;;; src/io/in-place.lisp --- Saving a large file by writing only what changed, in place, after a journal.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; Where the file system cannot share blocks between files (ext4, and every
;;;; one but btrfs, XFS, bcachefs and ZFS), a new file renamed over the old
;;;; costs a copy of every octet: a 10 GB file written whole for one line. A
;;;; large file the buffer reads where it is (mapped) holds most of the new
;;;; text already where it belongs, so with *SAVE-IN-PLACE*, where the changes,
;;;; with the journal below, come to less than two thirds of the file, it is
;;;; saved in place:
;;;;
;;;;   1. The octets the save overwrites or cuts off, in whole pages, are
;;;;      copied into a file with no name beside the file, after a header
;;;;      naming the file, its length and where they came from; that journal
;;;;      is flushed, named .NAME.tuatara-journal and the directory flushed.
;;;;   2. The buffer's text, and its undo history, read those octets from the
;;;;      journal from then on.
;;;;   3. The changes are written into the file, its length set and the file
;;;;      flushed.
;;;;   4. The journal's name is taken away and the directory flushed.
;;;;
;;;; The file on disk is the old one until step 1 is on disk and the new one
;;;; once step 4 is. In between, after a crash or a power cut, it may be part
;;;; old and part new, with the journal beside it: the next time Tuatara opens
;;;; the file, or saves or reverts it, it puts the old file back exactly from
;;;; the journal before reading it (RECOVER-FROM-JOURNAL), whatever
;;;; *SAVE-IN-PLACE* is by then. Other programs that read the file before then
;;;; see it part saved. A save that fails, or is
;;;; stopped, while it writes the file puts the old one back at once.
;;;; FALLOC_FL_INSERT_RANGE and COLLAPSE_RANGE, which open or close a gap of
;;;; whole blocks, are not used: an edit of text is almost never a whole
;;;; number of blocks long and at a block boundary.

(in-package #:tuatara.io)

(defvar *save-in-place* nil
  "True to save a large file by writing only what changed into the file
itself, where that is quicker than writing it whole; NIL, as standard, to
write every save as a new file put in the old one's place.

It is used on ext4 and the other file systems that cannot share blocks
between files (all but btrfs, XFS, bcachefs and ZFS, where a new file costs
no more than the change anyway), for a file of over 64 MB on a local drive,
when the edit keeps the length of the text or lies in about the last third
of the file. A 10 GB file with one line changed is then saved in a few
hundredths of a second, where writing it whole takes about a minute. An
edit nearer the start moves all that follows it, so it is saved as a new
file either way.

What such a save will overwrite is first copied into .NAME.tuatara-journal
beside the file and put on the disk; the journal goes once the changes are
on the disk too. The trade-off: should the computer crash or lose power
during such a save, another program that reads the file before Tuatara
opens it again may find it half saved. Tuatara itself always gets the old
file or the new one: the next time it opens, saves or reverts the file, it
puts the old file back exactly from the journal, whatever this setting is
then. With NIL, every program finds the old file or the new one. A file
opened safely, or in an evidence folder, is never written, so never put
back either.")

(defvar *in-place-most* 2/3
  "The most a save in place writes, journal and changes together, as a share
of the file's new length; a save that would write more writes a new file
renamed over the old instead. Written twice, once to the journal and once in
place, an octet costs more than one written once to a new file, so a save
in place is the quicker only below about this share.")

(defvar *saving-step* nil
  "NIL, or a function called with each step of a save in place as it is
reached, in order: :JOURNAL-WRITTEN, :JOURNAL-FLUSHED, :JOURNAL-NAMED,
:DIRECTORY-FLUSHED, :TEXT-MOVED, :WRITING (after each piece of the file
written), :FILE-WRITTEN, :FILE-FLUSHED, :JOURNAL-REMOVED and
:DIRECTORY-FLUSHED again. Tests stop a save at each, as a crash would.")

(defconstant +journal-version+ 1
  "The version of the journal's header this release writes and reads.")

(defparameter *journal-heading* "Tuatara save journal"
  "The words a journal's header begins with, before its version.")

(defun saving-step (step)
  "Tell *SAVING-STEP* that a save in place has reached STEP."
  (when *saving-step* (funcall *saving-step* step)))

(defun journal-path (target)
  "Where the journal of a save in place of the file TARGET is: beside it,
named .NAME.tuatara-journal."
  (format nil "~a/.~a.tuatara-journal" (string-right-trim "/" (native (parent-path target))) (native-file-name target)))

(defun memory-page-size ()
  "The size of a page of memory, which mapped octets are moved in."
  (sb-posix:getpagesize))

;;; What a save changes

(defun reads-file-p (fd existing)
  "True when the file open as file descriptor (FD) is the one EXISTING describes."
  (let ((stamp (tuatara.runtime:ignoring-errors-reported ("examining a mapped file") (fd-stamp fd "a mapped file"))))
    (and stamp (= (stamp-dev stamp) (stamp-dev existing)) (= (stamp-ino stamp) (stamp-ino existing)))))

(defun in-place-test (mappings existing)
  "A function telling of a stretch of text from a mapped file, as
WRITE-ROPE's KEPT does, whether the file EXISTING describes holds its first
octets already where they go, and how many from there are alike: those of
MAPPINGS read from that very file at the offset they go to are."
  (let ((known (make-hash-table)))
    (lambda (mapping offset length position)
      (if (member mapping mappings)
          (multiple-value-bind (fd at end) (tuatara.text:mapping-source mapping offset)
            (values (and (= offset position) (= at offset)
                         (multiple-value-bind (answer found) (gethash fd known)
                           (if found answer (setf (gethash fd known) (reads-file-p fd existing)))))
                    (min length (- end offset))))
          (values nil length)))))

(defun reads-file-elsewhere-p (rope mappings existing)
  "True when ROPE has octets from a mapping other than MAPPINGS that reads
the file EXISTING describes, which a write in place could change under it."
  (let ((seen '()))
    (tuatara.text:map-rope-sources
     (lambda (kind mapping b c)
       (declare (ignore b c))
       (when (and (eq kind :file) (not (member mapping mappings)) (not (member mapping seen)))
         (push mapping seen)
         (when (some (lambda (source) (reads-file-p (third source) existing)) (tuatara.text:mapping-sources mapping))
           (return-from reads-file-elsewhere-p t))))
     rope)
    nil))

(defun rope-changes (rope kept)
  "The stretches (START . END) of ROPE, in order and apart, a file must be
given for it to hold ROPE: all but what KEPT (see IN-PLACE-TEST) says it
holds already."
  (let ((changes '()) (position 0))
    (flet ((changed (n)
             (if (and changes (= (cdar changes) position))
                 (incf (cdar changes) n)
                 (push (cons position (+ position n)) changes))
             (incf position n)))
      (tuatara.text:map-rope-sources
       (lambda (kind a b c)
         (ecase kind
           (:memory (changed (- c b)))
           (:file (let ((offset b) (length c))
                    (loop while (plusp length)
                          do (multiple-value-bind (in-place n) (funcall kept a offset length position)
                               (if in-place (incf position n) (changed n))
                               (incf offset n) (decf length n)))))))
       rope))
    (nreverse changes)))

(defun changes-test (changes)
  "A function as WRITE-ROPE's KEPT that says a stretch is in place but where
CHANGES (from ROPE-CHANGES, in order) has it; asked of stretches in order."
  (lambda (mapping offset length position)
    (declare (ignore mapping offset))
    (loop while (and changes (<= (cdar changes) position)) do (pop changes))
    (let ((change (first changes)))
      (cond ((null change) (values t length))
            ((< position (car change)) (values t (min length (- (car change) position))))
            (t (values nil (min length (- (cdr change) position))))))))

(defun journal-ranges (changes old-size new-size)
  "The stretches (START . END) of a file of OLD-SIZE octets that a save of
CHANGES, leaving it NEW-SIZE long, overwrites or cuts off, in whole pages,
in order and apart."
  (let ((page (memory-page-size)) (ranges '()))
    (dolist (change (append changes (and (< new-size old-size) (list (cons new-size old-size)))))
      (let ((start (* page (floor (car change) page)))
            (end (* page (ceiling (cdr change) page))))
        (when (< start old-size)
          (if (and ranges (<= start (cdar ranges)))
              (setf (cdar ranges) (max end (cdar ranges)))
              (push (cons start end) ranges)))))
    (nreverse ranges)))

(defun ranges-octets (ranges)
  "The octets the stretches RANGES (START . END) cover."
  (loop for (start . end) in ranges sum (- end start)))

;;; The journal

(defun journal-header (existing ranges data)
  "The header of the journal of a save in place of the file EXISTING
describes, keeping RANGES of it from offset DATA on, as octets."
  (tuatara.text:string-octets
   (format nil "~a ~d~%data ~d~%file ~d ~d~%size ~d~%time ~d~%~:{range ~d ~d~%~}end~%"
           *journal-heading* +journal-version+ data (stamp-dev existing) (stamp-ino existing) (stamp-size existing)
           (stamp-mtime existing) (mapcar (lambda (range) (list (car range) (cdr range))) ranges))))

(defun journal-header-lines (fd path)
  "The lines of the header of the journal open as file descriptor (FD): its
second line says how long the header is (\"data N\"), up to 256 MB. PATH
names it in messages."
  (flet ((head (octets)
           (let* ((head (make-array octets :element-type '(unsigned-byte 8)))
                  (got (read-fd-at fd head 0 :path path)))
             (uiop:split-string (tuatara.text:octets-string (subseq head 0 got)) :separator '(#\Newline)))))
    (let* ((start (head 4096))
           (data (second start))
           (octets (and data (uiop:string-prefix-p "data " data)
                        (every #'digit-char-p (subseq data 5)) (< 5 (length data) 15)
                        (parse-integer data :start 5))))
      (if (and octets (< 4096 octets (* 256 1024 1024))) (head octets) start))))

(defun read-journal-header (fd path)
  "What the journal open as file descriptor (FD) says, as a property list:
:DEV, :INO, :SIZE and :MTIME of the file it is of, :DATA, where its octets begin,
and :RANGES, the stretches (START . END) of the file they are; NIL when it is
not a journal this release reads. PATH names it in messages."
  (let* ((lines (journal-header-lines fd path))
         (fields '()) (ranges '()))
    (flet ((numbers (line words)
             (let ((parts (uiop:split-string line :separator " ")))
               (and (= (length parts) (1+ words))
                    (every (lambda (part) (and (plusp (length part)) (every #'digit-char-p part))) (rest parts))
                    (mapcar #'parse-integer (rest parts))))))
      (unless (equal (first lines) (format nil "~a ~d" *journal-heading* +journal-version+))
        (return-from read-journal-header nil))
      (dolist (line (rest lines) nil)
        (let ((word (subseq line 0 (or (position #\Space line) (length line)))))
          (cond ((string= word "end")
                 (return (and (getf fields :dev) (getf fields :size) (getf fields :mtime) (getf fields :data)
                              (list* :ranges (nreverse ranges) fields))))
                ((string= word "file")
                 (let ((n (numbers line 2)))
                   (unless n (return nil))
                   (setf (getf fields :dev) (first n) (getf fields :ino) (second n))))
                ((member word '("size" "data" "time") :test #'string=)
                 (let ((n (numbers line 1)))
                   (unless n (return nil))
                   (setf (getf fields (cond ((string= word "size") :size) ((string= word "data") :data) (t :mtime)))
                         (first n))))
                ((string= word "range")
                 (let ((n (numbers line 2)))
                   (unless (and n (< (first n) (second n))) (return nil))
                   (push (cons (first n) (second n)) ranges)))
                (t (return nil))))))))

(defun write-journal (file existing ranges directory target cancel progress)
  "Copy RANGES of the file open as FILE, which EXISTING describes, into a new
journal with no name in DIRECTORY, after its header, and flush it. Returns
its descriptor, open for reading and writing, and where its octets begin;
NIL when CANCEL's car turned true first. PROGRESS is called with the octets
copied so far."
  (let* ((journal (or (open-unnamed-file directory #o600)
                      (error 'file-system-error :operation "save" :path target
                                                :reason "its directory takes no file without a name")))
         (header (journal-header existing ranges 0))
         (data (* (memory-page-size) (ceiling (+ (length header) 64) (memory-page-size))))
         (done 0) (kept nil))
    (unwind-protect
         (progn
           (write-octets journal (journal-header existing ranges data) :path target)
           (loop for (start . end) in ranges
                 for length = (- (min end (stamp-size existing)) start)
                 do (sb-posix:lseek journal (+ data start) sb-posix:seek-set)
                    (unless (copy-fd-contents file journal length :from start :path target :cancel cancel
                                              :progress (and progress (lambda (n) (funcall progress (+ done n)))))
                      (return-from write-journal nil))
                    (incf done length))
           ;; whole pages, the last too, for the text to read them
           (sb-posix:ftruncate journal (+ data (if ranges (cdar (last ranges)) 0)))
           (saving-step :journal-written)
           (flush-fd journal target)
           (saving-step :journal-flushed)
           (setf kept t)
           (values journal data))
      (unless kept
        (tuatara.runtime:ignoring-errors-reported ("closing the journal of ~a" target) (sb-posix:close journal))))))

(defun name-journal (journal directory target)
  "Name JOURNAL, a descriptor of an unnamed file in DIRECTORY, as TARGET's
journal and flush the directory. NIL when the system will not name it or a
journal of TARGET is there already."
  (call-with-folder directory
                    (lambda (dirfd)
                      (and (zerop (sb-alien:alien-funcall
                                   (sb-alien:extern-alien "linkat" (function sb-alien:int sb-alien:int sb-alien:c-string
                                                                             sb-alien:int sb-alien:c-string sb-alien:int))
                                   -100 (format nil "/proc/self/fd/~d" journal) dirfd
                                   (native-file-name (journal-path target)) #x400)) ; AT_SYMLINK_FOLLOW
                           (progn (saving-step :journal-named)
                                  (sync-directory directory)
                                  (saving-step :directory-flushed)
                                  t)))
                    "save in"))

(defun remove-journal (directory target)
  "Take away TARGET's journal's name and flush the directory: what it kept is
not needed any more."
  (call-with-folder directory
                    (lambda (dirfd)
                      (unless (unlink-entry-at dirfd (native-file-name (journal-path target)))
                        (fail "remove the journal of" target)))
                    "save in")
  (saving-step :journal-removed)
  (sync-directory directory)
  (saving-step :directory-flushed))

(defun put-back (file journal header target)
  "Put the octets JOURNAL kept back into FILE, open for writing, and give it
its old length and modification time again: the file before the save,
exactly. HEADER says what JOURNAL keeps (READ-JOURNAL-HEADER). Flushes FILE."
  (let ((size (getf header :size)) (data (getf header :data)))
    (loop for (start . end) in (getf header :ranges)
          for length = (- (min end size) start)
          when (plusp length)
            do (sb-posix:lseek file start sb-posix:seek-set)
               (copy-fd-contents journal file length :from (+ data start) :path target))
    (sb-posix:ftruncate file size)
    (set-fd-modification-time file (getf header :mtime))
    (flush-fd file target)))

;;; Saving in place

(defun move-text-onto-journal (mappings existing journal data ranges)
  "Make what MAPPINGS read of the file EXISTING describes within RANGES read
from JOURNAL instead, whose octets begin at DATA."
  (dolist (mapping mappings)
    (let ((fd nil))
      (loop for (start end source delta) in (tuatara.text:mapping-sources mapping)
            when (and (zerop delta) (reads-file-p source existing))
              do (loop for (from . to) in ranges
                       for low = (max start from) and high = (min end to)
                       when (< low high)
                         do (unless fd (setf fd (sb-posix:dup journal)))
                            (unless (tuatara.text:retarget-mapping-range mapping fd low high data)
                              (sb-posix:close fd)
                              (return-from move-text-onto-journal)))))))

(defun in-place-plan (rope existing mappings)
  "How a save in place of ROPE over the file EXISTING describes would go, as a
property list (:CHANGES and :RANGES, the stretches written and journalled),
when that writes less than *IN-PLACE-MOST* of the file; else NIL. MAPPINGS
are those reading the file's own octets."
  (unless (reads-file-elsewhere-p rope mappings existing)
    (let* ((changes (rope-changes rope (in-place-test mappings existing)))
           (size (tuatara.text:rope-bytes rope))
           (ranges (journal-ranges changes (stamp-size existing) size)))
      (when (< (+ (ranges-octets changes) (ranges-octets ranges)) (* *in-place-most* (max 1 size)))
        (list :changes changes :ranges ranges)))))

(defun write-changes-in-place (rope target existing plan &key mappings cancel progress)
  "Save ROPE over TARGET, which EXISTING describes, by writing only the
changes PLAN (from IN-PLACE-PLAN) gives, in place, after a journal of what
they overwrite (this file's header); MAPPINGS read TARGET's own octets and
read the journal for those after. Returns :DONE; :STOPPED with the file as
it was; or NIL, the file untouched, when a journal of it is there already. A
failure puts the file back as it was before it is signalled."
  (let* ((directory (parent-path target))
         (changes (getf plan :changes)) (ranges (getf plan :ranges))
         (size (tuatara.text:rope-bytes rope))
         (journalled (ranges-octets ranges))
         (changed (ranges-octets changes))
         (file (open-for-reading target t :accept '(:regular) :operation "save"))
         (journal nil) (data 0) (named nil) (out nil) (leases '()))
    (labels ((report (octets)
               ;; counted in the text's octets, as every save's progress is
               (when progress (funcall progress (floor (* size octets) (max 1 (+ journalled changed))) size)))
             (give-back ()
               ;; the file as it was and no journal; said when that cannot be
               (when named
                 (setf named nil)
                 (unless (tuatara.runtime:ignoring-errors-reported ("putting back ~a from its journal" target)
                           (when out
                             (put-back out journal (list :size (stamp-size existing) :mtime (stamp-mtime existing)
                                                  :data data :ranges ranges) target))
                           (remove-journal directory target)
                           t)
                   (tell-writing (format nil "The save of ~a failed part way; its old content is kept in ~a and put back when it is next opened"
                                         (native target) (journal-path target)))))))
      (unwind-protect
           (handler-bind ((error (lambda (e) (declare (ignore e)) (give-back))))
             (unless (same-content-p existing (fd-stamp file target))
               (error 'file-changed-on-disk :path target))
             (multiple-value-setq (journal data) (write-journal file existing ranges directory target cancel #'report))
             (unless journal
               (return-from write-changes-in-place :stopped))
             ;; a journal there already is another save's: this one writes a new file instead
             (unless (name-journal journal directory target)
               (return-from write-changes-in-place nil))
             (setf named t)
             (move-text-onto-journal mappings existing journal data ranges)
             (saving-step :text-moved)
             (setf leases (step-aside-leases mappings)
                   out (handler-case (sb-posix:open (native target) (logior sb-posix:o-wronly +o-nofollow+ +o-cloexec+))
                         (sb-posix:syscall-error (e) (fail "open for writing" target (sb-posix:syscall-errno e)))))
             (check-writing target "save" :fd out)
             (unless (write-rope out rope :path target :cancel cancel :kept (changes-test changes)
                                          :progress (lambda (done total)
                                                      (saving-step :writing)
                                                      (report (+ journalled (floor (* changed done) (max 1 total))))))
               (give-back)
               (return-from write-changes-in-place :stopped))
             (sb-posix:ftruncate out size)
             (saving-step :file-written)
             (flush-fd out target)
             (saving-step :file-flushed)
             (remove-journal directory target)
             (setf named nil)
             (report (+ journalled changed))
             :done)
        (when out (tuatara.runtime:ignoring-errors-reported ("closing ~a" target) (sb-posix:close out)))
        (take-back-leases leases)
        (when journal (tuatara.runtime:ignoring-errors-reported ("closing the journal of ~a" target)
                        (sb-posix:close journal)))
        (sb-posix:close file)))))

;;; A save cut short

(defun recover-from-journal (fd path &key safe)
  "Put back the file open as file descriptor (FD), at PATH, as it was before
a save in place that was cut short (by a crash or a power cut), when its
journal is beside it: the file is then exactly the old one, and the journal
gone. Said in words (TELL-WRITING). Nothing is written, and that is said,
for a file opened SAFE (the journal not even read), where nothing may be
written (WRITING-REFUSAL), where the system refuses, and for a journal of
another file or one this release cannot read. True when the file was put
back."
  (let* ((target (or (fd-real-path fd) (native path)))
         (journal-path (journal-path target)))
    (when (path-stamp journal-path :follow nil :failure nil)
      (let ((refusal (if safe "it is opened safely" (writing-refusal target :fd fd))))
        (when refusal
          (tell-writing (format nil "~a may be part saved: a save of it was cut short, and its journal ~a is not used here, as ~a"
                                target journal-path refusal))
          (return-from recover-from-journal nil)))
      (handler-case
          (let ((journal (handler-case (sb-posix:open journal-path (logior sb-posix:o-rdonly +o-nofollow+ +o-cloexec+))
                           (sb-posix:syscall-error (e) (fail "read" journal-path (sb-posix:syscall-errno e))))))
            (unwind-protect
                 (let ((header (read-journal-header journal journal-path))
                       (stamp (fd-stamp fd path)))
                   (if (or (null header)
                           (/= (getf header :dev) (stamp-dev stamp)) (/= (getf header :ino) (stamp-ino stamp)))
                       (progn
                         (tell-writing (format nil "~a is not the journal of a save of ~a this release can put back: left as it is"
                                               journal-path target))
                         nil)
                       (let ((out (handler-case (sb-posix:open target (logior sb-posix:o-wronly +o-nofollow+ +o-cloexec+))
                                    (sb-posix:syscall-error (e) (fail "open for writing" target (sb-posix:syscall-errno e))))))
                         (unwind-protect
                              (progn (check-writing target "put back" :fd out)
                                     (put-back out journal header target))
                           (sb-posix:close out))
                         (remove-journal (parent-path target) target)
                         (tell-writing (format nil "A save of ~a was cut short: the file is put back as it was before it" target))
                         t)))
              (sb-posix:close journal)))
        (file-system-error (e)
          (tell-writing (format nil "~a may be part saved: a save of it was cut short, and it could not be put back from ~a: ~a"
                                target journal-path (file-system-error-reason e)))
          nil)))))

(defun recover-before-saving (target existing)
  "Put TARGET, which EXISTING describes, back as it was before a save in
place of it that was cut short, when its journal is beside it, before it is
saved again (RECOVER-FROM-JOURNAL): its stamp then, or NIL when nothing was
put back."
  (when (and existing (eq (stamp-kind existing) :regular)
             (path-stamp (journal-path target) :follow nil :failure nil))
    (let ((fd (open-for-reading target t :accept '(:regular) :operation "save")))
      (unwind-protect (and (recover-from-journal fd target) (fd-stamp fd target))
        (sb-posix:close fd)))))
