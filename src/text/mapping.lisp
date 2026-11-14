;;;; src/text/mapping.lisp --- Files mapped into memory, read outside the Lisp heap.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.text)

;;; Mapped files

(define-record (mapping (:constructor %make-mapping (sap length path fd resources
                                                   &aux (sources (list (list 0 length fd 0)))))
                       (:copier nil))
  "A memory-mapped file. SAP addresses LENGTH octets of PATH, read-only,
outside the Lisp heap, mapped from the open file descriptor (FD), which the
mapping keeps (so saving can copy straight from it).
SOURCES lists, in order, the stretches (START END FD DELTA) its octets are
read from. Those from START below END come from FD at START + DELTA.
RETARGET-MAPPING-RANGE moves one stretch to another file. LIVE turns false when
the mapping is closed.
RESOURCES is a cons whose car lists every (ADDRESS LENGTH FD) the mapping has
held. RETARGET-MAPPING adds one when the octets are moved to a snapshot, and
RETARGET-MAPPING-RANGE one with no ADDRESS, as the octets it moves lie within
a region listed already.
WANTED is the stretch (START . END) of its octets a window shows and reads
ahead (READ-AHEAD), which a pass reading the file through leaves in memory
\(WITH-READ-PASS). SEEN is where in the mapping the octets a window showed last
began. ASKED is the stretch (START . END) asked about last outside a pass
\(ASK-ABSENT). RELEASING is held while pieces of it are let go of, which
closing it waits for (LET-GO-RELEASED). PIECES is, of each piece passes
reading it through hold (HOLD-PIECE), how many hold it and which of its pages
the system held before.
HOLES is the stretches (START . END) of a file still arriving from elsewhere
that are not here yet, read as zeros by the system, which reading them refuses
\(NOT-ARRIVED), a new list each time, never changed in place. TORN is true when
the file changed where it was read from while it came, so that what is here is
no one version of it, and any read of it is refused.
Of a large mapping (*LARGE-MAPPING-OCTETS*) the system reads in no more than
is asked (LARGE-MAPPING-ADVICE): its readers ask (NOTE-READ, READ-AHEAD)."
  (sap (sb-sys:int-sap 0) :type sb-sys:system-area-pointer :documentation "The foreign pointer to the mapped octets.")
  (length 0 :type fixnum :read-only t :documentation "Number of mapped octets.")
  (path nil :read-only t :documentation "The mapped file's path.")
  (fd -1 :type fixnum :documentation "The open descriptor the mapped octets came from when last moved whole.")
  (sources '() :type list :documentation "The stretches (START END FD DELTA) a mapping's octets are read from.")
  (resources (list '()) :read-only t :documentation "A cons whose car lists the (ADDRESS LENGTH FD)
the mapping holds, ADDRESS NIL for a descriptor alone.")
  (live t :type boolean :documentation "True until the mapping is closed with CLOSE-MAPPING.")
  (wanted (cons 0 0) :type cons :documentation "The stretch (START . END) of a mapping a window shows and reads ahead.")
  (seen 0 :type fixnum :documentation "Where in the mapping the octets a window showed last began.")
  (asked (cons 0 0) :type cons :documentation "The stretch (START . END) of a mapping asked about last outside a pass.")
  (holes '() :type list :documentation "The stretches of a mapping's file that have not arrived, in order.")
  (torn nil :type boolean :documentation "True when a mapping's file changed where it came from while it arrived.")
  (pieces (make-hash-table :synchronized t) :read-only t
          :documentation "The pieces of a mapping passes hold, each (HOLDERS . HELD).")
  (releasing (sb-thread:make-mutex :name "tuatara mapping let go of") :read-only t
             :documentation "Held while pieces of a mapping are let go of."))

(define-condition not-arrived (tuatara.runtime:refusal) ()
  (:documentation "Signalled for a read of text that crosses a part of a file not arrived yet (MAPPING-HOLES),
so that what the system gives for it, zeros, is never taken for the file's contents."))

(defun refuse-not-arrived ()
  "Refuse a read of a part of a file that has not arrived (NOT-ARRIVED)."
  (error 'not-arrived :format-control "Part of the file has not arrived"))

(defun check-holes (mapping start end)
  "Refuse a read of MAPPING's octets from START below END when any of them has
not arrived, or none can be trusted (MAPPING-TORN)."
  (when (mapping-torn mapping)
    (error 'not-arrived :format-control "The file changed on the server while it came"))
  (loop for (from . to) in (mapping-holes mapping)
        do (when (and (< from end) (> to start))
             (refuse-not-arrived))))

(defun release-resources (resources)
  "Unmap and close every (ADDRESS LENGTH file descriptor (FD)) in the car of
RESOURCES, once: an FD listed with several stretches is closed once, and a
region with no ADDRESS is not unmapped (it lies within another)."
  (let ((closed '()))
    (loop for (address length fd) in (shiftf (car resources) '())
          do (when address
               (tuatara.runtime:ignoring-errors-reported ("unmapping ~d octets" length)
                 (sb-posix:munmap (sb-sys:int-sap address) length)))
             (unless (member fd closed)
               (push fd closed)
               (tuatara.runtime:close-fd-quietly fd "the mapped file's descriptor ~d" fd)))))

(sb-ext:defglobal **mapping-lock** (sb-thread:make-mutex :name "tuatara mappings")
  "Held while a mapping is moved or closed, which threads other than the
editor's may do.")

(sb-ext:defglobal **live-mappings** (make-hash-table :test 'eq :weakness :key)
  "The mappings made and not yet closed, as keys; one nothing refers to any
more goes by itself. Changed with **MAPPING-LOCK** held.")

(defun live-mappings ()
  "The mappings made and not closed yet, which may be reading any file."
  (sb-thread:with-mutex (**mapping-lock**)
    (loop for mapping being the hash-keys of **live-mappings**
          when (mapping-live mapping) collect mapping)))

(defun map-fd (fd size path)
  "Map SIZE octets of the file open as file descriptor (FD) read-only and
return a MAPPING that owns FD from now on, closing it with the mapping.
PATH names the file in messages. The mapping is released when it is garbage
collected or closed explicitly. Of a large file the system reads in no more
than is asked (LARGE-MAPPING-ADVICE)."
  (declare (type fixnum fd size))
  (let* ((sap (sb-posix:mmap nil size sb-posix:prot-read sb-posix:map-private fd 0))
         (resources (list (list (list (sb-sys:sap-int sap) size fd))))
         (mapping (%make-mapping sap size path fd resources)))
    (large-mapping-advice mapping 0 size)
    (sb-thread:with-mutex (**mapping-lock**)
      (setf (gethash mapping **live-mappings**) t))
    (sb-ext:finalize mapping (lambda () (release-resources resources)) :dont-save t)
    mapping))

(defun map-file (path)
  "Map the regular file at PATH read-only and return a MAPPING, or NIL for an
empty file; an error for anything else, a named pipe or a device, which is
never waited on. The mapping keeps the file open and is released when it is
garbage collected or closed explicitly."
  (let ((fd (sb-posix:open (if (pathnamep path) (sb-ext:native-namestring path) path)
                           (logior sb-posix:o-rdonly sb-posix:o-nonblock)))
        (mapped nil))
    (unwind-protect
         (let ((stat (sb-posix:fstat fd)))
           (unless (= (logand (sb-posix:stat-mode stat) sb-posix:s-ifmt) sb-posix:s-ifreg)
             (error "~a is not a regular file." path))
           (unless (zerop (sb-posix:stat-size stat))
             (setf mapped (map-fd fd (sb-posix:stat-size stat) path))))
      (unless mapped (sb-posix:close fd)))))

(defun retarget-mapping (mapping fd)
  "Make MAPPING read its octets from file descriptor (FD), an open file
holding the very same octets (a snapshot of the original), so that every
rope using MAPPING is safe from changes made to the original file
afterwards. The old region stays mapped until the mapping is released, for
any reader in the middle of it. Any thread may do this. Returns MAPPING, or
NIL (FD left open) when it was closed meanwhile."
  (sb-thread:with-mutex (**mapping-lock**)
    (when (mapping-live mapping)
      (let ((sap (sb-posix:mmap nil (mapping-length mapping) sb-posix:prot-read sb-posix:map-private fd 0)))
        (push (list (sb-sys:sap-int sap) (mapping-length mapping) fd) (car (mapping-resources mapping)))
        (setf (mapping-sap mapping) sap (mapping-fd mapping) fd
              (mapping-sources mapping) (list (list 0 (mapping-length mapping) fd 0)))
        (large-mapping-advice mapping 0 (mapping-length mapping))
        mapping))))

(defun retarget-mapping-range (mapping fd start end delta)
  "Make the octets of MAPPING from START below END read from file descriptor
\(FD) at START + DELTA, an open file holding the very same octets there, so
that they are safe from changes made to where they were read before. START
and DELTA are multiples of the page size; END is one too, or MAPPING's
length; the stretch is read from one file now. The octets are swapped in
place, so a reader in the middle of them reads the one or the other, alike.
MAPPING keeps FD open from then on, and closes it with the rest. Any thread
may do this. Returns MAPPING, or NIL (FD left open) when it was closed
meanwhile. When the system will not, the stretch is read from where it was
again and the error signalled, FD left open."
  (sb-thread:with-mutex (**mapping-lock**)
    (when (mapping-live mapping)
      (let ((address (sb-sys:sap+ (mapping-sap mapping) start))
            (flags (logior sb-posix:map-private sb-posix:map-fixed)))
        (handler-bind ((sb-posix:syscall-error
                         (lambda (e)
                           (declare (ignore e))
                           (multiple-value-bind (was at) (mapping-source mapping start)
                             (tuatara.runtime:ignoring-errors-reported ("mapping ~a where it was"
                                                                        (mapping-path mapping))
                               (sb-posix:mmap address (- end start) sb-posix:prot-read flags was at))))))
          (sb-posix:mmap address (- end start) sb-posix:prot-read flags fd (+ start delta)))
        (large-mapping-advice mapping start end)
        ;; within the region mapped whole, which unmaps it
        (push (list nil 0 fd) (car (mapping-resources mapping)))
        (let ((kept '()))
          ;; each stretch keeps what lies outside the one moved
          (loop for (s e f d) in (mapping-sources mapping)
                do (when (< s start) (push (list s (min e start) f d) kept))
                   (when (> e end) (push (list (max s end) e f d) kept)))
          (setf (mapping-sources mapping) (sort (cons (list start end fd delta) kept) #'< :key #'first)))
        mapping))))

(defun mapping-source (mapping offset)
  "Where the octet at OFFSET of MAPPING is read from, as three values: the
open file descriptor (FD), the offset in that file, and the offset in MAPPING
where that file's stretch ends."
  (loop for (start end fd delta) in (mapping-sources mapping)
        when (< offset end)
          return (values fd (+ offset delta) end)
        finally (error "Offset ~d is past the end of the mapping of ~a." offset (mapping-path mapping))))

(defun close-mapping (mapping)
  "Unmap MAPPING now, once pieces of it being let go of are (LET-GO-RELEASED).
Ropes still referring to it signal an error when read."
  (sb-thread:with-mutex ((mapping-releasing mapping))
    (sb-thread:with-mutex (**mapping-lock**)
      (when (mapping-live mapping)
        (setf (mapping-live mapping) nil)
        (remhash mapping **live-mappings**)
        (sb-ext:cancel-finalization mapping)
        (release-resources (mapping-resources mapping)))))
  nil)

(declaim (inline check-mapping))
(defun check-mapping (mapping)
  "Signal an error unless MAPPING is still mapped."
  (unless (mapping-live mapping)
    (error "The mapping of ~a has been closed." (mapping-path mapping))))

;;; Telling the system what is read

(defvar *large-mapping-octets* (* 64 1024 1024)
  "Octets from which a mapped file counts as large: a pass reading all of it
leaves the memory as it found it (WITH-READ-PASS), and what a window shows of it
is read in ahead (READ-AHEAD). A smaller file is left to the system, which
keeps what it reads.")

(defvar *read-pass* nil
  "The READ-PASS what is read now belongs to (WITH-READ-PASS), or NIL. The
tasks of TUATARA.RUNTIME:RUN-TASKS read in the pass of the thread that runs
them (READ-PASS-TASK).")

(defun large-mapping-advice (mapping start end)
  "When MAPPING is large (*LARGE-MAPPING-OCTETS*), have the system read in no
more of its octets from START below END than is asked: on some file systems
\(btrfs) a page read brings in megabytes around it otherwise. Its readers ask
for what they read (NOTE-READ), and for what a window shows next
\(READ-AHEAD)."
  (when (>= (mapping-length mapping) *large-mapping-octets*)
    (advise-memory (sb-sys:sap+ (mapping-sap mapping) start) (- end start) 1)))

(defun advise-memory (sap length advice)
  "Give the system ADVICE, one of madvise(2)'s on Linux (0 to read ahead of
each page read as it sees fit, 1 to read no more than each page, 3 to read
the pages in soon, 4 to let go of them), about LENGTH octets of this
program's memory from SAP, a page boundary. Changes nothing a reader sees;
advice not taken is fine."
  (sb-alien:alien-funcall
   (sb-alien:extern-alien "madvise" (function sb-alien:int sb-sys:system-area-pointer
                                              sb-alien:unsigned-long sb-alien:int))
   sap length advice)
  nil)

(defun file-advise (fd offset length advice)
  "Give the system ADVICE, one of posix_fadvise(2)'s on Linux (0 to read ahead
as it sees fit, 1 to read no more than is asked, 4 to let go of what it
holds), about LENGTH octets from OFFSET of the file open as file descriptor
\(FD), a LENGTH of 0 for all from OFFSET on, through every descriptor that
shares its reading with FD. Changes nothing a reader sees; advice not taken is
fine."
  (sb-alien:alien-funcall
   (sb-alien:extern-alien "posix_fadvise" (function sb-alien:int sb-alien:int (sb-alien:signed 64)
                                                    (sb-alien:signed 64) sb-alien:int))
   fd offset length advice)
  nil)

(defun drop-cached (fd offset length)
  "Tell the system the octets of the file open as file descriptor (FD) from
OFFSET, LENGTH of them, are not needed in memory: those no program has mapped
leave it, any not written out yet once they are, so that they need not crowd
out what is in use."
  (when (plusp length)
    (file-advise fd offset length 4)))

(defun pages-in-memory (sap length)
  "For each page of the LENGTH octets of memory from SAP, a page boundary, 1
when the system holds it in memory and 0 when reading it would wait for the
disk, as a vector of octets (mincore(2)), asked in pieces of 64 MB in
parallel when there are several; 1 where the system does not say.
Complexity: O(LENGTH / page size), shared out between the processors."
  (let* ((page (sb-posix:getpagesize))
         (pages (make-array (ceiling length page) :element-type '(unsigned-byte 8) :initial-element 1))
         (piece (* 64 1024 1024)))
    (flet ((ask (from)
             (let ((to (min length (+ from piece))))
               (sb-sys:with-pinned-objects (pages)
                 (unless (zerop (sb-alien:alien-funcall
                                 (sb-alien:extern-alien "mincore" (function sb-alien:int sb-sys:system-area-pointer
                                                                            sb-alien:unsigned-long
                                                                            sb-sys:system-area-pointer))
                                 (sb-sys:sap+ sap from) (- to from)
                                 (sb-sys:sap+ (sb-sys:vector-sap pages) (floor from page))))
                   (fill pages 1 :start (floor from page) :end (ceiling to page))))
               ;; the other bits of each are the system's own
               (loop for i from (floor from page) below (ceiling to page)
                     do (setf (aref pages i) (logand 1 (aref pages i)))))))
      (if (<= length piece)
          (ask 0)
          ;; asking reads nothing: no pass to carry to the tasks
          (let ((*read-pass* nil))
            (tuatara.runtime:run-tasks (coerce (loop for from from 0 below length by piece
                                                     collect (let ((from from)) (lambda () (ask from))))
                                               'simple-vector)))))
    pages))

(defconstant +read-piece+ (* 2 1024 1024)
  "Octets of a mapping a pass reading it through (WITH-READ-PASS) asks the
disk for, and lets go of, together.")

(defconstant +read-pass-keeps+ 32
  "Pieces of +READ-PIECE+ a pass reading a mapping through keeps in memory
after reading them, the last read: the pieces several threads read side by
side, or one reread soon, are let go of only once well behind.")

(define-record (read-pass (:constructor make-read-pass ()) (:copier nil))
  "A pass reading large mapped files through once (WITH-READ-PASS). For each
mapping read, it holds a PASSAGE, made as it is first read. LOCK is held while
they change. USERS is how many tasks of other threads read in it
\(READ-PASS-TASK). ENDED is true once the work it was begun for is done,
FINISHED once that and its tasks are, everything it asked for let go of. Locks
are taken in this order: a pass's LOCK, a mapping's RELEASING
\(MAPPING-RELEASING), then **MAPPING-LOCK**."
  (passages '() :type list :documentation "What a READ-PASS has read of each mapping, as PASSAGEs.")
  (users 0 :type fixnum :documentation "How many tasks of other threads read in a READ-PASS.")
  (ended nil :type boolean :documentation "True once the work a READ-PASS was begun for is done.")
  (finished nil :type boolean :documentation "True once a READ-PASS and its tasks are done.")
  (lock (sb-thread:make-mutex :name "tuatara read pass") :read-only t
        :documentation "Held while a READ-PASS's passages change."))

(define-record (passage (:constructor make-passage (pass mapping)) (:copier nil))
  "What PASS, a READ-PASS, has read of MAPPING. PIECES is a table from the
number of each piece of +READ-PIECE+ it has asked for to its state, :AHEAD
\(asked for, not read yet), :READ or :LET-GO. KEPT is the numbers of the pieces
read and not let go of, the last read first. LETTING is those let go of and
not yet given back (LET-GO-PIECES). LAST is the piece read last. FRESH is what
the system held of pieces about to be asked for, taken before the pass's lock
\(PIECE-IN-MEMORY). The pieces themselves are held for all passes together
\(HOLD-PIECE)."
  (pass nil :type read-pass :read-only t :documentation "The READ-PASS a PASSAGE belongs to.")
  (mapping nil :type mapping :read-only t :documentation "The mapping a PASSAGE reads.")
  (pieces (make-hash-table :synchronized t) :read-only t
          :documentation "The pieces of its mapping a PASSAGE has asked for, by number.")
  (kept '() :type list :documentation "The pieces a PASSAGE has read and not let go of, the last first.")
  (letting '() :type list :documentation "The pieces a PASSAGE let go of and has not given back yet.")
  (last -1 :type fixnum :documentation "The piece a PASSAGE read last.")
  (fresh (make-hash-table :synchronized t) :read-only t
         :documentation "What the system held of pieces a PASSAGE is about to ask for."))

(defun read-pass-task (task)
  "TASK, a function run by TUATARA.RUNTIME:RUN-TASKS, made to read in the pass
in force on the calling thread, which is not finished before TASK is, though
the work it was begun for may be cut short first."
  (let ((pass *read-pass*))
    (if (null pass)
        task
        (progn
          (sb-thread:with-mutex ((read-pass-lock pass)) (incf (read-pass-users pass)))
          (lambda ()
            (unwind-protect (let ((*read-pass* pass)) (funcall task))
              (give-back (sb-sys:without-interrupts
                           (sb-thread:with-mutex ((read-pass-lock pass))
                             (when (and (zerop (decf (read-pass-users pass))) (read-pass-ended pass))
                               (finish-read-pass pass)))))))))))

(pushnew 'read-pass-task tuatara.runtime:*task-wrappers*)

(defmacro with-read-pass (() &body body)
  "Run BODY as a pass reading large mapped files through once. What it reads
of a large mapping (*LARGE-MAPPING-OCTETS*) through its spans, on this thread
and the tasks it runs, is asked of the disk a piece at a time ahead of the
reading. It is let go of again once well behind it and when BODY is done, the
system letting go too of the pages it did not hold before and no window wants
\(MAPPING-WANTED). Meanwhile the system reads in no more of the mapping than is
asked. So a file larger than the memory read through leaves in memory what was
there, the program's own memory and other programs' included. Within another
pass, BODY reads in that one."
  `(call-with-read-pass (lambda () ,@body)))

(defun call-with-read-pass (function)
  "Call FUNCTION as WITH-READ-PASS runs its body, and return what it returns."
  (if *read-pass*
      (funcall function)
      (let ((pass (make-read-pass)))
        (unwind-protect (let ((*read-pass* pass)) (funcall function))
          ;; never in place of what the work itself signalled
          (tuatara.runtime:ignoring-errors-reported ("ending a pass through a file") (end-read-pass pass))))))

(pushnew 'call-with-read-pass tuatara.runtime:*around-long-work*)

(defun note-read (mapping start end)
  "Say that the octets of MAPPING from START below END are about to be read.
In the pass in force (*READ-PASS*), each piece of them not read in it yet is
asked of the disk first, with the pieces beside it, and once more than
+READ-PASS-KEEPS+ pieces are read, the one read longest ago is let go of.
Outside a pass, the pages of the stretch not in memory are asked for
together, as the system reads in no more than is asked of a large mapping.
Nothing for a mapping that is not large. Complexity: O(1) for each piece of
the stretch read before in the pass; outside one, O(pages of the stretch)."
  (declare (type fixnum start end))
  (let ((pass *read-pass*) (end (min end (mapping-length mapping))))
    (when (and (< start end) (>= (mapping-length mapping) *large-mapping-octets*))
      (let ((passage (and pass
                          (or (find mapping (read-pass-passages pass) :key #'passage-mapping)
                              (sb-sys:without-interrupts
                                (sb-thread:with-mutex ((read-pass-lock pass))
                                  (unless (read-pass-finished pass)
                                    (or (find mapping (read-pass-passages pass) :key #'passage-mapping)
                                        (begin-passage pass mapping)))))))))
        (if (null passage)
            (ask-absent mapping start end)
            (loop for piece from (floor start +read-piece+) to (floor (1- end) +read-piece+)
                  ;; read in this pass already: nothing to ask (looked at without the lock; asked again with it)
                  unless (or (= piece (passage-last passage)) (eq :read (gethash piece (passage-pieces passage))))
                    do (note-fresh passage piece)
                       ;; the pass's records changed without being cut short, the disk asked after
                       (ask-pieces mapping (sb-sys:without-interrupts
                                             (sb-thread:with-mutex ((read-pass-lock pass))
                                               (unless (read-pass-finished pass)
                                                 (read-piece passage piece)))))
                  ;; what it let go of given back after its lock, which the pass's other readers wait for
                  finally (when (>= (length (passage-letting passage)) +read-pass-keeps+)
                            (let-go-pieces passage))))))))

(defun note-fresh (passage piece)
  "Take what the system holds of PIECE of PASSAGE's mapping and the pieces
beside it that PASSAGE has not asked for (PASSAGE-FRESH), before the pass's
lock is taken: taking it waits for nothing, but every reader of the pass
would wait for it under the lock."
  (let ((mapping (passage-mapping passage)))
    (dolist (next (list piece (1+ piece) (1- piece)))
      (when (and (>= next 0) (< (* next +read-piece+) (mapping-length mapping))
                 (not (member (gethash next (passage-pieces passage)) '(:ahead :read)))
                 (null (gethash next (passage-fresh passage))))
        (let ((held (piece-in-memory mapping next)))
          (sb-sys:without-interrupts (setf (gethash next (passage-fresh passage)) held)))))))

(defconstant +kept-ends+ (* 4 1024 1024)
  "Octets at each end of a large mapping that a pass reading it through never
lets the system go of (LET-GO-RELEASED): going to the start or the end of a
file is common, its end most of all in a log, and is not to wait for the
disk after a pass.")

(defconstant +read-around+ (* 1024 1024)
  "Octets on each side of a read outside a pass, not in memory, that are asked
for with it (ASK-ABSENT): a scan going on from there waits for the disk once
for each 2 MB, not once a page.")

(defun ask-absent (mapping start end)
  "Have the pages of MAPPING from START below END read in from the disk at once
with those +READ-AROUND+ about them, when any of them is not in memory; else
nothing. Not asked about within the stretch asked about last (MAPPING-ASKED)
or within what a window reads ahead (MAPPING-WANTED), as the octets a window
shows are: those cost two comparisons. Makes no garbage for a stretch of up
to 128 KB; a longer one is asked for whole."
  (declare (type fixnum start end))
  (let ((asked (mapping-asked mapping)) (wanted (mapping-wanted mapping)))
    (unless (or (and (<= (the fixnum (car asked)) start) (<= end (the fixnum (cdr asked))))
                (and (<= (the fixnum (car wanted)) start) (<= end (the fixnum (cdr wanted)))))
      (let* ((page (sb-posix:getpagesize))
             (from (* page (floor start page)))
             (length (- end from))
             (sap (sb-sys:sap+ (mapping-sap mapping) from))
             (around-from (max 0 (* page (floor (- from +read-around+) page))))
             (around-to (min (mapping-length mapping) (+ end +read-around+))))
        (if (> length (* 32 page))
            (advise-memory sap length 3)
            (let ((pages (make-array 32 :element-type '(unsigned-byte 8) :initial-element 1)))
              (declare (dynamic-extent pages))
              (sb-sys:with-pinned-objects (pages)
                (sb-alien:alien-funcall
                 (sb-alien:extern-alien "mincore" (function sb-alien:int sb-sys:system-area-pointer
                                                            sb-alien:unsigned-long sb-sys:system-area-pointer))
                 sap length (sb-sys:vector-sap pages)))
              (if (loop for i below (ceiling length page) thereis (evenp (aref pages i)))
                  (progn (advise-memory (sb-sys:sap+ (mapping-sap mapping) around-from) (- around-to around-from) 3)
                         (setf from around-from length (- around-to around-from)))
                  (setf length (* page (ceiling length page))))))
        (setf (car asked) from (cdr asked) (+ from length))))))

(defun begin-passage (pass mapping)
  "A PASSAGE of PASS through MAPPING, kept in PASS. Called with PASS's lock
held."
  (let ((passage (make-passage pass mapping)))
    (push passage (read-pass-passages pass))
    passage))

(defun piece-in-memory (mapping piece)
  "For each page of PIECE of MAPPING, a piece of +READ-PIECE+, a bit: 1 when the
system holds it in memory (PAGES-IN-MEMORY)."
  (let ((from (* piece +read-piece+)))
    (coerce (pages-in-memory (sb-sys:sap+ (mapping-sap mapping) from)
                             (min +read-piece+ (- (mapping-length mapping) from)))
            'simple-bit-vector)))

(defconstant +late-looks+ 4
  "How many times pieces let go of are looked at again for pages the disk
brought in afterwards (LOOK-LATER), as long as each look finds some.")

(defparameter *late-look-seconds* 1/20
  "Seconds after pieces are let go of that they are first looked at again
\(LOOK-LATER); each look after waits twice as long as the one before.")

(define-record (late-look (:constructor make-late-look (mapping pieces time)) (:copier nil))
  "PIECES of MAPPING, each (PIECE . HELD), let go of while the disk may still
have been reading them, to be let go of again at TIME, an internal real time,
after LOOKS looks so far (LOOK-LATER)."
  (mapping nil :read-only t :documentation "The mapping a LATE-LOOK lets go of pieces of.")
  (pieces '() :documentation "The pieces a LATE-LOOK lets go of again, each (PIECE . HELD).")
  (time 0 :documentation "The internal real time a LATE-LOOK is due.")
  (looks 0 :documentation "How many times a LATE-LOOK has looked."))

(sb-ext:defglobal **late-looks** '()
  "The LATE-LOOKs to come, guarded by **MAPPING-LOCK**.")

(sb-ext:defglobal **closing-later** '()
  "Mappings to close once no LATE-LOOK refers to them (CLOSE-MAPPING-LATER),
guarded by **MAPPING-LOCK**.")

(sb-ext:defglobal **late-wake** (sb-thread:make-semaphore :name "tuatara letting go late")
  "Signalled when a LATE-LOOK is added.")

(sb-ext:defglobal **late-thread** nil
  "The thread looking at pieces again (LATE-LOOK-LOOP), started the first time
one is let go of, and again when it has ended; NIL before.")

(defun hold-piece (mapping piece held)
  "Note that a pass holds PIECE of MAPPING, a piece of +READ-PIECE+, until
FORGET-PIECES. The first to hold it keeps HELD (PIECE-IN-MEMORY, taken before
any pass asked for it) in MAPPING-PIECES. One taking it while it is let go
of, or soon after (TAKE-LATE-LOOK), keeps what was held before. So passes
reading it side by side or one after another let go of what none of them
found there. Called with **MAPPING-LOCK** held."
  (let ((entry (or (gethash piece (mapping-pieces mapping))
                   (setf (gethash piece (mapping-pieces mapping)) (cons 0 (or (take-late-look mapping piece) held))))))
    (incf (car entry))))

(defun take-late-look (mapping piece)
  "What the system held of PIECE of MAPPING before it was asked for, when the
piece was let go of lately and is still to be looked at again (LOOK-LATER):
the look is left to the pass taking it now, as what the disk brings in
meanwhile was not held before. NIL when it is not. Called with
**MAPPING-LOCK** held."
  (dolist (look **late-looks**)
    (when (eq mapping (late-look-mapping look))
      (let ((entry (assoc piece (late-look-pieces look))))
        (when entry
          (setf (late-look-pieces look) (remove entry (late-look-pieces look)))
          (return (cdr entry)))))))

(defun drop-unheld (mapping piece held &optional present)
  "Have the system let go of the pages of PIECE of MAPPING it did not hold
before (HELD, PIECE-IN-MEMORY), outside what a window wants (MAPPING-WANTED)
and the ends of the file (+KEPT-ENDS+), from the files they are read from;
with PRESENT, only of those it holds now. True when PRESENT and some were let
go of. Called with MAPPING's RELEASING held, MAPPING live."
  (let* ((page (sb-posix:getpagesize))
         (wanted (mapping-wanted mapping))
         (from (* piece +read-piece+))
         (end (min (mapping-length mapping) (+ from +read-piece+)))
         (now (and present (pages-in-memory (sb-sys:sap+ (mapping-sap mapping) from) (- end from))))
         (dropped nil)
         (run nil))
    (flet ((drop (a b)
             ;; outside the file's ends and what a window wants, from each file the octets are read from
             (loop for (s e) on (let ((a (max a +kept-ends+))
                                      (b (min b (- (mapping-length mapping) +kept-ends+))))
                                  (list a (min b (car wanted)) (max a (cdr wanted)) b))
                   by #'cddr
                   do (loop while (< s e)
                            do (multiple-value-bind (fd at stretch-end) (mapping-source mapping s)
                                 (let ((stop (min e stretch-end)))
                                   (drop-cached fd at (- stop s))
                                   (setf dropped t s stop)))))))
      (loop for i from 0 below (min (length held) (ceiling (- end from) page))
            for at = (+ from (* i page))
            do (if (and (zerop (sbit held i)) (or (null now) (= 1 (aref now i))))
                   (unless run (setf run at))
                   (when run (drop run at) (setf run nil))))
      (when run (drop run end))
      (and present dropped))))

(defun let-go-released (mapping released &optional lately)
  "Let go of RELEASED, pieces of MAPPING no pass holds now (FORGET-PIECES), each
\(PIECE . HELD): of those the system did not hold whole before, this program
lets go of all, a run of pieces one after another at a time, and the system
of the pages it did not hold before (DROP-UNHELD); then their records are
forgotten, unless a pass holds one again. LATELY lists the pieces among them
asked of the disk lately, which may still be on their way: they are let go
of again once they have come (LOOK-LATER). Nothing cuts it short
\(WITHOUT-INTERRUPTS), so that no page read is left held. Called with no
pass's lock held; it waits for no other reader: only closing MAPPING waits
for it (MAPPING-RELEASING)."
  (let ((free (sort (remove-if-not (lambda (entry) (find 0 (the simple-bit-vector (cdr entry)))) released)
                    #'< :key #'car)))
    (flet ((piece-end (piece) (min (mapping-length mapping) (* (1+ piece) +read-piece+))))
      (sb-sys:without-interrupts
        (unwind-protect
             (sb-thread:with-mutex ((mapping-releasing mapping))
               ;; a mapping closed meanwhile is unmapped, and its addresses may be another's
               (when (mapping-live mapping)
                 ;; this program lets go of each run of pieces one after another at once
                 (loop with rest = free
                       while rest
                       do (let* ((first (car (first rest))) (last first))
                            (loop while (and (cdr rest) (= (car (second rest)) (1+ last)))
                                  do (setf rest (cdr rest) last (car (first rest))))
                            (setf rest (cdr rest))
                            (advise-memory (sb-sys:sap+ (mapping-sap mapping) (* first +read-piece+))
                                           (- (piece-end last) (* first +read-piece+)) 4)))
                 (loop for (piece . held) in free
                       do (drop-unheld mapping piece held))))
          (sb-thread:with-mutex (**mapping-lock**)
            (dolist (entry released)
              (let ((now (gethash (car entry) (mapping-pieces mapping))))
                (when (and now (zerop (car now)))
                  (remhash (car entry) (mapping-pieces mapping))))))
          (let ((later (remove-if-not (lambda (entry) (member (car entry) lately)) free)))
            (when later
              (look-later mapping later))))))))

(defun look-later (mapping pieces)
  "Have PIECES of MAPPING, each (PIECE . HELD), let go of again once the disk
has done reading what was asked of them: what it brings in after they were
let go of is let go of then (LATE-LOOK-LOOP)."
  (sb-thread:with-mutex (**mapping-lock**)
    (push (make-late-look mapping pieces (+ (get-internal-real-time)
                                            (round (* *late-look-seconds* internal-time-units-per-second))))
          **late-looks**)
    (let ((thread **late-thread**))
      (unless (and thread (sb-thread:thread-alive-p thread))
        (setf **late-thread** (tuatara.runtime:make-guarded-thread #'late-look-loop :name "tuatara letting go late")))))
  (sb-thread:signal-semaphore **late-wake**))

(defun close-mapping-later (mapping)
  "Close MAPPING (CLOSE-MAPPING) once no LATE-LOOK refers to it, now when none does."
  (when (sb-thread:with-mutex (**mapping-lock**)
          (if (find mapping **late-looks** :key #'late-look-mapping)
              (progn (push mapping **closing-later**) nil)
              t))
    (close-mapping mapping)))

(defun take-closing-later (mapping)
  "True when MAPPING was left to be closed later (CLOSE-MAPPING-LATER) and no
LATE-LOOK refers to it any more; it is then taken off that list, for the caller
to close. Called with **MAPPING-LOCK** held."
  (when (and (member mapping **closing-later**)
             (not (find mapping **late-looks** :key #'late-look-mapping)))
    (setf **closing-later** (remove mapping **closing-later**))
    t))

(defun late-look-loop ()
  "What the thread letting go late does: wait for a LATE-LOOK to come due, let
go of the pages of its pieces that the system holds now and did not before,
unless a pass holds the piece again, and look again later, twice as long
after, while a look finds some; then close a mapping left to it
\(CLOSE-MAPPING-LATER)."
  (loop
    (let ((wait (sb-thread:with-mutex (**mapping-lock**)
                  (and **late-looks**
                       (max 0 (/ (- (reduce #'min **late-looks** :key #'late-look-time) (get-internal-real-time))
                                 internal-time-units-per-second))))))
      ;; until the first look comes due, or one is added
      (cond ((null wait) (sb-thread:wait-on-semaphore **late-wake**))
            ((plusp wait) (sb-thread:wait-on-semaphore **late-wake** :timeout wait))))
    (let* ((now (get-internal-real-time))
           (due (sb-thread:with-mutex (**mapping-lock**)
                  (let ((due (remove-if (lambda (look) (> (late-look-time look) now)) **late-looks**)))
                    (setf **late-looks** (set-difference **late-looks** due))
                    due))))
      (dolist (look due)
        (let* ((mapping (late-look-mapping look))
               (found (sb-thread:with-mutex ((mapping-releasing mapping))
                        (and (mapping-live mapping)
                             (let ((found nil))
                               (loop for (piece . held) in (late-look-pieces look)
                                     unless (gethash piece (mapping-pieces mapping))
                                       do (when (drop-unheld mapping piece held t) (setf found t)))
                               found))))
               (close (sb-thread:with-mutex (**mapping-lock**)
                        (if (and found (< (incf (late-look-looks look)) +late-looks+))
                            (progn (setf (late-look-time look)
                                         (+ (get-internal-real-time)
                                            (round (* *late-look-seconds* (expt 2 (late-look-looks look))
                                                      internal-time-units-per-second))))
                                   (push look **late-looks**)
                                   nil)
                            (take-closing-later mapping)))))
          (when close
            (close-mapping mapping)))))))

(defun forget-pieces (mapping pieces)
  "Count one holder less for each of PIECES of MAPPING (HOLD-PIECE), and return
those none holds now, each (PIECE . HELD), their records kept until they are
let go of (LET-GO-RELEASED), for a pass holding one meanwhile to take what was
held before. Nothing cuts it short (WITHOUT-INTERRUPTS), so that no count is
left standing."
  (let ((released '()))
    (sb-sys:without-interrupts
      (sb-thread:with-mutex (**mapping-lock**)
        (dolist (piece pieces)
          (let ((entry (gethash piece (mapping-pieces mapping))))
            (when (and entry (zerop (decf (car entry))))
              (push (cons piece (cdr entry)) released))))))
    released))

(defun ask-piece (passage piece state)
  "Have PIECE of PASSAGE's mapping held by PASSAGE's pass from now on unless it
was (HOLD-PIECE), give it STATE, and return it, to be asked of the disk
\(ASK-PIECES); NIL past the mapping's end, or when the system held all of it
before. Called with the pass's lock held."
  (let* ((mapping (passage-mapping passage))
         (new (not (member (gethash piece (passage-pieces passage)) '(:ahead :read))))
         ;; taken before the pass's lock, as a rule (NOTE-FRESH)
         (held (and new (< (* piece +read-piece+) (mapping-length mapping))
                    (or (shiftf (gethash piece (passage-fresh passage)) nil) (piece-in-memory mapping piece)))))
    (when (< (* piece +read-piece+) (mapping-length mapping))
      (sb-thread:with-mutex (**mapping-lock**)
        ;; a mapping closed meanwhile is unmapped, and its addresses may be another's
        (when (mapping-live mapping)
          (when new
            (hold-piece mapping piece held))
          (setf (gethash piece (passage-pieces passage)) state)
          ;; nothing to ask for where the system held every page already
          (and (find 0 (the simple-bit-vector (cdr (gethash piece (mapping-pieces mapping)))))
               piece))))))

(defun ask-pieces (mapping pieces)
  "Ask the disk for PIECES of MAPPING, without any lock, as the asking may wait
for the disk: advice on a stretch unmapped meanwhile is no harm."
  (dolist (piece pieces)
    (let ((from (* piece +read-piece+)))
      (advise-memory (sb-sys:sap+ (mapping-sap mapping) from)
                     (min +read-piece+ (- (mapping-length mapping) from)) 3))))

(defun read-piece (passage piece)
  "Note PIECE of PASSAGE's mapping as read, and the pieces beside it as asked
for ahead; let go of the piece read longest ago once more than
+READ-PASS-KEEPS+ are kept. Returns the pieces to ask the disk for
\(ASK-PIECES): PIECE unless it was, and those beside it. Called with the
pass's lock held."
  (let ((state (gethash piece (passage-pieces passage))) (asked '()))
    (setf (passage-last passage) piece)
    (unless (eq :read state)
      (if (eq :ahead state)
          (setf (gethash piece (passage-pieces passage)) :read)
          (push (ask-piece passage piece :read) asked))
      (push piece (passage-kept passage))
      ;; the pieces beside it, whichever way the reading goes
      (dolist (next (list (1+ piece) (1- piece)))
        (when (and (>= next 0) (null (gethash next (passage-pieces passage))))
          (push (ask-piece passage next :ahead) asked)))
      (when (> (length (passage-kept passage)) +read-pass-keeps+)
        (let ((oldest (car (last (passage-kept passage)))))
          (setf (passage-kept passage) (butlast (passage-kept passage)))
          (let-go-piece passage oldest))))
    (remove nil (nreverse asked))))

(defun let-go-piece (passage piece)
  "Have PASSAGE's pass hold PIECE of its mapping no more, with the others it let
go of, +READ-PASS-KEEPS+ at a time (LET-GO-PIECES, after the pass's lock is
let go of). Called with the pass's lock held."
  (when (member (gethash piece (passage-pieces passage)) '(:ahead :read))
    (setf (gethash piece (passage-pieces passage)) :let-go)
    (when (= piece (passage-last passage))
      (setf (passage-last passage) -1))
    (push piece (passage-letting passage))))

(defun let-go-pieces (passage)
  "Have PASSAGE's pass hold the pieces it let go of no more (FORGET-PIECES), and
let go of those none holds (LET-GO-RELEASED): the pieces are taken and their
holders counted without being cut short."
  (let ((released (sb-sys:without-interrupts
                    (forget-pieces (passage-mapping passage)
                                   (sb-thread:with-mutex ((read-pass-lock (passage-pass passage)))
                                     (shiftf (passage-letting passage) '()))))))
    (when released
      (let-go-released (passage-mapping passage) released))))

(defun end-read-pass (pass)
  "End PASS, the work it was begun for done: finish it (FINISH-READ-PASS) at
once, or as its last task still reading ends."
  (give-back (sb-sys:without-interrupts
               (sb-thread:with-mutex ((read-pass-lock pass))
                 (setf (read-pass-ended pass) t)
                 (when (zerop (read-pass-users pass))
                   (finish-read-pass pass))))))

(defun finish-read-pass (pass)
  "Have PASS hold no piece it asked for any more (FORGET-PIECES); what is read
in it after is let be. Returns what to give back (GIVE-BACK) once PASS's lock
is let go of. Called with PASS's lock held, without interrupts."
  (setf (read-pass-finished pass) t)
  (prog1 (loop for passage in (read-pass-passages pass)
               collect (let ((lately '()))
                         ;; the piece read last and those asked for ahead may still be on their way from the disk
                         (maphash (lambda (piece state)
                                    (when (or (eq state :ahead) (= piece (passage-last passage)))
                                      (push piece lately))
                                    (let-go-piece passage piece))
                                  (passage-pieces passage))
                         (setf (passage-kept passage) '())
                         (list (passage-mapping passage)
                               (forget-pieces (passage-mapping passage) (shiftf (passage-letting passage) '()))
                               lately)))
    (setf (read-pass-passages pass) '())))

(defun give-back (releases)
  "Let go of what a finished pass held (FINISH-READ-PASS), each of RELEASES
\(MAPPING RELEASED LATELY), what it asked of the disk lately again once it
has come (LET-GO-RELEASED)."
  (loop for (mapping released lately) in releases
        do (let-go-released mapping released lately)))

(define-record (file-read-pass (:constructor %make-file-read-pass (fd mapping pass)) (:copier nil))
  "A pass reading a large file open as file descriptor (FD) through once
with read(2) or the like, not through a mapping (BEGIN-FILE-READ-PASS):
MAPPING maps it only to learn what the system holds and to ask for it, in
PASS, a READ-PASS of its own."
  (fd -1 :type fixnum :read-only t :documentation "The descriptor a FILE-READ-PASS reads.")
  (mapping nil :type mapping :read-only t :documentation "The mapping a FILE-READ-PASS asks the system through.")
  (pass nil :type read-pass :read-only t :documentation "The READ-PASS of a FILE-READ-PASS."))

(defun begin-file-read-pass (fd end path)
  "Begin a pass reading the file open as file descriptor (FD) through once,
its first END octets, with read(2) or the kernel's copying rather than a
mapping, as WITH-READ-PASS reads a mapping: each stretch noted with
FILE-READ-PASS-NOTE before it is read, the pass ended with
END-FILE-READ-PASS. NIL, nothing to note, for a file that is not a regular
one or not large (*LARGE-MAPPING-OCTETS*). PATH names the file in messages."
  (declare (type fixnum end))
  (when (and (>= end *large-mapping-octets*)
             (= (logand (sb-posix:stat-mode (sb-posix:fstat fd)) sb-posix:s-ifmt) sb-posix:s-ifreg))
    (let ((own (sb-posix:dup fd)))
      (let ((mapping (handler-case (map-fd own end path)
                       (sb-posix:syscall-error () (tuatara.runtime:close-fd-quietly own) nil))))
        (when mapping
          (file-advise fd 0 0 1)
          (%make-file-read-pass fd mapping (make-read-pass)))))))

(defun file-read-pass-note (file-pass start end)
  "Say that the octets of FILE-PASS's file (BEGIN-FILE-READ-PASS) from START
below END are about to be read, as NOTE-READ says of a mapping's; nothing
when FILE-PASS is NIL."
  (when file-pass
    (let ((*read-pass* (file-read-pass-pass file-pass)))
      (note-read (file-read-pass-mapping file-pass) start end))))

(defun end-file-read-pass (file-pass)
  "End FILE-PASS (BEGIN-FILE-READ-PASS), when it is not NIL, as a pass reading a
mapping ends, and let go of the mapping it asked through."
  (when file-pass
    (end-read-pass (file-read-pass-pass file-pass))
    (close-mapping-later (file-read-pass-mapping file-pass))
    (file-advise (file-read-pass-fd file-pass) 0 0 0)))

(sb-ext:defglobal **read-ahead-request** nil
  "The stretch the thread reading ahead (WANT-READ) is to read next, (MAPPING
START END) with MAPPING held by a weak pointer, or NIL: only the latest asked
for, a later one taking its place.")

(sb-ext:defglobal **read-ahead-wake** (sb-thread:make-semaphore :name "tuatara reading ahead")
  "Signalled when a stretch is asked for (**READ-AHEAD-REQUEST**).")

(sb-ext:defglobal **read-ahead-thread** nil
  "The thread reading ahead, started the first time a stretch is wanted, and
again when it has ended; NIL before.")

(defun read-ahead-loop ()
  "What the thread reading ahead does: wait for a stretch to be asked for, and
ask the system to read it in, a step at a time, as long as it is still the
stretch its mapping wants (MAPPING-WANTED)."
  (loop
    (sb-thread:wait-on-semaphore **read-ahead-wake**)
    (let ((request (loop for request = **read-ahead-request**
                         until (eq request (sb-ext:compare-and-swap (symbol-value '**read-ahead-request**)
                                                                    request nil))
                         finally (return request))))
      (when request
        (destructuring-bind (pointer start end) request
          (let ((mapping (sb-ext:weak-pointer-value pointer))
                (step (* 32 (sb-posix:getpagesize))))
            (when mapping
              ;; in steps the system reads whole, as it reads no more at a time than it reads ahead itself
              (loop for at from start below end by step
                    while (and (mapping-live mapping)
                               (let ((wanted (mapping-wanted mapping)))
                                 (and (= start (car wanted)) (= end (cdr wanted)))))
                    do (advise-memory (sb-sys:sap+ (mapping-sap mapping) at) (min step (- end at)) 3)))))))))

(defun want-read (mapping start end)
  "Have the octets of MAPPING from START below END, START a page boundary,
read in from the disk on a thread of its own, so that showing them later
waits for nothing, and note them as wanted (MAPPING-WANTED), so that a pass
through the file leaves them in memory (WITH-READ-PASS). Only the latest
stretch asked for is read, and only while it is the one wanted; the thread
holds no mapping alive while it waits."
  (declare (type fixnum start end))
  (let ((wanted (mapping-wanted mapping)))
    (setf (car wanted) start (cdr wanted) end))
  (setf **read-ahead-request** (list (sb-ext:make-weak-pointer mapping) start end))
  (let ((thread **read-ahead-thread**))
    (unless (and thread (sb-thread:thread-alive-p thread))
      (sb-thread:with-mutex (**mapping-lock**)
        (let ((thread **read-ahead-thread**))
          (unless (and thread (sb-thread:thread-alive-p thread))
            (setf **read-ahead-thread**
                  (tuatara.runtime:make-guarded-thread #'read-ahead-loop :name "tuatara reading ahead")))))))
  (sb-thread:signal-semaphore **read-ahead-wake**))
