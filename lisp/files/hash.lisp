;;;; lisp/files/hash.lisp --- SHA-256 sums of a file on disk or of the selected text, in the background.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; M-x sha256-sum sums the file the buffer visits as it is on disk, or with a
;;;; selection the octets selected, or in a buffer with no file its whole text.
;;;; The sum is shown, kept in *Messages* and put on the kill ring to paste into
;;;; notes. A thread of its own reads the octets in pieces, a file through a
;;;; descriptor opened without changing its access time where the system allows,
;;;; and sums them with OpenSSL's libcrypto, whose SHA-256 uses the processor's
;;;; SHA instructions, several times as fast as GNU sha256sum. Without that
;;;; library the octets are written to sha256sum through a pipe instead.
;;;; Progress shows in the mode line, and M-x stop-job stops it. Nothing of the
;;;; file is run: the library only reads the octets it is given, and the program
;;;; is started with an argument list, never through a shell.

(in-package #:tuatara.defaults)

(defvar *hash-library* "libcrypto.so.3"
  "OpenSSL's crypto library, which sums with the processor's SHA instructions;
NIL, or a library that cannot be loaded, and *HASH-PROGRAM* sums instead.")

(defvar *hash-program* "sha256sum"
  "The program that sums when *HASH-LIBRARY* cannot: GNU coreutils' sha256sum,
found on the PATH, which reads octets on its standard input and prints their
SHA-256 sum in hex.")

(defvar *hash-piece* (* 8 1024 1024)
  "Octets read and summed at a time: the step of the progress shown and of stopping.")

(defparameter *hash-carried-settings* '(*hash-library* *hash-program* *hash-piece*)
  "The settings a sum reads as it works, carried to the thread it works on as
they are on the editor's when it starts (TUATARA.RUNTIME:CARRY-SETTINGS).")

(defvar *hash-wait* 1/2
  "Seconds sha256-sum waits for a sum before letting the work go on in the background.")

(define-record (hash-job (:include tuatara.runtime:job) (:constructor %make-hash-job) (:copier nil))
  "A SHA-256 sum computed for BUFFER, a job (TUATARA.RUNTIME:JOB): WHAT is
summed, for messages, TOTAL octets of it, the car of DONE counting those
summed so far; SUM, once done."
  (what nil :documentation "What HASH-JOB sums, as its messages name it.")
  (total 0 :documentation "How many octets HASH-JOB sums in all.")
  (done (list 0) :documentation "A list whose car counts the octets HASH-JOB has summed so far.")
  (sum nil :documentation "The SHA-256 sum HASH-JOB computed, NIL until it is done."))

(tuatara.runtime:define-job-words hash-job
  :at-exit :quiet
  :doing "Summing"
  :description (format nil "Summing ~a" (hash-job-what job))
  :verb (format nil "sum ~a" (hash-job-what job))
  :left-words "nothing summed")

(defun hash-library-p ()
  "True when *HASH-LIBRARY* is loaded, loading it for the first sum (FOREIGN-LIBRARY-P)."
  (and *hash-library* (foreign-library-p *hash-library*)))

(defun hash-progress (job)
  "How much of JOB is summed, from 0 to 99 while it runs."
  (tuatara.runtime:running-percent (car (hash-job-done job)) (hash-job-total job)))

(defmethod tuatara.runtime:job-percent ((job hash-job))
  "How much of the sum JOB is done, from 0 to 99 while it runs."
  (hash-progress job))

(defun close-source (source)
  "Have SOURCE, a source for SUM-SOURCE, let go of what it holds (a file read
through, FILE-SOURCE), however the sum ended: called with NIL instead of a
buffer, a source does so and gives 0. Twice is as once."
  (funcall source nil))

(defun sum-source (job source buffer &key (digest :sha256))
  "The sum, in hex, of the octets SOURCE gives, by DIGEST (:SHA256, or :MD5),
summed by *HASH-LIBRARY* when it is there, else by *HASH-PROGRAM* (md5sum for
an MD5 digest); NIL when JOB is stopped first. SOURCE is a function that fills
BUFFER with the next octets and returns how many, 0 at the end; each piece
counts in JOB's DONE. SOURCE is closed at the end, however it comes
\(CLOSE-SOURCE)."
  (unwind-protect (if (hash-library-p)
                      (library-sum job source buffer digest)
                      (program-sum job source buffer (if (eq digest :md5) "md5sum" *hash-program*)
                                   (if (eq digest :md5) 32 64)))
    (close-source source)))

(tuatara.io:define-c-function ("EVP_MD_CTX_new" evp-md-ctx-new) :pointer
  "A new digest context of *HASH-LIBRARY*, a null pointer when none could be made.")

(tuatara.io:define-c-function ("EVP_MD_CTX_free" evp-md-ctx-free) :void
  "Let go of the digest context CONTEXT."
  (context :pointer))

(tuatara.io:define-c-function ("EVP_sha256" evp-sha256) :pointer
  "The SHA-256 digest of *HASH-LIBRARY*.")

(tuatara.io:define-c-function ("EVP_md5" evp-md5) :pointer
  "The MD5 digest of *HASH-LIBRARY*.")

(tuatara.io:define-c-function ("EVP_DigestInit_ex" evp-digest-init) :int
  "Begin a sum by DIGEST in CONTEXT, ENGINE a null pointer; 1 when it began."
  (context :pointer) (digest :pointer) (engine :pointer))

(tuatara.io:define-c-function ("EVP_DigestUpdate" evp-digest-update) :int
  "Add the COUNT octets at DATA to the sum in CONTEXT."
  (context :pointer) (data :pointer) (count :size))

(tuatara.io:define-c-function ("EVP_DigestFinal_ex" evp-digest-final) :int
  "Write the sum in CONTEXT at OUT, SIZE a null pointer."
  (context :pointer) (out :pointer) (size :pointer))

(defun library-sum (job source buffer digest)
  "The sum SUM-SOURCE gives with DIGEST, summed by *HASH-LIBRARY*, SOURCE left
open."
  (let ((ctx (evp-md-ctx-new))
        (null (sb-sys:int-sap 0)))
    (when (sb-sys:sap= ctx null) (refuse "~a could not start a sum" *hash-library*))
    (unwind-protect
         (progn
           (unless (= 1 (evp-digest-init ctx (ecase digest (:sha256 (evp-sha256)) (:md5 (evp-md5))) null))
             (refuse "~a could not start a ~:[SHA-256~;MD5~] sum" *hash-library* (eq digest :md5)))
           (loop for n = (if (car (hash-job-cancel job)) (return-from library-sum nil) (funcall source buffer))
                 while (plusp n)
                 do (sb-sys:with-pinned-objects (buffer)
                      (evp-digest-update ctx (sb-sys:vector-sap buffer) n))
                    (incf (car (hash-job-done job)) n))
           (let ((sum (make-array (if (eq digest :md5) 16 32) :element-type '(unsigned-byte 8))))
             (sb-sys:with-pinned-objects (sum)
               (evp-digest-final ctx (sb-sys:vector-sap sum) null))
             (tuatara.text:octets-hex sum)))
      (evp-md-ctx-free ctx))))

(defun program-sum (job source buffer program digits)
  "The sum SUM-SOURCE gives, DIGITS hex digits long, written through a pipe to
PROGRAM, which prints it, SOURCE left open. Signals, with what the program
said, when it prints no sum."
  (tuatara.io:call-with-program
   (list (or (tuatara.io:find-program program) (refuse "There is no program ~a to take the sum with" program)))
   (lambda (process)
     (let ((fd (sb-sys:fd-stream-fd (sb-ext:process-input process))))
       (loop for n = (if (car (hash-job-cancel job)) 0 (funcall source buffer))
             while (plusp n)
             do (handler-case (tuatara.io:write-octets fd buffer :end n :path program)
                  ;; it ended without reading everything: what it printed says why
                  (tuatara.io:file-system-error () (return)))
                (incf (car (hash-job-done job)) n)))
     (ignoring-errors-reported ("closing the input of ~a" program) (close (sb-ext:process-input process)))
     (sb-ext:process-wait process)
     (unless (car (hash-job-cancel job))
       (let* ((output (with-output-to-string (s)
                        (loop for line = (read-line (sb-ext:process-output process) nil) while line
                              do (write-line line s))))
              (said (string-trim '(#\Newline #\Space) output)))
         (if (and (eql (sb-ext:process-exit-code process) 0) (>= (length said) digits)
                  (every (lambda (c) (digit-char-p c 16)) (subseq said 0 digits)))
             (string-downcase (subseq said 0 digits))
             (refuse "~a failed~@[: ~a~]" program (and (plusp (length said)) said))))))
   :job job :input :stream :output :stream :error :output :external-format :default))

(defun file-source (fd path)
  "A source for SUM-SOURCE of the octets of the file open as file descriptor
(FD) (PATH for messages) to its end, read at explicit offsets while the
system reads ahead; to the end found, not the size reported, which is 0 for
the files the kernel makes up as they are read (/proc). A large file is read
through once (TUATARA.TEXT:BEGIN-FILE-READ-PASS), so that summing it leaves
in memory what was there; called with NIL, it lets go of that (CLOSE-SOURCE)."
  (let ((at 0)
        (pass (tuatara.text:begin-file-read-pass fd (sb-posix:stat-size (sb-posix:fstat fd)) path)))
    (lambda (buffer)
      (if (null buffer)
          ;; closed (CLOSE-SOURCE)
          (progn (tuatara.text:end-file-read-pass (shiftf pass nil)) 0)
          (progn
            (tuatara.text:file-read-pass-note pass at (+ at (length buffer)))
            (let ((n (handler-bind ((error (lambda (e)
                                             (declare (ignore e))
                                             (tuatara.text:end-file-read-pass (shiftf pass nil)))))
                       (tuatara.io:read-fd-at fd buffer at :path path))))
              (if (plusp n)
                  (progn (unless pass (tuatara.io:advise-will-need fd (+ at n) (length buffer)))
                         (incf at n))
                  (tuatara.text:end-file-read-pass (shiftf pass nil)))
              n))))))

(defun vector-source (octets start end)
  "A source for SUM-SOURCE of OCTETS from START below END."
  (let ((at start))
    (lambda (buffer)
      (let ((n (if buffer (min (length buffer) (- end at)) 0)))
        (replace buffer octets :start2 at :end2 (+ at n))
        (incf at n)
        n))))

(defun text-source (rope start end)
  "A source for SUM-SOURCE of ROPE's octets from START below END."
  (let ((at start))
    (lambda (buffer)
      (let ((n (if buffer (min (length buffer) (- end at)) 0)))
        (when (plusp n)
          (tuatara.text:rope-copy-into rope at (+ at n) buffer)
          (incf at n))
        n))))

(defun hash-piece-octets (total)
  "How many octets a sum of TOTAL octets reads at a time: *HASH-PIECE*, or
TOTAL when less; *HASH-PIECE* for a file that says it holds none, as the files
the kernel makes up as they are read do (/proc, /sys)."
  (if (plusp total) (min *hash-piece* total) *hash-piece*))

(defun start-hash (buffer what total source &optional finish)
  "Start a job summing the TOTAL octets SOURCE gives for BUFFER, WHAT describing
them (TUATARA.RUNTIME:START-JOB-THREAD), which calls FINISH, when given, with
the sum once it has one (to check or signal) and at last with NIL (to let go
of what SOURCE reads), with *HASH-CARRIED-SETTINGS* bound there as they are
here. Returns the job."
  (let ((job (%make-hash-job :buffer buffer :what what :total total)))
    (tuatara.runtime:start-job-thread
     job (tuatara.runtime:carry-settings
          *hash-carried-settings*
          (lambda (job)
            (let ((sum (unwind-protect
                            (let ((sum (sum-source job source (make-array (hash-piece-octets total)
                                                                          :element-type '(unsigned-byte 8)))))
                              (when (and sum finish) (funcall finish sum))
                              sum)
                         (when finish (ignoring-errors-reported ("ending the sum") (funcall finish nil))))))
              (setf (hash-job-sum job) sum (hash-job-status job) (if sum :done :stopped)))))
     :name "tuatara sum" :details (format nil "~:d octets" total))))

(defun hash-file (buffer path)
  "Start summing the file at PATH as it is on disk, for BUFFER; or say why not
and return NIL. A file that changes while it is summed has no one sum: that is
said instead, except for the files the kernel makes up as they are read."
  (let ((fd (tuatara.io:open-for-reading path t)) (started nil) (source nil))
    (unwind-protect
         (let ((stamp (tuatara.io:fd-stamp fd path))
               (made-up (eq :pseudo (tuatara.io:file-system-class (tuatara.io:fd-file-system fd)))))
           (if (not (eq (tuatara.io:stamp-kind stamp) :regular))
               (message "~a is not a regular file: nothing summed" (abbreviated-path path))
               (setf started
                     (start-hash buffer (format nil "~a on disk~:[~;, not the buffer's unsaved changes~]"
                                                (abbreviated-path path) (tuatara.buffer:buffer-modified-p buffer))
                                 (tuatara.io:stamp-size stamp) (setf source (file-source fd path))
                                 (lambda (sum)
                                   (if sum
                                       (unless (or made-up
                                                   (tuatara.io:same-content-p stamp (tuatara.io:fd-stamp fd path)))
                                         (refuse "~a changed while being summed; sum it again" (abbreviated-path path)))
                                       (sb-posix:close fd)))))))
      (unless started
        (when source (close-source source))
        (sb-posix:close fd)))
    started))

(defun fd-sha256 (fd path &key (cancel (list nil)) (done (list 0)) buffer)
  "The SHA-256 sum, in hex, of the file open as file descriptor (FD) (PATH,
for messages), read from its start at explicit offsets on the thread that
asks, as FILE-SHA256 sums; NIL when it cannot be read or CANCEL's car turns
true first."
  (let* ((carrier (%make-hash-job :what path :cancel cancel :done done))
         (buffer (or buffer (make-array *hash-piece* :element-type '(unsigned-byte 8)))))
    (ignoring-errors-reported ("summing ~a" path)
      (sum-source carrier (file-source fd path) buffer))))

(defun file-sha256 (path &key (cancel (list nil)) (done (list 0)) buffer)
  "The SHA-256 sum, in hex, of the file at PATH as it is on disk, summed on the
thread that asks (as \\[sha256-sum] sums, read without changing its access time
where the system allows), the car of DONE counting the octets summed; NIL when
it cannot be read or CANCEL's car turns true first. BUFFER, octets to read
into, is made when not given."
  (let ((fd (ignoring-errors-reported ("opening ~a" path) (tuatara.io:open-for-reading path t))))
    (when fd
      (unwind-protect (fd-sha256 fd path :cancel cancel :done done :buffer buffer)
        (ignoring-errors-reported ("closing ~a" path) (sb-posix:close fd))))))

(defun copy-file-checked (source target &key sum (cancel (list nil)) check)
  "Copy the file SOURCE (where it leads, when it is a link) to TARGET, a new
file, as TUATARA.IO:COPY-REGULAR-FILE copies (never over anything there,
never in a place nothing may be written in, its mode and times kept), then
check the copy's SHA-256 against the original's. The original is summed,
when SUM is not given, and copied through the one descriptor opened on it,
and found the same afterwards (its size and time of change), so that its
sum is that of the octets copied. Returns the copy's sum and, as a second
value, the original's; an error naming TARGET when the two differ (the copy
taken away), or SOURCE when it cannot be summed or changed meanwhile. Stopped
when CANCEL's car turns true: what was copied taken away, a job's work ended
there (TUATARA.RUNTIME:JOB-CHECK-STOP), else NIL returned. Both are summed
in one piece of memory, the file's size or *HASH-PIECE* octets when that is
less. CHECK, when given, is called with where the file opened really is
\(TUATARA.IO:FD-REAL-PATH of the descriptor, NIL when the system does not
say) before anything is summed or copied; an error it signals stops the copy."
  (let ((in (tuatara.io:open-for-reading source t :accept '(:regular) :operation "copy")))
    (unwind-protect
         (let* ((before (progn (when check (funcall check (tuatara.io:fd-real-path in)))
                               (tuatara.io:fd-stamp in source)))
                (buffer (make-array (max 4096 (min *hash-piece* (tuatara.io:stamp-size before)))
                                    :element-type '(unsigned-byte 8))))
           (flet ((stopped ()
                    (tuatara.runtime:job-check-stop)
                    (return-from copy-file-checked nil))
                  (wrong (control &rest arguments)
                    (ignoring-errors-reported ("taking away ~a" target) (tuatara.io:remove-file target))
                    (apply #'refuse control arguments)))
             (let ((original (or sum (fd-sha256 in source :cancel cancel :buffer buffer))))
               (unless original
                 (when (car cancel) (stopped))
                 (refuse "~a could not be summed: nothing copied" source))
               (unless (tuatara.io:copy-open-file-to in source target :cancel cancel)
                 (ignoring-errors-reported ("taking away ~a" target) (tuatara.io:remove-file target))
                 (stopped))
               (unless (tuatara.io:same-content-p before (tuatara.io:fd-stamp in source))
                 (wrong "~a changed while it was copied: the copy at ~a taken away" source target))
               (let ((copy (file-sha256 target :cancel cancel :buffer buffer)))
                 (unless (equal original copy)
                   (when (car cancel)
                     (ignoring-errors-reported ("taking away ~a" target) (tuatara.io:remove-file target))
                     (stopped))
                   (wrong "the copy at ~a does not have its original's SHA-256: taken away" target))
                 (values copy original)))))
      (sb-posix:close in))))

(defun octets-sum (octets start end digest)
  "The sum of OCTETS from START below END in hex, by DIGEST (:SHA256 or :MD5),
from *HASH-LIBRARY*, else from *HASH-PROGRAM* or md5sum."
  (let* ((end (or end (length octets)))
         (carrier (%make-hash-job :what "octets" :cancel (list nil) :done (list 0)))
         (buffer (make-array (max 1 (min *hash-piece* (- end start))) :element-type '(unsigned-byte 8))))
    (sum-source carrier (vector-source octets start end) buffer :digest digest)))

(defun octets-sha256 (octets &key (start 0) end)
  "The SHA-256 sum, in lower-case hex, of OCTETS from START below END (their
end when NIL), summed as \\[sha256-sum] sums."
  (octets-sum octets start end :sha256))

(defun octets-md5 (octets &key (start 0) end)
  "The MD5 digest (RFC 1321), in lower-case hex, of OCTETS from START below END
\(their end when NIL): a check some file formats carry, not a safe sum."
  (octets-sum octets start end :md5))

(defmethod tuatara.runtime:job-take-in ((job hash-job))
  "Say how JOB, which has ended, went: show its sum, which *Messages* keeps, and
put it on the kill ring; or say why there is none. Nothing is said once its
buffer was killed."
  (unless (killed-job-buffer-p job)
    (ecase (hash-job-status job)
      (:done (kill-text (hash-job-sum job) :append nil)
             (message "SHA-256 of ~a: ~a (copied)" (hash-job-what job) (hash-job-sum job)))
      ((:stopped :failed) (message "~a" (tuatara.runtime:job-ending-words job))))))

(define-command sha256-sum ()
  "Show the SHA-256 sum of the file the buffer visits as it is on disk, or with
a selection of the octets selected, or in a buffer with no file of its whole
text; keep it in *Messages* and put it on the kill ring. The file is read
without changing its access time where the system allows. A large one is summed
in the background with its progress in the mode line; \\[stop-job] stops it.
While another job runs in the buffer it waits its turn (\\[list-jobs])."
  (let ((b (current-buffer)))
    (multiple-value-bind (start end) (and (tuatara.buffer:buffer-mark-active-p b) (tuatara.buffer:buffer-region b))
      (let ((selection (and start (< start end))))
        (unless (queue-if-busy b (cond (selection (format nil "Summing the selection (~:d octet~:p)" (- end start)))
                                       ((tuatara.buffer:buffer-file b)
                                        (format nil
                                                "Summing ~a on disk"
                                                (abbreviated-path (tuatara.buffer:buffer-file b))))
                                       (t (format nil "Summing ~a's text" (tuatara.buffer:buffer-name b))))
                               (and selection (list (cons start :before) (cons end :after)))
                               (lambda (&optional start end) (start-sum b start end)))
          (start-sum b (and selection start) (and selection end)))))))

(defun start-sum (b start end)
  "Start the sum \\[sha256-sum] makes for B: of its text from START to END when
given, else of its file on disk, or of its whole text without one; say how it
goes."
  (let ((job (cond (start
                    (start-hash b (format nil "the selection (~:d octet~:p)" (- end start)) (- end start)
                                (text-source (tuatara.buffer:buffer-text b) start end)))
                   ((tuatara.buffer:buffer-file b)
                    (hash-file b (tuatara.buffer:buffer-file b)))
                   (t (let ((size (tuatara.buffer:buffer-size b)))
                        (start-hash b (format nil "~a's text (~:d octet~:p)" (tuatara.buffer:buffer-name b) size) size
                                    (text-source (tuatara.buffer:buffer-text b) 0 size)))))))
    (when job
      (take-in-soon job *hash-wait* (format nil "Summing ~a" (hash-job-what job))))))
