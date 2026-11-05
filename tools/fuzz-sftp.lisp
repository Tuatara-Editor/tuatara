;;;; tools/fuzz-sftp.lisp --- Fuzz the reader of what an SFTP server answers.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; Loaded by tools/fuzz-sftp.sh, which sets the limits. Writes what a server
;;;; answers over a session (its version and extensions, handles, attributes
;;;; with every field, names, data, statuses with words, an extended reply),
;;;; spoils copies ROUNDS times from SEED (octets changed, cut, put in, lengths
;;;; and counts made huge, packets repeated and mixed) and reads each as the
;;;; client reads a server, packet by packet. A failure is an error other than
;;;; a malformed message, a stream read past the deadline, or the heap growing
;;;; past the cap; the octets that made it are written to build/fuzz/ and the
;;;; run ends with exit 1.

(defpackage #:tuatara.fuzz-sftp
  (:use #:cl)
  (:documentation "The SFTP client's fuzzer, run by tools/fuzz-sftp.sh."))

(in-package #:tuatara.fuzz-sftp)

(defvar *root* (pathname (concatenate 'string (string-right-trim "/" (sb-ext:posix-getenv "FUZZ_ROOT")) "/"))
  "The Tuatara source tree fuzzed, as tools/fuzz-sftp.sh gives it.")

(load (merge-pathnames "tools/quicklisp.lisp" *root*))
(push *root* asdf:*central-registry*)
(ql:quickload :tuatara/defaults :silent t)

(defun environment-integer (name default)
  "The whole number the environment variable NAME holds, or DEFAULT."
  (let ((text (sb-ext:posix-getenv name)))
    (or (and text (parse-integer text :junk-allowed t)) default)))

(defparameter *rounds* (environment-integer "FUZZ_ROUNDS" 200000) "How many spoilt streams are read.")

(defparameter *seed* (environment-integer "FUZZ_SEED" 1) "The seed of the run's random choices.")

(defparameter *deadline* (environment-integer "FUZZ_DEADLINE" 10)
  "The most seconds one stream may take to be read before it counts as a hang.")

(defparameter *heap-cap* (* (environment-integer "FUZZ_HEAP_MB" 1024) 1024 1024)
  "The most octets of heap in use after a stream is read, once collected.")

(defparameter *random* (sb-ext:seed-random-state *seed*) "The run's random state.")

(defun packets-octets (packets)
  "The octets a server sends as PACKETS, each (KIND . FIELDS) as the client's
SFTP-PUT writes fields."
  (let ((box (tuatara.frontend:make-outbox)))
    (dolist (packet packets)
      (let ((at (tuatara.frontend:outbox-fill box)))
        (tuatara.defaults::sftp-put-integer box 0 4)
        (tuatara.defaults::sftp-put-integer box (second (assoc (first packet) tuatara.defaults::*sftp-types*)) 1)
        (dolist (field (rest packet)) (tuatara.defaults::sftp-put box field))
        (tuatara.io:octets-put-integer (tuatara.frontend:outbox-octets box) (- (tuatara.frontend:outbox-fill box) at 4)
                                       4 :at at :big-endian t)))
    (subseq (tuatara.frontend:outbox-octets box) 0 (tuatara.frontend:outbox-fill box))))

(defun attributes-fields (size)
  "The fields of a file's attributes with every flag, SIZE its size, two attributes of the server's own."
  (list #x8000000F (list :u64 size) 1000 1000 #o100644 1793889089 1793889089 2 "a@x" "1" "b@x" "two"))

(defparameter *session*
  (packets-octets
   (list (list :version 3 "posix-rename@openssh.com" "1" "fsync@openssh.com" "1")
         (list* :attrs 1 (attributes-fields 12))
         (list :handle 2 "handle-one")
         (list :data 3 "hello there, world")
         (list :status 4 1 "End of file" "")
         (list* :name 5 3 (append (list* "a.txt" "-rw-r--r-- a.txt" (attributes-fields 1))
                                  (list "sub" "drwxr-xr-x sub" 4 #o40755)
                                  (list (format nil "odd~cname" #\Newline) "x" 0)))
         (list :status 6 2 "No such file" "en")
         (list :status 7 4)
         (list :extended-reply 8 (list :u64 4096))
         (list :status 9 0 "" "")))
  "What a server answers over a short session, the version first.")

(defun spoilt (octets)
  "A copy of OCTETS spoilt in one to four ways."
  (let ((out (copy-seq octets)))
    (dotimes (k (1+ (random 4 *random*)) out)
      (let ((n (length out)))
        (when (plusp n)
          (setf out
                (ecase (random 6 *random*)
                  (0 (let ((at (random n *random*))) (setf (aref out at) (random 256 *random*)) out))
                  (1 (subseq out 0 (random n *random*)))
                  (2 (let ((at (random n *random*)))
                       (concatenate 'tuatara.text:octets (subseq out 0 at)
                                    (coerce (loop repeat (1+ (random 8 *random*)) collect (random 256 *random*))
                                            'tuatara.text:octets)
                                    (subseq out at))))
                  ;; a length or count made huge or zero
                  (3 (let ((at (random (max 1 (- n 4)) *random*)))
                       (tuatara.io:octets-put-integer out (if (zerop (random 2 *random*)) #xFFFFFFFF 0) (min 4 (- n at))
                                                      :at at :big-endian t)
                       out))
                  ;; a piece repeated
                  (4 (let* ((at (random n *random*)) (end (min n (+ at 1 (random 64 *random*)))))
                       (concatenate 'tuatara.text:octets (subseq out 0 end) (subseq out at))))
                  ;; two halves swapped
                  (5 (let ((at (random n *random*)))
                       (concatenate 'tuatara.text:octets (subseq out at) (subseq out 0 at)))))))))))

(defun read-stream (octets)
  "Read OCTETS as the client reads what a server says, packet by packet, each
answer's number taken and its fields read; ends at their end or the first
malformed packet."
  (let ((inbox (tuatara.frontend:make-inbox nil :editor :octets (copy-seq octets) :end (length octets))))
    (handler-case
        (loop for kind = (tuatara.defaults::sftp-take-packet inbox)
              while kind
              do (if (eq kind :version)
                     (tuatara.defaults::sftp-take-version inbox)
                     (progn (tuatara.defaults::sftp-take-integer inbox 4)
                            (tuatara.defaults::sftp-take-answer inbox kind))))
      (tuatara.frontend:malformed-message () nil))))

(defun failed (round octets control &rest arguments)
  "Say the run FAILED at ROUND, as CONTROL and ARGUMENTS say, keep OCTETS, and end with exit 1."
  (let ((path (merge-pathnames (format nil "build/fuzz/sftp-failed-~d-~d.bin" *seed* round) *root*)))
    (ensure-directories-exist path)
    (alexandria:write-byte-vector-into-file octets path :if-exists :supersede)
    (format t "~&FAILED at round ~:d: ~? (kept as ~a)~%" round control arguments path)
    (finish-output)
    (sb-ext:exit :code 1 :abort t)))

(handler-case (read-stream *session*)
  (error (e) (failed 0 *session* "the session itself does not read: ~a" e)))

(dotimes (round *rounds*)
  (let ((octets (spoilt *session*)) (began (get-internal-real-time)))
    (handler-case (read-stream octets)
      (error (e) (failed round octets "~a" e)))
    (when (> (- (get-internal-real-time) began) (* *deadline* internal-time-units-per-second))
      (failed round octets "read for more than ~d seconds" *deadline*))
    (when (zerop (mod (1+ round) 10000))
      (sb-ext:gc :full t)
      (when (> (sb-kernel:dynamic-usage) *heap-cap*)
        (failed round octets "~:d octets of heap in use" (sb-kernel:dynamic-usage))))))

(format t "~&~:d spoilt streams read, seed ~d~%" *rounds* *seed*)
(finish-output)
(sb-ext:exit :code 0)
