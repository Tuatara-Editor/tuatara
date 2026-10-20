;;;; lisp/files/wipe.lisp --- Files wiped: overwritten with random octets, then deleted, so that they are not found again.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; M-x wipe-file wipes the file shown (g w in a list of a directory, the
;;;; files marked), after asking: its contents overwritten with random
;;;; octets (*WIPE-PASSES* times), put on disk, the file emptied, renamed and
;;;; deleted (TUATARA.IO:WIPE-FILE), in the background. The question says
;;;; plainly where that cannot work: on a file system that writes changes
;;;; elsewhere (btrfs, zfs), and on a solid-state drive, which moves writes
;;;; about, the old blocks stay until reused; only the disk's own encryption
;;;; keeps them from being read. Encrypting a file (M-x encrypt-file) offers to
;;;; wipe the original once it is done; with a passphrase, only after the
;;;; passphrase, asked again, is seen to open the encrypted file to the same
;;;; octets, so that a passphrase mistyped or forgotten never loses the file.

(in-package #:tuatara.defaults)

(defvar *wipe-passes* 1
  "How many times a file wiped is overwritten with random octets before it is
deleted: once is enough for a disk that writes in place; more do not help one
that does not (a copy-on-write file system, a solid-state drive).")

(defun wipe-caveat (paths)
  "What wiping PATHS cannot promise, in a few words: a copy-on-write file
system keeps old copies of their blocks; a solid-state drive may."
  (if (some (lambda (path) (ignore-errors (tuatara.io:copy-on-write-p path))) paths)
      "its file system keeps old copies (copy-on-write)"
      "an SSD may keep old copies"))

(defstruct (wipe-job (:include tuatara.runtime:job) (:constructor make-wipe-job (buffer paths total)) (:copier nil))
  "PATHS being wiped, a job: TOTAL octets to overwrite, all passes counted;
DONE, so far; WIPED, those gone; REFUSED, why none was wiped, when one was
found not to hold what it had to."
  paths total (done 0) (wiped '()) (refused nil))

(setf (documentation 'wipe-job-p 'function) "True when OBJECT is a WIPE-JOB.")

(tuatara.runtime:define-job-words wipe-job
  :doing "Wiping"
  :description (format nil "Wiping ~{~a~^, ~}" (mapcar #'tuatara.io:native-file-name (wipe-job-paths job)))
  :verb "wipe"
  :left-words "the files partly overwritten, still there")

(defmethod tuatara.runtime:job-percent ((job wipe-job))
  "How far the wiping has got."
  (tuatara.runtime:running-percent (wipe-job-done job) (max 1 (wipe-job-total job))))

(defmethod tuatara.runtime:job-take-in ((job wipe-job))
  "Say what was wiped, and show the lists of their folders again."
  (let ((wiped (wipe-job-wiped job)))
    (paths-changed wiped)
    (cond ((wipe-job-refused job) (message "~a" (wipe-job-refused job)))
          ((eq (tuatara.runtime:job-status job) :done)
           (message "Wiped: ~{~a~^, ~}" (mapcar #'tuatara.io:native-file-name (reverse wiped))))
          (t (message "~@[Wiped ~{~a~^, ~}; ~]~a" (and wiped (mapcar #'tuatara.io:native-file-name (reverse wiped)))
                      (tuatara.runtime:job-ending-words job))))))

(defun file-holds-p (path octets)
  "True when the file at PATH holds OCTETS and nothing else, compared a piece
at a time, each piece read overwritten after."
  (with-open-file (in (sb-ext:parse-native-namestring path) :element-type '(unsigned-byte 8))
    (and (= (file-length in) (length octets))
         (let ((piece (make-array (* 1024 1024) :element-type '(unsigned-byte 8))))
           (unwind-protect
                (loop for at from 0 by (length piece)
                      for n = (read-sequence piece in)
                      while (plusp n)
                      always (not (mismatch piece octets :end1 n :start2 at :end2 (+ at n))))
             (fill piece 0))))))

(defun start-wiping (paths &key same-as)
  "Wipe PATHS in the background (TUATARA.IO:WIPE-FILE, *WIPE-PASSES* times);
with SAME-AS, octets, the one path only once it is seen to hold them
\(FILE-HOLDS-P), none wiped else, and SAME-AS overwritten with zeros after."
  (let ((total (* *wipe-passes* (loop for path in paths sum (or (ignore-errors (tuatara.io:stamp-size (tuatara.io:path-stamp path))) 0)))))
    (tuatara.runtime:start-job-thread
     (make-wipe-job (current-buffer) paths total)
     (let ((passes *wipe-passes*))
       (lambda (job)
         (if (and same-as (not (unwind-protect (ignore-errors (file-holds-p (first paths) same-as))
                                 (fill same-as 0))))
             (setf (wipe-job-refused job)
                   (format nil "~a is not wiped: what was encrypted does not hold the same octets"
                           (tuatara.io:native-file-name (first paths))))
             (let ((before 0))
               (dolist (path (wipe-job-paths job))
                 (let ((size (* passes (or (ignore-errors (tuatara.io:stamp-size (tuatara.io:path-stamp path))) 0))))
                   (when (eq :stopped (tuatara.io:wipe-file path :passes passes :cancel (tuatara.runtime:job-cancel job)
                                                                  :progress (lambda (n) (setf (wipe-job-done job) (+ before n)))))
                     (tuatara.runtime:job-check-stop))
                   (push path (wipe-job-wiped job))
                   (incf before size)))))))
     :name "tuatara wipe")))

(defun confirm-wipe (paths &optional what)
  "True when, asked, you say to wipe PATHS for good, what it cannot promise said;
WHAT, words naming them (\"3 files in 2 directories\"), or the name of one or
how many."
  (yes-or-no-p* (format nil "Wipe ~a for good? (~a)"
                        (or what (list-question nil "file" paths :the nil :name-of #'tuatara.io:native-file-name))
                        (wipe-caveat paths))))

(define-command wipe-file ()
  "Wipe the file shown (in a list of a directory, the one on the line; with
\\[universal-argument], or where there is none, asked for) after asking: its contents
overwritten with random octets, put on disk, then the file renamed and deleted,
in the background, so that it is not found again where the disk writes in
place. Where it does not, the old blocks stay until reused, and only the
disk's own encryption keeps them from being read: a copy-on-write file system
(btrfs, zfs) keeps old copies, and a solid-state drive may, which the question
says. A link, a folder, and a file with other names are refused."
  (let ((path (shown-file-argument "Wipe file: ")))
    (when (confirm-wipe (list path))
      (start-wiping (list path)))))

;;; After encrypting

(defun encryption-yours-p (encryption)
  "True when ENCRYPTION can be opened by you again: with a passphrase, or to
one of your own keys among its :KEYS."
  (or (getf encryption :symmetric)
      (let ((ids (getf encryption :keys)))
        (some (lambda (key)
                (and (getf key :secret)
                     (some (lambda (id) (member id ids :test #'string-equal)) (cons (getf key :id) (getf key :subkeys)))))
              (ignore-errors (gpg-keys))))))

(defun wipe-original-question (what encryption caveat)
  "The question whether to wipe WHAT once encrypted as ENCRYPTION says, CAVEAT
said: the passphrase is asked again first; to keys none of which is yours, it
cannot be opened again."
  (format nil "Wipe ~a~:[~; (passphrase asked again)~]?~:[ None of the keys is yours: it cannot be opened again.~;~] (~a)"
          what (getf encryption :symmetric) (encryption-yours-p encryption) caveat))

(defun wipe-original (path target encryption &key (passphrase nil passphrase-p))
  "Wipe the file at PATH, now encrypted as TARGET with ENCRYPTION: at once
when it is encrypted to keys only; with a passphrase, once the passphrase,
asked again (gpg-agent's cache passed by), opens TARGET to the very octets of
PATH, so that a passphrase mistyped or forgotten never loses the file.
PASSPHRASE, for the minibuffer's loopback, is asked for when not given, and
wiped once gpg has it."
  (if (not (getf encryption :symmetric))
      (start-wiping (list path))
      (run-gpg-work (format nil "checking the passphrase opens ~a" (tuatara.io:native-file-name target))
                    (list "--no-symkey-cache" "--decrypt" "--output" "-")
                    (lambda (output statuses ok said)
                      (if (and ok output (gpg-status-p statuses "DECRYPTION_OKAY"))
                          ;; compared with the file on the wiping's thread, never here
                          (start-wiping (list path) :same-as output)
                          (progn
                            (message "~a is not wiped: the passphrase given does not open ~a (~a)"
                                     (tuatara.io:native-file-name path) (tuatara.io:native-file-name target)
                                     (gpg-failure-words statuses said))
                            (when output (fill output 0)))))
                    :input-path target
                    :passphrase (if passphrase-p
                                    passphrase
                                    (and (gpg-minibuffer-p)
                                         (gpg-read-passphrase (format nil "The passphrase again, to check it opens ~a: " (tuatara.io:native-file-name target))))))))

(defun offer-wipe-original (path target encryption)
  "Ask, as the editor's next command, whether to wipe the file at PATH now
encrypted as TARGET (WIPE-ORIGINAL)."
  (defer-action (lambda ()
                  (when (and (tuatara.io:path-stamp path)
                             (yes-or-no-p* (wipe-original-question (format nil "the original ~a" (tuatara.io:native-file-name path))
                                                                   encryption (wipe-caveat (list path)))))
                    (wipe-original path target encryption)))))
