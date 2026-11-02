;;;; src/text/mapping.lisp --- Files mapped into memory, read outside the Lisp heap.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.text)

;;; Mapped files

(defstruct (mapping (:constructor %make-mapping (sap length path fd resources
                                                &aux (sources (list (list 0 length fd 0)))))
                    (:copier nil))
  "A memory-mapped file: SAP addresses LENGTH octets of PATH, read-only, outside
the Lisp heap, mapped from the open file descriptor (FD), which the mapping
keeps (so saving can copy straight from it). SOURCES lists, in order, the
stretches (START END FD DELTA) its octets are read from: those from START
below END come from FD at START + DELTA; RETARGET-MAPPING-RANGE moves one
stretch to another file. LIVE turns false when the mapping is closed.
RESOURCES is a cons whose car lists every (ADDRESS LENGTH FD) the mapping has
held; RETARGET-MAPPING adds one when the octets are moved to a snapshot."
  (sap (sb-sys:int-sap 0) :type sb-sys:system-area-pointer)
  (length 0 :type fixnum :read-only t)
  (path nil :read-only t)
  (fd -1 :type fixnum)
  (sources '() :type list)
  (resources (list '()) :read-only t)
  (live t :type boolean))

(setf (documentation 'mapping-p 'function) "True when OBJECT is a MAPPING."
      (documentation 'mapping-sap 'function) "The foreign pointer to the mapped octets."
      (documentation 'mapping-length 'function) "Number of mapped octets."
      (documentation 'mapping-path 'function) "The mapped file's path."
      (documentation 'mapping-fd 'function) "The open descriptor the mapped octets came from when last moved whole."
      (documentation 'mapping-sources 'function) "The stretches (START END FD DELTA) a mapping's octets are read from."
      (documentation 'mapping-resources 'function) "A cons whose car lists the (ADDRESS LENGTH FD) the mapping holds."
      (documentation 'mapping-live 'function) "True until the mapping is closed with CLOSE-MAPPING.")

(defun release-resources (resources)
  "Unmap and close every (ADDRESS LENGTH file descriptor (FD)) in the car of
RESOURCES, once: an FD listed with several stretches is closed once."
  (let ((closed '()))
    (loop for (address length fd) in (shiftf (car resources) '())
          do (tuatara.runtime:ignoring-errors-reported ("unmapping ~d octets" length)
               (sb-posix:munmap (sb-sys:int-sap address) length))
             (unless (member fd closed)
               (push fd closed)
               (tuatara.runtime:ignoring-errors-reported ("closing the mapped file's descriptor ~d" fd)
                 (sb-posix:close fd))))))

(defun map-fd (fd size path)
  "Map SIZE octets of the file open as file descriptor (FD) read-only and
return a MAPPING that owns FD from now on, closing it with the mapping.
PATH names the file in messages. The mapping is released when it is garbage
collected or closed explicitly."
  (declare (type fixnum fd size))
  (let* ((sap (sb-posix:mmap nil size sb-posix:prot-read sb-posix:map-private fd 0))
         (resources (list (list (list (sb-sys:sap-int sap) size fd))))
         (mapping (%make-mapping sap size path fd resources)))
    ;; Ask the kernel to start reading the file in; the index pass follows soon.
    ;; madvise(2) with MADV_WILLNEED (3 on Linux); sb-posix does not wrap it.
    (tuatara.runtime:ignoring-errors-reported ("asking for ~a to be read in" path)
      (sb-alien:alien-funcall
       (sb-alien:extern-alien "madvise" (function sb-alien:int sb-sys:system-area-pointer
                                                  sb-alien:unsigned-long sb-alien:int))
       sap size 3))
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

(sb-ext:defglobal **mapping-lock** (sb-thread:make-mutex :name "tuatara mappings")
  "Held while a mapping is moved or closed, which threads other than the
editor's may do.")

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
        mapping))))

(defun retarget-mapping-range (mapping fd start end delta)
  "Make the octets of MAPPING from START below END read from file descriptor
\(FD) at START + DELTA, an open file holding the very same octets there, so
that they are safe from changes made to where they were read before. START
and DELTA are multiples of the page size; END is one too, or MAPPING's
length. The octets are swapped in place, so a reader in the middle of them
reads the one or the other, alike. MAPPING keeps FD open from then on, and
closes it with the rest. Any thread may do this. Returns MAPPING, or NIL (FD
left open) when it was closed meanwhile."
  (sb-thread:with-mutex (**mapping-lock**)
    (when (mapping-live mapping)
      (let ((address (sb-sys:sap+ (mapping-sap mapping) start)))
        (sb-posix:mmap address (- end start) sb-posix:prot-read (logior sb-posix:map-private sb-posix:map-fixed)
                       fd (+ start delta))
        (push (list (sb-sys:sap-int address) (- end start) fd) (car (mapping-resources mapping)))
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
  "Unmap MAPPING now. Ropes still referring to it signal an error when read."
  (sb-thread:with-mutex (**mapping-lock**)
    (when (mapping-live mapping)
      (setf (mapping-live mapping) nil)
      (sb-ext:cancel-finalization mapping)
      (release-resources (mapping-resources mapping))))
  nil)

(declaim (inline check-mapping))
(defun check-mapping (mapping)
  "Signal an error unless MAPPING is still mapped."
  (unless (mapping-live mapping)
    (error "The mapping of ~a has been closed." (mapping-path mapping))))
