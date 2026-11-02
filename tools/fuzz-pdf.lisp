;;;; tools/fuzz-pdf.lisp --- Fuzz the PDF reader: PDFs made here, spoilt, and read by every reader.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; Loaded by tools/fuzz-pdf.sh, which sets the limits. Makes a handful of
;;;; PDFs of its own (pages, saves, object streams, every filter and
;;;; predictor), spoils copies of them ROUNDS times from SEED, and reads each
;;;; with every reader of lisp/viewers/pdf-reader.lisp and M-x file-metadata's
;;;; PDF part: objects, their fields, streams decoded, object streams, content,
;;;; saves, cross-reference tables and dictionaries. A failure is an error
;;;; escaping a reader, a call running past the deadline, or the heap growing
;;;; past the cap; the file that made it is written to build/fuzz/ and the run
;;;; ends with exit 1.

(defpackage #:tuatara.fuzz-pdf
  (:use #:cl)
  (:documentation "The PDF reader's fuzzer, run by tools/fuzz-pdf.sh."))

(in-package #:tuatara.fuzz-pdf)

(defvar *root* (pathname (concatenate 'string (string-right-trim "/" (sb-ext:posix-getenv "FUZZ_ROOT")) "/"))
  "The Tuatara source tree fuzzed, as tools/fuzz-pdf.sh gives it.")

(load (merge-pathnames "tools/quicklisp.lisp" *root*))
(push *root* asdf:*central-registry*)
(ql:quickload :tuatara/defaults :silent t)

(defun environment-integer (name default)
  "The whole number the environment variable NAME holds, or DEFAULT."
  (let ((text (sb-ext:posix-getenv name)))
    (or (and text (parse-integer text :junk-allowed t)) default)))

(defparameter *rounds* (environment-integer "FUZZ_ROUNDS" 20000)
  "How many spoilt PDFs are read.")

(defparameter *seed* (environment-integer "FUZZ_SEED" 1)
  "The seed of the run's random choices: the same seed makes the same files.")

(defparameter *deadline* (environment-integer "FUZZ_DEADLINE" 10)
  "The most seconds one reader may take on one file before it counts as a hang.")

(defparameter *heap-cap* (* (environment-integer "FUZZ_HEAP_MB" 1024) 1024 1024)
  "The most octets of heap in use after a reader returns, once collected.")

(defparameter *random* (sb-ext:seed-random-state *seed*)
  "The run's random state.")

;;; Seed PDFs, made here

(defun octets (&rest parts)
  "PARTS joined as octets: strings as Latin-1, integers as one octet, vectors as they are."
  (let ((out (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)))
    (dolist (part parts (coerce out 'tuatara.text:octets))
      (etypecase part
        (string (loop for c across part do (vector-push-extend (logand 255 (char-code c)) out)))
        (integer (vector-push-extend part out))
        (vector (loop for o across part do (vector-push-extend o out)))))))

(defun zlib-stored (data)
  "DATA as zlib data of stored deflate blocks, with its Adler-32."
  (let ((parts (list (octets #x78 #x01))) (a 1) (b 0))
    (loop for o across data do (setf a (mod (+ a o) 65521) b (mod (+ b a) 65521)))
    (loop for start from 0 by 65535
          for end = (min (length data) (+ start 65535))
          for last = (= end (length data))
          do (let ((n (- end start)))
               (push (octets (if last 1 0) (logand n 255) (ash n -8)
                             (logand (lognot n) 255) (logand (ash (lognot n) -8) 255)
                             (subseq data start end))
                     parts))
          until last)
    (push (octets (ldb (byte 8 24) b) (ldb (byte 8 16) b) (ldb (byte 8 8) a) (ldb (byte 8 0) a)) parts)
    (apply #'octets (nreverse parts))))

(defun ascii-hex (data)
  "DATA in ASCII hex, ending with >."
  (octets (format nil "~{~2,'0X~^ ~}>" (coerce data 'list))))

(defun run-length (data)
  "DATA run-length coded as literal runs, ending with 128."
  (apply #'octets (append (loop for start from 0 below (length data) by 128
                                collect (let ((end (min (length data) (+ start 128))))
                                          (octets (- end start 1) (subseq data start end))))
                          (list 128))))

(defun ascii-85 (data)
  "DATA in ASCII base-85, ending with ~>."
  (let ((out (make-string-output-stream)))
    (loop for start from 0 below (length data) by 4
          do (let* ((group (subseq data start (min (length data) (+ start 4))))
                    (value (loop for k below 4 sum (ash (if (< k (length group)) (aref group k) 0) (* 8 (- 3 k)))))
                    (digits (loop for k from 4 downto 0 collect (code-char (+ 33 (mod (floor value (expt 85 k)) 85))))))
               (if (and (= (length group) 4) (zerop value))
                   (write-char #\z out)
                   (write-string (coerce (subseq digits 0 (1+ (length group))) 'string) out))))
    (write-string "~>" out)
    (octets (get-output-stream-string out))))

(defun png-up-rows (data columns)
  "DATA in rows of COLUMNS octets, each led by PNG's Up predictor."
  (apply #'octets (loop for start from 0 below (length data) by columns
                        collect (octets 2 (map 'vector (lambda (o p) (logand 255 (- o p)))
                                               (subseq data start (min (length data) (+ start columns)))
                                               (if (>= start columns)
                                                   (subseq data (- start columns) start)
                                                   (make-array columns :initial-element 0)))))))

(defun stream-object (dictionary data)
  "A stream object's body: DICTIONARY's entries (text) with its /Length, then DATA."
  (octets (format nil "<< /Length ~d ~a >>~%stream~%" (length data) dictionary) data (format nil "~%endstream")))

(defun pdf-save (objects &key (first-number 1) prefix trailer)
  "A save of a PDF: OBJECTS (bodies, numbered from FIRST-NUMBER) written
after PREFIX (an earlier save, or the header), its cross-reference table
and its TRAILER's entries (text)."
  (let* ((start (or prefix (octets (format nil "%PDF-1.7~%%") 226 227 207 211 10)))
         (offsets '()) (body (list start)) (at (length start)))
    (loop for object in objects for number from first-number
          do (let ((written (octets (format nil "~d 0 obj~%" number) object (format nil "~%endobj~%"))))
               (push at offsets) (push written body) (incf at (length written))))
    (let ((xref at))
      (push (octets (format nil "xref~%0 1~%0000000000 65535 f ~%~d ~d~%" first-number (length objects))
                    (format nil "~{~10,'0d 00000 n ~%~}" (reverse offsets))
                    (format nil "trailer << /Size ~d ~a >>~%startxref~%~d~%%%EOF~%"
                            (+ first-number (length objects)) (or trailer "") xref))
            body))
    (apply #'octets (nreverse body))))

(defparameter *content*
  (octets "BT /F1 12 Tf 72 720 Td (Hello \\(world\\) \\101\\n) Tj [(A) -20 <42 43>] TJ T* ET"
          " q 1 0 0 1 0.5 -.25 cm 0 0 10 10 re f Q /P << /MCID 0 >> BDC EMC true false null d0"
          " BI /W 2 /H 1 /BPC 8 /CS /G ID " 0 255 " EI [[[1 [2]] /N] 3] 4 5 6 sc")
  "A page's content using most of what content holds.")

(defun xref-stream-pdf ()
  "A PDF whose cross-reference is a stream, its information packed in an
object stream and written again outside it."
  (let* ((header "6 0 ") (packed "<< /Title (Packed) >>")
         (out (make-string-output-stream)) (at 0) (offsets '()))
    (flet ((put (number text)
             (when number (push (cons number at) offsets))
             (write-string text out) (incf at (length text))))
      (put nil (format nil "%PDF-1.7~%"))
      (put 1 (format nil "1 0 obj~%<< /Type /Catalog >>~%endobj~%"))
      (put 2 (format nil "2 0 obj~%<< /Type /ObjStm /N 1 /First ~d /Length ~d >>~%stream~%~a~a~%endstream~%endobj~%"
                     (length header) (+ (length header) (length packed)) header packed))
      (put 6 (format nil "6 0 obj~%<< /Title (Outside) >>~%endobj~%"))
      (let* ((xref at)
             (rows (octets 0 0 0 255
                           1 (ash (cdr (assoc 1 offsets)) -8) (logand (cdr (assoc 1 offsets)) 255) 0
                           1 (ash (cdr (assoc 2 offsets)) -8) (logand (cdr (assoc 2 offsets)) 255) 0
                           2 0 2 0)))
        (octets (get-output-stream-string out)
                (format nil "7 0 obj~%<< /Type /XRef /Size 8 /Index [0 3 6 1] /W [1 2 1] /Root 1 0 R /Info 6 0 R ~
                             /Length ~d >>~%stream~%"
                        (length rows))
                rows (format nil "~%endstream~%endobj~%startxref~%~d~%%%EOF~%" xref))))))

(defun seeds ()
  "The PDFs spoilt in the run, made here: a page with its content in each
filter, two saves, an object stream, predictors and pictures."
  (let ((page (list "<< /Type /Catalog /Pages 2 0 R >>"
                    "<< /Type /Pages /Kids [3 0 R] /Count 1 >>"
                    (concatenate 'string "<< /Type /Page /Parent 2 0 R /MediaBox [0 0 612 792]"
                                 " /Contents [4 0 R 5 0 R 6 0 R 7 0 R] /Resources << /Font << /F1 8 0 R >> >> >>")
                    (stream-object "/Filter /FlateDecode" (zlib-stored *content*))
                    (stream-object "/Filter [/AHx /RL]" (ascii-hex (run-length *content*)))
                    (stream-object "/Filter /A85" (ascii-85 *content*))
                    (stream-object "/Filter /FlateDecode /DecodeParms << /Predictor 12 /Columns 8 >>"
                                   (zlib-stored (png-up-rows *content* 8)))
                    "<< /Type /Font /Subtype /Type1 /BaseFont /Helvetica >>"
                    (concatenate 'string "<< /Title (Seed \\(page\\)) /Author <FEFF00410042>"
                                 " /CreationDate (D:20240301120000+13'00') /JS (no) >>"))))
    (let* ((first (pdf-save page :trailer "/Root 1 0 R /Info 9 0 R"))
           (second (pdf-save (list "<< /Title (Saved again) /ModDate (D:20250101) >>") :first-number 9 :prefix first
                             :trailer (format nil "/Root 1 0 R /Info 9 0 R /Prev ~d"
                                              (1+ (tuatara.io:octets-search (octets (format nil "~%xref~%")) first
                                                                            :from-end t))))))
      (list first
            second
            (xref-stream-pdf)
            (let* ((packed "<< /Type /Font /Subtype /Type1 /BaseFont /Courier >> [1 2 (three) /four]")
                   (header "10 0 11 55 "))
              (pdf-save (list "<< /Type /Catalog /Pages 2 0 R >>"
                              "<< /Type /Pages /Kids [] /Count 0 >>"
                              (stream-object (format nil "/Type /ObjStm /N 2 /First ~d /Filter /FlateDecode"
                                                     (length header))
                                             (zlib-stored (octets header packed)))
                              (stream-object "/Filter /LZWDecode /DecodeParms << /EarlyChange 0 >>"
                                             (octets #x80 #x0B #x60 #x50 #x22 #x0C #x0C #x85 #x01))
                              (stream-object "/Filter /DCTDecode /Width 1 /Height 1" (octets 255 216 255 217))
                              (stream-object "/F (elsewhere.dat)" (octets "x"))
                              (stream-object "/Filter /Crypt /DecodeParms << /Name /Identity >>" (octets "plain"))
                              "<< /Title (Packed) /Producer (Kea \\(fuzz\\)) >>")
                        :trailer "/Root 1 0 R /Info 10 0 R"))))))

;;; Spoiling

(defparameter *tokens*
  '("obj" "endobj" "stream
" "endstream" "<<" ">>" "[" "]" "(" ")" "<" ">" "R" " 0 R" " 7 0 R " "%%EOF
" "startxref
" "xref
0 3
" "trailer" "/Length 99999999" "/Length -1" "/Filter /FlateDecode" "/Filter [/AHx /LZW /RL]" "BI" " ID " " EI "
    "/Type /ObjStm /N 99999 /First 0" "/DecodeParms << /Predictor 15 /Columns 100000 /Colors 32 /BitsPerComponent 16 >>"
    "999999999999999999999" "-" "." "\\" "#" "/" "%"
    "1 0 obj" "D:" "~>" "z")
  "Pieces of PDF put into the files being spoilt.")

(defun pick (n)
  "A whole number from 0 below N (at least 1)."
  (random (max 1 n) *random*))

(defun spoilt (octets others)
  "A copy of OCTETS spoilt from one to eight times: octets changed, PDF
words put in, parts cut out, doubled or cut short, or the end of one of
OTHERS put after a part of it."
  (let ((o octets))
    (dotimes (k (1+ (pick 8)) o)
      (let ((at (pick (length o))))
        (when (zerop (length o)) (return o))
        (setf o (ecase (pick 7)
                  (0 (let ((c (copy-seq o))) (setf (aref c at) (pick 256)) c))
                  (1 (octets (subseq o 0 at) (nth (pick (length *tokens*)) *tokens*) (subseq o at)))
                  (2 (octets (subseq o 0 at) (subseq o (min (length o) (+ at (pick 200))))))
                  (3 (let ((end (min (length o) (+ at (pick 400))))) (octets (subseq o 0 end) (subseq o at))))
                  (4 (subseq o 0 (max 1 at)))
                  (5 (let ((other (nth (pick (length others)) others)))
                       (octets (subseq o 0 at) (subseq other (pick (length other))))))
                  (6 (let ((c (copy-seq o)))
                       (loop for i from at below (min (length c) (+ at (pick 16)))
                             do (setf (aref c i) (char-code (char "0123456789" (pick 10)))))
                       c))))))))

;;; Reading

(define-condition fuzz-failure (error)
  ((words :initarg :words :reader fuzz-failure-words))
  (:report (lambda (condition stream) (write-string (fuzz-failure-words condition) stream)))
  (:documentation "A reader failed the fuzzer: an error, a hang, or too much memory."))

(defun checked (label function)
  "Call FUNCTION, a reader called LABEL, and return what it returns; a
FUZZ-FAILURE when it signals an error, runs past *DEADLINE* seconds, or
leaves more than *HEAP-CAP* octets in use."
  (handler-case
      (multiple-value-prog1
          (sb-ext:with-timeout *deadline* (funcall function))
        (when (> (sb-kernel:dynamic-usage) *heap-cap*)
          (sb-ext:gc :full t)
          (when (> (sb-kernel:dynamic-usage) *heap-cap*)
            (error 'fuzz-failure :words (format nil "~a left ~:d octets in use" label (sb-kernel:dynamic-usage))))))
    (fuzz-failure (failure) (error failure))
    (sb-ext:timeout () (error 'fuzz-failure :words (format nil "~a ran past ~d seconds" label *deadline*)))
    (storage-condition () (error 'fuzz-failure :words (format nil "~a ran out of memory" label)))
    (error (e)
      (error 'fuzz-failure
             :words (format nil "~a: ~a" label (cl-ppcre:regex-replace-all "\\s+" (princ-to-string e) " "))))))

(defun read-every-way (octets)
  "Read OCTETS with every reader of the PDF reader, and file-metadata's PDF part."
  (let ((objects '()))
    (checked "map-pdf-written-objects"
             (lambda () (tuatara:map-pdf-written-objects (lambda (o) (push o objects) nil) octets)))
    (checked "pdf-eof-lines"
             (lambda ()
               (tuatara:pdf-eof-lines octets)
               (tuatara:pdf-eof-lines octets (coerce (reverse objects) 'vector))))
    (checked "the cross-reference" (lambda () (tuatara.defaults::pdf-cross-reference octets)))
    (checked "pdf-startxref" (lambda () (tuatara:pdf-startxref octets)))
    (checked "map-pdf-cross-reference-table"
             (lambda ()
               (loop with mark = (tuatara.text:string-octets "xref")
                     for at = (tuatara.io:octets-search mark octets)
                       then (tuatara.io:octets-search mark octets :start (1+ at))
                     while at
                     do (tuatara:map-pdf-cross-reference-table (constantly nil) octets at))))
    (checked "map-pdf-dictionary and pdf-object-at"
             (lambda ()
               (dotimes (k 50)
                 (let ((at (pick (length octets))))
                   (tuatara:map-pdf-dictionary (constantly nil) octets at)
                   (tuatara:pdf-object-at octets at)))))
    (dolist (object objects)
      (checked "pdf-indirect-get"
               (lambda ()
                 (dolist (field '(:number :generation :value :at :end :stream-start :stream-end :length :packed-in))
                   (tuatara:pdf-indirect-get object field))))
      (let ((start (tuatara:pdf-indirect-get object :stream-start))
            (dictionary (tuatara:pdf-indirect-get object :value)))
        (when start
          (let ((data (checked "pdf-stream-octets"
                               (lambda ()
                                 (tuatara:pdf-stream-filters dictionary)
                                 (tuatara:pdf-stream-octets
                                  dictionary (subseq octets start (tuatara:pdf-indirect-get object :stream-end))
                                  :limit (* 8 1024 1024))))))
            (when data
              (checked "map-pdf-content" (lambda () (tuatara:map-pdf-content (constantly nil) data)))
              (checked "map-pdf-cross-reference-stream"
                       (lambda () (tuatara:map-pdf-cross-reference-stream (constantly nil) dictionary data)))
              (checked "pdf-object-stream-offsets and pdf-packed-object"
                       (lambda ()
                         (let ((offsets (tuatara:pdf-object-stream-offsets dictionary data)))
                           (when offsets
                             (loop for (number) across offsets for index from 0 repeat 100
                                   do (tuatara:pdf-packed-object data offsets number :index index)))))))))))
    (checked "pdf-metadata" (lambda () (tuatara.defaults::pdf-metadata octets)))))

(defun fuzz ()
  "Read *ROUNDS* spoilt PDFs; on a failure write the file to build/fuzz/ and
exit 1, else say how it went and exit 0."
  (let* ((seeds (seeds)) (start (get-internal-real-time)) (slowest 0))
    (dolist (seed seeds) (read-every-way seed))
    (dotimes (round *rounds*)
      (let ((file (spoilt (nth (pick (length seeds)) seeds) seeds))
            (began (get-internal-real-time)))
        (handler-case (read-every-way file)
          (fuzz-failure (failure)
            (let ((path (merge-pathnames (format nil "build/fuzz/pdf-failed-~d-~d.pdf" *seed* round) *root*)))
              (ensure-directories-exist path)
              (alexandria:write-byte-vector-into-file file path :if-exists :supersede)
              (format t "~&FAILED with seed ~d, round ~d: ~a; the file is ~a~%" *seed* round failure (namestring path))
              (finish-output)
              (sb-ext:exit :code 1 :abort t))))
        (setf slowest (max slowest (/ (- (get-internal-real-time) began) internal-time-units-per-second)))))
    (format t "~&~:d spoilt PDFs read with seed ~d in ~,1f s, the slowest in ~,2f s~%"
            *rounds* *seed* (/ (- (get-internal-real-time) start) internal-time-units-per-second) slowest)
    (finish-output)
    (sb-ext:exit :code 0)))

(fuzz)
