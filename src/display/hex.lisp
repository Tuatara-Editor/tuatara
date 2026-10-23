;;;; src/display/hex.lisp --- The hex view: a buffer's octets as a hex dump, laid out as shown.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; A buffer whose :HEX-VIEW local is true is shown as a hex dump: each row the
;;;; offset, sixteen octets in hex in two groups of eight, and the text they make
;;;; with a dot for an octet that is no printable ASCII character. Only the rows
;;;; in the window are made, from the rope as it is, so a file of any size shows
;;;; at once and nothing about the text changes. The cursor sits on an octet,
;;;; any octet (the hex point); point stays at the start of the character that
;;;; octet is part of, so that everything else goes on working on characters.

(in-package #:tuatara.display)

(defconstant +hex-row+ 16 "Octets in a row of the hex view.")

(defun hex-view-p (buffer)
  "True when BUFFER is shown as a hex dump (its :HEX-VIEW local)."
  (and (tuatara.buffer:buffer-local buffer :hex-view) t))

(defun hex-point (buffer)
  "The octet the hex view's cursor is on in BUFFER, as a byte offset: any octet,
not only a character's first. It follows point whenever something other than
the hex view's own motion has moved point."
  (let ((anchor (tuatara.buffer:buffer-local buffer :hex-anchor))
        (point (tuatara.buffer:buffer-point buffer)))
    (if (and anchor (eql (tuatara.buffer:buffer-local buffer :hex-anchor-point) point))
        (min (tuatara.text:anchor-position anchor (tuatara.buffer:buffer-version buffer))
             (tuatara.buffer:buffer-size buffer))
        point)))

(defun (setf hex-point) (byte buffer)
  "Put the hex view's cursor on octet BYTE of BUFFER (kept within the text),
and point at the start of the character that octet is part of."
  (let* ((rope (tuatara.buffer:buffer-text buffer))
         (byte (max 0 (min byte (tuatara.text:rope-bytes rope))))
         (start (tuatara.text:rope-char-start rope byte)))
    (setf (tuatara.buffer:buffer-point buffer) start
          (tuatara.buffer:buffer-local buffer :hex-anchor)
          (tuatara.text:make-anchor (tuatara.buffer:buffer-version buffer) byte :bias :after)
          (tuatara.buffer:buffer-local buffer :hex-anchor-point) start
          ;; a half octet typed is over once the cursor moves
          (tuatara.buffer:buffer-local buffer :hex-nibble) nil)
    byte))

(defun hex-offset-digits (total)
  "How many hex digits the offsets of a text of TOTAL octets take: eight, or more for a text past 4 GB."
  (max 8 (length (format nil "~x" total))))

(defparameter *hex-pairs*
  (let ((pairs (make-array 256)))
    (dotimes (i 256 pairs)
      (setf (aref pairs i) (format nil "~(~2,'0x~)" i))))
  "The two lower-case hex digits of each octet, by its value.")

(defun hex-digits (n digits)
  "N in lower-case hex, at least DIGITS digits long, zeros before it."
  (let* ((length (max digits (ceiling (integer-length n) 4)))
         (string (make-string length :initial-element #\0)))
    (loop for i downfrom (1- length) to 0
          for value = n then (ash value -4)
          while (plusp value)
          do (setf (char string i) (char-downcase (digit-char (logand value 15) 16))))
    string))

(defun row-add-octets (row string byte face)
  "Append STRING to ROW, a column a character, each standing for the octet at
BYTE (-1 for none), in FACE."
  (loop for c across string do (row-add-char row c byte face)))

(defun row-add-char (row char byte face)
  "Append CHAR to ROW, standing for the octet at BYTE (-1 for none), in FACE."
  (vector-push-extend char (row-string row))
  (vector-push-extend face (row-faces row))
  (vector-push-extend byte (row-char-bytes row))
  (vector-push-extend byte (row-bytes row))
  (incf (row-columns row)))

(defun hex-layout (rope start height &optional spares)
  "Lay out ROPE as a hex dump from byte START (a multiple of +HEX-ROW+) into at
most HEIGHT rows (see this file's header). The hex digits and the text column
of each octet map back to its offset, other columns to none (-1), so that the
cursor shows on an octet's hex digits; the rows are marked MIXED, their
characters' offsets not in order, for APPLY-HIGHLIGHTS. Rows are taken from
SPARES, rows no longer shown, while it has any (MAKE-ROW). Complexity: O(HEIGHT)."
  (let* ((total (tuatara.text:rope-bytes rope))
         (digits (hex-offset-digits total))
         (rows (make-array 0 :adjustable t :fill-pointer 0)))
    (loop for r from 0 below height
          for offset = (+ start (* r +hex-row+))
          while (<= offset total)
          do (let* ((n (min +hex-row+ (- total offset)))
                    (octets (tuatara.text:rope-octets rope :start offset :end (+ offset n)))
                    (row (make-row offset (+ digits 70) spares)))
               (setf (row-mixed row) t)
               (row-add-octets row (hex-digits offset digits) -1 :hex-offset)
               (row-add-octets row "  " -1 nil)
               (dotimes (k +hex-row+)
                 (if (< k n)
                     (progn (row-add-octets row (aref *hex-pairs* (aref octets k)) (+ offset k) nil)
                            (row-add-octets row " " -1 nil))
                     (row-add-octets row "   " -1 nil))
                 (when (= k 7) (row-add-octets row " " -1 nil)))
               (row-add-octets row " " -1 nil)
               (dotimes (k n)
                 (let ((b (aref octets k)))
                   (row-add-char row (if (<= 32 b 126) (code-char b) #\.) (+ offset k)
                                 (if (<= 32 b 126) nil :hex-unprintable))))
               (setf (row-end row) (+ offset n) (row-shown-end row) (+ offset n)
                     (row-last-p row) (= (+ offset n) total))
               (vector-push-extend row rows)
               (when (row-last-p row) (return))))
    (coerce rows 'simple-vector)))

(defun hex-row-start (byte)
  "The offset of the row of the hex view holding BYTE."
  (* +hex-row+ (floor byte +hex-row+)))
