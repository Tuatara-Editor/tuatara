;;;; src/buffer/newlines.lisp --- Text put into a buffer with its lines ended as the buffer's are.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.buffer)

(defun lf-newlines (text)
  "TEXT with each carriage return that comes before a newline taken away."
  (if (not (find #\Return text))
      text
      (with-output-to-string (out)
        (loop for i from 0 below (length text)
              for c = (char text i)
              unless (and (char= c #\Return) (< (1+ i) (length text)) (char= (char text (1+ i)) #\Newline))
                do (write-char c out)))))

(defun crlf-newlines (text)
  "TEXT with a carriage return put before each newline that has none."
  (if (not (find #\Newline text))
      text
      (with-output-to-string (out)
        (loop for i from 0 below (length text)
              for c = (char text i)
              do (when (and (char= c #\Newline) (not (and (plusp i) (char= (char text (1- i)) #\Return))))
                   (write-char #\Return out))
                 (write-char c out)))))

(defun cr-newlines (text)
  "TEXT with each newline made a carriage return, one that comes after a
carriage return taken away."
  (if (not (find #\Newline text))
      text
      (with-output-to-string (out)
        (loop for i from 0 below (length text)
              for c = (char text i)
              do (cond ((char/= c #\Newline) (write-char c out))
                       ((not (and (plusp i) (char= (char text (1- i)) #\Return))) (write-char #\Return out)))))))

(defun text-for-buffer (kill buffer &optional at)
  "KILL, text to put into BUFFER (from the kill ring, a template, a file),
with BUFFER's line ends: where BUFFER's lines end with a carriage return and
a newline, a newline alone given a carriage return before it; where they end
with a carriage return alone, each newline made one; elsewhere, the carriage
return before a newline taken away. Given AT, where it goes, no line end of
its edges joins one there (UNJOINED-LINE-ENDS). A kill kept as
a rope, too large for the system clipboard, goes in as it is."
  (cond ((not (stringp kill)) kill)
        (t (let ((text (case (buffer-line-ending buffer)
                         (:crlf (crlf-newlines kill))
                         (:cr (cr-newlines kill))
                         (t (lf-newlines kill)))))
             (if at (unjoined-line-ends buffer at text) text)))))
