;;;; lisp/editing/case.lisp --- Changing case: M-u M-l M-c for words, C-x C-u C-x C-l for the region.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.defaults)

(defun capitalised (text)
  "TEXT with each word's first letter in upper case and the rest in lower."
  (let ((result (copy-seq text)) (in-word nil))
    (dotimes (i (length result) result)
      (let ((c (char result i)))
        (if (word-char-p c)
            (setf (char result i) (if in-word (char-downcase c) (char-upcase c))
                  in-word t)
            (setf in-word nil))))))

(defun case-changed (text how)
  "TEXT in HOW's case: :UP, :DOWN, or :CAPITAL."
  (ecase how (:up (string-upcase text)) (:down (string-downcase text)) (:capital (capitalised text))))

(defun change-case (buffer start end how)
  "Put BUFFER's text from START below END in HOW's case (see CASE-CHANGED), one
change; point stays by the character it was at. Returns where the text changed
ends now: a letter's other case may take more or fewer octets."
  (let* ((text (tuatara.buffer:buffer-string buffer :start start :end end))
         (new (case-changed text how)))
    (if (string= text new)
        end
        (let* ((p (tuatara.buffer:buffer-point buffer))
               (offset (and (<= start p end) (length (tuatara.buffer:buffer-string buffer :start start :end p)))))
          (tuatara.buffer:buffer-replace buffer start end new)
          (when offset
            (setf (tuatara.buffer:buffer-point buffer)
                  (+ start (tuatara.text:string-octet-length new :end (min offset (length new))))))
          (+ start (tuatara.text:string-octet-length new))))))

(defun word-case (how)
  "Change the case of the count's words after point and move over them; with
a negative count, of the words before point, point staying."
  (let* ((b (current-buffer)) (n (prefix)) (p (point)))
    (if (minusp n)
        (change-case b (words-backward b p (- n)) p how)
        (goto (change-case b p (words-forward b p n) how)))))

(defun case-report (how)
  "A job's REPORT for changing the case of a region to HOW's."
  (let ((done (ecase how (:up "in upper case") (:down "in lower case") (:capital "capitalised"))))
    (lambda (job status changes seconds kept)
      (declare (ignore job))
      (case status
        (:done (format nil "Put the region ~a ~a" done
                       (job-done-words seconds kept (format nil "~:d line~:p changed" changes))))
        (:none "Nothing in the region to change the case of")))))

(defun ascii-case (octets start end how)
  "The ASCII text of OCTETS from START below END in HOW's case, as CASE-CHANGED
puts it, as new octets; NIL when that changes nothing."
  (declare (type (simple-array (unsigned-byte 8) (*)) octets) (type fixnum start end) (optimize speed))
  (let ((new (make-array (- end start) :element-type '(unsigned-byte 8)))
        (changed nil))
    (macrolet ((each ((b) form)
                 `(loop for p of-type fixnum from start below end
                        for i of-type fixnum from 0
                        do (let* ((,b (aref octets p)) (c ,form))
                             (declare (type (unsigned-byte 8) ,b c))
                             (setf (aref new i) c)
                             (unless (= ,b c) (setf changed t))))))
      (ecase how
        (:up (each (b) (if (<= 97 b 122) (- b 32) b)))
        (:down (each (b) (if (<= 65 b 90) (+ b 32) b)))
        (:capital
         (let ((in-word nil))
           (each (b) (let ((word (word-char-p (code-char b))))
                       (prog1 (cond ((not word) b)
                                    (in-word (if (<= 65 b 90) (+ b 32) b))
                                    (t (if (<= 97 b 122) (- b 32) b)))
                         (setf in-word word))))))))
    (and changed new)))

(defun case-line (how from to)
  "A function changing each line for START-LINE-JOB: the text of the line from
FROM below TO put in HOW's case, as CASE-CHANGED puts it; a line of ASCII
changed octet by octet, another read as text."
  (lambda (function line line-end next rope block block-start block-end)
    (declare (ignore next) (type fixnum line line-end block-start block-end))
    (let ((a (max line from)) (z (min line-end to)))
      (declare (type fixnum a z))
      (when (< a z)
        ;; the line's octets from the block read, or read now when it is not in it
        (multiple-value-bind (octets start)
            (if (and (>= a block-start) (<= z block-end))
                (values block (- a block-start))
                (values (tuatara.text:rope-octets rope :start a :end z) 0))
          (declare (type (simple-array (unsigned-byte 8) (*)) octets) (type fixnum start))
          (let* ((end (+ start (- z a)))
                 (new (if (loop for p of-type fixnum from start below end always (< (aref octets p) 128))
                          (ascii-case octets start end how)
                          (let* ((text (tuatara.text:octets-string octets :start start :end end))
                                 (changed (case-changed text how)))
                            (and (string/= text changed) (tuatara.text:string-octets changed))))))
            (when new (funcall function a z nil new))))))))

(defun region-case (how)
  "Change the case of the region, or say there is none. A region of more than
*REWRITE-THRESHOLD* octets is changed in the background, a word at a time."
  (let ((b (current-buffer)))
    (multiple-value-bind (start end) (tuatara.buffer:buffer-region b)
      (cond ((not (and start (tuatara.buffer:buffer-mark-active-p b))) (message "The mark is not active now"))
            ((large-for-here-p start end)
             (check-writable b)
             (setf (tuatara.buffer:buffer-mark-active-p b) nil)
             (start-line-job b (tuatara.buffer:buffer-line-start b start) (tuatara.buffer:buffer-line-end b (range-last-place b start end))
                             (case-line how start end)
                             :doing "Changing case" :what "Changing the case of the region"
                             :verb "change the case" :leaves "the case is as it was"
                             :report (case-report how)
                             :announce "Changing the case of the region"))
            (t (change-case b start end how))))))

(define-command upcase-word ()
  "Put the next word, or the count's, in upper case and move over it; with a
negative count, the words before point, point staying."
  (word-case :up))

(define-command downcase-word ()
  "Put the next word, or the count's, in lower case and move over it; with a
negative count, the words before point, point staying."
  (word-case :down))

(define-command capitalize-word ()
  "Capitalise the next word, or the count's: its first letter in upper case
and the rest in lower. Moves over it; with a negative count, the words before
point, point staying."
  (word-case :capital))

(define-command upcase-region ()
  "Put the region in upper case."
  (region-case :up))

(define-command downcase-region ()
  "Put the region in lower case."
  (region-case :down))

(define-command capitalize-region ()
  "Capitalise each word of the region."
  (region-case :capital))

(define-command upcase-dwim ()
  "Put the region in upper case, or with none the next word, as upcase-word does."
  (if (tuatara.buffer:buffer-mark-active-p (current-buffer)) (region-case :up) (word-case :up)))

(define-command downcase-dwim ()
  "Put the region in lower case, or with none the next word, as downcase-word does."
  (if (tuatara.buffer:buffer-mark-active-p (current-buffer)) (region-case :down) (word-case :down)))

(define-command capitalize-dwim ()
  "Capitalise the region, or with none the next word, as capitalize-word does."
  (if (tuatara.buffer:buffer-mark-active-p (current-buffer)) (region-case :capital) (word-case :capital)))
