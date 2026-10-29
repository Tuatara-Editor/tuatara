;;;; test/line-endings.lisp --- Files whose lines end with a carriage return and a newline.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.tests)

(define-test line-endings
  :description "A Windows file (lines ending CR LF) is shown and edited without a ^M on every line, and saved exactly as it was apart from the edits: RET makes CR LF there, and a CR LF is one step.")

(define-test (line-endings detection)
  (flet ((of (string) (tuatara.buffer:detect-line-ending (tuatara.text:string->rope string))))
    (is eq :crlf (of (format nil "a~c~%b~c~%" #\Return #\Return)))
    (is eq :lf (of (format nil "a~%b~c~%" #\Return)))           ; the first line end decides
    (is eq :lf (of "no line end"))
    (is eq :lf (of "")))
  ;; of a part of the text, as the files searched together are each judged
  (let ((rope (tuatara.text:string->rope (format nil "a~%b~c~%c~%~c~%" #\Return #\Return))))
    (is eq :lf (tuatara.buffer:detect-line-ending rope :start 0 :end 2))
    (is eq :crlf (tuatara.buffer:detect-line-ending rope :start 2 :end 5))
    (is eq :lf (tuatara.buffer:detect-line-ending rope :start 5 :end 7))
    (is eq :lf (tuatara.buffer:detect-line-ending rope :start 4 :end 5))
    (is eq :crlf (tuatara.buffer:detect-line-ending rope :start 7))
    ;; the text of a line: before its carriage return with CRLF, a carriage return at the end too
    (is = 3 (tuatara.text:rope-line-text-end rope 2 4 t))
    (is = 4 (tuatara.text:rope-line-text-end rope 2 4 nil))
    (is = 7 (tuatara.text:rope-line-text-end rope 7 7 t))
    (is = 7 (tuatara.text:rope-line-text-end rope 7 8 t))))

(define-test (line-endings showing-line-endings)
  (let ((rope (tuatara.text:string->rope (format nil "ab~c~%cd~c~%lone~cCR" #\Return #\Return #\Return))))
    ;; shown as lines, the carriage returns before newlines unseen; a lone one still shows
    (is equal '("ab" "cd" "lone^MCR")
        (map 'list (lambda (row) (coerce (tuatara.display::row-string row) 'string))
             (tuatara.display:layout rope 0 40 3 :crlf t)))
    ;; in a buffer that ends lines with a newline alone, every carriage return shows
    (is string= "ab^M" (coerce (tuatara.display::row-string (aref (tuatara.display:layout rope 0 40 1) 0)) 'string))))

(define-test (line-endings motion-and-editing)
  (let ((b (tuatara.buffer:make-buffer "*crlf-test*" :text (format nil "ab~c~%cd~c~%" #\Return #\Return))))
    (unwind-protect
         (progn
           (setf (tuatara.buffer:buffer-local b :line-ending) :crlf)
           (is = 2 (tuatara.buffer:buffer-line-end b 0))          ; before the carriage return
           (is = 4 (tuatara.buffer:buffer-forward-char b 2))      ; over both at once
           (is = 2 (tuatara.buffer:buffer-forward-char b 4 -1))
           (is = 6 (tuatara.buffer:buffer-line-end b 4))
           (is string= (coerce '(#\Return #\Newline) 'string) (tuatara.buffer:buffer-newline b)))
      (tuatara.buffer:kill-buffer b))))

(define-test (line-endings files)
  (with-test-directory (dir)
    (let ((path (merge-pathnames "windows.txt" dir))
          (crlf (coerce '(#\Return #\Newline) 'string)))
      (put-file path (format nil "ab~acd~a" crlf crlf))
      ;; C-e stops before the carriage return; RET there makes a CR LF; the mode line says CRLF
      (multiple-value-bind (f editor) (run-keys "C-e i RET Z C-g C-x C-s" :file path :width 100)
        (true (tuatara.frontend.fake:fake-frame-showing f "CRLF"))
        (false (tuatara.frontend.fake:fake-frame-showing f "^M"))
        (is string= (format nil "ab~aZ~acd~a" crlf crlf crlf) (file-text path))
        (tuatara.buffer:kill-buffer (session-buffer editor)))
      ;; C-d at the end of a line joins it with the next, taking both octets
      (multiple-value-bind (f editor) (run-keys "C-e C-d C-x C-s" :file path :width 100)
        (declare (ignore f))
        (is string= (format nil "abZ~acd~a" crlf crlf) (file-text path))
        (tuatara.buffer:kill-buffer (session-buffer editor)))
      ;; o opens a line below with a CR LF of its own
      (multiple-value-bind (f editor) (run-keys "o Y C-g C-x C-s" :file path :width 100)
        (declare (ignore f))
        (is string= (format nil "abZ~aY~acd~a" crlf crlf crlf) (file-text path))
        (tuatara.buffer:kill-buffer (session-buffer editor))))))

(define-test (line-endings searching-and-pasting)
  (let ((crlf (format nil "ab~c~%cb~c~%" #\Return #\Return)))
    (flet ((after (keys &optional (text crlf))
             (session-text (nth-value 1 (run-keys-in-turns (list keys "") :text text :crlf t)))))
      ;; $ matches before the carriage return; . takes neither it nor the newline
      (is string= (format nil "aX~c~%cX~c~%" #\Return #\Return) (after (format nil "r r ~a RET" (key-string "%b$/X"))))
      (is string= (format nil "aY~c~%cY~c~%" #\Return #\Return) (after (format nil "r r ~a RET" (key-string "%.$/Y"))))
      (is string= crlf (after (format nil "r r ~a RET" (key-string "%b./Z"))))
      ;; f the same
      (is string= (format nil "aW~c~%cW~c~%" #\Return #\Return) (after "% f b $ RET c W C-g"))
      ;; a kill takes the line ends of the buffer it is pasted in: here, into a
      ;; buffer whose lines end with a newline alone
      (let ((other (after "V j y C-x b o t h e r C-j p")))
        (false (find #\Return other))
        (true (search (format nil "ab~%cb~%") other)))
      ;; and the other way, into this one
      (let ((b (session-buffer (nth-value 1 (run-keys-in-turns '("") :text crlf :crlf t)))))
        (is string= (format nil "x~c~%y~c~%" #\Return #\Return)
            (tuatara.defaults::text-for-buffer (format nil "x~%y~c~%" #\Return) b)))
      ;; lines pasted one to a cursor carry no carriage return with them
      (let ((pasted (after "V j y g e k k t p" (format nil "1~c~%2~c~%ab~c~%cd~c~%" #\Return #\Return #\Return #\Return))))
        (is = 4 (count #\Return pasted))
        (is = 2 (count #\2 pasted))))))
