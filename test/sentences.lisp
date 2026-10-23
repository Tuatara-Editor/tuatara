;;;; test/sentences.lisp --- Where sentences end: moving by them, and counting them, alike.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.tests)

(define-test sentences
  :description "Sentences: moving to the next one reads no further than its end, however long the paragraph;
count-words counts the sentences moving by them finds.")

(defun sentence-buffer (text &key double-space)
  "A new buffer holding TEXT, its sentences two spaces apart with DOUBLE-SPACE."
  (let ((buffer (tuatara.buffer:make-buffer (tuatara.buffer:unique-buffer-name "sentences") :text text)))
    (setf (tuatara.buffer:buffer-local buffer :sentence-end-double-space) double-space)
    buffer))

(defun sentence-steps (buffer)
  "How many sentences moving forward from the start of BUFFER finds before
its text ends."
  (let* ((size (tuatara.buffer:buffer-size buffer))
         (last (or (position-if-not (lambda (c) (member c '(#\Space #\Tab #\Newline #\Return)))
                                    (tuatara.buffer:buffer-string buffer) :from-end t)
                   -1)))
    (loop with p = 0
          for next = (tuatara.defaults::sentences-forward buffer p 1)
          while (and (> next p) (<= next size) (<= p last))
          count t
          do (setf p next))))

(define-test (sentences far-without-blank-lines)
  ;; the paragraph's end is looked for only up to the sentence's: a sentence ending on the
  ;; next line is found at once, in a paragraph of megabytes or one past the scan's limit
  (let* ((rest (with-output-to-string (o) (dotimes (i 200000) (format o "a line of words without an end~%"))))
         (big (sentence-buffer (format nil "First words~%of a sentence. Then more~%~a" rest)))
         (small (sentence-buffer (format nil "First words~%of a sentence. Then more~%a line~%"))))
    (unwind-protect
         (progn
           (is = 26 (tuatara.defaults::sentences-forward big 0 1))
           (is = 27 (tuatara.defaults::sentences-backward big 40 1))
           (let ((far (least-seconds (lambda () (dotimes (i 20) (tuatara.defaults::sentences-forward big 0 1)))))
                 (near (least-seconds (lambda () (dotimes (i 20) (tuatara.defaults::sentences-forward small 0 1))))))
             (true (< far (+ 0.01 (* 5 near))) "~,4fs for a sentence in 6 MB, ~,4fs in a few lines" far near))
           (let ((tuatara.defaults::*text-scan-limit* 1000))
             (is = 26 (tuatara.defaults::sentences-forward big 0 1))
             (is = 27 (tuatara.defaults::sentences-backward big 40 1))))
      (tuatara.buffer:kill-buffer big)
      (tuatara.buffer:kill-buffer small))))

(define-test (sentences counted-as-moved)
  ;; count-words' sentences are those M-e moves through, with one space after a full stop
  ;; and with two, closing marks, abbreviations, other ends and paragraphs
  (dolist (text (list "Hello. World." "Hello.  World." "Dr. Smith came. He left." "One!	Two? Three…"
                      "He said \"Stop.\" Then (quietly.) Last." "It costs 1.5 dollars. Cheap."
                      (format nil "A. B.~%~%C.") (format nil "First line.~%Second line") "Wait..  Next one"
                      "e.g. this one. That one." "Ends?! Really.  Yes"))
    (dolist (double-space '(nil t))
      (let ((buffer (sentence-buffer text :double-space double-space)))
        (unwind-protect
             (is = (sentence-steps buffer)
                 (nth-value 1 (tuatara.defaults::count-text (tuatara.buffer:buffer-text buffer) 0 (tuatara.buffer:buffer-size buffer)
                                                            :double-space double-space))
                 "~s~:[~;, two spaces apart~]" text double-space)
          (tuatara.buffer:kill-buffer buffer))))))
