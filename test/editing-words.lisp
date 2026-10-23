;;;; test/editing-words.lisp --- What a word is, the same for every command that looks for one.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.tests)

(define-test editing-words
  :description "Words alike everywhere: moving by words, counting them, double-clicking one, a snippet's
value names, capitalising, and the case of a small region and a large one.")

(defparameter *word-test-chars*
  (append (loop for code from 0 below 128 collect (code-char code))
          (map 'list #'identity "éÉßøŁłΩωЖжאב中文٣４ـ_-'’…·"))
  "The characters a word's edges are tried on: ASCII, and letters, digits and marks of other scripts.")

(define-test (editing-words one-definition)
  ;; a double click's word and a snippet's value name are the letters, digits and _ a name holds
  (dolist (char *word-test-chars*)
    (unless (member char '(#\Newline #\Return))
      (is eq (and (tuatara.defaults::name-char-p char) t) (eq :word (tuatara.defaults::click-class char)) "~s" char))
    (let ((text (format nil "~ax" char)))
      (is = (if (tuatara.defaults::name-char-p char) 2 0) (tuatara.defaults::snippet-word-end text 0) "~s" char)))
  ;; count-words counts the words moving by words finds
  (dolist (text (list "kia ora koutou" "tūī_2, kea-3 ٣٤ 中文 x'y" "  one  " "a.b,c;d" ""))
    (let ((buffer (tuatara.buffer:make-buffer (tuatara.buffer:unique-buffer-name "words") :text text)))
      (unwind-protect
           (is = (loop with p = 0
                       for next = (tuatara.defaults::words-forward buffer p 1)
                       while (and (> next p) (tuatara.defaults::word-char-p (tuatara.buffer:buffer-char-at buffer (tuatara.buffer:buffer-forward-char buffer next -1))))
                       count t
                       do (setf p next))
               (nth-value 2 (tuatara.defaults::count-text (tuatara.buffer:buffer-text buffer) 0 (tuatara.buffer:buffer-size buffer)))
               "~s" text)
        (tuatara.buffer:kill-buffer buffer)))))
