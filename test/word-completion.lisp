;;;; test/word-completion.lisp --- Completing a word from the words in the buffers: M-/, C-M-/ and C-M-i.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.tests)

(define-test word-completion
  :description "M-/ puts in the nearest word that starts with the text before point and the next nearest
when typed again; C-M-/ lists them; words come from other buffers but not from private ones; the case typed
is kept; only a stretch around point is read.")

(defun word-keys (keys &key (text "") (width 80) other private own-private)
  "Run KEYS in a scratch buffer holding TEXT, point at its end, after making a buffer named \"other-words\"
holding OTHER, safely opened when PRIVATE, and the scratch buffer opened safely first when OWN-PRIVATE;
return the text after, and the frontend."
  (let ((made (tuatara.buffer:find-buffer "other-words")))
    (when made (tuatara.buffer:kill-buffer made)))
  (when other
    (let ((b (tuatara.buffer:make-buffer "other-words" :text other)))
      (when private (setf (tuatara.buffer:buffer-local b :safe-visit) t))))
  (multiple-value-bind (f editor)
      (run-keys (list "M->"
                      (lambda ()
                        (when own-private
                          (setf (tuatara.buffer:buffer-local (tuatara.command:current-buffer) :safe-visit) t)))
                      keys)
                :text text :width width)
    (values (session-text editor) f)))

(define-test (word-completion nearest-first-and-cycling)
  ;; before point nearest first, then after it
  (is string= "apricot apple apple" (word-keys "M-/" :text "apricot apple ap"))
  (is string= "apricot apple apricot" (word-keys "M-/ M-/" :text "apricot apple ap"))
  ;; the words run out: the text as typed comes back, then the first again
  (is string= "apricot apple ap" (word-keys "M-/ M-/ M-/" :text "apricot apple ap"))
  (is string= "apricot apple apple" (word-keys "M-/ M-/ M-/ M-/" :text "apricot apple ap"))
  ;; the word at point is not one of them, and words after point come last
  (is string= "apple almond apple" (word-keys "C-a M-f M-/" :text "ap almond apple"))
  (is string= "apex almond apple apex" (word-keys "C-a M-f M-/ M-/" :text "ap almond apple apex"))
  ;; nothing starts with it: said
  (is string= "qq zebra qq" (word-keys "M-/" :text "qq zebra qq"))
  (true (showing (nth-value 1 (word-keys "M-/" :text "qq zebra qq")) "No word")))

(define-test (word-completion other-buffers)
  (is string= "zebra" (word-keys "M-/" :text "ze" :other "zebra stripes"))
  ;; this buffer's words come before another's
  (is string= "zero zero" (word-keys "M-/" :text "zero ze" :other "zebra"))
  (is string= "zero zebra" (word-keys "M-/ M-/" :text "zero ze" :other "zebra"))
  ;; a file opened safely gives nothing to others, but to a buffer opened safely too
  (is string= "ze" (word-keys "M-/" :text "ze" :other "zebra" :private t))
  (is string= "zebra" (word-keys "M-/" :text "ze" :other "zebra" :private t :own-private t))
  ;; the same typing in the insert state
  (is string= "zero zero" (word-keys "i M-/" :text "zero ze")))

(define-test (word-completion case)
  (is string= "Foobar foobar" (word-keys "M-/" :text "Foobar foo"))
  (is string= "foobar FOOBAR" (word-keys "M-/" :text "foobar FOO"))
  (is string= "FOObar Foobar" (word-keys "M-/" :text "FOObar Foo"))
  (let ((tuatara-user:*word-completion-keep-case* nil))
    (is string= "Foobar foo" (word-keys "M-/" :text "Foobar foo"))
    (is string= "foobar foobar" (word-keys "M-/" :text "foobar foo"))))

(define-test (word-completion listed-and-completion-at-point)
  (let ((f (nth-value 1 (word-keys "C-M-/" :text "apricot apple ap"))))
    (true (showing f " apple "))
    (true (showing f " apricot ")))
  (let ((f (nth-value 1 (word-keys "C-M-/" :text "ap" :other "apron apex"))))
    (true (showing f "apron" "other-words")))
  ;; RET puts in the chosen word
  (is string= "apricot apple apricot" (word-keys "C-M-/ C-n RET" :text "apricot apple ap"))
  ;; C-M-i where the mode offers nothing lists the words
  (true (showing (nth-value 1 (word-keys "M-x c o m p l e t i o n - a t - p o i n t RET" :text "apricot apple ap"))
                 " apple ")))

(define-test (word-completion bounded)
  ;; a word farther than the stretch read is not found, one inside it is
  (let* ((gap (make-string (+ tuatara.defaults::*word-completion-reach* 1000) :initial-element #\Space))
         (far (format nil "needle~a" gap))
         (near (format nil "needle~a" (make-string 100 :initial-element #\Space))))
    (is string= (format nil "~aneed" far) (word-keys "M-/" :text (format nil "~aneed" far)))
    (is string= (format nil "~aneedle" near) (word-keys "M-/" :text (format nil "~aneed" near)))))
