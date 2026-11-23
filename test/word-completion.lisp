;;;; test/word-completion.lisp --- Completing a word from the words in the buffers: M-/, C-M-/ and C-M-i.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.tests)

(define-test word-completion
  :description "M-/ puts in the nearest word that starts with the text before point and the next nearest
when typed again; C-M-/ lists them; words come from other buffers but not from private ones; the case typed
is kept; only a stretch around point is read.")

(defun set-text-kind (buffer kind)
  "Make BUFFER hold text of KIND, :SAFE or :ENCRYPTED, or leave it for NIL."
  (case kind
    (:safe (setf (tuatara.buffer:buffer-local buffer :safe-visit) t))
    (:encrypted (setf (tuatara.buffer:buffer-local buffer :encrypted) (list :keys nil)))))

(defun word-keys (keys &key (text "") (width 80) other other-kind own-kind)
  "Run KEYS in a scratch buffer holding TEXT, point at its end, after making a buffer named \"other-words\"
holding OTHER of kind OTHER-KIND, and the scratch buffer made of kind OWN-KIND first;
return the text after, and the frontend."
  (let ((made (tuatara.buffer:find-buffer "other-words")))
    (when made (tuatara.buffer:kill-buffer made)))
  (when other
    (let ((b (tuatara.buffer:make-buffer "other-words" :text other)))
      (set-text-kind b other-kind)))
  (multiple-value-bind (f editor)
      (run-keys (append (list "M->"
                              (lambda () (set-text-kind (tuatara.command:current-buffer) own-kind)))
                        (if (listp keys) keys (list keys)))
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

(define-modal-test (word-completion other-buffers)
  (is string= "zebra" (word-keys "M-/" :text "ze" :other "zebra stripes"))
  ;; this buffer's words come before another's
  (is string= "zero zero" (word-keys "M-/" :text "zero ze" :other "zebra"))
  (is string= "zero zebra" (word-keys "M-/ M-/" :text "zero ze" :other "zebra"))
  ;; text of a kind (safe, decrypted) gives words only to buffers of the same kind, never to another
  (is string= "ze" (word-keys "M-/" :text "ze" :other "zebra" :other-kind :safe))
  (is string= "ze" (word-keys "M-/" :text "ze" :other "zebra" :other-kind :encrypted))
  (is string= "ze" (word-keys "M-/" :text "ze" :other "zebra" :own-kind :safe))
  (is string= "ze" (word-keys "M-/" :text "ze" :other "zebra" :own-kind :encrypted))
  (is string= "ze" (word-keys "M-/" :text "ze" :other "zebra" :own-kind :safe :other-kind :encrypted))
  (is string= "ze" (word-keys "M-/" :text "ze" :other "zebra" :own-kind :encrypted :other-kind :safe))
  (is string= "zebra" (word-keys "M-/" :text "ze" :other "zebra" :own-kind :safe :other-kind :safe))
  ;; the same typing in the insert state
  (is string= "zero zero" (word-keys "i M-/" :text "zero ze")))

(define-test (word-completion name-characters-by-mode)
  ;; which characters make a name: hyphens where the mode joins names with them, Lisp's own in Lisp
  (flet ((name-p (mode char) (tuatara.defaults::mode-name-char-p (make-instance mode) char)))
    (true (name-p 'tuatara.defaults::text-mode #\-))
    (false (name-p 'tuatara.defaults::python-mode #\-))
    (true (name-p 'tuatara.defaults::python-mode #\_))
    (true (name-p 'tuatara.defaults::css-mode #\-))
    (true (name-p 'tuatara.defaults::lisp-mode #\*))
    (true (name-p 'tuatara.defaults::lisp-mode #\:))
    (false (name-p 'tuatara.defaults::lisp-mode #\())
    (false (name-p 'tuatara.defaults::text-mode #\Space))))

(define-test (word-completion lisp-names-whole)
  ;; in Lisp a word is a whole name as written, its stars and colons with it
  (is string= "*print-base* *print-base*"
      (word-keys (list "M-x l i s p - m o d e RET" "M-/") :text "*print-base* *pr")))

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

(define-test (word-completion text-made-from-files-and-evidence)
  ;; a buffer open before its directory became evidence is of the evidence kind
  (with-test-directory (dir)
    (let ((b (tuatara.buffer:make-buffer "evidence-words" :text "zebra"
                                                          :file (namestring (merge-pathnames "e.txt" dir)))))
      (unwind-protect
           (progn (is eq nil (tuatara.defaults::word-completion-kind b))
                  (with-evidence-directories ((list (namestring dir)))
                    (is eq :evidence (tuatara.defaults::word-completion-kind b)))
                  (is eq nil (tuatara.defaults::word-completion-kind b)))
        (tuatara.buffer:kill-buffer b))))
  ;; text a command made, read-only and with no file, gives no words; a file's buffer does, read-only or not
  (let ((made (tuatara.buffer:make-buffer "made-words" :text "zebra")))
    (setf (tuatara.buffer:buffer-local made :read-only) t)
    (is string= "ze" (word-keys "M-/" :text "ze"))
    (tuatara.buffer:kill-buffer made)))

(define-test (word-completion what-a-word-is)
  ;; combining marks and the joiner are part of a word
  (let ((accent (format nil "cafe~c" (code-char #x301))))
    (is string= (format nil "~a ~a" accent accent) (word-keys "M-/" :text (format nil "~a caf" accent))))
  ;; a hyphen is part of a word in prose and Lisp, not in C
  (is string= "total-count total-count" (word-keys "M-/" :text "total-count total-cou"))
  (is string= "count total-count" (word-keys (list (lambda () (tuatara.defaults::set-major-mode
                                                                    (tuatara.command:current-buffer)
                                                                    'tuatara.defaults::python-mode))
                                                       "M-/")
                                                 :text "count total-cou"))
  ;; the full case mapping
  (is string=
      (format nil "stra~ce STRASSE" (code-char #xDF))
      (word-keys "M-/" :text (format nil "stra~ce STR" (code-char #xDF)))))

(define-test (word-completion undo)
  ;; all the words put in one after another are one step to undo
  (is string= "apricot apple ap" (word-keys "M-/ M-/ M-x u n d o RET" :text "apricot apple ap")))
