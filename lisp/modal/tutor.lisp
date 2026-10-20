;;;; lisp/modal/tutor.lisp --- The tutor: a buffer to learn Tuatara's modal keys by editing it.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.defaults)

(define-face :tutor-key "Keys in the tutor." :inherit :key-hint-key)
(define-face :tutor-state "The names of the modal states in the tutor: NORMAL, INSERT, SUPPORT, TUATARA[N]." :inherit :keyword)
(define-face :tutor-arrow "The arrow before each line to practise on in the tutor." :inherit :builtin)

(defparameter *tuatara-tutor-text*
  #.(data-file-text "tutor.txt")
  "The tutor as written: \\=\\[COMMAND] stands for the keys that run
COMMAND, \\=\\`KEYS' for keys written as they are, and \\=\\= for the character
after it as it is.")

(define-major-mode tuatara-tutor-mode text-mode
  "The tutor: its own instructions, to be edited as it says, keys and
the names of the states in faces of their own."
  :lighter "Tutor" :interactive nil)

(defun tutor-keys-for (command)
  "The keys that run COMMAND in NORMAL state, or else anywhere, as written:
the fewest keys first, then the shortest (KEY-DESCRIPTION<). NIL when no key
runs it."
  (sort (copy-list (or (where-is command (list *tuatara-normal-keymap*))
                       (where-is command (list *global-keymap*))))
        #'key-description<))

(defun tutor-text ()
  "The tutor's text with every command named by its keys, and as a second value
where the keys stand in it, a list of (START . END) byte offsets."
  (let* ((source *tuatara-tutor-text*)
         (out (make-string-output-stream))
         (bytes 0) (spans '()) (i 0) (n (length source)))
    (flet ((emit (string &optional key)
             (let ((size (tuatara.text:string-octet-length string)))
               (when key (push (cons bytes (+ bytes size)) spans))
               (write-string string out)
               (incf bytes size))))
      (loop while (< i n)
            do (let ((c (char source i)))
                 (cond ((and (char= c #\\) (< (1+ i) n) (char= (char source (1+ i)) #\[))
                        (let* ((close (position #\] source :start i))
                               (name (subseq source (+ i 2) close))
                               (command (find-symbol (string-upcase name) '#:tuatara.defaults))
                               (keys (and command (tutor-keys-for command))))
                          (if keys (emit (first keys) t) (emit (format nil "M-x ~a" name) t))
                          (setf i (1+ close))))
                       ((and (char= c #\\) (< (1+ i) n) (char= (char source (1+ i)) #\`))
                        (let ((close (position #\' source :start (+ i 3))))
                          (emit (subseq source (+ i 2) close) t)
                          (setf i (1+ close))))
                       ((and (char= c #\\) (< (1+ i) n) (char= (char source (1+ i)) #\=))
                        (when (< (+ i 2) n) (emit (string (char source (+ i 2)))))
                        (incf i 3))
                       (t (emit (string c)) (incf i))))))
    (values (get-output-stream-string out) (nreverse spans))))

(defun tutor-key-highlights (buffer from to)
  "The keys of the tutor in BUFFER between FROM and TO, in :tutor-key."
  (let ((version (tuatara.buffer:buffer-version buffer)))
    (loop for (start . end) in (tuatara.buffer:buffer-local buffer 'tutor-keys)
          for s = (tuatara.text:anchor-position start version)
          for e = (tuatara.text:anchor-position end version)
          when (and (< s e) (< s to) (> e from))
            collect (list s e :tutor-key))))

(defun tutor-pattern-highlights (buffer from to)
  "The arrows and the names of the states in BUFFER between FROM and TO."
  (let* ((text (tuatara.buffer:buffer-string buffer :start from :end to))
         (ranges '()))
    (flet ((offset (i) (+ from (tuatara.text:string-octet-length text :end i))))
      (loop for (regex face) in '(("---->" :tutor-arrow)
                                  ("\\b(?:NORMAL|INSERT|SUPPORT)\\b|TUATARA\\[[NISE]\\]" :tutor-state))
            do (cl-ppcre:do-matches (s e regex text)
                 (push (list (offset s) (offset e) face) ranges))))
    ranges))

(defmethod mode-syntax-highlights ((mode tuatara-tutor-mode) buffer rows)
  "Show the tutor's patterns to type and the keys it names in ROWS of BUFFER."
  (when (plusp (length rows))
    (let ((from (tuatara.display:row-start (aref rows 0)))
          (to (tuatara.display:row-end (aref rows (1- (length rows))))))
      (append (tutor-pattern-highlights buffer from to) (tutor-key-highlights buffer from to)))))

(define-command tuatara-tutor ()
  "Open the tutor: an ordinary buffer holding its own lessons, to learn
the modal keys by editing it as it says, with every key the one you have. Coming back
to it finds it as you left it; kill the buffer and open it again for a fresh
copy."
  (let ((existing (tuatara.buffer:find-buffer "*Tutor*")))
    (if existing
        (show-buffer existing)
        (let ((b (tuatara.buffer:make-buffer "*Tutor*")))
          (multiple-value-bind (text spans) (tutor-text)
            (tuatara.buffer:buffer-insert b 0 text)
            (let ((version (tuatara.buffer:buffer-version b)))
              (setf (tuatara.buffer:buffer-local b 'tutor-keys)
                    (loop for (s . e) in spans
                          collect (cons (tuatara.text:make-anchor version s :bias :after)
                                        (tuatara.text:make-anchor version e :bias :before))))))
          (set-major-mode b 'tuatara-tutor-mode)
          ;; the lessons are where undo stops, not a change it takes back
          (setf (tuatara.buffer:buffer-undo-stack b) '() (tuatara.buffer:buffer-redo-stack b) '())
          (tuatara.buffer:buffer-mark-saved b)
          (setf (tuatara.buffer:buffer-point b) 0)
          (show-buffer b)))))
