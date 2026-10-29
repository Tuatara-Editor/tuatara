;;;; src/command/keys.lisp --- Keys named in text: \[command] and the rest.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; As Emacs's substitute-command-keys: documentation, help and messages name
;;;; a command's keys as \[COMMAND], and are shown with the keys that run it
;;;; at the time, so that a key bound anew is the one they name. The keys
;;;; written in are marked, so that wherever the text goes afterwards (put
;;;; together with more by FORMAT, cut to a sentence, wrapped) the places
;;;; that show it can take the marks out and colour the keys.

(in-package #:tuatara.command)

(defvar *text-spans* (make-hash-table :test 'eq :weakness :key :synchronized t)
  "The coloured parts of texts UNMARK-KEYS made, by text: each a list of
(START END FACE) in characters.")

(defun text-spans (text)
  "The coloured parts UNMARK-KEYS found in TEXT, each (START END FACE) in
characters; NIL for any other text."
  (and (stringp text) (values (gethash text *text-spans*))))

(defun unmark-keys (text &optional (face :help-key-binding))
  "TEXT without the marks SUBSTITUTE-COMMAND-KEYS puts round keys, and as a
second value the parts that were keys, each (START END FACE) in characters of
the text returned, FACE being :HELP-KEY-BINDING unless given; those are kept
for the text returned (TEXT-SPANS). TEXT itself when it has no marks."
  (if (or (not (stringp text)) (not (find-if #'key-mark-p text)))
      (values text (text-spans text))
      (let ((out (make-string-output-stream)) (spans '()) (position 0) (open nil))
        (loop for c across text
              do (cond ((char= c +key-open+) (setf open position))
                       ((char= c +key-close+)
                        (when (and open (< open position)) (push (list open position face) spans))
                        (setf open nil))
                       (t (write-char c out) (incf position))))
        ;; a key cut short, by a sentence cut or a line's end, goes on to the end
        (when (and open (< open position)) (push (list open position face) spans))
        (let ((plain (get-output-stream-string out)) (spans (nreverse spans)))
          (setf (gethash plain *text-spans*) spans)
          (values plain spans)))))

(defun mark-key (text)
  "TEXT between the marks of a key written into a text (SUBSTITUTE-COMMAND-KEYS)."
  (concatenate 'string (string +key-open+) text (string +key-close+)))

;;; Finding a command's keys

(defvar *key-keymaps-function* nil
  "A function of no arguments giving the keymaps \\=\\[COMMAND] looks in, in
order, when the text has not named one with \\=\\<KEYMAP> (or after that one):
the global keymap, then the NORMAL state's keymap. NIL means the editor's global
keymap alone.")

(defvar *keymap-named-function* nil
  "A function of a name, a string, giving the keymap \\=\\<NAME> and \\=\\{NAME}
mean, or NIL when there is none.")

(defun key-keymaps (keymap)
  "Where \\=\\[COMMAND] looks for COMMAND's keys: KEYMAP first when the text
named one, then the keymaps of *KEY-KEYMAPS-FUNCTION*."
  (let ((standard (cond (*key-keymaps-function* (funcall *key-keymaps-function*))
                        (*editor* (list (editor-global-keymap *editor*)))
                        (t '()))))
    (if keymap (cons keymap (remove keymap standard)) standard)))

(defun key-description< (a b)
  "True when the key description A (C-x C-f) is named before B where a command
has both: the fewer keys first, then the shorter to read, then by name."
  (let ((na (count #\Space a)) (nb (count #\Space b)))
    (or (< na nb)
        (and (= na nb) (< (length a) (length b)))
        (and (= na nb) (= (length a) (length b)) (string< a b)))))

(defun command-key (command keymaps)
  "The keys that run COMMAND in the first of KEYMAPS that has any, as a key
description (C-x C-f), the first by KEY-DESCRIPTION<; NIL when none has."
  (dolist (map keymaps nil)
    (let ((keys (where-is command (list map))))
      (when keys
        (return (first (sort (copy-list keys) #'key-description<)))))))

(defun keymap-called (name)
  "The keymap \\=\\<NAME> and \\=\\{NAME} mean, by *KEYMAP-NAMED-FUNCTION*, or NIL."
  (and *keymap-named-function* (funcall *keymap-named-function* name)))

(defun keymap-summary (keymap)
  "KEYMAP's keys, each on a line with the command it runs, keys marked."
  (let ((lines '()))
    (map-keymap-bindings (lambda (keys command)
                           (push (cons (tuatara.frontend:key-sequence-description keys) command) lines))
                         (list keymap))
    (let* ((lines (sort lines #'string< :key #'car))
           (width (reduce #'max lines :key (lambda (l) (length (car l))) :initial-value 0)))
      (format nil "~:{~a~va~(~a~)~%~}"
              (mapcar (lambda (l) (list (mark-key (car l)) (+ 2 (- width (length (car l)))) "" (cdr l)))
                      lines)))))

;;; The text

(defun valid-key-text-p (text)
  "True when TEXT is a key sequence (C-x C-f, RET, <f5>), or M-x and a
command's name."
  (or (and (> (length text) 4) (string= "M-x " text :end2 4))
      (and (plusp (length text))
           (ignore-errors (tuatara.frontend:parse-key-sequence text) t))))

(defvar *substituted-texts* (make-hash-table :test 'eq :weakness :key :synchronized t)
  "What SUBSTITUTE-COMMAND-KEYS gave lately, by the text it was given: for each
a list of (STAMP KEYMAPS MARKS FOR-FORMAT RESULT), STAMP saying how the
keymaps and the commands were then.")

(defun substitute-command-keys (string &key keymap marks for-format)
  "STRING with keys written in, as WRITE-COMMAND-KEYS writes them (which see
for KEYMAP, MARKS and FOR-FORMAT): worked out once for a text while the
keymaps and the commands stay as they are, and given again after that.
Complexity: O(1) for a text done before, else that of WRITE-COMMAND-KEYS."
  (if (or (not (stringp string)) (not (find #\\ string)))
      string
      (let ((stamp (cons *keymaps-generation* (hash-table-count *commands*)))
            (keymaps (key-keymaps keymap))
            (marks (and marks t))
            (for-format (and for-format t))
            (done (gethash string *substituted-texts*)))
        (or (fifth (find-if (lambda (entry)
                              (and (equal (first entry) stamp) (equal (second entry) keymaps)
                                   (eq (third entry) marks) (eq (fourth entry) for-format)))
                            done))
            (let ((result (write-command-keys string :keymap keymap :marks marks :for-format for-format)))
              (setf (gethash string *substituted-texts*)
                    (cons (list stamp keymaps marks for-format result)
                          (loop for entry in done
                                for i below 3
                                when (equal (first entry) stamp) collect entry)))
              result)))))

(defun write-command-keys (string &key keymap marks for-format)
  "STRING with keys written in: \\=\\[COMMAND] becomes the keys that run
COMMAND, or M-x COMMAND where none do, looked for in the keymap \\=\\<KEYMAP>
named last before it, then in the global keymap and the NORMAL state's keymap (a
major mode's keys only where its keymap is named, as \\=\\<buffers-mode>);
\\=\\{KEYMAP} becomes a list of KEYMAP's keys and the commands they run;
\\=\\`KEYS' becomes KEYS; \\=\\= keeps the character after it as it is. KEYMAP,
when given, is where to look before any \\=\\<KEYMAP>. With MARKS the keys come
between marks for UNMARK-KEYS, which the places that show text take out,
colouring the keys; without, the text comes plain, its keys' parts kept for it
(TEXT-SPANS). With FOR-FORMAT a ~ in what is written in is doubled, STRING
being a control string for FORMAT."
  (if (or (not (stringp string)) (not (find #\\ string)))
      string
      (let ((out (make-string-output-stream)) (i 0) (n (length string)) (map keymap))
        (flet ((put-key (text)
                 (let ((text (if for-format (cl-ppcre:regex-replace-all "~" text "~~") text)))
                   (write-string (mark-key text) out)))
               (closing (char start)
                 (position char string :start start)))
          (loop while (< i n)
                do (let ((c (char string i))
                         (next (and (< (1+ i) n) (char string (1+ i)))))
                     (cond ((not (and (char= c #\\) next (find next "=[<{`")))
                            (write-char c out) (incf i))
                           ;; \= keeps the next character as it is
                           ((char= next #\=)
                            (when (< (+ i 2) n) (write-char (char string (+ i 2)) out))
                            (incf i 3))
                           ((char= next #\[)
                            (let ((close (closing #\] (+ i 2))))
                              (if (null close)
                                  (progn (write-char c out) (incf i))
                                  (let* ((name (subseq string (+ i 2) close))
                                         (command (command-symbol name))
                                         (key (and command (command-key command (key-keymaps map)))))
                                    (put-key (or key (format nil "M-x ~(~a~)" name)))
                                    (setf i (1+ close))))))
                           ((or (char= next #\<) (char= next #\{))
                            (let ((close (closing (if (char= next #\<) #\> #\}) (+ i 2))))
                              (if (null close)
                                  (progn (write-char c out) (incf i))
                                  (let ((named (keymap-called (subseq string (+ i 2) close))))
                                    (if (char= next #\<)
                                        (setf map named)
                                        (when named
                                          (let ((summary (keymap-summary named)))
                                            (write-string (if for-format (cl-ppcre:regex-replace-all "~" summary "~~") summary)
                                                          out))))
                                    (setf i (1+ close))))))
                           ;; \`KEYS'
                           (t
                            (let ((close (closing #\' (+ i 2))))
                              (if (and close (valid-key-text-p (subseq string (+ i 2) close)))
                                  (progn (put-key (subseq string (+ i 2) close))
                                         (setf i (1+ close)))
                                  (progn (write-char c out) (incf i))))))))
          (let ((text (get-output-stream-string out)))
            (if marks text (values (unmark-keys text))))))))
