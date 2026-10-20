;;;; lisp/editing/pairs.lisp --- Pairs of delimiters: closed as they are typed, put around the region.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; *pair-delimiters* names the pairs: ( ), [ ], { }, quotes, and the rest.
;;;; With electric-pair-mode on, an opening one typed writes its closing half
;;;; after point, the closing half typed where it stands steps over it, and
;;;; DEL between an empty pair takes both. insert-pair (M-() puts a pair
;;;; around the region, or takes it off when it is already there, or inserts
;;;; an empty pair with point in it.

(in-package #:tuatara.defaults)

(defvar *pair-delimiters*
  '((#\{ . #\}) (#\[ . #\]) (#\( . #\)) (#\< . #\>)
    (#\" . #\") (#\' . #\') (#\` . #\`) (#\‘ . #\’) (#\“ . #\”)
    (#\« . #\») (#\‹ . #\›)
    (#\= . #\=) (#\* . #\*) (#\~ . #\~) (#\| . #\|) (#\\ . #\\)
    (#\/ . #\/) (#\: . #\:) (#\+ . #\+) (#\_ . #\_) (#\$ . #\$))
  "The pairs of delimiters, each (OPEN . CLOSE); a character that both opens
and closes is its own pair. They are what electric-pair-mode closes as they are
typed (those *ELECTRIC-PAIR-TYPING* names), what \\[insert-pair] puts around the
region, and what select-pair selects within and the wrap keys wrap with.")

(defvar *electric-pair-mode* nil
  "True to close delimiters as they are typed: an opening one writes its closing half after point, a closing half typed where
it stands steps over it, and \\[delete-backward-char] between an empty pair
takes both. Off unless asked for, by \\[electric-pair-mode] or in the init file.")

(defvar *electric-pair-typing* :safe
  "Which delimiters close themselves as they are typed: :safe, the pairs of
*PAIR-DELIMITERS* less those *ELECTRIC-PAIR-SAFE-EXCLUSIONS* names; :all, every
one of them; or a list of the opening characters, taken as it is (an empty
list pairs nothing).")

(defvar *electric-pair-safe-exclusions* '(#\< #\' #\` #\= #\* #\~ #\| #\\ #\/ #\: #\+ #\_ #\$)
  "The delimiters :safe leaves out: ordinary text far more often than a
delimiter, such as the apostrophe in don't, a colon, less-than.")

(defun char-before-position (buffer position)
  "The character before POSITION in BUFFER, or NIL at its start."
  (and (plusp position)
       (tuatara.buffer:buffer-char-at buffer (tuatara.buffer:buffer-forward-char buffer position -1))))

(defun pair-delimiters ()
  "The pairs of *PAIR-DELIMITERS*, read as a list of pairs however it was set
(SETTING-LIST)."
  (setting-list '*pair-delimiters* *pair-delimiters* :pairs t))

(defun bind-free-delimiter-keys (keymap command)
  "Bind each half of every pair of *PAIR-DELIMITERS* to COMMAND in KEYMAP,
where the key is not bound already."
  (dolist (pair (pair-delimiters))
    (dolist (char (remove-if-not #'characterp (remove-duplicates (list (car pair) (cdr pair)))))
      (let ((key (list (list char))))
        (unless (lookup-key keymap key)
          (define-key keymap key command))))))

(defun pair-open-for (char)
  "The opening half of the pair CHAR is part of, or NIL."
  (let ((pairs (pair-delimiters)))
    (cond ((assoc char pairs) char)
          ((rassoc char pairs) (car (rassoc char pairs))))))

(defun pair-closer (open)
  "The closing half of the pair OPEN opens."
  (cdr (assoc open (pair-delimiters))))

(defun wrap-pair (char)
  "The pair a wrap key CHAR puts around text, as two values, its opening and
closing halves: the pair of *PAIR-DELIMITERS* CHAR is either half of, or CHAR
on both sides when it is in no pair. Every wrap, of a selection, a split's
places, the cursors' selections and a rectangle's rows, takes its pair from
here."
  (let ((open (or (pair-open-for char) char)))
    (values open (or (pair-closer open) open))))

;;; Closed as they are typed

(defun pair-typing-openers ()
  "The opening delimiters that close themselves as they are typed."
  (let ((openers (mapcar #'car (pair-delimiters))))
    (case *electric-pair-typing*
      (:safe (let ((exclusions (setting-list '*electric-pair-safe-exclusions* *electric-pair-safe-exclusions*)))
               (remove-if (lambda (c) (member c exclusions)) openers)))
      (:all openers)
      (t (intersection (setting-list '*electric-pair-typing* *electric-pair-typing*) openers)))))

(defun electric-pair-typed (buffer char)
  "After CHAR was typed into BUFFER: when it closes a pair and the same
character follows, step over that one instead of keeping two; when it opens
a pair, write the closing half after point."
  (when *electric-pair-mode*
    (let* ((openers (pair-typing-openers))
           (p (point))
           (open (pair-open-for char)))
      (cond ((and open (member open openers)
                  (eql (tuatara.buffer:buffer-char-at buffer p) char)
                  (eql char (pair-closer open)))
             ;; typed over a closer already there: keep one, and go past it
             (let ((before (tuatara.buffer:buffer-forward-char buffer p -1)))
               (tuatara.buffer:buffer-edit buffer (vector (tuatara.text:make-edit before p)) :group t)
               (goto (tuatara.buffer:buffer-forward-char buffer before))))
            ((member char openers)
             (tuatara.buffer:buffer-insert buffer p (string (pair-closer char)) :group t)
             (goto p))))))

(defun electric-pair-delete (buffer)
  "\\[delete-backward-char] between the two halves of an empty pair: take both.
True when it did."
  (when *electric-pair-mode*
    (let* ((p (point))
           (before (char-before-position buffer p))
           (after (tuatara.buffer:buffer-char-at buffer p)))
      (when (and before after (member before (pair-typing-openers)) (eql after (pair-closer before)))
        (let ((start (tuatara.buffer:buffer-forward-char buffer p -1))
              (end (tuatara.buffer:buffer-forward-char buffer p)))
          (tuatara.buffer:buffer-delete buffer start end)
          (goto start)
          t)))))

(define-command electric-pair-mode ()
  "Turn closing delimiters as they are typed on or off. With it on, ( { [ \" and the other pairs
*ELECTRIC-PAIR-TYPING* names write their closing half after point, typing that
half where it stands steps over it, and \\[delete-backward-char] between an
empty pair takes both; a delimiter typed with a count is typed that many
times and paired with none. This command with a count: on when it is
positive, off when it is zero or less."
  (setf *electric-pair-mode* (switched-by-count *electric-pair-mode*))
  (message "Electric pair mode ~:[off~;on~]" *electric-pair-mode*))

;;; Put around the region

(defun already-wrapped-p (buffer start end open close)
  "True when OPEN stands just before START and CLOSE just at END, unescaped."
  (and (plusp start) (< end (tuatara.buffer:buffer-size buffer))
       (eql (char-before-position buffer start) open)
       (eql (tuatara.buffer:buffer-char-at buffer end) close)
       (not (tuatara.text:rope-escaped-p (tuatara.buffer:buffer-text buffer) (tuatara.buffer:buffer-forward-char buffer start -1)))))

(defun wrap-between (buffer start end char)
  "Put the pair CHAR belongs to around BUFFER's octets START below END, or take
it off when it stands just outside them already; one change for undo
\(DELETE-SPANS). Where point goes, as two values: the first octet of what the
pair holds, and whether the pair was taken off."
  (multiple-value-bind (open close) (wrap-pair char)
    (if (already-wrapped-p buffer start end open close)
        (values (aref (delete-spans buffer (list (cons (tuatara.buffer:buffer-forward-char buffer start -1) start)
                                                 (cons end (tuatara.buffer:buffer-forward-char buffer end))))
                      0)
                t)
        (values (aref (delete-spans buffer (list (cons start start) (cons end end))
                                    :replacements (list (string open) (string close)))
                      0)
                nil))))

(defun pair-needs-space-p (char)
  "True when a pair inserted next to CHAR is set apart from it by a space:
next to a letter, a digit or the other side of a pair."
  (and char (or (alphanumericp char) (member char '(#\) #\] #\} #\( #\[ #\{ #\")))))

(define-command insert-pair ()
  "Put the pair of the delimiter of the key pressed around the region
\(\\`M-(' for parentheses), point on the first character inside; when the pair stands just outside the
region already, take it off. With no region, insert an empty pair with point
in it, a space set before or after it where it would touch a word. The pairs
are *PAIR-DELIMITERS*."
  (let* ((buffer (current-buffer))
         (key (editor-this-key *editor*))
         (char (or (and key (characterp (tuatara.frontend:key-base key)) (pair-open-for (tuatara.frontend:key-base key))) #\()))
    (if (and (tuatara.buffer:buffer-mark-active-p buffer)
             (multiple-value-bind (start end) (tuatara.buffer:buffer-region buffer) (< start end)))
        (multiple-value-bind (start end) (tuatara.buffer:buffer-region buffer)
          (let ((at (wrap-between buffer start end char)))
            (setf (tuatara.buffer:buffer-mark-active-p buffer) nil)
            (goto at)))
        (let* ((p (point))
               (open (string char))
               (close (string (pair-closer char)))
               (before (and (pair-needs-space-p (char-before-position buffer p)) " "))
               (after (and (pair-needs-space-p (tuatara.buffer:buffer-char-at buffer p)) " "))
               (text (format nil "~@[~a~]~a~a~@[~a~]" before open close after)))
          (tuatara.buffer:buffer-insert buffer p text)
          (goto (+ p (tuatara.text:string-octet-length (format nil "~@[~a~]~a" before open))))))))

(defun install-pairs ()
  "Have typing and \\[delete-backward-char] consult electric-pair-mode."
  (pushnew 'electric-pair-typed *post-self-insert-functions*)
  (pushnew 'electric-pair-delete *delete-backward-functions*))
