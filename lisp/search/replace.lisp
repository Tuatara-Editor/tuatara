;;;; lisp/search/replace.lisp --- Applying replacements: all at once, or asking at each.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; The two ways a replacement typed at the search prompt is carried out
;;;; (substitute.lisp): every match as one edit batch, or match by
;;;; match with y n ! . q. There are no separate replace commands; M-%, C-M-%
;;;; and NORMAL state's r q and r r open the search prompt set up for replacing.

(in-package #:tuatara.defaults)

(defun replaced-words (count &optional lines)
  "How a replacement says what it did: Replaced 43,501 occurrences, on 2 lines
when LINES is given, the numbers grouped in thousands."
  (format nil "Replaced ~:d occurrence~:p~@[ on ~:d line~:p~]" count lines))

(defun matches-on-lines-words (matches lines)
  "How a count of matches says what it found: 43,501 matches on 2 lines, the
numbers grouped in thousands."
  (format nil "~:d ~:*~[matches~;match~:;matches~] on ~:d line~:p" matches lines))

(defun replace-matches (buffer matches replacement-for)
  "Replace MATCHES, a list of (START END ...) in order, with what REPLACEMENT-FOR
(a function of the match) returns, as one edit batch. A match in folded text
is replaced too, its fold kept folded and what replaces it hidden with it
\(TUATARA.DISPLAY:HOLD-FOLDS). Returns the count."
  (let ((edits (map 'simple-vector
                    (lambda (m) (tuatara.text:make-edit (first m) (second m)
                                                        (let ((r (funcall replacement-for m)))
                                                          (and (plusp (length r)) (tuatara.text:string-octets r)))))
                    matches)))
    (when (plusp (length edits))
      (let ((held (tuatara.display:hold-folds buffer matches)))
        (unwind-protect (tuatara.buffer:buffer-edit buffer edits)
          (tuatara.display:release-folds buffer held))))
    (length edits)))

(defvar *asked-match* nil
  "While a replacement asks at a match, the match as (BUFFER START END).")

(defun asked-match-highlights (editor window rows)
  "Highlight function: the match a replacement asks at (*ASKED-MATCH*) in
:search-current, in the windows showing its buffer."
  (declare (ignore editor rows))
  (let ((asked *asked-match*))
    (when (and asked (eq (tuatara.display:window-buffer window) (first asked)))
      (list (list (second asked) (third asked) :search-current)))))

(defun query-replace-loop (buffer next-match replacement-for from-description to-description &key rest)
  "Walk the matches from point: NEXT-MATCH (a function of a byte offset) returns
the next (START END ...) or NIL; REPLACEMENT-FOR gives each match's replacement
text. At each match, lit in :search-current with point after it, ask \\`y'
\(replace), \\`n' (skip), \\`!' (replace all remaining as one batch), \\`.'
\(replace and stop), \\`q' or \\`RET' (stop).
REST, when given, gathers all the remaining matches for \\`!' from the start
of the match asked at, as three values: the list, NIL or a reason it gave
up (:STOPPED, :OVER, or :BACKGROUND when it handed them to a job of their
own) and a count; without it they are gathered through NEXT-MATCH. A match
in folded text
is asked at too: its fold is opened first, so that it is seen, the other folds
left as they are; \\`!' replaces those still folded with their folds kept.
Returns the number replaced."
  (let ((pos (point)) (replaced 0) (*asked-match* nil))
    ;; painted after every other highlight, over the search's other matches
    (setf *highlight-functions* (append (remove 'asked-match-highlights *highlight-functions*) (list 'asked-match-highlights)))
    (unwind-protect
        (flet ((past (m at)
                 ;; after an empty match (^, $) the next is looked for a character on, so that it is never met again
                 (if (= (first m) (second m)) (tuatara.buffer:buffer-forward-char buffer at 1) at)))
        (loop
          (let ((m (and (<= pos (tuatara.buffer:buffer-size buffer)) (funcall next-match pos))))
            (unless m (return))
            (let ((folds (tuatara.display:folds-now buffer)))
              (when (and folds (tuatara.display:folds-hiding folds (first m) (second m)))
                (tuatara.display:unfold-region buffer (first m) (second m))))
            (goto (second m))
            (setf *asked-match* (list buffer (first m) (second m)))
            (let* ((key (read-key (format nil (substitute-command-keys "Query replacing ~a with ~a: (\\`y' \\`n' \\`!' \\`.' \\`q')"
                                                                        :marks t :for-format t)
                                          from-description to-description)))
                   (base (tuatara.frontend:key-base key)))
              (setf *asked-match* nil)
              (case base
                ((#\y #\Space)
                 (let ((after (tuatara.buffer:buffer-replace buffer (first m) (second m) (funcall replacement-for m))))
                   (incf replaced) (goto after)
                   (setf pos (if (= after (tuatara.buffer:buffer-size buffer)) (1+ after) (past m after)))))
                ((#\n #\Backspace :backspace)
                 (setf pos (if (= (second m) (tuatara.buffer:buffer-size buffer)) (1+ (second m)) (past m (second m)))))
                (#\!
                 (multiple-value-bind (remaining status count)
                     (if rest
                         (funcall rest (first m))
                         (loop for match = (funcall next-match pos)
                                 then (and (< (second match) (tuatara.buffer:buffer-size buffer)) (funcall next-match (past match (second match))))
                               while match collect match))
                   (when status
                     ;; :background, the rest gone to a job of their own, which says how it ends
                     (unless (eq status :background)
                       (message "~:[Stopped~;Too many to replace in memory~] after ~:d more; ~:d replaced, the rest unchanged"
                                (eq status :over) count replaced))
                     (return-from query-replace-loop replaced))
                   (incf replaced (replace-matches buffer remaining replacement-for))
                   (return)))
                (#\.
                 (tuatara.buffer:buffer-replace buffer (first m) (second m) (funcall replacement-for m))
                 (incf replaced) (return))
                (t (return)))))))
      (setf *highlight-functions* (remove 'asked-match-highlights *highlight-functions*)))
    (message "~a" (replaced-words replaced))
    replaced))
