;;;; lisp/search/light-filters.lisp --- The matches of a list's filters lit as those of a search are.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; The patterns of the filters narrowing a list joined into one, so that a
;;;; search with no pattern typed goes to the matches.

(in-package #:tuatara.defaults)

(defun shift-group-references (pattern by)
  "PATTERN, a regular expression, with each back reference (\\1, \\12) naming
the group BY further on, as it is when BY groups come before it in a longer
pattern. A number more than PATTERN has groups is an octal character, kept
as it is, and so is what a set ([...]) or \\Q...\\E holds."
  (if (zerop by)
      pattern
      (let ((groups (tuatara.syntax:pattern-group-count pattern)) (n (length pattern)) (i 0))
        (with-output-to-string (out)
          (loop while (< i n)
                do (let ((c (char pattern i))
                         (run (pattern-run-end pattern i)))
                     (cond (run
                            (write-string pattern out :start i :end run)
                            (setf i run))
                           ((and (char= c #\\) (< (1+ i) n) (digit-char-p (char pattern (1+ i)))
                                 (char/= (char pattern (1+ i)) #\0))
                            (let* ((end (or (position-if-not #'digit-char-p pattern :start (1+ i)) n))
                                   (number (parse-integer pattern :start (1+ i) :end end)))
                              (if (<= number groups)
                                  (format out "\\~d" (+ by number))
                                  (write-string pattern out :start i :end end))
                              (setf i end)))
                           ((and (char= c #\\) (< (1+ i) n))
                            (write-char c out) (write-char (char pattern (1+ i)) out) (incf i 2))
                           (t (write-char c out) (incf i)))))))))

(defun light-search-filters (list buffer)
  "Light in BUFFER the matches of the filters narrowing LIST, as the matches
of a search are lit after it ends (\\[search-toggle-highlight] takes them
away and back): all of them at once, each ignoring case as it does, its
groups its own (\\1 its first). Those that hide what they match, and those
that look at one column of a record or of a table, light nothing; one that
looks at a part of the entries lights what the list's LIGHT-PART gives
\(SET-SEARCH-FILTERING), or nothing. An empty \\[regex-search-forward]
\\<search>\\[search-repeat-forward] then goes to them."
  (let* ((light-part (and list (list-filtering list) (list-filtering-light-part (list-filtering list))))
         (filters (remove-if (lambda (filter)
                               (let ((where (search-filter-where filter)))
                                 (or (search-filter-hide filter) (search-filter-test filter) (consp where)
                                     (and where (null light-part)))))
                             (and list (search-filters list))))
         (groups 0))
    (when filters
      (let* ((string (format nil "~{~a~^|~}"
                             (mapcar (lambda (filter)
                                       (let* ((pattern (search-filter-string filter))
                                              (regex (if (search-filter-regex-p filter)
                                                         (prog1 (shift-group-references pattern groups)
                                                           (incf groups (tuatara.syntax:pattern-group-count pattern)))
                                                         (search-quote pattern t :parsed nil)))
                                              (where (search-filter-where filter)))
                                         (format nil "(?~:[-~;~]i:~a)" (search-filter-fold filter)
                                                 (if where (funcall light-part where regex) regex))))
                                     filters)))
             (state (make-search-state 0 t t nil nil buffer)))
        (setf (search-state-string state) string
              *search-lit* (cons state buffer) *search-lit-shown* t
              *last-search-string* string)))))
