;;;; lisp/search/filter-commands.lisp --- Typing, keeping and taking away search filters.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; C-x / s narrows the list or file as the pattern is typed and RET keeps it as
;;;; a filter; C-x / l lists the filters, and the list's keys take them away.
;;;; The commands that narrow a file of records by a column or a part of the
;;;; entries are here too, and the hooks that keep the lines hidden.

(in-package #:tuatara.defaults)

;;; Typing, keeping and taking away filters

(define-major-mode search-filters-mode special-mode
  "The search filters narrowing a list (\\[search-filter-list]), in the order
they were added: each pattern, whether it is a regular expression or literal
text, whether it ignores case, and how many entries it leaves of those the
filters before it left. \\<search-filters-mode>\\[search-filter-remove] takes
away the filter on the line, the list widening, and \\[search-filter-remove-all]
all of them; \\[revert-buffer] lists them again; \\[quit-window] returns."
  :lighter "Filters" :interactive nil)

(defun filter-list-as-typed (state)
  "Narrow the list STATE, a search's state, filters (SEARCH-STATE-FILTERING)
to what its string matches, as the filter being typed; nothing when the
pattern does not compile yet, the list as it was; with nothing typed, as the
filters kept narrow it."
  (let* ((buffer (search-state-filtering state))
         (filtering (list-filtering-made buffer))
         (typing (list-filtering-typing filtering))
         (string (search-state-string state)))
    (setf (search-state-failing state) nil)
    (cond ((search-problem state))
          ((zerop (length (search-line-pattern (search-line-of state))))
           (when typing
             (setf (list-filtering-typing filtering) nil)
             (refilter buffer)))
          ((and typing (string= string (search-filter-string typing))
                (eq (and (search-state-regex state) t) (search-filter-regex-p typing))))
          (t (setf (list-filtering-typing filtering)
                   (make-search-filter string :regex (search-state-regex state) :matcher (search-matcher state)))
             (let ((*filter-as-typed* t))
               (refilter buffer))))))

(defun keep-typed-filter (buffer)
  "Keep the filter being typed on BUFFER, a list, as one of its filters; the
next narrows what it leaves."
  (let* ((filtering (list-filtering buffer))
         (typing (and filtering (list-filtering-typing filtering))))
    (when typing
      (start-due-filter-job buffer)
      (setf (list-filtering-filters filtering) (append (list-filtering-filters filtering) (list typing))
            (list-filtering-typing filtering) nil)
      (show-filtered buffer)
      (if (list-filtering-job filtering)
          (message "Filter ~a kept; the ~:[list narrows~;records narrow~] once searched" (search-filter-words typing)
                   (list-filtering-lines filtering))
          (multiple-value-bind (shown total) (filtered-counts buffer)
            (message "~:d of ~:d ~a left; \\[search-filter-list] lists the filters"
                     shown (or total 0) (list-filtering-nouns filtering)))))))

(defun drop-typed-filter (buffer)
  "Let the filter being typed on BUFFER, a list, go: the list shows what the
filters kept leave."
  (let ((filtering (list-filtering buffer)))
    (when (and filtering (list-filtering-typing filtering))
      (setf (list-filtering-typing filtering) nil)
      (refilter buffer))))

(defun take-away-filters (buffer filters)
  "Take FILTERS away from those kept on BUFFER, a list: the filters after the
first taken away are worked out again from what those before it leave, and
the list widens."
  (let* ((filtering (list-filtering buffer))
         (kept (list-filtering-filters filtering))
         (first (position-if (lambda (filter) (member filter filters)) kept)))
    (when first
      (forget-worked-out (nthcdr first kept))
      (setf (list-filtering-filters filtering) (remove-if (lambda (filter) (member filter filters)) kept))
      (refilter buffer))))

(defun filtered-list-at-hand ()
  "The list the filter commands act on: the current buffer when it is a list,
or the list the list of filters there is for; NIL when there is none."
  (let ((buffer (current-buffer)))
    (if (typep (buffer-major-mode buffer) 'search-filters-mode)
        (let ((list (tuatara.buffer:buffer-local buffer 'filtered-list)))
          (and list (live-buffer-p list) list))
        (and (search-filterable-p buffer) buffer))))

;;; The commands

(define-command search-filter-regex ()
  "Narrow the list here to the entries a regular expression matches, as it is
typed: those whose line matches, or what more the list holds of them (a mailbox
the headers and contents of its messages). Case is ignored while the pattern has
no capital letter and names no case ([[:upper:]], \\p{Lu}), as a search ignores
it. The pattern is typed in the minibuffer and edited there as any answer is,
the list narrowed again at each change. \\<search>\\[search-exit] keeps the
pattern as a filter on the list, and the next one narrows what it leaves;
\\[search-abort] lets the one typed go, the filters kept staying.
\\[search-toggle-regex] switches to literal text;
\\<minibuffer>\\[minibuffer-previous-history] and \\[minibuffer-next-history]
bring back the patterns typed before, in any question that takes one
\(*PATTERN-HISTORY*). The list shows above its entries how many are left and by
what; \\[search-filter-list] lists the filters, to take one away, and
\\[search-filter-remove-all] takes them all away. A list made again is narrowed
again. In a file of records, a line each, the records are narrowed the same way:
in a comma-separated values (CSV) or tab-separated values (TSV) file, a log, a
JSON Lines file and the others *LINE-RECORD-MODES* names. A line, or an entry's
line in a list, is matched when any part of a match lies on it. That runs from
the line holding the match's first character through the line holding its last,
its line end counted with it. So 1\\ny keeps the line ending in 1 and the line
starting with y, and a block wrapped over several lines keeps all of them. The
lines left out are hidden, never deleted, a CSV's header line always shown, and
the mode line says how many are left. Saving writes the whole file; editing the
lines shown is allowed, and a line edited so that it no longer matches stays in
sight until the filters are worked out again (a filter added or taken away, the
file read again or sorted), but nothing changes the lines hidden. Where a list
or file is large, what is typed is matched from the top of the window first and
shown at once (*SEARCH-FILTER-SCREEN-SECONDS*), and the rest in the background
on all but two cores, the mode line saying how far it has got (\\[stop-job]
stops it, taking the filter away); the editor goes on meanwhile. So is a pattern
so slow on some line that matching it there would hold the editor up, the prompt
saying slow pattern. A pattern that cannot be used shows why in the prompt, and
\\<search>\\[search-exit] waits for it to be mended."
  (let ((buffer (current-buffer)))
    (if (search-filterable-p buffer)
        (progn (list-filtering-made buffer)
               (search-start t t :filtering buffer))
        (say-unfilterable "this buffer has none"))))

(defparameter *search-filters-columns*
  '(("Filter" :width 40) ("Kind" :width 26) ("Leaves" :width 12 :right t))
  "The columns of the list of search filters, as TABLE-HEADING-PIECES takes them.")

(defun search-filters-render (list filtered)
  "Fill LIST, a list of search filters, with those narrowing FILTERED, a list."
  (let ((filters (search-filters filtered)))
    (render-pieces
     list
     (append (list (format nil "The filters narrowing ~a, each narrowing what those above it leave:~%~%"
                           (tuatara.buffer:buffer-name filtered)))
             (table-heading-pieces *search-filters-columns*)
             (loop for filter in filters
                   append (table-row-pieces *search-filters-columns*
                                            (list (search-filter-words filter)
                                                  (format nil "~:[literal~;regex~], ~:[case kept~;case ignored~]"
                                                          (search-filter-regex-p filter) (search-filter-fold filter))
                                                  (if (search-filter-kept filter)
                                                      (format nil "~:d" (search-filter-count filter))
                                                      "working out"))))
             (and (null filters) (list (format nil "~%   No filters: the whole list is shown~%"))))
     :point :line :entries filters :first-line 4
     :refill (lambda (list) (search-filters-render list filtered)))))

(defun search-filters-key (list)
  "What the list of search filters LIST shows, cheaply: each filter and
how many entries it leaves; another key means it is out of date."
  (let ((filtered (tuatara.buffer:buffer-local list 'filtered-list)))
    (and filtered (mapcar (lambda (filter) (list filter (and (search-filter-kept filter) (search-filter-count filter))))
                          (search-filters filtered)))))

(keep-shown-current 'search-filters-mode 'search-filters-key)

(define-command search-filter-list ()
  "List the search filters narrowing the list here (\\[search-filter-regex])
in a buffer of their own, each with how many entries it leaves; there
\\<search-filters-mode>\\[search-filter-remove] takes away the filter on the
line and \\[search-filter-remove-all] all of them."
  (let ((filtered (filtered-list-at-hand)))
    (if (null filtered)
        (message "Not a list: no filters to list")
        (let ((list (buffer-in-mode (format nil "*Filters of ~a*" (tuatara.buffer:buffer-name filtered))
                                    'search-filters-mode)))
          (setf (tuatara.buffer:buffer-local list 'filtered-list) filtered)
          (search-filters-render list filtered)
          (show-buffer list)))))

(defun refresh-filters-list ()
  "Fill the current buffer again when it is a list of search filters."
  (let ((buffer (current-buffer)))
    (when (typep (buffer-major-mode buffer) 'search-filters-mode)
      (search-filters-render buffer (tuatara.buffer:buffer-local buffer 'filtered-list)))))

(define-command search-filter-remove ()
  "Take away the search filter on the current line of the list of filters
\(\\[search-filter-list]): the list it narrowed widens, the filters after
it narrowing again what those before it leave, in the background where that
takes time."
  (let ((filter (list-entry-at)) (filtered (filtered-list-at-hand)))
    (cond ((not (search-filter-p filter)) (message "No filter on this line"))
          ((null filtered) (message "The list it narrowed is gone"))
          (t (take-away-filters filtered (list filter))
             (refresh-filters-list)
             (message "Filter ~a taken away" (search-filter-words filter))))))

(define-command search-filter-remove-all ()
  "Take away every search filter narrowing the list here, or the list the
list of filters here is for: the whole list is shown again."
  (let ((filtered (filtered-list-at-hand)))
    (cond ((or (null filtered) (null (search-filters filtered))) (message "No filters to take away"))
          (t (let ((count (length (search-filters filtered))))
               (take-away-filters filtered (search-filters filtered))
               (refresh-filters-list)
               (message "~d filter~:p taken away: ~:[the whole list is~;every record is~] shown"
                        count (filters-lines-p filtered)))))))

(defun add-search-filter (buffer pattern &key (regex t) part hide (ignore-case :as-typed))
  "Narrow BUFFER, a list or a file of records, by PATTERN, kept with its other
filters as one typed at the search prompt is (\\[search-filter-regex]): a
regular expression when REGEX is true, else literal text; case ignored when it
has no capital letter, or as IGNORE-CASE says when it is T or NIL. With PART,
one of the parts the list names (SET-SEARCH-FILTERING's PARTS), it looks at that
part of each entry alone; with HIDE, it leaves the entries it does not match.
Returns the filter, or NIL and why when the pattern cannot be used."
  (multiple-value-bind (filter why) (make-search-filter pattern :regex regex :where part :part part :hide hide
                                                                :fold ignore-case)
    (when filter
      (keep-new-filter buffer filter))
    (values filter why)))

(defun keep-new-filter (buffer filter)
  "Narrow BUFFER, a list or a file of records, by FILTER, kept after its other filters."
  (setf (list-filtering-typing (list-filtering-made buffer)) filter)
  (refilter buffer)
  (keep-typed-filter buffer))

(defun read-list-part (list prompt)
  "Ask with PROMPT for one of the parts LIST names (SET-SEARCH-FILTERING's
PARTS), each with the words the list gives of it beside it, or the whole
entry: its name, NIL for the whole entry."
  (values (read-offered-field (mapcar (lambda (part)
                                        (if (consp part) (list (car part) (car part) (cdr part)) (list part part nil)))
                                      (list-filtering-parts (list-filtering list)))
                              nil prompt :words #'identity)))

(defun refilter-search-filters (buffer)
  "Work BUFFER's search filters out again over the whole list, from the first,
as when the text a list gives of its entries changed (a label put on): the
entries they leave now are shown, in the background where that takes time."
  (let ((filtering (list-filtering buffer)))
    (when (and filtering (list-filtering-filters filtering))
      (forget-worked-out (list-filtering-filters filtering))
      (refilter buffer))))

(defun entries-left-by-filters (list)
  "The entries of LIST, a list made with its entries, that its search filters
leave, in order, an entry shown on several lines once: those in sections
folded too, as folding hides an entry from sight but not from the filters.
T when no filter narrows it, and :WORKING while they are still worked out."
  (let* ((filtering (list-filtering list))
         (last (and filtering (car (last (list-filtering-filters filtering)))))
         (source (and filtering (list-filtering-source filtering))))
    (cond ((null last) t)
          ((or (null source) (not (eq (search-filter-source last) source)) (null (search-filter-kept last))) :working)
          (t (let ((entries (list-source-entries source)) (bits (search-filter-kept last)))
               (loop for i from 0 below (length entries)
                     when (and (= 1 (sbit bits i)) (entry-start-p entries i)
                               (not (heading-entry-p filtering (svref entries i))))
                       collect (svref entries i)))))))

(defun filter-records-by (command hide)
  "Narrow the list or file of records here by a filter asked for as
READ-LINE-FILTER asks (a column first in a file of records, a part of the
entries in a list that names them), kept with the others; with HIDE, one that
leaves what it does not match. COMMAND names the command in messages."
  (let ((buffer (current-buffer)))
    (if (not (search-filterable-p buffer))
        (say-unfilterable (format nil "~(~a~) was not run" command))
        (let* ((filtering (list-filtering-made buffer))
               (words (format nil "~:[show~;hide~] ~a" hide (list-filtering-nouns filtering))))
          (multiple-value-bind (pattern regex matcher what where)
              (cond ((and (not (filters-lines-p buffer)) (list-filtering-parts filtering))
                     ;; a part of each entry asked for first, its text tried alone
                     (let ((part (read-list-part buffer "Filter by: ")))
                       (multiple-value-bind (pattern regex)
                           (read-pattern (filter-question words part))
                         (values pattern regex nil part part))))
                    ((and (not (filters-lines-p buffer)) (list-table-columns buffer))
                     ;; a table: a column asked for first, its cell tried alone
                     (multiple-value-bind (column title) (read-list-column buffer "Filter by the column: ")
                       (multiple-value-bind (pattern regex)
                           (read-pattern (filter-question words title))
                         (values pattern regex nil title (and column (list :column column buffer))))))
                    (t (multiple-value-bind (pattern regex matcher what)
                           (read-line-filter buffer words 0 (tuatara.buffer:buffer-size buffer))
                         (values pattern regex matcher what nil))))
            (if (zerop (length pattern))
                (say-not-run command "No pattern")
                (multiple-value-bind (filter why)
                    (make-search-filter pattern :regex regex :test (and what matcher) :part what :hide hide
                                                :where where)
                  (if (null filter)
                      (say-not-run command (string-capitalize why :end 1))
                      (keep-new-filter buffer filter)))))))))

(define-command search-filter-show-records ()
  "Show only the records whose line a regular expression matches, hiding the
others, as \\[search-filter-regex] does, deleting nothing. In a file of records
a column is asked for first, and the pattern is tried on its value alone.
Nothing, \"whole record\" or $* gives the whole line. Each column is offered
with the first record's value beside it, by its name or by its number from 0
\($0, $1). A comma-separated values (CSV) or tab-separated values (TSV) file is
split by its separator, quotes taken off, and named by its header. A log is
split into words, apart by spaces, named by its header when
\\[list-toggle-header] makes its first line one. The hosts file offers address
and name. The password file offers user, password, uid, gid, info, home and
shell. An environment file (.env) offers name and value. A Python requirements
file offers name, version and markers. todo.txt offers done, priority,
completed, created, text, project, context and the keys of its key:value tags.
JSON Lines offers its keys, strings read with their escapes undone, a path of
keys apart by dots (user.id) reaching inside. A .gitignore and every list try
the whole line. A line longer than *REWRITE-LINE-LIMIT* octets (16 MiB as
shipped) has its column tried on its first part alone, and the mode line says
how many lines were. The filter is kept
with the others, the next narrowing what it leaves; \\[search-filter-list]
lists them and \\[search-filter-remove-all] takes them all away.
\\[list-keep-records] deletes the others instead."
  (filter-records-by 'search-filter-show-records nil))

(define-command search-filter-hide-records ()
  "Hide the records whose line a regular expression matches, showing the
others, deleting nothing: asked for as \\[search-filter-show-records] asks, kept with
the other filters. \\[list-delete-records] deletes them instead."
  (filter-records-by 'search-filter-hide-records t))

(defun install-search-filters ()
  "Bind the keys of the search filters: \\[search-filter-regex],
\\[search-filter-list] and \\[search-filter-remove-all] in every buffer, and
those of the list of filters. In a file whose lines they hide, say in the mode
line what they leave, and keep the lines hidden from every edit. Work the
filters out again after a change that reached into those lines, or once the file
is read again."
  (pushnew 'filters-mode-line *mode-line-functions*)
  (pushnew 'refilter-touched *tick-functions*)
  (pushnew 'refilter-reloaded *after-reload-functions*)
  (pushnew 'guard-hidden-lines tuatara.buffer:*before-edit-functions*)
  (define-keys (mode-keymap 'search-filters-mode) '(("d" search-filter-remove) ("D" search-filter-remove-all)))
  (commands-for-mode 'search-filters-mode "the list of search filters (\\[search-filter-list])"
                     'search-filter-remove))
