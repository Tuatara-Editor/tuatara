;;;; lisp/org/capture.lisp --- Notes and tasks taken down from anywhere and filed: org-capture.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; As Emacs's org-capture: a key from anywhere, a template chosen by its key
;;;; (Tuatara's own and yours, a file each in ~/.config/tuatara/capture-templates/),
;;;; its questions answered, the text written in a small *Capture* buffer and
;;;; filed where the template says with C-c C-c: under a heading of a file,
;;;; at a file's end, or in today's journal entry; you are back where you were.
;;;; A template may clock the entry while it is written, and the clock running
;;;; before goes on after.

(in-package #:tuatara.defaults)

(defvar *capture-template-directories* nil
  "The folders your capture templates are in, a template a file, as
\\[org-capture] offers them with Tuatara's own (t, n, j, m); NIL, as standard,
for capture-templates/ in your configuration folder
\(~/.config/tuatara/capture-templates/, XDG_CONFIG_HOME's when it is set).
One of yours with the key of one of Tuatara's takes its place. A file's head names it, then its
text follows after # --:

  # name: TODO for the agenda
  # key: t
  # file: *org-agenda-files*
  # heading: Tasks
  # --
  * TODO %?  %^g
  %U

The key is the character that chooses it. Where the text is filed:
  # file: FILE and # heading: HEADLINE  under the heading HEADLINE of FILE, at
                                       the end of what is under it; the
                                       heading made at the file's end when it
                                       has none such
  # file: FILE and # outline: A / B    under the heading of FILE whose titles
                                       from the top are A then B (\\[org-refile]'s)
  # file: FILE alone                   at FILE's end
FILE written *org-agenda-files* is the first of the files the agenda reads:
that file, or agenda.org in the first folder (or a pattern's folder) named.
  # journal: t                         at the end of today's journal entry
                                       (\\[journal-new-or-existing-entry])
With # clock-in: t the entry is clocked from when its capture begins; with
# clock-resume: t too, the clock stops when it is filed and the clock that ran
before goes on. The text's headings (* ...) are put a level below the heading
it goes under, with these filled in:
  %?  where point is left to write    %i  the text selected when capturing
  %U  [2026-10-06 Tue 14:03]          %u  [2026-10-06 Tue]
  %T  <2026-10-06 Tue 14:03>          %t  <2026-10-06 Tue>
  %a  a link to where you captured from (as \\[org-store-link] makes it)
  %^{Prompt}       asked for          %^{Prompt|one|two}  chosen among
  %^g  tags asked for, with the tags used in the target and the agenda
  %^t, %^T  a date asked for as \\[org-time-stamp] asks it (fri 10:00, +3d,
       every week 10:00), written as an active timestamp: <2026-10-30 Fri 10:00>;
       several days chosen in the calendar, a timestamp each, a run a range
  %<%H:%M>  the time as the pattern says (format-time's %-specifiers)
  %%  a %
\\[copy-capture-template] makes one of Tuatara's yours to change.")

(defparameter *built-in-capture-templates* (built-in-data-files "capture-templates/")
  "Tuatara's own capture templates, each (FILE-NAME TEXT): a TODO for the
agenda (t), a note to the inbox (n), a journal entry (j), meeting notes (m).")

(defstruct (capture-template (:constructor make-capture-template (key name target text &key clock-in clock-resume file own))
                             (:copier nil))
  "A template \\[org-capture] offers: KEY, the character choosing it, as text;
NAME, what it is; TARGET, where its text is filed, (:FILE-HEADLINE FILE
HEADLINE), (:OLP FILE TITLE ...), (:FILE FILE) or (:JOURNAL), NIL when its
file says none; TEXT, its %-escapes not filled in yet; CLOCK-IN and
CLOCK-RESUME; FILE, its file's name; OWN, true for one of Tuatara's."
  key name target text clock-in clock-resume file own)

(setf (documentation 'capture-template-p 'function) "True when OBJECT is a CAPTURE-TEMPLATE.")

(defun read-capture-template (file-name text &optional own)
  "The capture template a file called FILE-NAME holding TEXT is (OWN for one
of Tuatara's), from its head (*CAPTURE-TEMPLATE-DIRECTORIES*), or NIL when it
has no key."
  (multiple-value-bind (head body) (template-head text)
    (flet ((field (name) (let ((value (cdr (assoc name head :test #'string=)))) (and value (plusp (length value)) value)))
           (yes (name) (member (cdr (assoc name head :test #'string=)) '("t" "yes") :test #'string-equal)))
      (let ((key (field "key")) (file (field "file")))
        (when key
          (make-capture-template
           key (or (field "name") (pathname-name file-name))
           (cond ((yes "journal") (list :journal))
                 ((and file (field "outline"))
                  (list* :olp file (mapcar (lambda (title) (string-trim " " title))
                                           (uiop:split-string (field "outline") :separator "/"))))
                 ((and file (field "heading")) (list :file-headline file (field "heading")))
                 (file (list :file file)))
           body
           :clock-in (yes "clock-in") :clock-resume (yes "clock-resume") :file file-name :own own))))))

(defun capture-template-directories ()
  "The folders your capture templates are in (*CAPTURE-TEMPLATE-DIRECTORIES*):
capture-templates/ in your configuration folder when it names none."
  (or (setting-list '*capture-template-directories* *capture-template-directories*)
      (list (config-path "capture-templates/"))))

(defun capture-templates ()
  "The templates \\[org-capture] offers, in the order of their keys: Tuatara's
own and yours (*CAPTURE-TEMPLATE-DIRECTORIES*), one of yours taking the place
of one of Tuatara's with the same key."
  (let ((yours (loop for (file . text) in (folder-files (capture-template-directories))
                     for template = (and text (read-capture-template file text))
                     when template collect template))
        (own (loop for (file text) in *built-in-capture-templates*
                   for template = (read-capture-template file text t)
                   when template collect template)))
    (stable-sort (in-place-of-own (remove-duplicates yours :key #'capture-template-key :test #'string= :from-end t)
                                  own #'capture-template-key)
                 #'string< :key #'capture-template-key)))

(define-command copy-capture-template ()
  "Copy one of Tuatara's own capture templates, chosen by its name, into your
capture templates folder (the first of *CAPTURE-TEMPLATE-DIRECTORIES*) and
visit it: your copy, changed as you like (where it files, its text), takes
the place of Tuatara's on the same key. One you have already is visited as it
is."
  (copy-chosen-own-file "Copy capture template: "
                        (loop for (file text) in *built-in-capture-templates*
                              for template = (read-capture-template file text t)
                              when template
                                collect (make-candidate (capture-template-name template) :value (list nil file text)
                                                        :annotation (format nil "~a  Tuatara's" (capture-template-key template))))
                        (capture-template-directories)
                        "capture template"))

(defun org-capture-target-text (target)
  "TARGET of a template, in words, for messages."
  (case (first target)
    (:file-headline (format nil "under ~a in ~a" (third target) (tuatara.io:native-file-name (capture-target-path (second target)))))
    (:file (format nil "at the end of ~a" (tuatara.io:native-file-name (capture-target-path (second target)))))
    (:journal "in today's journal entry")
    (:olp (format nil "under ~a" (org-olp-text target)))
    (t (format nil "~s" target))))

(defun org-capture-fill (template &key link selection target-buffer day)
  "TEMPLATE with its %-escapes filled in (*CAPTURE-TEMPLATE-DIRECTORIES* lists them),
asking for what it asks, in order, as two values: the text, and where %? is
in it (its end when it has none). LINK is (LINK LABEL) for %a, SELECTION the
text for %i, TARGET-BUFFER the file whose tags %^g offers first, DAY a day
number that %t and %T give instead of today."
  (let ((now (org-now)) (point nil) (i 0) (n (length template)))
    (values
     (with-output-to-string (out)
       (loop while (< i n)
             do (if (or (char/= (char template i) #\%) (= i (1- n)))
                    (progn (write-char (char template i) out) (incf i))
                    (multiple-value-bind (text after) (org-capture-escape template (1+ i) now link selection target-buffer day)
                      (if (eq text :point)
                          (setf point (file-position out))
                          (write-string text out))
                      (setf i after)))))
     point)))

(defun org-capture-escape (template at now link selection target-buffer day)
  "What the %-escape of TEMPLATE whose letter is at AT is filled in with, and
the index after it, as two values; :POINT for %?. NOW is the time of the
capture; LINK, SELECTION, TARGET-BUFFER and DAY are as ORG-CAPTURE-FILL has them."
  (let ((next (1+ at)))
    (flet ((done (text) (values text next)))
      (case (char template at)
        (#\? (done :point))
        (#\% (done "%"))
        (#\U (done (org-timestamp-text now)))
        (#\T (done (if day (org-day-timestamp-text day :active t) (org-timestamp-text now :active t))))
        (#\u (done (org-timestamp-text now :clock nil)))
        (#\t (done (if day (org-day-timestamp-text day :active t) (org-timestamp-text now :active t :clock nil))))
        (#\a (done (if link (org-link-text (first link) (or (second link) "")) "")))
        (#\i (done (or selection "")))
        (#\< (let ((close (position #\> template :start next)))
               (if close
                   (values (format-time (subseq template next close) now) (1+ close))
                   (done "%<"))))
        (#\^ (org-capture-question template next target-buffer))
        (t (done (format nil "%~a" (char template at))))))))

(defun org-capture-question (template at target-buffer)
  "What a %^ escape of TEMPLATE, the rest of it beginning at AT, is filled in
with, asked for, and the index after it, as two values: for %^g tags, those
used in TARGET-BUFFER and the agenda offered; for %^t and %^T a date, asked
as \\[org-time-stamp] asks it, again till one is read, written as an active
timestamp, several days chosen in the calendar one after another; for %^{Prompt} an
answer; for %^{Prompt|one|two} one of those."
  (let ((letter (and (< at (length template)) (char template at)))
        (close (position #\} template :start at)))
    (cond ((eql letter #\g)
           (let ((tags (read-org-tags target-buffer)))
             (values (if tags (format nil ":~{~a~^:~}:" tags) "") (1+ at))))
          ((member letter '(#\t #\T))
           ;; asked again till a date is given: an entry planned with none would be missing from the agenda
           (let ((date (loop for prompt = "When: " then "No date read, when: "
                             for date = (read-org-date prompt :repeating t :several t)
                             when date return date)))
             (values (if (org-ts-p date) (org-ts-text date) date) (1+ at))))
          ((and (eql letter #\{) close)
           (destructuring-bind (&optional prompt &rest choices) (uiop:split-string (subseq template (1+ at) close) :separator "|")
             (let ((question (format nil "~a: " (if (plusp (length prompt)) prompt "Answer"))))
               (values (if choices (completing-read question choices) (read-string question))
                       (1+ close)))))
          (t (values "%^" at)))))

;;; The capture buffer

(define-major-mode org-capture-mode org-mode
  "The text being captured, an Org note: \\<org-capture-mode>\\[org-capture-finalize] files it where its template
says, \\[org-capture-kill] throws it away; either way you are back where you were."
  :lighter "Capture" :interactive nil)

(defvar *org-capture-keymap* (mode-keymap 'org-capture-mode)
  "The keys of the *Capture* buffer, over Org's.")

(define-key *org-capture-keymap* "C-c C-c" 'org-capture-finalize)
(define-key *org-capture-keymap* "C-c C-k" 'org-capture-kill)
(define-key *org-capture-keymap* "C-c C-w" 'org-capture-refile)

(defun org-agenda-main-file ()
  "The agenda's main file, a native path, which a capture template's # file:
*org-agenda-files* names: the first of *ORG-AGENDA-FILES* when it is a file,
else agenda.org in the folder it names or its pattern's folder."
  (let* ((first (or (first *org-agenda-files*) "~/org/agenda.org"))
         (path (tuatara.io:native (tuatara.io:expand-path first))))
    (multiple-value-bind (parent name) (tuatara.io:path-folder-and-name path)
      (cond ((find-if (lambda (c) (find c "*?")) name) (tuatara.io:path-join parent "agenda.org"))
            ((or (uiop:string-suffix-p first "/") (eq :directory (tuatara.io:path-kind path)))
             (tuatara.io:path-join path "agenda.org"))
            (t path)))))

(defun capture-target-path (file)
  "FILE, as a capture template's # file: names it, as an absolute path."
  (if (string-equal file "*org-agenda-files*")
      (org-agenda-main-file)
      (tuatara.io:expand-path file)))

(defun org-capture-target-buffer (target &optional (day (org-today)))
  "The buffer of the file TARGET files into, read when it is not open,
without showing it, editable when its folder is not there yet but can be
made (saving asks to make it); for the journal, the entry of DAY (a day
number, today unless given), made when there is none."
  (case (first target)
    ((:file-headline :file :olp)
     (multiple-value-bind (buffer existing) (tuatara.io:visit-file (capture-target-path (second target)))
       (unless existing (say-new-file-folder buffer))
       buffer))
    (:journal (let ((path (first (journal-entries-of day))))
                (if path
                    (tuatara.io:visit-file path)
                    (let ((window (current-window)))
                      (prog1 (journal-make-entry day)
                        (show-buffer (tuatara.display:window-buffer window)))))))
    (t (error "No such capture target: ~s" target))))

(defun org-capture-file (text target &optional (day (org-today)) at)
  "File TEXT where TARGET says (ORG-INSERT-ENTRY), one change to undo, the
journal's entry being DAY's, an :OLP heading the one beginning at the
character AT of several with its titles; returns the buffer it went into and
where it begins there (an octet)."
  (let ((buffer (org-capture-target-buffer target day)))
    (check-writable buffer)
    (values buffer
            (case (first target)
              (:file-headline
               (let ((headline (org-headline-titled (org-tree-now buffer) (third target))))
                 (if headline
                     (org-insert-entry buffer text :headline headline)
                     (org-insert-entry buffer text :title (third target)))))
              (:olp
               (let ((headline (org-headline-at-path (org-tree-now buffer) (cddr target) at)))
                 (unless headline (error "~a is not there" (org-olp-text target)))
                 (org-insert-entry buffer text :headline headline)))
              (t (org-insert-entry buffer text))))))

(defun org-capture-clock (buffer at started resume)
  "Clock the heading of BUFFER at the octet AT from the universal time
STARTED; with RESUME, stop it now and clock again what ran before."
  ;; the clock's line held by its anchor, which moves with the lines put in above it
  (let ((before (and *org-clock* (list (org-clock-buffer *org-clock*) (org-clock-line *org-clock*)))))
    (setf (tuatara.buffer:buffer-point buffer) at)
    (org-clock-in-here buffer :now started)
    (when resume
      (org-clock-out-now)
      (when (and before (live-buffer-p (first before)))
        (let ((b (first before)))
          (setf (tuatara.buffer:buffer-point b)
                (tuatara.text:anchor-position (second before) (tuatara.buffer:buffer-version b)))
          (org-clock-in-here b))))))

(defun org-capture-prompt (templates)
  "The question choosing among TEMPLATES: each key and name, yours marked, as
short as it can be said on one line."
  (format nil "Capture: ~{[~a] ~a~^  ~}"
          (loop for template in templates
                append (list (capture-template-key template)
                             (format nil "~a~:[ (yours)~;~]" (capture-template-name template)
                                     (capture-template-own template))))))

(defun org-capture-choose (templates)
  "The one of TEMPLATES whose key is pressed next, asked with all of them
listed, or NIL for a key none has; \\`C-g' leaves."
  (let ((key (read-key (org-capture-prompt templates))))
    (when (quit-key-p key) (keyboard-quit-signal))
    (find (tuatara.frontend:key-description (tuatara.frontend:key-spec key)) templates
          :key #'capture-template-key :test #'string=)))

(defun org-capture-selection (origin)
  "The text selected in ORIGIN as its windows show it (REGION-TEXT), its lines
as an Org buffer has them, or NIL."
  (multiple-value-bind (start end) (tuatara.buffer:buffer-region origin)
    (and start (< start end) (tuatara.buffer:buffer-mark-active-p origin)
         (lf-newlines (region-text origin start end :string t)))))

(defun org-capture-on-day-text (text point day)
  "TEXT, a template filled in with no %t or %T, with the active timestamp of
the day number DAY on the line after its first, so that the agenda lists it
that day; and POINT, an index in TEXT or NIL, moved with what follows it."
  (let* ((stamp (org-day-timestamp-text day :active t))
         (newline (or (position #\Newline text) (length text))))
    (values (concatenate 'string (subseq text 0 newline) (string #\Newline) stamp
                         (if (< newline (length text)) (subseq text newline) ""))
            (and point (if (> point newline) (+ point 1 (length stamp)) point)))))

(defun org-capture-begin (template origin &key day)
  "Fill TEMPLATE, a CAPTURE-TEMPLATE, in for a capture begun in the buffer
ORIGIN, and show it in a *Capture* buffer of its own, point where its %? is,
for \\<org-capture-mode>\\[org-capture-finalize] to file. With DAY, a day number,
the capture is for that day (ORG-CAPTURE-ON-DAY)."
  (let* ((started (org-now))
         (target (capture-template-target template))
         (link (multiple-value-list (org-link-to-here origin)))
         (target-buffer (and (member (first target) '(:file-headline :file :olp))
                             (ignore-errors (org-capture-target-buffer target)))))
    (multiple-value-bind (text point)
        (org-capture-fill (capture-template-text template) :link (and (first link) link)
                                                           :selection (org-capture-selection origin)
                                                           :target-buffer target-buffer
                                                           :day day)
      (when (and day (not (eq (first target) :journal))
                 (not (search "%t" (capture-template-text template)))
                 (not (search "%T" (capture-template-text template))))
        (multiple-value-setq (text point) (org-capture-on-day-text text point day)))
      (let ((buffer (tuatara.buffer:make-buffer (tuatara.buffer:unique-buffer-name "*Capture*") :text text))
            (description (capture-template-name template)))
        (set-major-mode buffer 'org-capture-mode)
        (setf (tuatara.buffer:buffer-local buffer 'org-capture)
              (list :description description :target target :started started :day day
                    :clock-in (capture-template-clock-in template) :clock-resume (capture-template-clock-resume template))
              (tuatara.buffer:buffer-local buffer :header-line)
              (let ((line (substitute-command-keys
                           (format nil " \\<org-capture-mode>\\[org-capture-finalize] files it, \\[org-capture-refile] elsewhere, \\[org-capture-kill] throws it away: ~a, ~a"
                                   description (org-capture-target-text target)))))
                (lambda (window) (declare (ignore window)) line)))
        (tuatara.buffer:buffer-mark-saved buffer)
        (pop-to-buffer buffer)
        (setf (tuatara.buffer:buffer-point buffer)
              (tuatara.text:string-octet-length (subseq text 0 (or point (length text)))))
        (set-modal-state :insert buffer)))))

(defun org-capture-chosen (&optional day)
  "Capture with the template whose key is pressed next, from the current
buffer; with DAY, a day number, for that day (ORG-CAPTURE-ON-DAY)."
  (let* ((origin (current-buffer))
         (chosen (org-capture-choose (capture-templates))))
    (cond
      ((null chosen) (message "No capture template on that key"))
      ((null (capture-template-target chosen))
       (message "The capture template ~a says nowhere to file it: # file:, # heading:, # outline: or # journal: t"
                (capture-template-file chosen)))
      (t (org-capture-begin chosen origin :day day)))))

(defun org-capture-with (name target text)
  "Capture as \\[org-capture] does, with a template given here rather than
read from a file: NAME, what it is; TARGET, where it is filed, as a
template's (:OLP FILE TITLE ...) or (:FILE-HEADLINE FILE HEADLINE); TEXT,
its %-escapes not filled in yet (%^T asks the time, %? is where point goes)."
  (org-capture-begin (make-capture-template "" name target text) (current-buffer)))

(define-command org-capture ()
  "Take a note, a task or a journal entry down from wherever you are:
press the key of one of the capture templates (Tuatara's and yours, in
*CAPTURE-TEMPLATE-DIRECTORIES*), answer what it asks, write in the *Capture*
buffer and file it with \\<org-capture-mode>\\[org-capture-finalize] (or throw it away
with \\[org-capture-kill]); you are back where you were. \\<calendar-mode>\\[org-capture-on-day] in the
calendar or the agenda captures for the day at point instead of today."
  (org-capture-chosen))

(define-command org-capture-on-day ()
  "Capture, as \\[org-capture] does, for the day at point in the calendar or on
the agenda (the day of its line, or of the date above it), so that the agenda
lists the entry that day: the template's %t and %T are that day's date, and
a template with neither gets that date on the line after its heading; a
journal template files into that day's entry. %U and %u stay the time it is
written."
  (let ((day (cond ((typep (buffer-major-mode (current-buffer)) 'calendar-mode) (calendar-cursor-date))
                   ((org-agenda-here) (org-agenda-day-at-point (current-buffer))))))
    (if day
        (org-capture-chosen day)
        (message "No day here: on the calendar or a day of the agenda, ~a captures for it"
                 (substitute-command-keys "\\<calendar-mode>\\[org-capture-on-day]")))))

(defun org-capture-done (buffer)
  "Close the capture BUFFER and go back to where the capture began."
  (leave-window (current-window))
  (tuatara.buffer:kill-buffer buffer))

(define-command org-capture-finalize ()
  "File the text of this *Capture* buffer where its template says, and go
back to where you were; with its template's clock, clock it."
  (org-capture-finish))

(define-command org-capture-refile ()
  "File the text of this *Capture* buffer under a heading chosen as
\\<org-mode>\\[org-refile] chooses one, instead of where its template says."
  (let ((capture (tuatara.buffer:buffer-local (current-buffer) 'org-capture)))
    (unless capture (error "This is no capture buffer"))
    (multiple-value-bind (target at) (org-read-refile-target "File the capture under: " (current-buffer))
      (org-capture-finish target at))))

(defun org-capture-finish (&optional elsewhere elsewhere-at)
  "File the text of the current *Capture* buffer where its template says, or
under ELSEWHERE, a target (:olp FILE TITLE ...), the heading beginning at the
character ELSEWHERE-AT of several with those titles; go back to where the
capture began."
  (let* ((buffer (current-buffer))
         (capture (tuatara.buffer:buffer-local buffer 'org-capture))
         (text (string-right-trim '(#\Space #\Tab #\Newline) (tuatara.buffer:buffer-string buffer))))
    (unless capture (error "This is no capture buffer"))
    (destructuring-bind (&key description target clock-in clock-resume started day) capture
      (when elsewhere (setf target elsewhere))
      (if (zerop (length (string-trim '(#\Space #\Tab #\Newline) text)))
          (progn (org-capture-done buffer) (message "Nothing captured: it was empty"))
          (let* ((day (or day (org-today started)))
                 (was-modified (tuatara.io:changes-unsaved-p (org-capture-target-buffer target day))))
            (multiple-value-bind (into at) (org-capture-file text target day elsewhere-at)
              ;; filed: whatever happens after, the capture is closed, never filed twice
              (unwind-protect
                   (progn (when clock-in (org-capture-clock into at started clock-resume))
                          (org-save-changed into was-modified))
                (org-capture-done buffer))
              (message "~a filed ~a" description (org-capture-target-text target))))))))

(define-command org-capture-kill ()
  "Throw away what this *Capture* buffer holds, and go back to where you were."
  (let ((buffer (current-buffer)))
    (unless (tuatara.buffer:buffer-local buffer 'org-capture) (error "This is no capture buffer"))
    (org-capture-done buffer)
    (message "Capture thrown away")))
