;;;; lisp/help/man.lisp --- M-x man: the system's manual pages, read in a buffer.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; As Emacs's M-x man does, with man-db and groff: a page is asked for with the
;;;; names of the system's pages to complete on, formatted in the background to
;;;; the width of the window, and shown read-only, its headings, bold and
;;;; underlined text in faces and its references to other pages (ls(1)) as
;;;; links.  man runs in the sandbox (TUATARA.IO:SANDBOX-RUN), seeing only the
;;;; system's pages and its own configuration, with an environment of Tuatara's
;;;; making: no pager, no shell, nothing of the person's.

(in-package #:tuatara.defaults)

(define-major-mode man-mode help-mode
  "A manual page, read-only: its section headings, bold and underlined text
shown in faces, and every reference to another page (ls(1)) a link.
\\<man-mode>\\[push-button] on a link shows that page, \\[forward-button] and \\[backward-button] go from one
link to the next, \\[man-next-section] and \\[man-previous-section] from one section to the next.
\\[man-back] shows the page the link was followed from, and \\[quit-window] closes the window.
\\[revert-buffer] formats the page again for the width of the window it is in now."
  :lighter "Man" :interactive nil)

(defparameter *man-output-limit* (* 16 1024 1024)
  "The most octets a formatted page may be, 16 MB; a page that comes out longer is not shown.")

(defparameter *man-page-list-age* 3600
  "The seconds the names of the system's pages, once found, are used before they are looked for again.")

(defparameter *man-directories*
  '("/etc/man_db.conf" "/etc/manpath.config" "/var/cache/man" "/usr/local/share/man" "/usr/local/man" "/opt/man")
  "What man is shown besides the system's programs and /usr, read only: its
configuration, the databases that index the pages, and the folders of pages
some systems keep outside /usr.")

(defparameter *man-seconds* 20
  "The seconds man may run to format a page or list the pages before it is stopped.")

(defun own-pages-folder ()
  "The folder of pages installed for oneself (~/.local/share/man), or NIL when
there is none: the only folder of the home man is shown."
  (let ((folder (concatenate 'string (tuatara.io:home-directory) ".local/share/man")))
    (and (eq :directory (tuatara.io:path-kind folder)) folder)))

(defun man-environment (width)
  "The environment man is run with, WIDTH columns wide: the formatting kept as
the page is written, bold and underline, no pager, and the folder of one's own
pages searched besides the system's."
  (let ((own (own-pages-folder)))
    (append (list (list "MANWIDTH" (princ-to-string width)) (list "MAN_KEEP_FORMATTING" "1")
                  (list "MANPAGER" "cat") (list "PAGER" "cat"))
            (and own (list (list "MANPATH" (concatenate 'string own ":")))))))

(defun man-program ()
  "The path of the man program, or NIL when the system has none."
  (tuatara.io:find-program "man"))

(defun man-health-words ()
  "What shows manual pages here, as \\[check-health] says it."
  (outside-program-health-words :man "manual pages (M-x man)" (man-program)))

(defun man-run (arguments &key (width 80) cancel file)
  "Run man with ARGUMENTS shut in the sandbox, as the person's environment
would not change it, and return the octets it printed. With FILE, the file at
that path is given on its standard input, so that it is the only file man sees.
An error saying why when man is missing, was stopped, or failed (a page it has
not got)."
  (unless (man-program)
    (tuatara.io:program-missing "man" :why (outside-program-install-words :man)))
  (let ((own (own-pages-folder)))
    (multiple-value-bind (output outcome said)
        (apply (if file
                   (lambda (&rest keys)
                     (apply #'tuatara.io:sandbox-run-on-file file "man" (append arguments (list "-")) keys))
                   (lambda (&rest keys) (apply #'tuatara.io:sandbox-run "man" arguments keys)))
               :environment (man-environment width) :cancel cancel :seconds *man-seconds*
               :directories (if own (cons own *man-directories*) *man-directories*)
               :output-limit *man-output-limit* nil)
      (unless (eq outcome :done)
        (error "~a" (tuatara.io:sandbox-outcome-words outcome said :label "man" :seconds-setting '*man-seconds*)))
      output)))

;;; Reading what man printed

(defun man-sgr-codes (text from end)
  "The numbers between FROM and END of TEXT, separated by semicolons (an empty
one is 0), as a list: the parameters of a control sequence for graphics."
  (loop with code = 0
        for i from from to end
        for char = (if (< i end) (char text i) #\;)
        if (char= char #\;) collect code and do (setf code 0)
        else do (let ((digit (digit-char-p char)))
                  (when digit (setf code (min 1000 (+ (* code 10) digit)))))))

(defun man-formatting (text)
  "TEXT, what man printed, as two values: its plain text, and a vector holding,
for each character of it, 1 for bold, 2 for underlined, 3 for both, 0 for
neither. Bold and underline are read as groff writes them, as escape
sequences (SGR) or as a character struck twice and an underscore struck under
one, whichever; every other escape sequence and every control character,
those of the C1 range too, is left out, a device control string with its
payload."
  (let* ((end (length text))
         (out (make-string end))
         (marks (make-array end :element-type '(unsigned-byte 8)))
         (count 0) (mark 0) (i 0))
    (declare (type fixnum count i mark end))
    (flet ((strike (bits) (setf (aref marks (1- count)) (logior (aref marks (1- count)) bits))))
      (loop while (< i end)
            do (let ((c (char text i)))
                 (cond ((or (char= c #\Escape) (tuatara.text:c1-control-p c))
                        (multiple-value-bind (stop final from to) (tuatara.text:escape-sequence-end text i :c1 t)
                          (when (eql final #\m)
                            (dolist (code (man-sgr-codes text from to))
                              (case code
                                (0 (setf mark 0))
                                (1 (setf mark (logior mark 1)))
                                ((3 4) (setf mark (logior mark 2)))
                                (22 (setf mark (logandc2 mark 1)))
                                ((23 24) (setf mark (logandc2 mark 2))))))
                          (setf i (1- stop))))
                       ((and (char= c #\Backspace) (plusp count) (< (1+ i) end))
                        (let ((before (schar out (1- count))) (next (char text (1+ i))))
                          (cond ((char= before next) (strike 1))
                                ((char= before #\_) (setf (schar out (1- count)) next) (strike 2))
                                ((char= next #\_) (strike 2))
                                (t (setf (schar out (1- count)) next)))
                          (incf i)))
                       ((or (char= c #\Newline) (char= c #\Tab) (and (>= (char-code c) 32) (/= (char-code c) 127)))
                        (setf (schar out count) c (aref marks count) mark)
                        (incf count))))
               (incf i)))
    (if (= count end)
        (values out marks)
        (values (subseq out 0 count) (subseq marks 0 count)))))

(defparameter *man-reference-pattern*
  (cl-ppcre:create-scanner "([A-Za-z0-9_][A-Za-z0-9_.+:@-]*)\\(([0-9][A-Za-z0-9]*)\\)")
  "A reference to another page as pages write it: ls(1), printf(3p).")

(defun man-pieces (text follow)
  "The pieces (SHOW-HELP-PIECES) of the page whose printed output is TEXT:
a line of bold with no indent a heading (:MARKUP.HEADING), other bold :STRONG,
underlined :EMPHASIS, and each reference to another page a button calling
FOLLOW with the reference's name and section. The text is cut where its face
or button changes. Worked out by position in the text, so that what it makes
besides the pieces is a few octets for each character."
  (multiple-value-bind (plain faces) (man-formatting text)
    (let* ((size (length plain))
           (links '())
           (pieces '())
           (footer (let ((shown (string-right-trim '(#\Newline #\Space) plain)))
                     (1+ (or (position #\Newline shown :from-end t) -1)))))
      (declare (type fixnum size footer))
      ;; FACES becomes 0 plain, 1 strong, 2 emphasis, 4 heading
      (loop for line-start fixnum = 0 then (1+ line-end)
            for line-end fixnum = (or (position #\Newline plain :start line-start) size)
            do (let ((heading (and (< line-start line-end) (char/= (char plain line-start) #\Space)
                                   (loop for k from line-start below line-end
                                         always (or (char= (char plain k) #\Space) (logtest 1 (aref faces k)))))))
                 (loop for k from line-start below line-end
                       for bits = (aref faces k)
                       do (setf (aref faces k) (cond (heading 4) ((logtest bits 1) 1) ((= bits 0) 0) (t 2))))
                 ;; the page's own name in its first and last line is not a link
                 (unless (or (zerop line-start) (>= line-start footer))
                   (cl-ppcre:do-scans (from to starts ends *man-reference-pattern* plain nil
                                       :start line-start :end line-end)
                     (push (vector from to (svref starts 1) (svref ends 1)) links))))
            while (< line-end size))
      (setf links (nreverse links))
      (let ((start 0))
        (declare (type fixnum start))
        (loop while (< start size)
              do (let* ((link (and links (= start (svref (first links) 0)) (pop links)))
                        (limit (if links (svref (first links) 0) size))
                        (end (if link
                                 (svref link 1)
                                 (let ((end (1+ start)))
                                   (declare (type fixnum end))
                                   (loop while (and (< end limit) (= (aref faces end) (aref faces start)))
                                         do (incf end))
                                   end)))
                        (piece (subseq plain start end)))
                   (push (cond (link (let ((name (subseq plain (svref link 0) (1- (svref link 2))))
                                           (section (subseq plain (svref link 2) (svref link 3))))
                                       (list piece :button (lambda () (funcall follow name section)))))
                               ((= (aref faces start) 0) piece)
                               (t (cons piece (case (aref faces start) (1 :strong) (2 :emphasis) (t :markup.heading)))))
                         pieces)
                   (setf start end))))
      (nreverse pieces))))

;;; The names of the pages

(defvar *man-pages* nil
  "The system's pages as completion candidates, \"ls(1)\" with the line that
describes each, once found by \\[man], and when (universal time); a list (WHEN . CANDIDATES), or NIL.")

(defun man-page-candidates (listing)
  "The candidates for the pages in LISTING, what man --apropos . printed
\(octets): one line each page, its names, its section in brackets, and what it
is, \"ls (1) - list directory contents\"; a page with several names a
candidate for each."
  (let ((seen (make-hash-table :test 'equal)) (found '()))
    (dolist (line (uiop:split-string (tuatara.text:lenient-string listing) :separator '(#\Newline)))
      (let ((dash (search " - " line)) (open (position #\( line)))
        (when (and dash open (< open dash))
          (let ((close (position #\) line :start open)))
            (when (and close (< close dash))
              (let ((section (subseq line (1+ open) close))
                    (about (subseq line (+ dash 3))))
                (dolist (name (uiop:split-string (subseq line 0 open) :separator ","))
                  (let ((shown (format nil "~a(~a)" (string-trim " " name) section)))
                    (unless (or (< (length name) 1) (gethash shown seen))
                      (setf (gethash shown seen) t)
                      (push (make-candidate shown :annotation about) found))))))))))
    (nreverse found)))

(defvar *man-pages-said* nil
  "True once the lack of an index of the pages has been said.")

(defun start-man-page-list ()
  "Start finding the names of the system's pages in the background, man shut in
the sandbox printing them; a GATHERING (TAKE-GATHERED) that gives the
candidates at its end, and keeps them in *MAN-PAGES*, or, when man could not
list them (it has no index: mandb makes it), the words saying so, which are
kept as well, an empty list for the hour, so that it is not tried at every
question. NIL, nothing started, when they were found less than
*MAN-PAGE-LIST-AGE* seconds ago, or there is no man."
  (when (and (man-program)
             (not (and *man-pages* (< (- (get-universal-time) (car *man-pages*)) *man-page-list-age*))))
    (setf *man-pages-said* nil)
    (start-gathering "tuatara man pages"
                     (lambda (give cancel)
                       (handler-case
                           (reporting-errors ("listing the manual pages")
                             (let ((candidates (man-page-candidates (man-run (list "--apropos" ".") :cancel cancel))))
                               (setf *man-pages* (cons (get-universal-time) candidates))
                               (funcall give candidates)))
                         (error (problem)
                           (setf *man-pages* (cons (get-universal-time) '()))
                           (funcall give (format nil "The pages' names are not listed (~a); mandb makes the index (an administrator runs it). A page can still be asked for by name."
                                                 problem))))))))

(defun man-file-p (path)
  "True when PATH is named as the source of a manual page: a number after the
last dot (ls.1, printf.3p), or man, mdoc or roff."
  (and path
       (cl-ppcre:scan "\\.([1-9][A-Za-z0-9]*|man|mdoc|roff)$" (tuatara.io:native-file-name path))
       t))

(defun read-man-page ()
  "Ask which page to show, completing on the names of the system's pages,
found in the background while the question is open. In a file that is the
source of a page, that file is the default. Returns the answer, a string."
  (let* ((file (let ((shown (shown-file (current-buffer)))) (and (man-file-p shown) shown)))
         (listing (start-man-page-list))
         (offered (and *man-pages* (cdr *man-pages*))))
    (string-trim " " (completing-read "Manual page: " (lambda (input) (declare (ignore input)) offered)
                                      :default (and file (tuatara.io:native-file-name file))
                                      :source (and listing
                                                   (gathering-source listing
                                                                     (lambda (new)
                                                                       (when (stringp (first new)) (message "~a" (pop new)))
                                                                       (when new (setf offered (first new)))
                                                                       (length offered))))))))

;;; Asking for a page

(defun man-request (asked)
  "What to give man for the page ASKED for, as the arguments after its name, or
an error saying why it cannot be: \"ls\", \"ls(1)\" and \"1 ls\" are the three
ways to name a page and its section. A name with a slash in it, or beginning
with a dash, is refused: man would take it for a file or an option."
  (multiple-value-bind (all groups)
      (cl-ppcre:scan-to-strings (concatenate 'string "^\\s*(?:([^\\s/()]+)\\(([0-9][A-Za-z0-9]*)\\)"
                                                    "|([0-9][A-Za-z0-9]*)\\s+([^\\s/()]+)|([^\\s/()]+))\\s*$")
                                asked)
    (unless all (error "~a is not the name of a manual page" (tuatara.display:shorten-string asked 40)))
    (let ((name (or (aref groups 0) (aref groups 3) (aref groups 4)))
          (section (or (aref groups 1) (aref groups 2))))
      (when (char= (char name 0) #\-)
        (error "~a is not the name of a manual page" (tuatara.display:shorten-string asked 40)))
      (append (list "--") (and section (list section)) (list name)))))

(defun man-buffer-name (asked file)
  "The name of the buffer for the page ASKED for, or the manual page source at FILE."
  (format nil "*Man ~a*" (if file (tuatara.io:native-file-name file) (string-trim " " asked))))

(defstruct (man-job (:include tuatara.runtime:job) (:constructor make-man-job (buffer asked))
                    (:copier nil))
  "A page being formatted for BUFFER, a job: ASKED, the page named; PIECES, the
page as text and links (MAN-PIECES) once it is."
  asked pieces)

(setf (documentation 'man-job-p 'function) "True when OBJECT is a MAN-JOB.")

(tuatara.runtime:define-job-words man-job
  :doing "Formatting"
  :description (format nil "Formatting the manual page ~a" (man-job-asked job))
  :verb "format"
  :left-words "no page formatted"
  :at-exit :quiet)

(defun man-width (buffer)
  "The columns to format a page for BUFFER in: those of the window showing it,
else of the selected one; at least 40."
  (let ((window (or (buffer-window buffer) (current-window))))
    (max 40 (if window (tuatara.display:window-text-width window) 80))))

(defun format-man-page (buffer)
  "Start formatting the page BUFFER shows, for the width it has now, in the
background; the page replaces the text when it is done (a job, \\[stop-job] ends it).
A formatting of the same page still going on is stopped."
  (dolist (job tuatara.runtime:*jobs*)
    (when (and (man-job-p job) (eq (tuatara.runtime:job-buffer job) buffer) (tuatara.runtime:job-running-p job))
      (tuatara.runtime:job-stop job)))
  (let ((file (tuatara.buffer:buffer-local buffer :man-file))
        (arguments (tuatara.buffer:buffer-local buffer :man-arguments))
        (width (man-width buffer)))
    (tuatara.runtime:start-job-thread
     (make-man-job buffer (or file (tuatara.buffer:buffer-local buffer :man-asked)))
     (lambda (job)
       (let ((text (tuatara.text:lenient-string
                    (man-run (if file (list "--local-file") arguments) :width width :file file
                             :cancel (tuatara.runtime:job-cancel job)))))
         (setf (man-job-pieces job)
               (man-pieces text (lambda (name section) (man-open (format nil "~a(~a)" name section) :from buffer))))))
     :name "tuatara man")))

(defmethod tuatara.runtime:job-take-in ((job man-job))
  "Show the page, or say why it could not be had: a page that is new then goes."
  (let ((buffer (tuatara.runtime:job-buffer job)))
    (when (live-buffer-p buffer)
      (if (eq (tuatara.runtime:job-status job) :done)
          (progn
            (setf (tuatara.buffer:buffer-local buffer :man-formatted) t)
            (render-pieces buffer (man-job-pieces job) :point :start :refill #'format-man-page))
          (progn
            (message "~a: ~a" (man-job-asked job) (tuatara.runtime:job-ending-words job))
            (unless (tuatara.buffer:buffer-local buffer :man-formatted)
              (tuatara.buffer:kill-buffer buffer)))))))

(defun man-open (asked &key file from)
  "Show the manual page ASKED for (\"ls(1)\"), or the source of one at FILE,
in a buffer of its own, formatted in the background for the width of the
window; a page shown already is shown again as it is. FROM is the page buffer
a link was followed in, which \\[man-back] returns to. Says why when the page
cannot be had."
  (let ((arguments (and (not file) (man-request asked))))
    (unless (man-program)
      (tuatara.io:program-missing "man" :why (outside-program-install-words :man)))
    (let* ((name (man-buffer-name asked file))
           (before (tuatara.buffer:find-buffer name))
           (buffer (if (and before (live-buffer-p before)) before (buffer-in-mode name 'man-mode))))
      (unless (eq buffer before)
        (setf (buffer-kind buffer) :help
              (tuatara.buffer:buffer-local buffer :man-asked) asked
              (tuatara.buffer:buffer-local buffer :man-file) file
              (tuatara.buffer:buffer-local buffer :man-arguments) arguments)
        (set-made-text buffer (format nil "Formatting ~a...~%" (if file (tuatara.io:native-file-name file) asked))
                       :refill #'format-man-page)
        (setf (tuatara.buffer:buffer-point buffer) 0)
        (format-man-page buffer))
      (when (and from (not (eq from buffer)))
        (setf (tuatara.buffer:buffer-local buffer :man-from) from))
      (let ((window (display-buffer buffer)))
        (when window (select-window window)))
      buffer)))

(define-command man ()
  "Show a manual page in a buffer of its own, read-only: ask for it by name
\(ls), with its section (ls(1)) or by section and name (1 ls), completing on
the names of the pages the system has. In a file that is the
source of a page (ls.1, printf.3p) the file is offered, and shown as a page. The page is
formatted in the background for the width of the window; headings, bold and
underlined text are shown in colour, a reference to another page (ls(1)) is a
link. \\<man-mode>\\[revert-buffer] there formats it again for the window's width now. man runs
shut in, with no network and no file but the pages; M-x check-health says
whether the system has it."
  (let* ((answer (read-man-page))
         (shown (shown-file (current-buffer))))
    (cond ((zerop (length answer)) (message "No page named"))
          ((and (man-file-p shown) (string= answer (tuatara.io:native-file-name shown))) (man-open answer :file shown))
          (t (man-open answer)))))

;;; Moving in a page

(defun man-heading-line-p (buffer line)
  "True when LINE of BUFFER begins with a character that is not a space: a section heading."
  (let ((char (tuatara.buffer:buffer-char-at buffer (tuatara.buffer:buffer-goto-line buffer line))))
    (and char (not (member char '(#\Space #\Tab #\Newline))))))

(defun man-step-section (direction)
  "Move to the start of the next section heading, or with DIRECTION -1 the
one before; say so when there is none."
  (let* ((buffer (current-buffer))
         (here (tuatara.buffer:buffer-line-number buffer (tuatara.buffer:buffer-point buffer)))
         (last (1- (tuatara.buffer:buffer-line-count buffer)))
         (found (loop for line = (+ here direction) then (+ line direction)
                      while (<= 1 line last)
                      when (man-heading-line-p buffer line) return line)))
    (if found
        (setf (tuatara.buffer:buffer-point buffer) (tuatara.buffer:buffer-goto-line buffer found))
        (message (if (plusp direction) "No more sections below" "No more sections above")))))

(define-command man-next-section ()
  "Go to the start of the next section heading of the manual page."
  (man-step-section 1))

(define-command man-previous-section ()
  "Go to the start of the section heading before the line point is on."
  (man-step-section -1))

(define-command man-back ()
  "Show the page a link was followed from; close the window when this page
was not reached by a link."
  (let* ((buffer (current-buffer))
         (from (tuatara.buffer:buffer-local buffer :man-from)))
    (if (and from (live-buffer-p from))
        (let ((window (display-buffer from)))
          (when window (select-window window)))
        (quit-window))))

(defun install-man ()
  "Bind the keys of manual pages: \\`l' follows the link at point, \\`h' goes back (\\[man-back]),
\\`n' and \\`p' go to the next section and the one before."
  (define-keys (mode-keymap 'man-mode) '(("l" push-button) ("h" man-back)
                                         ("n" man-next-section) ("p" man-previous-section)))
  (commands-for-mode 'man-mode "a manual page (\\[man])" 'man-back 'man-next-section 'man-previous-section))
