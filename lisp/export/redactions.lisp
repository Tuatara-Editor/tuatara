;;;; lisp/export/redactions.lisp --- Text hidden in one place, found shown in another.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; A name hidden by {{{redact(...)}}} in an Org note, or by \R{...} in a
;;;; LaTeX file, is no secret when it is written out plainly elsewhere in the
;;;; same file. M-x export-check-redactions gathers what the buffer hides and lists in
;;;; *Redactions* every other place it shows, RET on one showing it; the
;;;; enhanced export and latex-export-pdf look first, and ask before making a
;;;; PDF that would give a hidden text away.

(in-package #:tuatara.export)

(define-setting *latex-redaction-commands* '("R" "D" "redact")
  "The LaTeX commands that hide their argument in your LaTeX files, \\R{John
Otago} for one. \\[export-check-redactions] and \\[latex-export-pdf]
look for them in a LaTeX file, as hidden text to be found nowhere else.
- a list of texts: the commands' names without the backslash, \"R\", \"D\" and
  \"redact\" as standard."
  :values (list-of string))

(defparameter *redaction-shortest-part* 3
  "The fewest characters a word of a hidden text has to be looked for alone
\(Otago of John Otago), so that short words do not crowd what is found.")

(defun hidden-spans (text kind &optional directory)
  "The texts TEXT hides, each (START END INSIDE NAME): where the call is,
the text it hides, and the name of the call (redact). In Org, the calls of
redact and protect, by their short names R and P too, of the kinds its
export hides (ORG-HIDDEN-KINDS: redact unless redact:nil, protect with
protect:t, in #+OPTIONS: or a setup file's, DIRECTORY the note's directory); in
LaTeX, those of *LATEX-REDACTION-COMMANDS*: an argument holding groups of
its own (\\R{John \\textbf{Otago}}) hides all of them, INSIDE the words it
shows (LATEX-PLAIN-TEXT), and one never closed hides the rest of its
paragraph. Complexity: O(length of TEXT)."
  (let ((found '()))
    (ecase kind
      (:org
       (let ((hidden (org-hidden-kinds text directory)))
         (map-org-hiding-calls (lambda (start end written from to)
                                 (let ((name (org-hiding-kind written)))
                                   (when (member name hidden :test #'string=)
                                     (push (list start end (subseq text from to) name) found))))
                               text)))
      (:latex
       (let ((names (format nil "~{~a~^|~}" (mapcar #'cl-ppcre:quote-meta-chars *latex-redaction-commands*)))
             (groups nil)
             (at 0))
         (when (plusp (length names))
           ;; an argument may hold groups of its own (\R{John \textbf{Otago}}): it reaches its closing brace,
           ;; and the next call is looked for after it, so that a call inside another is part of that one
           (let ((scanner (cl-ppcre:create-scanner (format nil "\\\\(~a)[ \\t]*\\n?[ \\t]*\\{" names))))
             (loop
               (multiple-value-bind (start open rs re) (cl-ppcre:scan scanner text :start at)
                 (unless start (return))
                 (let ((close (and (evenp (loop for i downfrom (1- start) to 0 while (char= #\\ (char text i))
                                                count t))
                                   (gethash (1- open) (or groups (setf groups (latex-groups text)))))))
                   (if (null close)
                       ;; \\R{...}, a line break and then letters, or a call in a comment: nothing hidden
                       (setf at open)
                       (let* ((inside-end (if (eq close :unclosed)
                                              (or (cl-ppcre:scan "\\n[ \\t]*\\n" text :start open) (length text))
                                              close))
                              (end (if (eq close :unclosed) inside-end (1+ close))))
                         (push (list start end (latex-plain-text (subseq text open inside-end))
                                     (subseq text (aref rs 0) (aref re 0)))
                               found)
                         (setf at end)))))))))))
    (nreverse found)))

(defun latex-groups (text)
  "The groups of LaTeX TEXT, a table from where each opening brace is to where
its closing brace is, or :UNCLOSED for one never closed. A brace written \\=\\{
or \\=\\}, or in a comment (from % to the end of its line), is none. One pass
through TEXT. Complexity: O(length of TEXT)."
  (let ((groups (make-hash-table)) (open '()) (n (length text)) (i 0))
    (loop while (< i n)
          do (case (char text i)
               (#\\ (incf i))
               (#\% (setf i (1- (or (position #\Newline text :start i) n))))
               (#\{ (push i open))
               (#\} (when open (setf (gethash (pop open) groups) i))))
             (incf i))
    (dolist (brace open groups)
      (setf (gethash brace groups) :unclosed))))

(defun latex-plain-text (latex)
  "The words LATEX, a piece of a LaTeX file, shows, as one line: the names of
its commands (\\textbf) left out, a character written as a command (\\&) as
itself, a space written as one (\\, or \\\\) as a space, braces and comments left
out, and each run of blanks, line breaks and ties (~) one space."
  (let ((out (make-string-output-stream)) (n (length latex)) (i 0))
    (loop while (< i n)
          do (let ((c (char latex i)))
               (cond ((and (char= c #\\) (< (1+ i) n) (alpha-char-p (char latex (1+ i))))
                      (setf i (or (position-if-not #'alpha-char-p latex :start (1+ i)) n))
                      (when (and (< i n) (char= (char latex i) #\*)) (incf i))
                      (write-char #\Space out))
                     ((char= c #\\)
                      ;; a space of its own (\, \  \\), a break allowed (\- \/), or a character as itself (\&)
                      (when (< (1+ i) n)
                        (let ((next (char latex (1+ i))))
                          (cond ((find next "\\ ,;:>!") (write-char #\Space out))
                                ((not (find next "-/")) (write-char next out)))))
                      (incf i 2))
                     ((char= c #\%) (setf i (or (position #\Newline latex :start i) n)))
                     ((find c "{}") (incf i))
                     ((find c '(#\~ #\Space #\Tab #\Newline #\Return)) (write-char #\Space out) (incf i))
                     (t (write-char c out) (incf i)))))
    (string-trim " " (cl-ppcre:regex-replace-all " {2,}" (get-output-stream-string out) " "))))

(defun token-char-p (c)
  "True when C joins characters into one token, as in an address (90.233.1.40), a path or a name."
  (or (alphanumericp c) (find c ".:-/_@")))

(defun token-bounds (text start end)
  "Where the token of TEXT holding the stretch from START below END begins and
ends, as two values: widened over the characters TOKEN-CHAR-P joins on either
side."
  (values (loop for i downfrom start above 0 while (token-char-p (char text (1- i))) finally (return i))
          (loop for i from end below (length text) while (token-char-p (char text i)) finally (return i))))

(defun redaction-whole (text start end inside)
  "What INSIDE, hidden by the call from START below END of TEXT, stands for
there: INSIDE alone, or when the call is part of a token (90.233.1.{{{redact(40)}}}),
that token with INSIDE in the call's place (90.233.1.40), what would give it
away written elsewhere."
  (multiple-value-bind (from to) (token-bounds text start end)
    (if (and (= from start) (= to end))
        inside
        (concatenate 'string (subseq text from start) inside (subseq text end to)))))

(defparameter *redaction-trimmed* '(#\Space #\Tab #\( #\) #\[ #\] #\{ #\} #\, #\. #\; #\: #\' #\")
  "The characters taken off both ends of a hidden text before it is looked for
elsewhere (REDACTION-NEEDLES).")

(defun redaction-needles (inside &optional (whole inside))
  "What to look for elsewhere of INSIDE, a hidden text, WHOLE what it stands
for where it is (REDACTION-WHOLE): WHOLE, and when INSIDE has several words,
each word of *REDACTION-SHORTEST-PART* characters or more, without its
punctuation."
  (let* ((whole (string-trim *redaction-trimmed* whole))
         (words (remove-if (lambda (w) (< (length w) *redaction-shortest-part*))
                           (uiop:split-string (string-trim *redaction-trimmed* inside)
                                              :separator (remove-if #'token-char-p (remove-duplicates inside))))))
    (remove-duplicates (remove "" (cons whole (and (rest words) words)) :test #'string=) :test #'string-equal
                       :from-end t)))

(defun word-starts (text)
  "Where each word of TEXT begins (a run of letters, digits and _), by the
word in lower case: a table of lists of positions, latest first."
  (let ((table (make-hash-table :test 'equal)) (n (length text)) (i 0))
    (loop while (< i n)
          do (if (name-char-p (char text i))
                 (let ((end (or (position-if-not #'name-char-p text :start i) n)))
                   (push i (gethash (string-downcase (subseq text i end)) table))
                   (setf i end))
                 (incf i)))
    table))

(defun redaction-findings (text kind &optional directory (spans (hidden-spans text kind directory)))
  "Where TEXT shows a text it hides elsewhere: each (POSITION NEEDLE INSIDE
NAME), POSITION a character of TEXT outside every hiding call, NEEDLE what
stands there as written, found as a whole word in any case, INSIDE the
hidden text it gives away, NAME its call; in the order of the text. SPANS
are TEXT's hidden texts (HIDDEN-SPANS), found when not given. The text's
words are read once, and each text looked for once however often it is
hidden, so that a long note with hundreds of hidden texts is looked through
at once."
  (let* ((spans (sort (copy-list spans) #'< :key #'first))
         (starts (map 'vector #'first spans))
         (ends (map 'vector #'second spans))
         (words (word-starts text))
         (found (make-hash-table)))
    (labels ((hidden-at-p (position)
               ;; the last span beginning at or before POSITION; spans do not overlap
               (let ((low (bisect (mid 0 (length starts)) (<= (aref starts mid) position))))
                 (and (plusp low) (< position (aref ends (1- low))))))
             (places (needle)
               ;; where NEEDLE may stand: where its first word does, else anywhere it is
               (let ((first-end (or (position-if-not #'name-char-p needle) (length needle))))
                 (if (plusp first-end)
                     (gethash (string-downcase (subseq needle 0 first-end)) words)
                     (loop for at = (search needle text :test #'char-equal)
                           then (search needle text :test #'char-equal :start2 (1+ at))
                           while at collect at)))))
      ;; each text to look for once, with the first hidden text it gives away
      (let ((owners (make-hash-table :test 'equalp)) (needles '()))
        (dolist (span spans)
          (destructuring-bind (start end inside name) span
            (dolist (needle (redaction-needles inside (redaction-whole text start end inside)))
              (when (and (plusp (length needle)) (not (gethash needle owners)))
                (setf (gethash needle owners) (cons inside name))
                (push needle needles)))))
        (dolist (needle (nreverse needles))
          (destructuring-bind (inside . name) (gethash needle owners)
            (let ((n (length needle)))
              (dolist (at (places needle))
                (when (and (<= (+ at n) (length text))
                           (string-equal needle text :start2 at :end2 (+ at n))
                           (not (hidden-at-p at))
                           (or (zerop at) (not (name-char-p (char text (1- at)))))
                           (or (>= (+ at n) (length text)) (not (name-char-p (char text (+ at n))))))
                  ;; what stands there, as it is written; one finding a place
                  (unless (gethash at found)
                    (setf (gethash at found) (list at (subseq text at (+ at n)) inside name)))))))))
      ;; a word found inside a longer text found (Otago in John Otago) is that one place, said once:
      ;; in the order of the text, the longest first where two begin together, one is left out
      ;; when one before it reaches as far
      (let ((ordered (sort (loop for f being the hash-values of found collect f)
                           (lambda (f g) (or (< (first f) (first g))
                                             (and (= (first f) (first g))
                                                  (> (length (second f)) (length (second g))))))))
            (reach -1)
            (kept '()))
        (dolist (f ordered (nreverse kept))
          (let ((f-end (+ (first f) (length (second f)))))
            (unless (>= reach f-end) (push f kept))
            (setf reach (max reach f-end))))))))

(defun look-for-redactions (looked kind &optional directory)
  "Look through LOOKED, the text of a file of KIND, :ORG or :LATEX, a rope or a
string, for the texts it hides shown elsewhere in it, DIRECTORY the file's
directory or NIL. Four values: the text as a string, the texts it hides
\(HIDDEN-SPANS), where they show (REDACTION-FINDINGS), and, when they show
anywhere, the text as a rope, for the places to be named in it
\(SHOW-REDACTION-FINDINGS). In a job's thread, a stop asked of it is answered
between the two looks. Complexity: O(length of LOOKED)."
  (let* ((text (if (stringp looked) looked (tuatara.text:rope-string looked)))
         (spans (hidden-spans text kind directory)))
    (tuatara.runtime:job-check-stop)
    (let ((findings (and spans (redaction-findings text kind directory spans))))
      (values text spans findings
              (and findings (if (stringp looked) (tuatara.text:string->rope looked) looked))))))

(define-major-mode redactions-mode special-mode
  "*Redactions*: where a text the file hides shows plainly elsewhere in it,
the places under each hidden text. \\<redactions-mode>\\[list-visit] on a place
shows it in another window, \\[list-visit-other] the list staying selected;
\\[list-next-entry] and \\[list-previous-entry] go from one to the next;
\\[search-filter-regex] narrows the list to the places whose words match;
\\`q' closes."
  :lighter "Redactions" :interactive nil)

(defun show-redaction-findings (buffer text findings &optional (rope (tuatara.buffer:buffer-text buffer)))
  "List FINDINGS (REDACTION-FINDINGS) of BUFFER, whose text is TEXT, in
*Redactions*: under the hidden text each gives away, each place with its line.
ROPE is TEXT as a rope, BUFFER's text when TEXT was taken of it (its text as
it is now unless given): the lines are numbered in it, and each place shows
the same octet of BUFFER."
  (flet ((line-text (position)
           (subseq text (1+ (or (position #\Newline text :end position :from-end t) -1))
                   (or (position #\Newline text :start position) (length text))))
         (line-number (octet)
           (if (eq rope (tuatara.buffer:buffer-text buffer))
               (tuatara.buffer:buffer-line-number buffer octet)
               (tuatara.text:byte->line rope octet))))
    (multiple-value-bind (pieces entries)
        (entry-lines
         (lambda (put)
           (loop for group in (let ((groups '()))
                                (dolist (f findings (nreverse groups))
                                  (let ((g (assoc (third f) groups :test #'string=)))
                                    (if g (push f (cdr g)) (push (list (third f) f) groups)))))
                 do (destructuring-bind (inside . places) group
                      (funcall put "")
                      (funcall put (format nil "~a (~a), ~d place~:p:" inside (fourth (first places)) (length places))
                               :heading
                               (list :hidden inside))
                      (dolist (f (reverse places))
                        (destructuring-bind (position needle inside name) f
                          (declare (ignore inside name))
                          (let* ((octet (tuatara.text:char->byte rope position))
                                 (line (1+ (line-number octet)))
                                 ;; one entry, the place, on both its lines
                                 (entry (list :place buffer octet)))
                            (funcall put (format nil "  ~a:~d  ~a" (tuatara.buffer:buffer-name buffer) line needle)
                                     nil entry)
                            (funcall put (format nil "    | ~a" (string-trim '(#\Space #\Tab) (line-text position)))
                                     nil entry))))))))
      (let ((list (set-search-filtering (buffer-in-mode "*Redactions*" 'redactions-mode)
                                        :nouns "places" :heading (lambda (entry) (eq (first entry) :hidden)))))
        (render-pieces list
                       (cons (format nil
                                     "Text hidden in ~a, found shown elsewhere in it:~%"
                                     (tuatara.buffer:buffer-name buffer))
                             pieces)
                       :entries entries :first-line 1 :point :start)
        (display-buffer list)))))

(defmethod visit-entry ((mode redactions-mode) list entry other)
  "Show the place on the line in the file it is in, in another window,
selected, or with OTHER the list kept selected."
  (declare (ignore list))
  (destructuring-bind (kind &optional buffer octet) entry
    (cond ((not (eq kind :place)) (message "No place on this line"))
          ((not (live-buffer-p buffer)) (message "The buffer the place is in is gone"))
          (t (keeping-list-selected other (lambda () (show-buffer-position buffer octet :other t)))))))

(define-record (redactions-look (:include tuatara.runtime:job)
                                (:constructor make-redactions-look (buffer rope kind directory))
                                (:copier nil))
  "A look through BUFFER's text, ROPE, a file of KIND, :ORG or :LATEX, whose
directory is DIRECTORY or NIL, for the texts it hides shown elsewhere in it, a
job: once done, TEXT, the text looked through, SPANS, what it hides
\(HIDDEN-SPANS), and FINDINGS, where it shows (REDACTION-FINDINGS)."
  (rope nil :documentation "The text REDACTIONS-LOOK looks through, as a rope.")
  (kind nil :documentation "What the file REDACTIONS-LOOK looks through is, :ORG or :LATEX.")
  (directory nil :documentation "The directory of the file REDACTIONS-LOOK looks through, or NIL.")
  (text nil :documentation "The text REDACTIONS-LOOK looked through, as a string, once done.")
  (spans nil :documentation "The texts the file of REDACTIONS-LOOK hides (HIDDEN-SPANS), once done.")
  (findings nil :documentation "Where the file of REDACTIONS-LOOK shows a text it hides, once done."))

(tuatara.runtime:define-job-words redactions-look
  :doing "Looking for hidden text"
  :description "Looking for hidden text shown elsewhere"
  :verb "look for hidden text"
  :left-words "nothing listed")

(defparameter *redactions-wait* 1/10
  "How many seconds \\[export-check-redactions] waits for its look through the
file before it goes on in the background, the places listed once found.")

(defmethod tuatara.runtime:job-take-in ((job redactions-look))
  "Say what the look through the file found, listing the places in
*Redactions* when there are any (SHOW-REDACTION-FINDINGS); say why when it did
not end well."
  (let ((buffer (tuatara.runtime:job-buffer job))
        (spans (redactions-look-spans job))
        (findings (redactions-look-findings job)))
    (cond ((not (eq (tuatara.runtime:job-status job) :done))
           (message "~a" (tuatara.runtime:job-ending-words job)))
          ((not (live-buffer-p buffer)))
          ((null spans)
           (message "Nothing in ~a is hidden (redact unless redact:nil, protect with protect:t)"
                    (tuatara.buffer:buffer-name buffer)))
          ((null findings)
           (message "None of the ~d text~:p hidden in ~a shows anywhere else in it" (length spans)
                    (tuatara.buffer:buffer-name buffer)))
          (t
           (show-redaction-findings buffer (redactions-look-text job) findings (redactions-look-rope job))
           (message "~d place~:p in ~a show~:[~;s~] text hidden elsewhere in it: see *Redactions*"
                    (length findings) (tuatara.buffer:buffer-name buffer) (= 1 (length findings)))))))

(defun start-redactions-look (buffer)
  "Look through BUFFER's text, as it is now, for the texts it hides shown
elsewhere in it, in a job of BUFFER's (REDACTIONS-LOOK), the places listed in
*Redactions* once found. The job, or NIL, said why, for a buffer neither in
Org nor in LaTeX."
  (let ((kind (export-kind buffer)))
    (if (null kind)
        (progn (message "~a is neither an Org nor a LaTeX file: nothing hidden to look for"
                        (tuatara.buffer:buffer-name buffer))
               nil)
        (let ((job (make-redactions-look buffer (tuatara.buffer:buffer-text buffer) kind (note-directory buffer))))
          (tuatara.runtime:start-job-thread
           job (tuatara.runtime:carry-settings
                (append *latex-carried-settings* '(*latex-redaction-commands*))
                (lambda (job)
                  (multiple-value-bind (text spans findings)
                      (look-for-redactions (redactions-look-rope job) (redactions-look-kind job)
                                           (redactions-look-directory job))
                    (setf (redactions-look-text job) text
                          (redactions-look-spans job) spans
                          (redactions-look-findings job) findings))))
           :name "tuatara redactions")
          (take-in-soon job *redactions-wait* "Looking for hidden text in the background")
          job))))

(define-command export-check-redactions ()
  "Look for the texts this Org or LaTeX file hides shown plainly elsewhere in
it, and list each place in *Redactions*, where
\\<redactions-mode>\\[list-visit] shows it. Hidden are in an Org note what
{{{redact(...)}}} marks, and {{{protect(...)}}} when its #+options say
protect:t; in a LaTeX file the arguments of *LATEX-REDACTION-COMMANDS*
\(\\R{...}, \\D{...}). Each hidden text is looked for whole, and each of its
words alone, as whole words in any case: John Otago hidden is given away by
John Otago or by Otago written elsewhere. The enhanced export and
\\[latex-export-pdf] look first, and ask before making the PDF. The
file is looked through in the background, as it is when asked."
  (start-redactions-look (current-buffer))
  (values))

(defparameter *export-kept-directories*
  '(latex-templates-directory docx-templates-directory note-templates-directory logos-directories)
  "Functions giving the directories of yours an export never writes in unasked, each one
or a list of them (EXPORT-KEPT-PLACES).")

(defun export-kept-places ()
  "The directories of yours an export never writes in unasked, as a list: what
each of *EXPORT-KEPT-DIRECTORIES* gives now."
  (mapcan (lambda (f) (copy-list (alexandria:ensure-list (funcall f)))) *export-kept-directories*))

(defun export-target-allowed-p (buffer types &key text)
  "True when exporting BUFFER's Org note, or TEXT, the part of it exported,
may write its files of TYPES (endings, pdf or docx): when its
#+EXPORT_FILE_NAME puts them outside the note's own directory and the directories
in it (links followed), or in your templates or logos directories
\(*EXPORT-KEPT-DIRECTORIES*), only when you say to go on. A note opened from
an encrypted file is exported only when you say so: what the export writes is
not encrypted. Never into an evidence directory
\(*EVIDENCE-DIRECTORIES*): NIL then, and as a second value the words saying why
\(EVIDENCE-REFUSAL)."
  (unless (or (not (tuatara.io:buffer-plaintext-p buffer))
              (yes-or-no-p* "Export it all the same?"
                            :details (format nil "~a is encrypted, but its export and what is made for it are not."
                                             (tuatara.buffer:buffer-name buffer))))
    (return-from export-target-allowed-p nil))
  (let* ((file (tuatara.io:native (tuatara.io:expand-path (tuatara.buffer:buffer-file buffer))))
         (named (and (eq (export-kind buffer) :org)
                     (ignoring-errors-reported ("naming the export of ~a" file)
                                               (org-export-file-name (or text (tuatara.buffer:buffer-text buffer))
                                                                     file))))
         (kept (export-kept-places))
         ;; links followed: a directory in the note's that is a link to elsewhere is elsewhere
         (outside (and named (notevery (lambda (type)
                                         (let ((path (replaced-type named type)))
                                           (and (tuatara.io:really-within-p path (tuatara.io:parent-path file))
                                                (not (tuatara.io:path-in-directories-p path kept)))))
                                       types)))
         (existing (and outside
                        (find-if (lambda (path) (tuatara.io:path-stamp path))
                                 (mapcar (lambda (type) (replaced-type named type)) types))))
         (refusal (some (lambda (type) (evidence-refusal "export to" (replaced-type (or named file) type))) types)))
    (when refusal
      (return-from export-target-allowed-p (values nil refusal)))
    (or (not outside)
        (let ((target (abbreviated-path (or existing (replaced-type named (first types))))))
          (yes-or-no-p* (format nil "Export to ~a?" target)
                        :details (format nil "#+EXPORT_FILE_NAME writes ~:[~;over ~]~a, outside the note's ~
                                              directory or in your templates or logos directories."
                                         existing target))))))

;;; Wrapping text in a macro: R and P to hide it, or any the note declares

(defun macro-call-on-line (buffer point)
  "The macro call with arguments (*ORG-MACRO-CALL-RE*) on BUFFER's line that
holds the octet POINT, as (START END NAME INSIDE), octets; NIL when none."
  (let* ((line (tuatara.buffer:buffer-line-start buffer point))
         (text (tuatara.buffer:buffer-line-text buffer line)))
    (flet ((octet (char) (+ line (tuatara.text:string-octet-length text :end char))))
      (cl-ppcre:do-scans (start end rs re *org-macro-call-re* text)
        (when (<= (octet start) point (octet end))
          (return-from macro-call-on-line
            (list (octet start) (octet end)
                  (subseq text (aref rs 0) (aref re 0)) (subseq text (aref rs 1) (aref re 1)))))))))

(defun token-at (buffer point)
  "The token around the octet POINT of BUFFER (TOKEN-CHAR-P: a name, an address
as 90.233.1.40, a mail address or a path), as START and END octets; NIL when
point is on none."
  (let* ((line (tuatara.buffer:buffer-line-start buffer point))
         (text (tuatara.buffer:buffer-line-text buffer line))
         (at (length (tuatara.buffer:buffer-string buffer :start line :end point))))
    (multiple-value-bind (from to) (token-bounds text at at)
      ;; a full stop or colon ending a sentence is not the token's
      (loop while (and (> to from) (find (char text (1- to)) ".:")) do (decf to))
      (when (< from to)
        (values (+ line (tuatara.text:string-octet-length text :end from))
                (+ line (tuatara.text:string-octet-length text :end to)))))))

(defun org-macro-choices (buffer)
  "The macros text in BUFFER, an Org note, may be wrapped in, as candidates:
R and P, which hide it in the enhanced export's PDF, then each its #+macro:
lines declare, with what it stands for."
  (let ((declared (remove-if #'org-hiding-kind
                             (with-org-note-keywords ((tuatara.buffer:buffer-text buffer) (note-directory buffer))
                               (org-macro-definitions))
                             :key #'car)))
    (append (list (make-candidate
                   "R" :annotation "hidden in the PDF, a black box (redact; #+options: redact:nil prints it)")
                  (make-candidate "P" :annotation "hidden in the PDF only with #+options: protect:t (protect)"))
            (loop for (name . body) in declared
                  collect (make-candidate name :annotation (format nil "declared: ~a" body))))))

(defvar *org-last-macro* "R"
  "The macro \\[org-macro-wrap] wrapped text in last, offered first the next time.")

(define-command org-macro-wrap ()
  "In an Org note, wrap the selection, or the word at point, in a macro chosen
from a list: R, {{{R(John Otago)}}}, hidden in the enhanced export's PDF as a
black box; P, hidden there only with #+options: protect:t; or one the note
declares with a #+macro: line, what it stands for shown beside it. A word is
a name, an address as 90.233.1.40, a mail address or a path. On text in a
call already, change the call to another macro; \\[org-macro-unwrap] takes
the call away. While reading, a call shows as its text, in the face
:org-macro-text.
\\[export-check-redactions] finds hidden text written plainly elsewhere."
  (let* ((buffer (current-buffer))
         (point (tuatara.buffer:buffer-point buffer))
         (call (let ((call (macro-call-on-line buffer point)))
                 ;; a selection reaching out of the call is text to wrap, not the call
                 (and call (or (not (region-active-p buffer))
                               (multiple-value-bind (start end) (tuatara.buffer:buffer-region buffer)
                                 (and (<= (first call) start) (<= end (second call)))))
                      call))))
    (if call
        (destructuring-bind (start end name inside) call
          (let ((chosen (completing-read (format nil "Change {{{~a(...)}}} to: " name)
                                         (org-macro-choices buffer)
                                         :require-match t :keep-order t)))
            (tuatara.buffer:buffer-replace buffer start end (format nil "{{{~a(~a)}}}" chosen inside))
            (drop-selection)
            (setf (tuatara.buffer:buffer-point buffer) start
                  *org-last-macro* chosen)
            (message "~a: in ~a now" inside chosen)))
        (multiple-value-bind (start end)
            (if (region-active-p buffer) (selection-range) (token-at buffer point))
          (if (or (null start) (= start end))
              (message "Select the text to wrap, or put point on a word")
              (let ((inside (tuatara.buffer:buffer-string buffer :start start :end end)))
                (if (search ")}}}" inside)
                    (message "The text holds )}}}, which would end the call early")
                    (let ((chosen (completing-read "Wrap in: " (org-macro-choices buffer) :require-match t :keep-order t
                                                   :default *org-last-macro*)))
                      (let ((after (tuatara.buffer:buffer-replace buffer start end
                                                                  (format nil "{{{~a(~a)}}}" chosen inside))))
                        (drop-selection)
                        (setf *org-last-macro* chosen
                              (tuatara.buffer:buffer-point buffer) after)
                        (message "~a: in ~a" (string-trim '(#\Newline) inside) chosen))))))))))

(define-command org-macro-unwrap ()
  "In an Org note, take away the macro call point is in, its text left as it
was: {{{R(John Otago)}}} becomes John Otago. \\[org-macro-wrap] wraps text again."
  (let* ((buffer (current-buffer))
         (call (macro-call-on-line buffer (tuatara.buffer:buffer-point buffer))))
    (if (null call)
        (message "Point is in no macro call with text to leave")
        (destructuring-bind (start end name inside) call
          (tuatara.buffer:buffer-replace buffer start end inside)
          (drop-selection)
          (setf (tuatara.buffer:buffer-point buffer) start)
          (message "~a: the call of ~a taken away" inside name)))))

(defun install-redactions ()
  "Give *Redactions* the keys of a list whose entries are opened, offer
\\[export-check-redactions] in Org and LaTeX buffers, and bind \\`C-c m' and
\\`C-c M' in Org notes to wrap text in a macro (\\[org-macro-wrap]) and take a
call away (\\[org-macro-unwrap])."
  (entry-list-keys 'redactions-mode "*Redactions*")
  (commands-for-mode 'org-mode "Org and LaTeX files" 'export-check-redactions)
  (commands-for-mode 'org-mode "Org notes" 'org-macro-wrap 'org-macro-unwrap)
  (define-key (mode-keymap 'org-mode) "C-c m" 'org-macro-wrap)
  (define-key (mode-keymap 'org-mode) "C-c M" 'org-macro-unwrap)
  (commands-for-mode 'latex-mode "Org and LaTeX files" 'export-check-redactions))
