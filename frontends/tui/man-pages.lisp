;;;; frontends/tui/man-pages.lisp --- The manual pages tuatara(1) and tuatara-init(5), made from the program.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; The pages are written by hand in data/man/*.in, in man's own macros, with
;;;; a line "@NAME@" where a part goes that the program knows better than a
;;;; person writing it down: the ways of starting it and the options (the table
;;;; --help is made from), the environment variables, the settings of the init
;;;; file with the first paragraph of each one's documentation, and the files
;;;; that are kept
;;;; (test/api/kept-files.sexp).  tools/build-image.lisp writes them to
;;;; build/man/.

(in-package #:tuatara.terminal)

(defun roff-text (text)
  "TEXT as roff reads it back the same: backslashes and hyphens written so
that a page shows and copies them, characters outside ASCII by their roff
names, and no line beginning with a dot or an apostrophe, which would be a
request."
  (with-output-to-string (out)
    (loop for char across text
          for at-start = t then (char= previous #\Newline)
          for previous = char
          do (cond ((char= char #\\) (write-string "\\e" out))
                   ((char= char #\-) (write-string "\\-" out))
                   ((char= char (code-char #x2026)) (write-string "..." out))
                   ((member char (list (code-char #x2018) (code-char #x2019))) (write-string "\\(aq" out))
                   ((member char (list (code-char #x201C) (code-char #x201D))) (write-string "\"" out))
                   ((char= char (code-char #x2013)) (write-string "\\(en" out))
                   ((char= char (code-char #x2014)) (write-string "\\(em" out))
                   ((> (char-code char) 127) (format out "\\[u~4,'0X]" (char-code char)))
                   (t (when (and at-start (member char '(#\. #\')))
                        (write-string "\\&" out))
                      (write-char char out))))))

(defun roff-fill (text &key (width 72))
  "TEXT, a paragraph, as roff text broken into lines of at most WIDTH columns
\(one long word may be longer)."
  (with-output-to-string (out)
    (let ((column 0))
      (dolist (word (tuatara.text:split-words (roff-text text) " "))
        (when (plusp column)
          (if (> (+ column 1 (length word)) width)
              (progn (terpri out) (setf column 0))
              (progn (write-char #\Space out) (incf column))))
        (when (and (zerop column) (member (char word 0) '(#\. #\')))
          (write-string "\\&" out))
        (write-string word out)
        (incf column (length word))))
    (terpri out)))

(defun roff-word (word)
  "WORD of a synopsis (--gui, INIT-FILE, +LINE, FILE...) set in the type
manual pages use: an option bold, a name to be replaced italic, the rest as
it is."
  (let ((name-start (or (position-if-not (lambda (char) (member char '(#\+ #\:))) word) (length word))))
    (cond ((zerop (length word)) "")
          ((char= (char word 0) #\-) (format nil "\\fB~a\\fR" (roff-text word)))
          ((and (< name-start (length word))
                (every (lambda (char) (or (upper-case-p char) (member char '(#\- #\.)))) (subseq word name-start)))
           (format nil "~a\\fI~a\\fR" (roff-text (subseq word 0 name-start)) (roff-text (subseq word name-start))))
          (t (roff-text word)))))

(defun roff-synopsis-form (form)
  "FORM, a way of starting Tuatara as *USAGE-FORMS* writes it, as a line of
the SYNOPSIS: the program's name bold, options bold, names to replace italic,
the brackets as they are."
  (let ((out (make-string-output-stream)) (start 0) (named nil))
    (loop for end = (position-if (lambda (char) (member char '(#\Space #\[ #\]))) form :start start)
          for word = (subseq form start end)
          do (write-string (cond ((zerop (length word)) "")
                                 (named (roff-word word))
                                 (t (setf named t) (format nil "\\fB~a\\fR" word)))
                           out)
             (when end (write-char (char form end) out))
          while end
          do (setf start (1+ end)))
    (get-output-stream-string out)))

(defun roff-flags (flags)
  "FLAGS, the ways of writing an option (an entry of *OPTION-HELP*), as the
tag of its paragraph: each flag bold, its argument italic, separated by
commas."
  (format nil "~{~a~^, ~}"
          (mapcar (lambda (flag)
                    (format nil "\\fB~a\\fR~@[ ~a~]" (roff-text (first flag))
                            (and (second flag)
                                 (format nil "~{~a~^ ~}"
                                         (mapcar (lambda (word) (format nil "\\fI~a\\fR" (roff-text word)))
                                                 (tuatara.text:split-words (second flag) " "))))))
                  flags)))

(defun roff-entry-body (text why)
  "The body of a paragraph for an entry of *OPTION-HELP* or
*ENVIRONMENT-HELP*: what it does (TEXT, lines) as a paragraph and, when there
is one, why someone would change it (WHY, lines) as paragraphs under it, a
new one at each empty line."
  (flet ((paragraph (lines) (roff-fill (format nil "~{~a~^ ~}" lines))))
    (with-output-to-string (out)
      (write-string (paragraph text) out)
      (loop while why
            do (let ((end (or (position "" why :test #'string=) (length why))))
                 (format out ".IP~%~a" (paragraph (subseq why 0 end)))
                 (setf why (nthcdr (1+ end) why)))))))

(defun options-roff ()
  "The options of *OPTION-HELP* as paragraphs of OPTIONS: the ways of writing
each, what it does, and for an option with a choice why someone would take it
and what it costs."
  (with-output-to-string (out)
    (dolist (entry *option-help*)
      (format out ".TP~%~a~%~a" (roff-flags (getf entry :flags))
              (roff-entry-body (getf entry :text) (getf entry :why))))))

(defun environment-roff ()
  "The variables of *ENVIRONMENT-HELP* as paragraphs of ENVIRONMENT, each
with what it does and, for one with a choice, why someone would set it and
what to expect."
  (with-output-to-string (out)
    (dolist (entry *environment-help*)
      (format out ".TP~%.BR~{ ~a~^ \", \"~}~%~a" (mapcar #'roff-text (getf entry :names))
              (roff-entry-body (getf entry :text) (getf entry :why))))))

(defun synopsis-roff ()
  "The ways of starting Tuatara (*USAGE-FORMS*) as the lines of SYNOPSIS."
  (format nil "~{~a~^~%.br~%~}~%" (mapcar #'roff-synopsis-form *usage-forms*)))

;;; The init file

(defun documentation-paragraph (documentation)
  "The first paragraph of DOCUMENTATION, up to the first blank line, its lines
joined; a command written in a docstring as a key reference is replaced by its
name, and a keymap summary or a keymap name is left out."
  (let* ((paragraph (subseq documentation 0 (search (format nil "~%~%") documentation)))
         (joined (format nil "~{~a~^ ~}" (tuatara.text:split-words paragraph (list #\Newline #\Space))))
         (named (cl-ppcre:regex-replace-all "\\\\\\[([^]]*)\\]" joined "\\1")))
    (cl-ppcre:regex-replace-all "\\\\[<{][^>}]*[>}]" named "")))

(defun starter-settings ()
  "The settings the starter init file lists, in its order, as a list of (AREA
SYMBOL ...): the heading of the part of the file that sets them (its ;;;
line, without a trailing parenthesis) and the settings of TUATARA-USER named
there, each once, in the part where a commented-out form uses it, else where
the text first names it."
  (let ((groups '()) (area "General") (first-named '()) (placed '()))
    (flet ((place (symbol area)
             (unless (member symbol placed)
               (push symbol placed)
               (let ((group (assoc area groups :test #'string=)))
                 (if group
                     (push symbol (cdr group))
                     (push (list area symbol) groups))))))
      (with-input-from-string (in tuatara.defaults::*starter-config*)
        (loop for line = (read-line in nil)
              while line
              do (let ((trimmed (string-left-trim " " line)))
                   (cond ((uiop:string-prefix-p ";;; " trimmed)
                          (let ((heading (subseq trimmed 4)))
                            (setf area (string-trim " " (subseq heading 0 (or (search " (" heading)
                                                                              (length heading)))))))
                         ((uiop:string-prefix-p ";;" trimmed)
                          (let ((form (or (uiop:string-prefix-p ";; (" trimmed)
                                          (uiop:string-prefix-p ";;   " trimmed))))
                            (cl-ppcre:do-matches-as-strings (name "\\*[a-z0-9-]+\\*" trimmed)
                              (multiple-value-bind (symbol status) (find-symbol (string-upcase name) :tuatara-user)
                                (when (and (eq status :external) (boundp symbol))
                                  (if form
                                      (place symbol area)
                                      (pushnew (cons symbol area) first-named :key #'car))))))))))
        (dolist (named (reverse first-named))
          (place (car named) (cdr named)))))
    (mapcar (lambda (group) (cons (car group) (reverse (cdr group)))) (reverse groups))))

(defun settings-roff ()
  "The settings of the init file as paragraphs of the page: for each area a
sub-heading, then each setting with the first paragraph of its documentation."
  (with-output-to-string (out)
    (dolist (group (starter-settings))
      (format out ".SS ~s~%" (roff-text (car group)))
      (dolist (symbol (cdr group))
        (format out ".TP~%.B ~a~%~a" (roff-text (string-downcase (symbol-name symbol)))
                (roff-fill (documentation-paragraph (or (documentation symbol 'variable) ""))))))))

;;; The files that are kept

(defparameter *kept-file-descriptions*
  '(("bookmarks" . "the bookmarks you set")
    ("chosen-paths" . "the files and folders you chose in questions, offered again first")
    ("grammars" . "your answers about which grammars may run")
    ("look-only" . "the projects you looked at only, never trusted to run their own programs")
    ("lsp-trusted" . "the projects whose language servers and tools you trusted")
    ("package-trust" . "the sources of packages you trusted")
    ("packages" . "the packages fetched, with their source and version")
    ("packages-removed" . "the packages removed, not loaded though the init file names them")
    ("pdf-places" . "the page and zoom you left each PDF at")
    ("places" . "where point was in the files you visited")
    ("recent-files" . "the files you visited lately")
    ("ssh-servers" . "the machines whose daemons you trusted with more than keys and the screen, and with what")
    ("user-grammars" . "the checksums of the grammars you added")
    ("init.lisp" . "your init file")
    ("snippets/" . "your snippets")
    ("capture-templates/" . "your capture templates")
    ("latex-templates/" . "your LaTeX templates")
    ("docx-templates/" . "your Word templates")
    ("note-templates/" . "your note templates")
    ("logos/" . "the pictures your exported documents use")
    ("grammars/" . "the grammars you added")
    ("auto-save/" . "the auto-saved copies of buffers not saved")
    ("session" . "the files and windows open when you last left, for the next start")
    ("session-daemon-NAME" . "the session of the daemon NAME: its clients' folders and their windows")
    ("grammar-store/" . "the grammars Tuatara compiled")
    ("packages/" . "the packages fetched, compiled")
    ("" . "everything made again when it is gone: compiled packages, previews, copies of files opened outside"))
  "What each file of the kept-files pin is, by its name (a list's name or a
path in its folder).")

(defparameter *kept-file-folders*
  '(("config" . "~/.config/tuatara/") ("state" . "~/.local/state/tuatara/")
    ("data" . "~/.local/share/tuatara/") ("cache" . "~/.cache/tuatara/"))
  "The folders of the kept-files pin, by the name the pin gives them, with
where each is when the XDG folders are not set.")

(defun kept-file-rows ()
  "The rows of test/api/kept-files.sexp after the line that says the version."
  (with-open-file (in (asdf:system-relative-pathname :tuatara "test/api/kept-files.sexp"))
    (read in)
    (loop for row = (read in nil in) until (eq row in) collect row)))

(defun kept-file-place (row)
  "ROW of the pin as two values: the folder it is in, as the pin names it, and
its name there."
  (if (eq (first row) :state)
      (values "state" (second row))
      (values (second row) (third row))))

(defun kept-files-roff ()
  "The files of the pin as paragraphs, by their class: kept, remembered and
rebuilt, each with where it is and what it is."
  (with-output-to-string (out)
    (dolist (class '(:kept :remembered :rebuilt))
      (format out ".SS ~a~%" (ecase class (:kept "Kept") (:remembered "Remembered") (:rebuilt "Rebuilt")))
      (dolist (row (kept-file-rows))
        (multiple-value-bind (folder name) (kept-file-place row)
          (when (eq class (if (eq (first row) :state) (third row) (fourth row)))
            (format out ".TP~%.B ~a~a~%~a" (cdr (assoc folder *kept-file-folders* :test #'string=)) (roff-text name)
                    (roff-fill (or (cdr (assoc name *kept-file-descriptions* :test #'string=)) "")))))))))

;;; Putting the pages together

(defparameter *man-parts*
  '(("SYNOPSIS" . synopsis-roff) ("OPTIONS" . options-roff) ("ENVIRONMENT" . environment-roff)
    ("SETTINGS" . settings-roff)
    ("KEPT-FILES" . kept-files-roff))
  "The parts of the pages made by the program, by the name between @ signs on
a line of a page's source, with the function that makes each.")

(defun page-text (source)
  "The text of the page whose source is the file SOURCE of data/man/, with
@VERSION@ and each line @NAME@ of *MAN-PARTS* replaced."
  (let ((text (alexandria:read-file-into-string
               (asdf:system-relative-pathname :tuatara (format nil "data/man/~a" source)) :external-format :utf-8)))
    (with-output-to-string (out)
      (with-input-from-string (in text)
        (loop for line = (read-line in nil)
              while line
              do (let ((part (and (uiop:string-prefix-p "@" line) (uiop:string-suffix-p line "@")
                                  (cdr (assoc (string-trim "@" line) *man-parts* :test #'string=)))))
                   (if part
                       (write-string (funcall part) out)
                       (format out "~a~%" (cl-ppcre:regex-replace-all "@VERSION@" line (tuatara:version))))))))))

(defun write-man-pages (directory)
  "Write the pages tuatara.1 and tuatara-init.5 into DIRECTORY (made when it is
not there), over those there. The names of the files."
  (ensure-directories-exist directory)
  (loop for (source . target) in '(("tuatara.1.in" . "tuatara.1") ("tuatara-init.5.in" . "tuatara-init.5"))
        for file = (merge-pathnames target (uiop:ensure-directory-pathname directory))
        do (alexandria:write-string-into-file (page-text source) file :if-exists :supersede :external-format :utf-8)
        collect (namestring file)))

;;; Completion of the options in bash

(defparameter *completion-values*
  '(("--frames" . ("1" "2" "3" "4" "5" "6" "7" "8" "9" "10" "11" "12" "13" "14" "15" "16"))
    ("--dynamic-space-size" . :none))
  "The values bash offers for an option taking one where it is not a file: the
choices, or :NONE when nothing can be suggested.  Every other value an option
takes is a file name.")

(defun completion-options ()
  "The options of *OPTION-HELP* that bash completes, each as a list of its
name, the number of values that follow it and what they are (:FILES, :NONE or
a list of choices).  An option written with an optional =NAME is a name only."
  (let (options)
    (dolist (entry *option-help*)
      (loop for (flag argument) in (getf entry :flags)
            unless (char= (char flag 0) #\+)
              do (let ((name (bare-flag flag))
                       (values (if (and argument (not (search "=" flag)))
                                   (length (tuatara.text:split-words argument " "))
                                   0)))
                   (unless (find name options :key #'first :test #'string=)
                     (push (list name values
                                 (if (zerop values)
                                     :none
                                     (let ((known (assoc name *completion-values* :test #'string=)))
                                       (if known (cdr known) :files))))
                           options)))))
    (nreverse options)))

(defun completion-script ()
  "The bash function that completes the arguments of tuatara, made from
*OPTION-HELP*: the options, files after an option that takes one, the choices
of an option that has them, and files after -- or for a plain argument."
  (let ((options (completion-options)))
    (with-output-to-string (out)
      (flet ((line (control &rest arguments) (apply #'format out control arguments) (terpri out)))
        (line "# bash completion for tuatara ~a, made from the program's own option list." (tuatara:version))
        (line "_tuatara() {")
        (line "  local cur=\"${COMP_WORDS[COMP_CWORD]}\" prev=\"${COMP_WORDS[COMP_CWORD-1]}\"")
        (line "  local option=\"\" last=\"\" limit=$COMP_CWORD pending=0 i word")
        (line "  COMPREPLY=()")
        (line "  # bash splits --frames=4 into the words --frames, = and 4")
        (line "  if [[ $cur == = ]]; then option=$prev; cur=; limit=$((COMP_CWORD - 1))")
        (line "  elif [[ $prev == = ]]; then option=${COMP_WORDS[COMP_CWORD-2]}; limit=$((COMP_CWORD - 2)); fi")
        (line "  for ((i = 1; i < limit; i++)); do")
        (line "    word=${COMP_WORDS[i]}")
        (line "    if [[ $word == -- ]]; then")
        (line "      compopt -o filenames 2>/dev/null")
        (line "      mapfile -t COMPREPLY < <(compgen -f -- \"$cur\")")
        (line "      return")
        (line "    fi")
        (line "    if ((pending > 0)); then ((pending--)); continue; fi")
        (line "    case $word in")
        (dolist (option (remove-if (lambda (option) (zerop (second option))) options))
          (line "      ~a) last=$word; pending=~d ;;" (first option) (second option)))
        (line "    esac")
        (line "  done")
        (line "  if [[ -z $option ]] && ((pending > 0)); then option=$last; fi")
        (line "  if [[ -z $option && $cur == -* ]]; then")
        (line "    mapfile -t COMPREPLY < <(compgen -W \"~{~a~^ ~}\" -- \"$cur\")" (mapcar #'first options))
        (line "    return")
        (line "  fi")
        (line "  case $option in")
        (dolist (option options)
          (let ((kind (third option)))
            (unless (eq kind :files)
              (line "    ~a)" (first option))
              (if (eq kind :none)
                  (line "      ;;")
                  (progn (line "      mapfile -t COMPREPLY < <(compgen -W \"~{~a~^ ~}\" -- \"$cur\")" kind)
                         (line "      ;;"))))))
        (line "    *)")
        (line "      compopt -o filenames 2>/dev/null")
        (line "      mapfile -t COMPREPLY < <(compgen -f -- \"$cur\")")
        (line "      ;;")
        (line "  esac")
        (line "}")
        (line "complete -F _tuatara tuatara")))))

(defun write-bash-completion (directory)
  "Write the completion of tuatara's options for bash, the file tuatara, into
DIRECTORY (made when it is not there), over the one there.  The name of the
file."
  (ensure-directories-exist directory)
  (let ((file (merge-pathnames "tuatara" (uiop:ensure-directory-pathname directory))))
    (alexandria:write-string-into-file (completion-script) file :if-exists :supersede :external-format :utf-8)
    (namestring file)))
