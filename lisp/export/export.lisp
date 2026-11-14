;;;; lisp/export/export.lisp --- Org and LaTeX files made into PDFs, in the background.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; In an Org buffer M-x org-export-latex-pdf makes the file into a PDF beside
;;;; it, as Org's own export makes it; org-export-latex-pdf-buffer shows the PDF
;;;; too, beside the file when the screen is wide enough, org-export-latex-pdf-xdg
;;;; opens it in the desktop's program, and the -enhanced ones add Tuatara's own
;;;; improvements. In a LaTeX buffer export-latex-compile-pdf, -buffer and -xdg do the
;;;; same for the LaTeX file. An Org buffer is first written as LaTeX, as it is
;;;; in the buffer, by ORG-LATEX-DOCUMENT; a LaTeX file is saved first. The
;;;; LaTeX is then run through the first of *LATEX-ENGINES*, xelatex as
;;;; standard, and through the next when that fails, as often as it takes for
;;;; its cross-references to settle. Their working files (.aux, .toc, .log)
;;;; are kept in the cache, one directory for each file and program, so the
;;;; directory of the file gets only its .tex and .pdf, and making the PDF
;;;; again after a change usually takes one run. All of it runs as a job of
;;;; the buffer: P is lit in its mode line, C-x J (stop-job) stops it. A
;;;; view of the PDF already open is read again when the PDF is written.
;;;;
;;;; Exporting runs what the file says (LaTeX's programming), so it is refused
;;;; for a file opened safely. Org's code blocks are never evaluated, and LaTeX
;;;; may not run programs (no shell escape). The LaTeX program is shut in by
;;;; the sandbox (src/io/sandbox.lisp): it reads the document's directory, the
;;;; trusted ones and TeX's own, writes in its working directory alone, and
;;;; reaches no network. Without a sandbox it runs all the same, and the
;;;; export says so.

(in-package #:tuatara.export)

(define-setting *latex-engines* '("xelatex" "lualatex")
  "The programs that make a PDF of LaTeX, tried in this order: when one fails,
the next is tried. A file that names the program it is meant for, by a first
line such as \"% Intended LaTeX compiler: lualatex\" (which an Org file's
#+latex_compiler gives) or \"%!TEX program = lualatex\", has that one tried
first when it is in the list.
- a list of texts, \"xelatex\" and \"lualatex\" as standard. Each is a
  program's name, looked for on the PATH and then in TeX Live's own directories
  (~/.texlive/2026/bin/x86_64-linux/ and the like, the newest release
  first), or its full path, which may begin with ~ (\"~/texlive/bin/xelatex\"),
  for one installed elsewhere."
  :values (list-of string))

(defparameter *latex-runs* 4
  "The most times a LaTeX program is run on a file for one PDF: again only
while its cross-references, contents and outline are still changing.")

(defparameter *latex-error-lines* 60
  "The most lines of a LaTeX program's errors shown in *LaTeX errors*.")

(defparameter *latex-carried-settings*
  (append tuatara.io:*sandbox-carried-settings*
          '(*latex-engines* *latex-runs* *latex-error-lines* *export-trusted-directories*))
  "The settings an export reads as it works, carried to the thread it works on
as they are on the editor's when it starts (TUATARA.RUNTIME:CARRY-SETTINGS):
the sandbox's, and those of the export.")

;;; The programs

(defun setting-program (name)
  "The full path of the program NAME, a name looked for on the PATH or a path
\(a leading ~ the home directory); NIL when there is no such program."
  (and (stringp name) (plusp (length name))
       (tuatara.io:find-program (if (char= #\~ (char name 0))
                                    (tuatara.io:native (tuatara.io:expand-path name))
                                    name))))

(defun latex-engine-names ()
  "The programs *LATEX-ENGINES* names, as a list however it was set."
  (if (listp *latex-engines*) *latex-engines* (list *latex-engines*)))

(defparameter *tex-live-directories* '("~/.texlive/" "~/texlive/" "/usr/local/texlive/" "/opt/texlive/")
  "Where TeX Live is installed, each holding a directory for each year's
release, YEAR/bin/PLATFORM/ holding its programs: looked in, the newest
release first, for a program of *LATEX-ENGINES* the PATH does not have.")

(defun tex-live-program (name)
  "The full path of the program NAME in the newest TeX Live release under
*TEX-LIVE-DIRECTORIES* that has it, or NIL."
  (dolist (root *tex-live-directories*)
    (let ((root (ignoring-errors-reported ("expanding ~a" root) (tuatara.io:native (tuatara.io:expand-path root)))))
      (when root
        (dolist (release (sort (loop for (entry kind) in (tuatara.io:directory-entries root)
                                     when (and (eq kind :directory) (every #'digit-char-p entry)) collect entry)
                               #'string>))
          (let ((bin (tuatara.io:path-join (tuatara.io:path-join root release) "bin/")))
            (dolist (platform (tuatara.io:directory-entries bin))
              (when (eq (second platform) :directory)
                (let* ((directory (tuatara.io:path-join bin (concatenate 'string (first platform) "/")))
                       (found (tuatara.io:find-program (tuatara.io:path-join directory name))))
                  (when found (return-from tex-live-program found)))))))))))

(defun latex-engines-found ()
  "The programs of *LATEX-ENGINES* found here, each (NAME . PATH), in order: a
name on the PATH, else in TeX Live's own directories (*TEX-LIVE-DIRECTORIES*),
for a desktop whose PATH does not have them; a path as it is."
  (loop for name in (latex-engine-names)
        for path = (or (setting-program name)
                       (and (stringp name) (plusp (length name)) (not (find #\/ name)) (tex-live-program name)))
        when path collect (cons (tuatara.io:native-file-name name) path)))

(defun latex-export-words ()
  "What makes PDFs of Org and LaTeX files here, as M-x check-health says it:
the programs found, and whether they run shut in by the sandbox."
  (let ((engines (latex-engines-found)))
    (if engines
        (multiple-value-bind (status detail) (tuatara.io:sandbox-status)
          (format nil "PDFs of Org and LaTeX by ~{~a~^, then ~}, ~:[unconfined (~a): a document could read any of ~
your files~;shut in by the sandbox~*~]"
                  (mapcar #'cdr engines) (eq status :ready) detail))
        (format nil "PDFs of Org and LaTeX by none of ~{~a~^, ~}, on the PATH or in TeX Live's directories: ~a, or ~
give their paths in *latex-engines*"
                (latex-engine-names) (outside-program-install-words :latex)))))

(defun tex-program-beside (path name)
  "The full path of the program NAME of the TeX the program at PATH belongs to:
the one beside it, else the one on the PATH, else the one in TeX Live's own
directories (TEX-LIVE-PROGRAM); NIL when there is none. PATH may be NIL."
  (or (and path (tuatara.io:find-program (tuatara.io:path-join (tuatara.io:parent-path path) name)))
      (tuatara.io:find-program name)
      (tex-live-program name)))

(defun kpsewhich-for (engine)
  "The kpsewhich of the TeX the LaTeX program ENGINE (a name of *LATEX-ENGINES*)
belongs to (TEX-PROGRAM-BESIDE); NIL when there is none."
  (tex-program-beside (cdr (assoc engine (latex-engines-found) :test #'string-equal)) "kpsewhich"))

(defun hyphenation-files-found (kpsewhich patterns)
  "The names among PATTERNS, endings of hyphenation files (loadhyph-NAME.tex),
that the TeX KPSEWHICH belongs to has, or :UNKNOWN when it cannot be asked.
KPSEWHICH is asked shut in by the sandbox, as the LaTeX programs are, shown
TeX's own directories (TEX-TREES), for ten seconds at most, and ended at once when
the job asking is stopped; where there is no sandbox it is not asked."
  (multiple-value-bind (output outcome)
      (if kpsewhich
          (tuatara.io:sandbox-run kpsewhich (mapcar (lambda (name) (format nil "loadhyph-~a.tex" name)) patterns)
                                  :seconds 10 :ended-well '(0 1)
                                  :cancel (and tuatara.runtime:*job* (tuatara.runtime:job-cancel tuatara.runtime:*job*))
                                  :directories (tex-trees kpsewhich)
                                  :environment (latex-sandbox-environment kpsewhich))
          (values nil :unavailable))
    (if (eq outcome :done)
        (let ((said (tuatara.text:lenient-string output)))
          (remove-if-not (lambda (name) (search (format nil "/loadhyph-~a.tex" name) said)) patterns))
        :unknown)))

(defun latex-language-patterns-note (engine code)
  "What to say when the hyphenation patterns of the language CODE (a row of
*EXPORT-LANGUAGES*) are not in the TeX Live the LaTeX program ENGINE belongs to,
so that words of the PDF are not broken at the line ends in it; NIL when they
are there or cannot be asked."
  (let* ((row (export-language code))
         (found (and row (hyphenation-files-found (kpsewhich-for engine) (list (fifth row))))))
    (when (and row (listp found) (null found))
      (format nil "TeX Live has no hyphenation patterns for ~a, so its words are not broken at line ends: ~
                   install ~a (tlmgr install ~:*~a, or your system's TeX Live language packages)"
              (second row) (sixth row)))))

(defun latex-language-words ()
  "Which languages' hyphenation patterns are installed for the PDFs of Org and
LaTeX, as M-x check-health says it; NIL when TeX Live cannot be asked."
  (let* ((engine (first (first (latex-engines-found))))
         (rows (remove-duplicates *export-languages* :key #'fifth :test #'string= :from-end t))
         (found (and engine (hyphenation-files-found (kpsewhich-for engine) (mapcar #'fifth rows)))))
    (when (listp found)
      (flet ((names (present)
               (format nil "~{~a~^, ~}"
                       (loop for row in *export-languages*
                             when (eq present (and (member (fifth row) found :test #'string=) t))
                             collect (first row)))))
        (format nil "Hyphenation in PDFs by #+language: ~a~@[; patterns missing for ~a~]"
                (names t) (let ((missing (names nil))) (and (plusp (length missing)) missing)))))))

(defun intended-engine (tex)
  "The LaTeX program the file TEX says it is meant for, by its first lines, as
a name in lower case; NIL when it says none."
  (let ((head (tuatara.io:file-head tex 4096)))
    (when head
      (let ((text (tuatara.text:lenient-string head)))
        (dolist (mark '("% Intended LaTeX compiler: " "%!TEX TS-program = " "%!TEX program = "))
          (let ((at (search mark text :test #'char-equal)))
            (when at
              (let* ((start (+ at (length mark)))
                     (end (or (position-if (lambda (c) (member c '(#\Newline #\Return #\Space #\Tab))) text
                                           :start start)
                              (length text))))
                (return (string-downcase (subseq text start end)))))))))))

(defun engines-for (tex)
  "The programs found here to make a PDF of the file TEX with, each (NAME .
PATH), the one TEX says it is meant for first."
  (let* ((engines (latex-engines-found))
         (intended (intended-engine tex))
         (first (and intended (find intended engines :key #'car :test #'string-equal))))
    (if first (cons first (remove first engines)) engines)))

;;; The job

(define-record (pdf-export (:include tuatara.runtime:job)
                          (:constructor make-pdf-export (buffer source kind then save rope enhanced))
                          (:copier nil))
  "A PDF being made of SOURCE, the file of BUFFER, a job (TUATARA.RUNTIME:JOB):
KIND :ORG or :LATEX. THEN what is done with the PDF, :FILE (nothing), :VIEW
\(shown beside), :OUTSIDE (opened in the desktop's program), or :LATEX-ONLY, no
PDF made, the export stopping at the LaTeX file. SAVE, the save of BUFFER it
waits for first, or NIL. ROPE, an Org buffer's text as it was when asked.
ENHANCED, true for Tuatara's own improvements on Org's LaTeX. STEP says what it
does now. PROCESS is the program running for it. TEX, the LaTeX file. TEXT, the
Org text written as it, and MAP, which place in TEXT each of its lines comes
from (ORG-LATEX-DOCUMENT). PDF and ENGINE, the PDF made and the program that
made it. FAILED, the programs that failed before. ERRORS, what each said,
\(PROGRAM . ERRORS) (LATEX-ERRORS), for *LaTeX errors*; MISSING, the characters
no font here has, each (CHARACTER . LINE) by the LaTeX's line it is first on.
NOTES, what else to say of it. PART, the words for a part exported alone (the
subtree \"Findings\"), with ORIGINS, the line of the file each line of its text
comes from, a vector, and LINES, the file's lines, for what is said of them.
TRUSTED, the texts of its template and setup files that are your own, and
PICTURES, the files its pictures are drawn from (ORG-LATEX-DOCUMENT). AFTER,
a function of the job called once the PDF is made and THEN done, or NIL.
LINE-STARTS, where each line of TEXT begins (TEXT-LINE-STARTS), once asked."
  (source nil :documentation "The file of the buffer PDF-EXPORT makes a PDF of.")
  (kind nil :documentation "The kind of the source, :ORG or :LATEX.")
  (then nil
        :documentation "What is done with the PDF: :FILE (nothing), :VIEW (shown beside), :OUTSIDE (opened in the
desktop's program) or :LATEX-ONLY.")
  (save nil :documentation "The save of the buffer PDF-EXPORT waits for first, or NIL.")
  (rope nil :documentation "The text of an Org buffer as it was when the export was asked for.")
  (enhanced nil :documentation "True for Tuatara's own improvements on Org's LaTeX.")
  (step "starting" :documentation "What PDF-EXPORT does now, in words.")
  (tex nil :documentation "The LaTeX file PDF-EXPORT writes.")
  (text nil :documentation "The Org text written as LaTeX.")
  (map nil :documentation "Which place in the text of PDF-EXPORT each of its lines comes from (ORG-LATEX-DOCUMENT).")
  (pdf nil :documentation "The PDF made by PDF-EXPORT.")
  (engine nil :documentation "The program that made the PDF.")
  (failed '() :documentation "The programs that failed before.")
  (errors nil :documentation "What each failed program said, (PROGRAM . ERRORS) (LATEX-ERRORS), for *LaTeX errors*.")
  (missing '()
           :documentation "The characters no font here has, each (CHARACTER . LINE) by the LaTeX line it is first on.")
  (notes '() :documentation "What else to say of PDF-EXPORT.")
  (part nil :documentation "The words for a part exported alone, as the subtree \"Findings\", or NIL.")
  (origins nil :documentation "The line of the file each line of the text of a part comes from, a vector.")
  (lines nil :documentation "The lines of the file, for what is said of them.")
  (trusted '() :documentation "The texts of the template and setup files that are your own.")
  (pictures '() :documentation "The files the pictures of PDF-EXPORT are drawn from (ORG-LATEX-DOCUMENT).")
  (after nil :documentation "A function of the job called once the PDF is made and THEN done, or NIL.")
  (line-starts nil :documentation "Where each line of the text of PDF-EXPORT begins (TEXT-LINE-STARTS), once asked."))

(defun export-name (job)
  "The name of the file JOB makes a PDF of."
  (tuatara.io:native-file-name (pdf-export-source job)))

(defun export-doing (kind)
  "What making a PDF of a file of KIND is called: an Org file is exported, a LaTeX file compiled."
  (if (eq kind :latex) "Compiling" "Exporting"))

(tuatara.runtime:define-job-words pdf-export
    :doing (export-doing (pdf-export-kind job))
    :description (format nil "~a ~@[~a of ~]~a to ~a" (export-doing (pdf-export-kind job)) (pdf-export-part job)
                         (export-name job) (export-target-words job))
    :verb (format nil "~(~a~) ~a to ~a" (if (eq (pdf-export-kind job) :latex) "compile" "export") (export-name job)
                  (export-target-words job))
    :left-words "the PDF is as it was")

(defmethod tuatara.runtime:job-state-words ((job pdf-export))
  "What an export is doing now, and at which step, in words."
  (format nil "~a: ~a" (export-doing (pdf-export-kind job)) (pdf-export-step job)))

(defun export-target-words (job)
  "What JOB makes, in a word: LaTeX for an export that stops at the LaTeX file, else PDF."
  (if (eq (pdf-export-then job) :latex-only) "LaTeX" "PDF"))

(defmethod tuatara.runtime:job-holds-queue-p ((job pdf-export))
  "An export holds back no other work on its buffer: it only reads the note."
  nil)

(defun run-for-export (job program arguments &key directory environment)
  "Run PROGRAM with ARGUMENTS for JOB, in DIRECTORY, with ENVIRONMENT when
given, its input empty and what it prints dropped, and wait for it; never
through a shell. Returns its exit code. JOB stopping ends it."
  (tuatara.runtime:job-check-stop)
  (unwind-protect
       (apply #'tuatara.io:call-with-program (cons program arguments)
              (lambda (process)
                (sb-ext:process-wait process)
                (tuatara.runtime:job-check-stop)
                (sb-ext:process-exit-code process))
              :job job :directory directory
              (and environment (list :environment environment)))
    (setf (pdf-export-process job) nil)))

;;; LaTeX shut in

(defparameter *latex-sandbox-seconds* 600
  "The seconds one run of a LaTeX program may take, shut in, before it is
ended, and the processor time it may take: ten minutes, for a long document
with many pictures, or the first run after TeX was installed, when LuaLaTeX
makes its list of the fonts there are.")

(defparameter *latex-sandbox-memory* (* 2 1024 1024 1024)
  "The octets of memory a LaTeX program may take, shut in (2 GB).")

(defparameter *latex-sandbox-file-limit* (* 4 1024 1024 1024)
  "The octets a LaTeX program, shut in, may write to one file (4 GB): its PDF
and working files.")

(defparameter *latex-passed-environment* '("TEXMF" "OSFONTDIR" "SOURCE_DATE_EPOCH" "FORCE_SOURCE_DATE"
                                           "XDG_CONFIG_HOME" "XDG_DATA_HOME" "XDG_CACHE_HOME")
  "The names, or beginnings of names (TEXMF for TEXMFHOME and the rest), of
what in the editor's environment a LaTeX program shut in is given as it is:
where TeX and the fonts are, and the date a PDF is said to be made.")

(defparameter *tex-tree-variables* '("TEXMFCNF" "TEXMFROOT" "TEXMFDIST" "TEXMFLOCAL" "TEXMFSYSVAR" "TEXMFSYSCONFIG"
                                     "TEXMFHOME" "TEXMFCONFIG" "TEXMFAUXTREES" "TEXMFVAR")
  "TeX's own names of the directories it reads its settings (texmf.cnf), packages
and fonts from, as kpsewhich tells them; the last, TEXMFVAR, is where it keeps
what it makes for itself.")

(defvar *tex-trees* (make-hash-table :test 'equal :synchronized t)
  "The directories of TeX found for each LaTeX program (TEX-TREES), by its path and the home directory.")

(defun tex-tree-paths (value)
  "The directories VALUE, one of TeX's own settings as kpsewhich expands it,
names: each beginning with /, the {a,b} lists, : and ; between, and !! before
them taken apart."
  (loop for part in (uiop:split-string value :separator '(#\: #\; #\, #\{ #\}))
        for path = (string-right-trim "/" (string-left-trim "!" (string-trim " " part)))
        when (and (plusp (length path)) (char= #\/ (char path 0)))
        collect path))

(defun shown-whole-p (directory)
  "True when DIRECTORY may be shown whole to a LaTeX program shut in: neither
the root, nor your home directory, nor a directory holding it, nor a place
the sandbox shows no program (TUATARA.IO:SHOWN-PLACE-P), such as /tmp or
your runtime directory (XDG_RUNTIME_DIR), where other programs keep the
sockets they are reached by."
  (let ((real (tuatara.io:real-place directory))
        (home (tuatara.io:real-place (latex-home))))
    (not (or (string= real "") (string= real home) (uiop:string-prefix-p (concatenate 'string real "/") home)
             (not (tuatara.io:shown-place-p real))))))

(defun trusted-export-directories (&key whole)
  "The directories *EXPORT-TRUSTED-DIRECTORIES* names, expanded, as native
paths; with WHOLE, only those that may be shown whole (SHOWN-WHOLE-P). One
that cannot be expanded is left out, and that reported."
  (loop for directory in *export-trusted-directories*
        for native = (ignoring-errors-reported ("expanding ~a" directory)
                                               (tuatara.io:native (tuatara.io:expand-path directory)))
        when (and native (or (not whole) (shown-whole-p native)))
        collect native))

(defun tex-settings-said (path)
  "What the kpsewhich beside the LaTeX program at PATH says TeX's directories
\(*TEX-TREE-VARIABLES*) are, one to a line, asked shut in by the sandbox, shown
the program's own directory and the one two above it, where TeX Live keeps its
settings, for ten seconds at most, and ended at once when the job asking is
stopped. NIL when there is no kpsewhich there, no sandbox, or it failed. True as
a second value when the answer is to be kept: not when the asking was stopped or
ran out of time."
  (let ((kpsewhich (tuatara.io:find-program (tuatara.io:path-join (tuatara.io:parent-path path) "kpsewhich"))))
    (if (null kpsewhich)
        (values nil t)
        (let* ((own (string-right-trim "/" (tuatara.io:parent-path kpsewhich)))
               (root (string-right-trim "/" (tuatara.io:parent-path (tuatara.io:parent-path own)))))
          (multiple-value-bind (output outcome)
              (tuatara.io:sandbox-run kpsewhich (list (format nil "-expand-var=~{$~a~^~%~}" *tex-tree-variables*))
                                      :seconds 10
                                      :cancel (and tuatara.runtime:*job*
                                                   (tuatara.runtime:job-cancel tuatara.runtime:*job*))
                                      :directories (remove-if-not #'shown-whole-p
                                                                  (remove "" (list own root) :test #'string=))
                                      :environment (latex-sandbox-environment kpsewhich))
            (values (and (eq outcome :done) (tuatara.text:lenient-string output))
                    (member outcome '(:done :failed :unavailable))))))))

(defun tex-trees (path)
  "The directories of TeX that the LaTeX program at PATH reads, as two values: a
list of them, its own directory first; and the directory it keeps what it makes
for itself in (TEXMFVAR), or NIL. As kpsewhich beside it tells them, asked
once (TEX-SETTINGS-SAID) and kept; only the program's directory when there is
no kpsewhich there, or no sandbox to ask it in. Directories under /usr, which a program shut in sees
already, are left out, and so are those not to be shown whole
\(SHOWN-WHOLE-P), which a setting may name."
  (let* ((own (string-right-trim "/" (tuatara.io:parent-path path)))
         (key (list path (tuatara.io:own-variable "HOME")))
         (found (or (gethash key *tex-trees*)
                    (multiple-value-bind (said kept) (tex-settings-said path)
                      (let* ((lines (and said (org-text-lines said)))
                             (found (if (= (length lines) (length *tex-tree-variables*))
                                        (list (loop for line in (butlast lines) append (tex-tree-paths line))
                                              (first (tex-tree-paths (car (last lines)))))
                                        (list '() nil))))
                        (if kept (setf (gethash key *tex-trees*) found) found))))))
    (flet ((system-p (directory) (or (string= directory "/usr") (uiop:string-prefix-p "/usr/" directory)
                                     (not (shown-whole-p directory)))))
      (values (remove-duplicates (cons own (remove-if #'system-p (first found))) :test #'string= :from-end t)
              (let ((var (second found))) (and var (not (system-p var)) var))))))

(defun tex-cache-overlay (var)
  "How TeX's directory VAR, where it keeps what it makes for itself (its fonts'
list, its formats), is shown to a LaTeX program shut in. What is there is read
as it is, what it writes kept in a directory of the editor's own in the cache,
latex-texmf-var/, never in VAR, so that a document cannot leave there what a
LaTeX program run outside the sandbox would then read. As two values, the
entries for SANDBOX-RUN's OVERLAYS and WRITABLE (VAR made in the editor's
directory where VAR is not there yet)."
  (let* ((mine (private-cache-directory (format nil "latex-texmf-var/~a-~36r/" (tuatara.io:native-file-name var)
                                                (sxhash var))))
         (upper (string-right-trim "/" (tuatara.io:ensure-private-directory (tuatara.io:path-join mine "upper"))))
         (work (string-right-trim "/" (tuatara.io:ensure-private-directory (tuatara.io:path-join mine "work")))))
    (if (eq :directory (tuatara.io:path-kind var))
        (values (list (list var upper work)) '())
        (values '() (list (cons upper var))))))

(defun latex-home ()
  "Your home directory as a LaTeX program is told it, shut in: as the editor has
it in its environment, so that TeX and the fonts find your own where they
always do."
  (string-right-trim "/" (or (tuatara.io:own-variable "HOME") (tuatara.io:home-directory))))

(defparameter *latex-picture-file-kinds* '(:png :jpeg :pdf :postscript)
  "The kinds of file, by their contents, a LaTeX program shut in is shown when
the document includes them as pictures from outside the directories it sees.")

(defun export-file-text (path)
  "The text of the file at PATH an export reads (a LaTeX file, a working file
of its program's), up to 64 MB of it; NIL when it is not there or cannot be
read."
  (and (eq :regular (tuatara.io:path-kind path))
       (ignoring-errors-reported ("reading ~a" path)
                                 (tuatara.io:file-text path :limit (* 64 1024 1024)))))

(defun included-picture-names (text)
  "The names TEXT, LaTeX, includes pictures by (\\includegraphics), in order."
  (let ((names '()))
    (cl-ppcre:do-register-groups (name) ("\\\\includegraphics\\s*(?:\\[[^\\]]*\\])?\\s*\\{([^}\\n]+)\\}" text)
                                 (push name names))
    (nreverse names)))

(defun latex-picture-files (tex trusted)
  "The files the LaTeX document TEX includes as pictures (\\includegraphics) by
a full name or one from your home directory (~/), which one of TRUSTED, the
texts of its template and setup files that are your own, includes as a
picture by a name for the same file, and which are pictures or PDFs by their
contents (*LATEX-PICTURE-FILE-KINDS*). Those are the files a LaTeX program
shut in is shown, each alone and read only, wherever they are. A note's own
text, or a file that came with it, cannot have a file of yours from elsewhere
shown, however it writes its name."
  (flet ((native (name)
           (and (or (uiop:string-prefix-p "/" name) (uiop:string-prefix-p "~/" name))
                (ignoring-errors-reported ("expanding ~a" name)
                                          (tuatara.io:native (tuatara.io:expand-path name))))))
    (let ((text (export-file-text tex))
          (own (remove nil (mapcar #'native (mapcan #'included-picture-names trusted))))
          (files '()))
      (when text
        (dolist (name (included-picture-names text))
          (let ((native (native name)))
            (when (and native (member native own :test #'string=)
                       (member (nth-value 1 (ignoring-errors-reported ("reading ~a" native) (file-kind native)))
                               *latex-picture-file-kinds*))
              (pushnew native files :test #'string=)))))
      (nreverse files))))

(defun tex-program-places (path directory)
  "What the TeX program at PATH, working in DIRECTORY, is shown, shut in,
whatever it makes, as a plist of SANDBOX-RUN's :DIRECTORIES, :WRITABLE and
:OVERLAYS: TeX's own directories (TEX-TREES) and your fonts and their
settings, read only; to write in, DIRECTORY alone, and TeX's own cache
\(TEX-CACHE-OVERLAY)."
  (multiple-value-bind (trees var) (tex-trees path)
    (multiple-value-bind (overlays var-writable) (if var (tex-cache-overlay var) (values '() '()))
      (let* ((home (latex-home))
             (config (string-right-trim "/" (or (tuatara.io:own-variable "XDG_CONFIG_HOME")
                                                (format nil "~a/.config" home))))
             (cache (string-right-trim "/" (or (tuatara.io:own-variable "XDG_CACHE_HOME")
                                               (format nil "~a/.cache" home)))))
        (list :directories (append trees
                                   (list (format nil "~a/fontconfig" config) (format nil "~a/fontconfig" cache))
                                   (tuatara.io:user-font-directories home))
              :writable (cons directory var-writable)
              :overlays overlays)))))

(defun latex-sandbox-places (job tex path directory)
  "What the LaTeX program at PATH, making a PDF of TEX for JOB in DIRECTORY,
is shown, shut in, as a plist of SANDBOX-RUN's :DIRECTORIES, :WRITABLE and
:OVERLAYS: what any TeX program is shown (TEX-PROGRAM-PLACES), and read only
the directory of TEX and that of the file exported, and the directories
*EXPORT-TRUSTED-DIRECTORIES* names, each when it may be shown whole
\(SHOWN-WHOLE-P; else TEX alone). The files the note's pictures are drawn
from (those an export draws in latex/pictures/ among them), the pictures
your own template and setup files have TEX include from elsewhere
\(LATEX-PICTURE-FILES), or a LaTeX file exported as it is when it stands in
your own directories (EXPORT-TRUSTED-PATH-P), are shown too, each file
alone, read only."
  (let ((places (tex-program-places path directory)))
    (list :directories (append (let ((dirs (list (tuatara.io:parent-path tex)
                                                 (tuatara.io:parent-path (pdf-export-source job)))))
                                 (if (every #'shown-whole-p dirs) dirs (list tex)))
                               (trusted-export-directories :whole t)
                               (pdf-export-pictures job)
                               (latex-picture-files tex (if (eq (pdf-export-kind job) :latex)
                                                            ;; a LaTeX file of your own, in your own directories
                                                            (let ((text (and (export-trusted-path-p tex)
                                                                             (export-file-text tex))))
                                                              (and text (list text)))
                                                            (pdf-export-trusted job)))
                               (getf places :directories))
          :writable (getf places :writable)
          :overlays (getf places :overlays))))

(defun tex-search-environment (tex)
  "What a TeX program making a PDF of TEX has in its environment, each (NAME
VALUE), however it runs: TEX's directory searched first for the files the
document names (TEXINPUTS), its bibliographies and their styles (BIBINPUTS, BSTINPUTS)
and the styles of its index (INDEXSTYLE), when TEX is given; and what it writes
kept to where it runs, never a parent directory or a full name elsewhere
\(openout_any)."
  (append (and tex (let ((directory (format nil "~a:" (tuatara.io:parent-path tex))))
                     (mapcar (lambda (name) (list name directory))
                             '("TEXINPUTS" "BIBINPUTS" "BSTINPUTS" "INDEXSTYLE"))))
          (list (list "openout_any" "p"))))

(defun latex-sandbox-environment (path &optional tex)
  "What the TeX program at PATH, making a PDF of TEX, has in its environment,
shut in, each (NAME VALUE): its own directory first on the PATH, for the programs
it runs in turn (xdvipdfmx). Your home directory, where TeX and the fonts find your
own; TEX-SEARCH-ENVIRONMENT. And *LATEX-PASSED-ENVIRONMENT* as the editor has it."
  (let ((searched (tex-search-environment tex)))
    (append (list (list "PATH" (format nil "~a:~a" (string-right-trim "/" (tuatara.io:parent-path path))
                                       tuatara.io:*sandbox-path*))
                  (list "HOME" (latex-home)))
            searched
            (loop for setting in (tuatara.io:program-environment)
                  for at = (position #\= setting)
                  for name = (and at (subseq setting 0 at))
                  when (and name (not (assoc name searched :test #'string=)) (string/= name "TEXINPUTS")
                            (some (lambda (passed) (uiop:string-prefix-p passed name)) *latex-passed-environment*))
                  collect (list name (subseq setting (1+ at)))))))

(defun run-shut-in (job tex path arguments directory &key (ended-well '(0)) scratch places)
  "Run the TeX program at PATH (a LaTeX program, or one making its
bibliography or index) with ARGUMENTS for JOB, making a PDF of TEX in
DIRECTORY, shut in (TUATARA.IO:SANDBOX-RUN): it sees what PLACES shows it
\(a plist as LATEX-SANDBOX-PLACES gives, which it is unless given), read only
but for DIRECTORY and TeX's own cache, has no network, and is held to *LATEX-SANDBOX-SECONDS*,
*LATEX-SANDBOX-MEMORY* and *LATEX-SANDBOX-FILE-LIMIT*; what it prints is let
go. Returns two values: true when it ended well, with an exit code among
ENDED-WELL, and else how it ended, as a sentence naming it
\(SANDBOX-OUTCOME-WORDS). JOB stopping ends it. SCRATCH, a directory of the
editor's own, it may write in too and finds again on its next run, its
temporary directory (SCRATCH-ENVIRONMENT)."
  (tuatara.runtime:job-check-stop)
  (multiple-value-bind (output outcome said)
      (apply #'tuatara.io:sandbox-run path arguments
             :seconds *latex-sandbox-seconds* :memory *latex-sandbox-memory* :file-limit *latex-sandbox-file-limit*
             :output-limit nil :cancel (tuatara.runtime:job-cancel job) :directory directory :ended-well ended-well
             :environment (append (scratch-environment scratch) (latex-sandbox-environment path tex))
             (let ((places (or places (latex-sandbox-places job tex path directory))))
               (list :directories (getf places :directories) :overlays (getf places :overlays)
                     :writable (append (and scratch (list (string-right-trim "/" scratch))) (getf places :writable)))))
    (declare (ignore output))
    (tuatara.runtime:job-check-stop)
    (values (eq outcome :done)
            (sandbox-outcome-words outcome said :label (tuatara.io:native-file-name path)
                                   :seconds-setting '*latex-sandbox-seconds*))))

(defun run-unconfined (job path arguments directory environment &key (ended-well '(0)))
  "Run the TeX program at PATH with ARGUMENTS for JOB in DIRECTORY, with
ENVIRONMENT (each NAME=VALUE) in place of the editor's own of those names,
not shut in (RUN-FOR-EXPORT). Returns two values: true when it ended well,
with an exit code among ENDED-WELL, and else how it ended, as a sentence
naming it."
  (let* ((names (mapcar (lambda (setting) (subseq setting 0 (1+ (position #\= setting)))) environment))
         (code (run-for-export job path arguments
                               :directory directory
                               :environment (append environment
                                                    (remove-if (lambda (v)
                                                                 (some (lambda (name) (uiop:string-prefix-p name v))
                                                                       names))
                                                               (tuatara.io:program-environment))))))
    (values (and (member code ended-well) t)
            (format nil "~a ended with exit code ~a" (tuatara.io:native-file-name path) code))))

(defun scratch-environment (scratch)
  "What a TeX program given the directory SCRATCH to keep its own files in between
runs has in its environment for it, each (NAME VALUE): SCRATCH as its temporary
directory (TMPDIR, and PAR_GLOBAL_TMPDIR, where biber, a program packed with what
it needs, unpacks itself once rather than on every run). NIL without SCRATCH."
  (and scratch (let ((directory (string-right-trim "/" scratch)))
                 (list (list "TMPDIR" directory) (list "PAR_GLOBAL_TMPDIR" directory)))))

(defun run-tex-program (job tex path arguments directory shut-in &key (ended-well '(0)) scratch)
  "Run the TeX program at PATH with ARGUMENTS for JOB, making a PDF of TEX in
DIRECTORY: with SHUT-IN shut in by the sandbox (RUN-SHUT-IN), else with your
rights (RUN-UNCONFINED), its own directory first on the PATH, for the programs it
runs in turn (xdvipdfmx), and TEX-SEARCH-ENVIRONMENT. Returns two values: true
when it ended well, with an exit code among ENDED-WELL, and else how it ended,
as a sentence naming it. JOB stopping ends it. SCRATCH, a directory of the
editor's own, it may also write in and find again on its next run
\(SCRATCH-ENVIRONMENT)."
  (if shut-in
      (run-shut-in job tex path arguments directory :ended-well ended-well :scratch scratch)
      (run-unconfined job path arguments directory
                      (cons (format nil "PATH=~a:~a" (string-right-trim "/" (tuatara.io:parent-path path))
                                    (tuatara.io:program-search-path))
                            (loop for (name value) in (append (scratch-environment scratch)
                                                              (tex-search-environment tex))
                                  collect (format nil "~a=~a" name value)))
                      :ended-well ended-well)))

(defun latex-unconfined-allowed-p ()
  "True when a LaTeX program, and the programs making the document's
bibliography and index, may run for an export now: shut in by the sandbox,
or, where there is none (bwrap missing, or turned off by *sandbox*), when
you say yes to running them with your rights, as what the document says
could then read any of your files."
  (multiple-value-bind (status detail) (tuatara.io:sandbox-status)
    (or (eq status :ready)
        (yes-or-no-p* (format nil "~a. Run it all the same?" (latex-unconfined-words detail :would t))))))

(defun latex-unconfined-words (detail &key would)
  "What an export says when its LaTeX program ran without the sandbox, DETAIL
saying why; with WOULD, what it asks before it runs so."
  (format nil
          "LaTeX ~:[ran~;would run~] without the sandbox (~a), so what the document says could read any of your files"
          would detail))

(defun check-export-directory (path)
  "Signal an error saying so when the directory an export is to write PATH in
does not exist: an export never makes directories, wherever #+EXPORT_FILE_NAME
points."
  (unless (tuatara.io:path-stamp (tuatara.io:parent-path path))
    (if *secret-at-hand*
        (refuse "The directory #+export_file_name names does not exist")
        (refuse "~a does not exist (#+export_file_name)" (tuatara.io:parent-path path)))))

(defun org-to-latex (job engine)
  "Write the Org text of JOB as LaTeX for the program ENGINE (ORG-LATEX-PARTS),
beside its file or where its #+export_file_name says; returns the LaTeX
file's full name."
  (setf (pdf-export-step job) "writing LaTeX")
  (let ((source (pdf-export-source job))
        (text (let ((rope (pdf-export-rope job))) (if (stringp rope) rope (tuatara.text:rope-string rope)))))
    (let* ((parts (org-latex-parts text :file source :engine engine :enhanced (pdf-export-enhanced job)
                                   :lines (pdf-export-origins job)))
           (language (getf parts :language))
           (note (and language (latex-language-patterns-note engine language)))
           (tex (or (getf parts :output) (replaced-type source "tex"))))
      (setf (pdf-export-text job) text
            (pdf-export-map job) (getf parts :map)
            (pdf-export-missing job) (getf parts :missing)
            (pdf-export-notes job) (append (getf parts :notes) (and note (list note)))
            (pdf-export-trusted job) (getf parts :trusted)
            (pdf-export-pictures job) (getf parts :pictures))
      (check-export-directory tex)
      (tuatara.runtime:job-check-stop)
      (tuatara.io:replace-file tex (getf parts :latex))
      tex)))

(defun latex-build-place (tex)
  "The directory under latex/ in the cache directory that holds the working
files of the LaTeX file TEX, a directory in it for each program: the same
for every name leading to the file, links followed."
  (let ((real (tuatara.io:real-path-or-parent tex)))
    (tuatara.io:path-join (private-cache-directory "latex/")
                          (format nil "~a-~36r/" (tuatara.io:native-file-name real) (sxhash real)))))

(defun latex-build-directory (tex engine)
  "Where ENGINE keeps its working files for the LaTeX file TEX, between one
export and the next: latex/ in the cache directory, a directory for each file
and program, for you alone. Those not used for a day are deleted."
  (forget-old-copies (private-cache-directory "latex/"))
  (tuatara.io:ensure-private-directory (tuatara.io:path-join (latex-build-place tex) (format nil "~a/" engine))))

(defun latex-synctex-file (tex)
  "The SyncTeX file of the PDF beside the LaTeX file TEX, where the program
that made it left it among its working files (LATEX-BUILD-DIRECTORY): the
one whose PDF there is the size of the PDF beside TEX and not newer than it
\(the PDF beside is its copy), the newest when more than one is. NIL when
there is none, as for a PDF made elsewhere."
  (let* ((stem (tuatara.io:native-file-stem tex))
         (pdf (tuatara.io:path-stamp (replaced-type tex "pdf")))
         (place (latex-build-place tex))
         (best nil) (best-time 0))
    (when pdf
      ;; no working files yet is as normal as none of the right size
      (dolist (entry (handler-case (tuatara.io:directory-entries place) (tuatara.io:file-system-error () nil)))
        (when (eq (second entry) :directory)
          (let* ((directory (tuatara.io:path-join place (concatenate 'string (first entry) "/")))
                 (synctex (tuatara.io:path-join directory (concatenate 'string stem ".synctex")))
                 (made (tuatara.io:path-stamp (tuatara.io:path-join directory (concatenate 'string stem ".pdf")))))
            (when (and made (= (tuatara.io:stamp-size made) (tuatara.io:stamp-size pdf))
                       (<= (tuatara.io:stamp-mtime made) (tuatara.io:stamp-mtime pdf))
                       (tuatara.io:path-stamp synctex) (> (tuatara.io:stamp-mtime made) best-time))
              (setf best synctex best-time (tuatara.io:stamp-mtime made)))))))
    best))

(defparameter *latex-cross-reference-types* '("aux" "toc" "out" "lof" "lot" "nav" "snm")
  "The kinds of working file whose change after a LaTeX run calls for another.")

(defun cross-references (directory stem)
  "The working files of the LaTeX file STEM in DIRECTORY that a run changes
until its cross-references settle, each as its size and its SHA-256 sum
\(FILE-SHA256), so that two runs' are compared without holding them; NIL for
one not there."
  (mapcar (lambda (type)
            (let* ((path (tuatara.io:path-join directory (format nil "~a.~a" stem type)))
                   (stamp (tuatara.io:path-stamp path)))
              (and stamp (cons (tuatara.io:stamp-size stamp) (file-sha256 path)))))
          *latex-cross-reference-types*))

(defparameter *latex-log-tail* (* 1024 1024)
  "The most octets of a LaTeX program's log read after a run: its last ones,
where it says what went wrong and whether to run it again.")

(defun latex-log-text (log)
  "The end of LOG, a LaTeX program's log, as text: its last *LATEX-LOG-TAIL*
octets, from the first line that begins within them; an empty string when
there is none."
  (or (ignoring-errors-reported ("reading ~a" log)
                                (tuatara.io:call-with-file
                                 log
                                 (lambda (fd path size)
                                   (let* ((from (max 0 (- size *latex-log-tail*)))
                                          (octets (make-array (- size from) :element-type '(unsigned-byte 8)))
                                          (read (tuatara.io:read-fd-at fd octets from :path path))
                                          (text (tuatara.text:lenient-string octets :end read)))
                                     (if (plusp from)
                                         (subseq text (1+ (or (position #\Newline text) (1- (length text)))))
                                         text)))))
      ""))

(defun latex-errors (log)
  "The errors in LOG, a LaTeX program's log, in order, at most
*LATEX-ERROR-LINES* of them: each a plist of its :MESSAGE, the :FILE and
:LINE it names (FILE:LINE: lines, or an l.LINE line after a ! line, for the
file being made), and the two lines after it, :CONTEXT; not those that only
say it stopped (Emergency stop)."
  (let* ((lines (coerce (uiop:split-string log :separator '(#\Newline)) 'vector))
         (found '()))
    (loop for i from 0 below (length lines)
          for line = (aref lines i)
          for context = (loop for j from (1+ i) below (min (length lines) (+ i 3)) collect (aref lines j))
          do (multiple-value-bind (match parts)
                 (cl-ppcre:scan-to-strings "^(.*?\\.(?:tex|sty|cls|def|cfg)):([0-9]+): (.*)$" line)
               (cond (match
                      (push (list :file (aref parts 0)
                                  :line (parse-integer (aref parts 1))
                                  :message (aref parts 2)
                                  :context context)
                            found))
                     ((and (plusp (length line)) (char= #\! (char line 0)))
                      (let ((at (loop for j from (1+ i) below (min (length lines) (+ i 12))
                                      for l = (aref lines j)
                                      when (cl-ppcre:scan "^l\\.[0-9]+" l)
                                      return (parse-integer l :start 2 :junk-allowed t))))
                        (push (list :file nil :line at :message (string-left-trim "! " line) :context context)
                              found))))))
    ;; what only says that it stopped, after an error said already
    (last (remove-if (lambda (problem)
                       (cl-ppcre:scan "^\\s*(?:==> Fatal error occurred|Emergency stop)" (getf problem :message)))
                     (nreverse found))
          *latex-error-lines*)))

(defun rerun-asked-p (log)
  "True when LOG, a LaTeX program's log, says it should be run again."
  (some (lambda (words) (search words log))
        '("Rerun to get" "Please rerun" "Label(s) may have changed" "Rerun LaTeX")))

;;; Bibliographies and indexes

(defun bibliography-directories (engine)
  "The directories the programs making a bibliography and index, of the TeX the
LaTeX program at ENGINE belongs to, read shut in, besides the directory of the
LaTeX file: those of *EXPORT-TRUSTED-DIRECTORIES* and TeX's own (TEX-TREES)."
  (append (trusted-export-directories) (tex-trees engine)))

(defun document-file (name base directories)
  "The full name of the file NAME, as a document or a program working for it
names it, taken from BASE (the directory of the LaTeX file) unless it is a
full name (ORG-EXPORT-PATH); NIL when no such file is there, or when it is
outside BASE and DIRECTORIES (EXPORT-DIRECTORY-P): a file a program shut in
for the document could not read is never read for it either."
  (let ((path (ignoring-errors-reported ("expanding ~a" name) (org-export-path name base))))
    (and path (eq :regular (tuatara.io:path-kind path)) (export-directory-p path base directories) path)))

(defun tex-files-found (engine tex names format directory)
  "The full names of the files NAMES (with their types: refs.bib, plain.bst) as
the TeX the LaTeX program at ENGINE finds them for TEX, by its kpsewhich with
-format=FORMAT (bib, bst), each (NAME . PATH), PATH NIL for one not found.
Asked shut in by the sandbox, shown what the programs making the
bibliography are (BIBLIOGRAPHY-DIRECTORIES), in DIRECTORY, for ten seconds at
most, so that only a file they could read is found. :UNKNOWN when it cannot be
asked: no kpsewhich, no sandbox, or it failed."
  (let ((kpsewhich (tex-program-beside engine "kpsewhich")))
    (multiple-value-bind (output outcome)
        (if (and kpsewhich names)
            (tuatara.io:sandbox-run kpsewhich (cons (format nil "-format=~a" format) names)
                                    :seconds 10 :ended-well '(0 1) :directory directory
                                    :cancel (and tuatara.runtime:*job*
                                                 (tuatara.runtime:job-cancel tuatara.runtime:*job*))
                                    :directories (cons (tuatara.io:parent-path tex) (bibliography-directories engine))
                                    :environment (latex-sandbox-environment kpsewhich tex))
            (values nil (if names :unavailable :done)))
      (if (eq outcome :done)
          (let ((found (and output (org-text-lines (tuatara.text:lenient-string output)))))
            (mapcar (lambda (name)
                      (cons name (find (tuatara.io:native-file-name name) found
                                       :key #'tuatara.io:native-file-name :test #'string=)))
                    names))
          :unknown))))

(defun inputs-sum (engine tex directory text files)
  "What a program making the bibliography or index of TEX reads, as one
text to keep and compare: the SHA-256 sums of TEXT, its part of a working file,
and of each of FILES, each (NAME . FORMAT) as the working file names it (the
TeX of the LaTeX program at ENGINE finds those not in the directory of TEX,
TEX-FILES-FOUND, in DIRECTORY). NIL when one of them cannot be told, so that
the program is run: the sums then say nothing of whether it has anything new."
  (let* ((dir (tuatara.io:parent-path tex))
         (dirs (bibliography-directories engine))
         (local (mapcar (lambda (file) (document-file (car file) dir dirs)) files))
         (sums (list (octets-sha256 (tuatara.text:string-octets text)))))
    (loop for format in (remove-duplicates (mapcar #'cdr files) :test #'string=)
          for wanted = (loop for file in files for path in local
                             when (and (null path) (string= format (cdr file))) collect (car file))
          for found = (tex-files-found engine tex wanted format directory)
          do (when (eq found :unknown) (return-from inputs-sum nil))
          (tuatara.runtime:job-check-stop)
          (loop for (nil . path) in found
                do (push (or (and path (file-sha256 path)) "-") sums)))
    (dolist (path local)
      (when path (push (or (file-sha256 path) "-") sums)))
    (format nil "~{~a~^ ~}" (nreverse sums))))

(defun bibtex-lines (directory aux)
  "The lines of the .aux file AUX in DIRECTORY that bibtex reads, \\citation,
\\bibdata and \\bibstyle, with those of the .aux files it takes in
\(\\@input, one for each file \\include takes in), as one text."
  (with-output-to-string (out)
    (labels ((read-aux (path depth)
               (dolist (line (org-text-lines (or (export-file-text path) "")))
                 (cond ((some (lambda (start) (uiop:string-prefix-p start line))
                              '("\\citation{" "\\bibdata{" "\\bibstyle{"))
                        (write-line line out))
                       ((and (< depth 2) (uiop:string-prefix-p "\\@input{" line))
                        (let ((name (string-right-trim "}" (subseq line 8))))
                          (unless (or (find #\/ name) (uiop:string-prefix-p "." name))
                            (read-aux (tuatara.io:path-join directory name) (1+ depth)))))))))
      (read-aux aux 0))))

(defun with-type (name type)
  "NAME, a file a working file names, ending in .TYPE, which is added when it does not."
  (if (uiop:string-suffix-p name (concatenate 'string "." type)) name (format nil "~a.~a" name type)))

(defun latex-helpers (tex engine directory stem)
  "The programs the last LaTeX run on TEX (its file STEM in DIRECTORY, by the
LaTeX program at ENGINE) left work for, each a plist: biber when the
run wrote a .bcf, else bibtex when its .aux names a bibliography (\\bibdata),
and makeindex when it wrote an .idx. :NAME is the program's, :MAKES what it
makes in words, :ARGUMENTS what it is given; :SUM what it reads, its part of
the working file and the bibliographies and styles that names (INPUTS-SUM);
:MADE the type of the file it makes and :LOG that of the file it says what
went wrong in; :ENDED-WELL its exit codes for a run that went well; :SCRATCH
true for one that keeps files of its own between runs (biber unpacks itself)."
  (flet ((file (type) (tuatara.io:path-join directory (format nil "~a.~a" stem type)))
         (names (regex text &optional (type "bib"))
           (let ((names '()))
             (cl-ppcre:do-register-groups (list) (regex text)
                                          (dolist (name (uiop:split-string list :separator ","))
                                            (let ((name (string-trim " " name)))
                                              (when (plusp (length name))
                                                (push (cons (with-type name type) type) names)))))
             (nreverse names))))
    (let ((bcf (export-file-text (file "bcf")))
          (bibtex (bibtex-lines directory (file "aux")))
          (idx (file "idx")))
      (append (cond (bcf
                     ;; its full name: given a name alone, biber may take one beside the document
                     (list (list :name "biber" :makes "bibliography" :made "bbl" :log "blg" :ended-well '(0) :scratch t
                                 :arguments (list (file "bcf"))
                                 :sum (inputs-sum engine tex directory bcf
                                                  (names "<bcf:datasource[^>]*>([^<]+)</bcf:datasource>" bcf)))))
                    ((search "\\bibdata{" bibtex)
                     (list (list :name "bibtex" :makes "bibliography" :made "bbl" :log "blg" :ended-well '(0 1)
                                 :arguments (list (format nil "./~a" stem))
                                 :sum (inputs-sum engine tex directory bibtex
                                                  (append (names "\\\\bibdata\\{([^}]*)\\}" bibtex)
                                                          (names "\\\\bibstyle\\{([^}]*)\\}" bibtex "bst")))))))
              (and (eq :regular (tuatara.io:path-kind idx))
                   (list (list :name "makeindex" :makes "index" :made "ind" :log "ilg" :ended-well '(0)
                               :arguments (list (format nil "./~a.idx" stem))
                               :sum (inputs-sum engine tex directory (or (export-file-text idx) "") '()))))))))

(defun helper-problems (name log base directories)
  "What the program NAME (biber, bibtex or makeindex) wrote in LOG, its log's
text, of what went wrong, in order, at most *LATEX-ERROR-LINES* of them: each
a plist of its :MESSAGE, and when it names a bibliography and a line of it,
the :PATH of that file and the :LINE, for a file in BASE (the directory of the
LaTeX file, from which a name is taken) or DIRECTORIES."
  (let ((lines (coerce (uiop:split-string log :separator '(#\Newline)) 'vector))
        (sources '())
        (found '()))
    (flet ((put (message &optional file line)
             (let ((path (and file (uiop:string-suffix-p file ".bib") (document-file file base directories))))
               (push (list* :message (string-trim " " message) (and path (list :path path :line line))) found))))
      (loop for i from 0 below (length lines)
            for line = (aref lines i)
            for before = (if (plusp i) (aref lines (1- i)) "")
            do (cond ((string= name "biber")
                      ;; biber says each bibliography it reads by its full name, then reads a copy of it made in
                      ;; a directory of its own: its errors name the copy, taken back to the last read of that name
                      (cl-ppcre:register-groups-bind (source) ("Found BibTeX data source '([^']+)'" line)
                                                     (push source sources))
                      (let ((at (search "ERROR - " line)))
                        (when at
                          (let ((message (cl-ppcre:regex-replace-all "\\S*/([^/\\s]+\\.bib)_[0-9]+\\.utf8"
                                                                     (subseq line (+ at 8)) "\\1")))
                            (multiple-value-bind (match parts)
                                (cl-ppcre:scan-to-strings "([^\\s,]+\\.bib), line ([0-9]+)" message)
                              (put message
                                   (and match (or (find (aref parts 0) sources :key #'tuatara.io:native-file-name
                                                        :test #'string=)
                                                  (aref parts 0)))
                                   (and match (parse-integer (aref parts 1)))))))))
                     ((string= name "bibtex")
                      (multiple-value-bind (match parts)
                          (cl-ppcre:scan-to-strings "^(.*)---line ([0-9]+) of file (.*)$" line)
                        (if match
                            (put (if (plusp (length (aref parts 0))) (aref parts 0) before)
                                 (aref parts 2) (parse-integer (aref parts 1)))
                            (multiple-value-bind (match parts)
                                (cl-ppcre:scan-to-strings "^(.*)---while reading file (.*)$" line)
                              (when match
                                (put (format nil "~a (~a)" (if (plusp (length (aref parts 0))) (aref parts 0) before)
                                             (aref parts 1))))))))
                     ((uiop:string-prefix-p "!! " line)
                      (put (format nil "~a~@[: ~a~]" (subseq line 3)
                                   (and (< (1+ i) (length lines))
                                        (let ((next (string-trim " -" (aref lines (1+ i)))))
                                          (and (plusp (length next)) next)))))))))
    (last (nreverse found) *latex-error-lines*)))

(defun recorded-sums (record)
  "What RECORD, the file kept beside a PDF's working directory, says each program
making its bibliography or index last read when it went well, an alist of
\(NAME . SUM)."
  (loop for line in (org-text-lines (or (export-file-text record) ""))
        for at = (position #\Space line)
        when at collect (cons (subseq line 0 at) (subseq line (1+ at)))))

(define-record (helper-runs (:constructor make-helper-runs ()) (:copier nil))
  "What the programs making a PDF's bibliography and index did for one try at
making it (RUN-LATEX): TRIED, each (NAME . SUM) run; FAILURES, the last
failure of each that has not gone well since, (NAME . PROBLEMS) as
HELPER-PROBLEMS gives them, in order."
  (tried '() :documentation "The programs a HELPER-RUNS has run, each (NAME . SUM).")
  (failures '()
            :documentation "The last failure of each program of a HELPER-RUNS not
gone well since, each (NAME . PROBLEMS)."))

(defun made-from-file (directory &optional (type "made-from"))
  "The file of the editor's own beside the working directory DIRECTORY of a PDF,
named after it with TYPE, out of sight of the programs shut in there:
made-from, what its bibliography and index were last made from; par, the
directory biber keeps itself unpacked in."
  (let ((build (string-right-trim "/" directory)))
    (tuatara.io:path-join (tuatara.io:parent-path build)
                          (concatenate 'string (tuatara.io:native-file-name build) "." type))))

(defun make-bibliography-and-index (job tex engine directory stem shut-in runs)
  "Run the programs the last LaTeX run on TEX (by the LaTeX program at ENGINE,
in DIRECTORY, its file STEM there) left work for (LATEX-HELPERS), as JOB, each
of the TeX ENGINE belongs to (TEX-PROGRAM-BESIDE), as the LaTeX program runs
\(RUN-TEX-PROGRAM, shut in with SHUT-IN), in DIRECTORY. Each is run only when
what it reads has changed since it last went well there (kept in a file
beside DIRECTORY, MADE-FROM-FILE), cannot be told, or what it makes is not
there; and not when it has run on the same already in this try (RUNS, a
HELPER-RUNS, which keeps what went wrong). A program not installed is said in
JOB's notes. True when any ran."
  (let* ((record (made-from-file directory))
         (sums (recorded-sums record))
         (dir (tuatara.io:parent-path tex))
         (ran nil))
    (dolist (helper (latex-helpers tex engine directory stem))
      (destructuring-bind (&key name makes arguments sum made log ended-well scratch) helper
        (let ((made (tuatara.io:path-join directory (format nil "~a.~a" stem made))))
          (unless (or (member (cons name sum) (helper-runs-tried runs) :test #'equal)
                      (and sum (equal sum (cdr (assoc name sums :test #'string=)))
                           (eq :regular (tuatara.io:path-kind made))))
            (let ((program (tex-program-beside engine name)))
              (if (null program)
                  (pushnew (format nil "~a is not installed, so the ~a is not made" name makes) (pdf-export-notes job)
                           :test #'string=)
                  (progn
                    (push (cons name sum) (helper-runs-tried runs))
                    (setf (pdf-export-step job) (format nil "running ~a" name) ran t)
                    (multiple-value-bind (ended how)
                        (run-tex-program job tex program arguments directory shut-in
                                         :ended-well ended-well
                                         :scratch (and scratch
                                                       (tuatara.io:ensure-private-directory
                                                        (concatenate 'string (made-from-file directory "par") "/"))))
                      (let ((problems (helper-problems
                                       name
                                       (latex-log-text (tuatara.io:path-join directory (format nil "~a.~a" stem log)))
                                       dir (bibliography-directories engine))))
                        (setf (helper-runs-failures runs) (remove name (helper-runs-failures runs)
                                                                  :key #'car :test #'string=))
                        (cond ((not (and ended (null problems)))
                               (setf (helper-runs-failures runs)
                                     (append (helper-runs-failures runs)
                                             (list (cons name (or problems (list (list :message how))))))))
                              (sum
                               (setf sums (acons name sum (remove name sums :key #'car :test #'string=)))
                               (tuatara.io:replace-file record (format nil "~:{~a ~a~%~}"
                                                                       (mapcar (lambda (entry)
                                                                                 (list (car entry) (cdr entry)))
                                                                               sums))))))))))))))
    ran))

(defun latex-program-place (path)
  "The LaTeX program at PATH named by where its directory really is, every link
to the directory followed, under the name it was found by (xelatex, which may
itself be a link to another program that reads its name): so that it, its
directory and the directories of TeX kpsewhich names agree. PATH as it is when its
directory is not there."
  (let ((directory (ignoring-errors-reported ("finding where ~a is" path)
                                             (tuatara.io:real-path (tuatara.io:parent-path path)))))
    (if directory (tuatara.io:path-join directory (tuatara.io:native-file-name path)) path)))

(defun run-latex (job tex engine path)
  "Make a PDF of the LaTeX file TEX with the program ENGINE at PATH (by where
its directory really is, LATEX-PROGRAM-PLACE), for JOB, running it until its
cross-references settle, at most *LATEX-RUNS* times, in LATEX-BUILD-DIRECTORY,
where its working files are kept; after each run the bibliography and the index
it left to make are made (MAKE-BIBLIOGRAPHY-AND-INDEX), and it is run again when
they were, once past the most when they were made after the last. The files TEX names \(its pictures, files it
takes in) are found in TEX's directory, which it may not write in, so that a
document cannot change the files beside it. Tried once more without them when it
fails with some there, since those of an earlier version can trip it. Returns
the PDF made, in that directory, or NIL and what went wrong, as LATEX-ERRORS
gives it; as a third value, with the PDF, what the programs making its
bibliography and index said went wrong in the try that made it, each
\(PROGRAM . PROBLEMS). The program is shut in (RUN-SHUT-IN): what a document says, in LaTeX
or in Lua, can read only the directories it is shown and write only in that
directory, with no network. Where there is no sandbox (bwrap missing, or turned
off by *sandbox*), it is run not shut in only when you have said yes
\(LATEX-UNCONFINED-ALLOWED-P), and the export says so when it is done
\(LATEX-UNCONFINED-WORDS): it then has your rights, but for TeX's own limits
\(no programs run, nothing written outside that directory)."
  (let* ((path (latex-program-place path))
         (directory (latex-build-directory tex engine))
         (stem (tuatara.io:native-file-stem tex))
         (log (tuatara.io:path-join directory (concatenate 'string stem ".log")))
         (pdf (tuatara.io:path-join directory (concatenate 'string stem ".pdf")))
         (shut-in (multiple-value-bind (status detail) (tuatara.io:sandbox-status)
                    (or (eq status :ready)
                        (progn (pushnew (latex-unconfined-words detail) (pdf-export-notes job) :test #'string=)
                               nil))))
         ;; -synctex=-1: where each line went on the pages, unpacked, for the PDF's view (LATEX-SYNCTEX-FILE)
         (arguments (list "-interaction=nonstopmode" "-halt-on-error" "-file-line-error" "-no-shell-escape"
                          "-synctex=-1"
                          (concatenate 'string "-output-directory=" directory)
                          ;; its full name, which a name beginning with - cannot be taken for an option in
                          tex)))
    (flet ((attempt (&aux (runs (make-helper-runs)) (last *latex-runs*))
             ;; what says which bibliography and index to make counts only when this try writes it
             (dolist (file (list pdf (tuatara.io:path-join directory (concatenate 'string stem ".bcf"))
                                 (tuatara.io:path-join directory (concatenate 'string stem ".idx"))))
               (ignoring-errors-reported ("deleting ~a" file) (tuatara.io:remove-file-if-there file)))
             (loop for run from 1
                   while (<= run last)
                   do (setf (pdf-export-step job) (format nil "running ~a~:[~*~; (~:r time)~]" engine (> run 1) run))
                   (let ((before (cross-references directory stem)))
                     (multiple-value-bind (ended how) (run-tex-program job tex path arguments directory shut-in)
                       (let ((said (latex-log-text log)))
                         (unless (and ended (tuatara.io:path-stamp pdf))
                           (return (values nil (or (latex-errors said)
                                                   (let ((how (or how (format nil "~a made no PDF" engine))))
                                                     (list (list :message (format nil "~a~:[ and wrote no log~;~]"
                                                                                  how (plusp (length said))))))))))
                         (let ((helped (make-bibliography-and-index job tex path directory stem shut-in runs)))
                           ;; what they made is taken in by one more run, past the most if need be
                           (when (and helped (= run last *latex-runs*))
                             (incf last))
                           (unless (or helped (rerun-asked-p said)
                                       (not (equalp before (cross-references directory stem))))
                             (return (values pdf nil (helper-runs-failures runs))))))))
                   finally (return (values pdf nil (helper-runs-failures runs))))))
      (let ((had-files (some #'identity (cross-references directory stem))))
        (multiple-value-bind (made why failures) (attempt)
          (if (or made (not had-files))
              (values made why failures)
              (progn (dolist (entry (ignoring-errors-reported ("listing ~a" directory)
                                                              (tuatara.io:directory-entries directory)))
                       (let ((path (tuatara.io:path-join directory (first entry))))
                         (ignoring-errors-reported ("deleting ~a" path)
                                                   (tuatara.io:delete-tree path))))
                     (attempt))))))))

(defun put-pdf (made target)
  "Put the PDF MADE at TARGET, in place of what is there in one step: a view
of TARGET reads either the old PDF or the new, never half of one."
  (let ((temporary (tuatara.io:scratch-name (tuatara.io:parent-path target) "pdf")))
    (unless (tuatara.io:copy-regular-file made temporary)
      (ignoring-errors-reported ("deleting ~a" temporary) (tuatara.io:remove-file temporary))
      (refuse "~a could not be written" target))
    (handler-case (tuatara.io:rename-path temporary target :replace t)
      (error (e)
        (ignoring-errors-reported ("deleting ~a" temporary) (tuatara.io:remove-file temporary))
        (error e)))))

(defun text-line-starts (text)
  "Where each line of TEXT begins, a vector of positions, the first 0."
  (let ((starts (make-array 64 :adjustable t :fill-pointer 0)))
    (vector-push-extend 0 starts)
    (loop for at = (position #\Newline text) then (position #\Newline text :start (1+ at))
          while at
          do (vector-push-extend (1+ at) starts))
    starts))

(defun line-index-at (starts position)
  "Which line, from 0, of those beginning at STARTS (TEXT-LINE-STARTS) holds
POSITION: found by halves."
  (let ((low 0) (high (length starts)))
    (loop while (< (1+ low) high)
          do (let ((middle (floor (+ low high) 2)))
               (if (<= (aref starts middle) position) (setf low middle) (setf high middle))))
    low))

(defun error-place (job problem)
  "Where PROBLEM, one of LATEX-ERRORS, is in what JOB was asked to make a PDF
of, as two values, the file and the line. For an Org file it is the line its
LaTeX came from (its MAP: the element's first line, counted on as far as the
error is past the element's first line in the LaTeX, never past its last), or
line 1 for the preamble. For a LaTeX file, its own; for a problem of a
bibliography's, its :PATH (HELPER-PROBLEMS); NIL for one in another file, or
with no line."
  (let* ((line (getf problem :line))
         (file (getf problem :file))
         (tex (pdf-export-tex job))
         (ours (and line tex (or (null file)
                                 (string= (tuatara.io:native-file-name file) (tuatara.io:native-file-name tex))))))
    (cond ((getf problem :path) (values (getf problem :path) line))
          ((not ours) nil)
          ((and (eq (pdf-export-kind job) :org) (pdf-export-map job))
           (let* ((map (pdf-export-map job))
                  (entry (find-if (lambda (e) (<= (first e) line)) map :from-end t))
                  (text (pdf-export-text job)))
             (flet ((line-at (position)
                      (1+ (line-index-at (or (pdf-export-line-starts job)
                                             (setf (pdf-export-line-starts job) (text-line-starts text)))
                                         position))))
               (let ((at (if entry
                             ;; an element's lines go to LaTeX one for one, as near as can be told: counted on, within
                             ;; it
                             (destructuring-bind (marked begin end) entry
                               (min (line-at end) (+ (line-at begin) (- line marked))))
                             1))
                     (origins (pdf-export-origins job)))
                 ;; a part exported alone: the line of the file its line comes from
                 (values (pdf-export-source job)
                         (if origins (aref origins (min (1- (length origins)) (max 0 (1- at)))) at))))))
          (t (values tex line)))))

(defun missing-characters-words (job)
  "What to say of what JOB made besides that it is written: the characters no
font here has, in words with the place of each in the file written
\(ERROR-PLACE), and its notes (a picture not shown); NIL when there is none."
  (let ((missing (pdf-export-missing job))
        (notes (pdf-export-notes job)))
    (when (or missing notes)
      (format nil "~:[~*~;no font here has ~{~a~^, ~}~]~:[~;; ~]~{~a~^; ~}" missing
              (loop for (c . line) in missing
                    collect (multiple-value-bind (file at) (error-place job (list :line line))
                              (format nil "~c (U+~4,'0x~@[, ~a~]~@[:~d~])" c (char-code c)
                                      (and file (tuatara.io:native-file-name file)) (and file at))))
              (and missing notes) notes))))

(defun error-words (job problem)
  "PROBLEM, one of LATEX-ERRORS, in a line: where it is (ERROR-PLACE), then its
message; only where it is while a secret is at hand, the message quoting the
text."
  (multiple-value-bind (file line) (error-place job problem)
    (if *secret-at-hand*
        (format nil "~:[a place not told~;~:*~a~]~@[:~d~]"
                (and file (tuatara.io:native-file-name file)) (and file line))
        (format nil "~@[~a~]~@[:~d~]~:[~;: ~]~a" (and file (tuatara.io:native-file-name file)) (and file line) file
                (getf problem :message)))))

(defun export-work (job)
  "What JOB's thread does: wait for the save of its buffer, write an Org
buffer as LaTeX, and make the PDF of the LaTeX with each of the programs for
it in turn until one does, put beside the LaTeX file."
  (let ((save (pdf-export-save job)))
    (when save
      (setf (pdf-export-step job) "waiting for the save")
      (sb-thread:join-thread (tuatara.io:save-job-thread save) :default nil)
      (unless (eq :done (tuatara.io:save-job-status save))
        (refuse "~a was not saved" (export-name job)))))
  (let* ((tex (if (eq (pdf-export-kind job) :org)
                  (org-to-latex job (or (car (first (latex-engines-found))) "xelatex"))
                  (pdf-export-source job)))
         (target (replaced-type tex "pdf")))
    (setf (pdf-export-tex job) tex)
    (when (eq (pdf-export-then job) :latex-only)
      (return-from export-work))
    (dolist (engine (engines-for tex))
      (multiple-value-bind (made errors failures) (run-latex job tex (car engine) (cdr engine))
        (when made
          (setf (pdf-export-errors job) (append (pdf-export-errors job) failures))
          (setf (pdf-export-step job) "writing the PDF")
          (put-pdf made target)
          (setf (pdf-export-pdf job) target (pdf-export-engine job) (car engine))
          (return-from export-work))
        (push (car engine) (pdf-export-failed job))
        (setf (pdf-export-errors job)
              (append (pdf-export-errors job) (list (cons (car engine) errors))))))
    ;; the first problem the first program met, where it is in what was written
    (let ((first (first (cdr (assoc (car (last (pdf-export-failed job))) (pdf-export-errors job) :test #'string=)))))
      (refuse "~{~a~^ and ~} failed~@[ (~a)~]; *LaTeX errors* has what they said"
              (reverse (pdf-export-failed job)) (and first (error-words job first))))))

(defun helpers-failed (job)
  "The names of the programs making JOB's bibliography or index that failed, in order."
  (loop for (program) in (pdf-export-errors job)
        unless (member program (pdf-export-failed job) :test #'string=)
        collect program))

(defun show-latex-errors (job)
  "Show what the programs of JOB that failed said, in *LaTeX errors*, a list
of the problems under the name of the program that met them: each where it
is in the file written (ERROR-PLACE), with the line's text, then the lines
LaTeX printed with it; JOB's notes (a program not installed) above them. The
list is made the list of places \\[next-place] and \\[previous-place] go
through, each problem with a place one of them."
  (let ((org-lines (or (pdf-export-lines job)
                       (and (pdf-export-text job) (uiop:split-string (pdf-export-text job) :separator '(#\Newline))))))
    (multiple-value-bind (pieces entries)
        (entry-lines
         (lambda (put)
           (loop for (engine . errors) in (pdf-export-errors job)
                 do (funcall put "")
                 (funcall put (format nil "~a failed:" engine) :heading (list :engine engine))
                 (dolist (problem errors)
                   (multiple-value-bind (file line) (error-place job problem)
                     ;; one entry, the problem, on each of its lines
                     (let ((entry (list :error file line)))
                       (funcall put
                                (format nil
                                        "  ~@[~a: ~]~a"
                                        (and file (format nil "~a:~d" (tuatara.io:native-file-name file) line))
                                        (getf problem :message))
                                nil entry)
                       ;; the Org line it came from, as written
                       (when (and file (eq (pdf-export-kind job) :org) (equal file (pdf-export-source job)))
                         (funcall put (format nil "    | ~a" (or (nth (1- line) org-lines) "")) nil entry))
                       (dolist (context (getf problem :context))
                         (funcall put (format nil "    ~a" context) nil entry))))))))
      (let ((list (set-search-filtering (buffer-in-mode "*LaTeX errors*" 'latex-errors-mode)
                                        :nouns "errors" :heading (lambda (entry) (eq (first entry) :engine)))))
        (render-pieces list (cons (format nil "Making a PDF of ~a~@[, written as LaTeX in ~a~]~%~{~a~%~}"
                                          (abbreviated-path (pdf-export-source job))
                                          (and (eq (pdf-export-kind job) :org) (pdf-export-tex job)
                                               (abbreviated-path (pdf-export-tex job)))
                                          (pdf-export-notes job))
                                  pieces)
                       :entries entries :first-line (1+ (length (pdf-export-notes job))) :point :start)
        (use-places (entry-places list))
        (display-buffer list)))))

(defun show-exported-pdf (pdf)
  "Show PDF in its view where the rule for :exported buffers puts it (beside,
or alone on a narrow screen), the focus staying where it is."
  (pdf-view-visit pdf (lambda (buffer)
                        (setf (buffer-kind buffer) :exported)
                        (display-buffer buffer))))

(defmethod tuatara.runtime:job-take-in ((job pdf-export))
  "Say how the export went: the LaTeX written for an export to LaTeX alone;
else read again the views of the PDF open, show or open the PDF as asked,
show what the programs said when none made it."
  (let ((pdf (pdf-export-pdf job)))
    ;; an export to LaTeX alone is done once the LaTeX is written
    (when (and (eq (pdf-export-status job) :done) (eq (pdf-export-then job) :latex-only))
      (message "Wrote ~a~@[; ~a~]" (abbreviated-path (pdf-export-tex job)) (missing-characters-words job))
      (return-from tuatara.runtime:job-take-in))
    (case (pdf-export-status job)
      (:done
       (dolist (buffer (copy-list *pdf-views*))
         (let ((view (pdf-view-of buffer)))
           (when (and view (equal (pdf-view-path view) pdf))
             (pdf-view-reload buffer))))
       (let ((helpers (helpers-failed job)))
         (message "Wrote ~a (~a~@[; ~{~a~^ and ~} failed~]) in ~,1f s~@[; ~a~]~@[; ~{~a~^ and ~} failed: *LaTeX ~
errors* has what ~[~;it~:;they~] said~]"
                  (abbreviated-path pdf)
                  (pdf-export-engine job)
                  (reverse (pdf-export-failed job))
                  (tuatara.runtime:seconds-since (pdf-export-started job))
                  (missing-characters-words job)
                  helpers (length helpers))
         (case (pdf-export-then job)
           (:view (show-exported-pdf pdf))
           (:outside (open-externally-file pdf)))
         (when (pdf-export-after job)
           (funcall (pdf-export-after job) job))
         (when helpers
           (show-latex-errors job))))
      (:stopped (message "~a" (tuatara.runtime:job-ending-words job)))
      (:failed
       (message "~a" (tuatara.runtime:job-ending-words job))
       (when (pdf-export-errors job)
         (show-latex-errors job))))))

;;; The commands

(defun export-kind (buffer)
  "What BUFFER's file is to be exported as, by its major mode: :ORG or :LATEX; NIL for neither."
  (cond ((buffer-mode-named-p buffer 'org-mode) :org)
        ((buffer-mode-named-p buffer 'latex-mode) :latex)))

(defparameter *enhanced-commands*
  '((:file . org-export-latex-pdf-enhanced) (:view . org-export-latex-pdf-enhanced-buffer)
    (:outside . org-export-latex-pdf-enhanced-xdg) (:latex-only . org-export-latex-enhanced))
  "The enhanced export's command for each THEN of START-LATEX-EXPORT.")

(defun note-directory (buffer)
  "The directory of BUFFER's file (BUFFER-DIRECTORY), or NIL for a buffer of no file."
  (and (tuatara.buffer:buffer-file buffer) (buffer-directory buffer)))

(defun only-enhanced-macro (text &optional directory)
  "The name of a macro of TEXT, an Org document, that only the enhanced export
knows, redact or protect (*ORG-HIDING-MACROS*), used with no #+MACRO: line
for it, its setup files' read too (DIRECTORY the note's directory), as written;
NIL when none."
  (let ((defined nil) (read nil))
    (map-org-hiding-calls (lambda (start end name from to)
                            (declare (ignore start end from to))
                            (unless read
                              (setf defined (with-org-note-keywords (text directory) (org-macro-definitions)) read t))
                            (unless (assoc name defined :test #'string-equal)
                              (return-from only-enhanced-macro name)))
                          text)
    nil))

(defun org-export-part (buffer scope)
  "The part of BUFFER, an Org buffer, to export alone for SCOPE, :SELECTION
\(the lines the selection is on) or :SUBTREE (the subtree at point, its headline
the title, its EXPORT_TITLE and EXPORT_FILE_NAME properties used), as four
values: its text, the document's keyword lines put before it, as whole lines of
the file. The line of the file each of its lines comes from, a vector. The
file's lines. And words for it (the subtree \"Findings\"). NIL when there is no
such part (no headline at point)."
  (let ((parse (org-buffer-parse buffer)))
    (let* ((tree (org-parse-tree parse))
           (text (org-parse-text parse))
           (lines (coerce (uiop:split-string text :separator '(#\Newline)) 'vector))
           (starts (text-line-starts text))
           (line-of (lambda (position) (line-index-at starts position)))
           ;; at the very end of the text, its last character's headline
           (point (min (org-parse-point parse) (max 0 (1- (length text)))))
           (headline (and (eq scope :subtree) (org-current-headline tree point)))
           (range (ecase scope
                    (:selection (multiple-value-bind (start end) (selection-range)
                                  (and start (cons (org-parse-char parse start)
                                                   (org-parse-char parse (max start (1- end)))))))
                    (:subtree (and headline
                                   (let ((begin (org-node-begin headline)))
                                     (cons begin (max begin (1- (- (org-node-end headline)
                                                                   (org-node-post-blank headline)))))))))))
      (when range
        (let* ((first (funcall line-of (car range)))
               (last (max first (funcall line-of (cdr range))))
               (kept '()) (origins '()))
          (flet ((keep (line text) (push text kept) (push (1+ line) origins)))
            ;; the document's keywords, but for its title when the subtree gives one
            (org-walk-tree tree (lambda (node)
                                  (when (and (eq (org-node-type node) 'keyword)
                                             (not (and headline (string-equal (org-property node :key) "TITLE")))
                                             (not (<= first (funcall line-of (org-node-begin node)) last)))
                                    (let ((line (funcall line-of (org-node-begin node))))
                                      (keep line (aref lines line))))))
            (when headline
              (let ((line (funcall line-of (org-node-begin headline))))
                (keep line (format nil "#+title: ~a" (or (org-headline-property headline "EXPORT_TITLE")
                                                         (org-property headline :raw-value))))
                (let ((name (org-headline-property headline "EXPORT_FILE_NAME")))
                  (when name (keep line (format nil "#+export_file_name: ~a" name))))))
            (loop for line from first to last do (keep line (aref lines line))))
          (values (format nil "~{~a~%~}" (reverse kept))
                  (coerce (reverse origins) 'simple-vector)
                  lines
                  (if headline
                      (format nil "the subtree \"~a\"" (org-property headline :raw-value))
                      "the selection")))))))

(defun export-refused-p (buffer text &key wrong-kind running redactions)
  "True, having said why, when BUFFER, whose text is TEXT, is not to be
exported now. Looked at in turn: opened safely; WRONG-KIND, the words to say
when it is not of a kind the export takes; no file yet; RUNNING, the words
to say when an export of it is running already; and with REDACTIONS, a
hidden text shown elsewhere in it, unless you say to go on."
  (let ((name (tuatara.buffer:buffer-name buffer)))
    ;; a file opened safely has no major mode: said as what it is, before its kind
    (cond ((tuatara.io:safe-visit-p buffer)
           (message "~a was opened safely: exporting would run what it says (~a)" name (end-safe-visit-words buffer)))
          (wrong-kind (message "~a" wrong-kind))
          ((null (tuatara.buffer:buffer-file buffer)) (message "~a has no file yet: save it first" name))
          (running (message "~a" running))
          ;; a hidden text shown elsewhere in the file: listed, and asked about, before anything is made
          ((and redactions (not (redactions-allow-export-p buffer text)))
           (message "Not exported: *Redactions* lists where hidden text shows"))
          (t (return-from export-refused-p nil)))
    t))

(defun export-target-refused-p (buffer types text)
  "True, having said why, when exporting BUFFER, or TEXT, the part of it
exported, may not write its files of TYPES (EXPORT-TARGET-ALLOWED-P)."
  (multiple-value-bind (allowed why) (export-target-allowed-p buffer types :text text)
    (unless allowed
      (message "Not exported~@[: ~a~]" why)
      t)))

(defun start-latex-export (then &key enhanced after)
  "Start making a PDF of the current buffer's Org text, or of its LaTeX file,
saved first when it has changes, and then do THEN with it (PDF-EXPORT), with
ENHANCED Tuatara's own improvements on Org's LaTeX (Org files only), and call
AFTER, when given, with the job once the PDF is made; or say why not."
  (let* ((buffer (current-buffer))
         (name (tuatara.buffer:buffer-name buffer))
         (file (tuatara.buffer:buffer-file buffer))
         (kind (export-kind buffer))
         (text (and kind (tuatara.buffer:buffer-string buffer)))
         (running (find-if #'pdf-export-p (jobs-of buffer :running t))))
    (cond ((export-refused-p buffer text
                             :wrong-kind (and (null kind)
                                              (format nil "~a is neither an Org nor a LaTeX file: nothing to export"
                                                      name))
                             :running (and running (format nil "~a is being ~(~a~) already (~a)" name
                                                           (if (eq kind :latex) "compiled" "exported")
                                                           (pdf-export-step running)))
                             :redactions (or (eq kind :latex) enhanced)))
          ;; names to hide, which the export as Org makes it would refuse in the background: said at once
          ((and (eq kind :org) (not enhanced)
                (let ((macro (only-enhanced-macro text (note-directory buffer))))
                  (when macro
                    (message "~a hides text with {{{~a(...)}}}, which only the enhanced export does: M-x ~(~a~)"
                             name macro (cdr (assoc then *enhanced-commands*)))
                    t))))
          ((and (not (eq then :latex-only)) (null (latex-engines-found)))
           (message "No program makes PDFs here: none of ~{~a~^, ~} was found on the PATH or in TeX Live's ~
directories (install TeX Live, or give their paths in *latex-engines*)"
                    (latex-engine-names)))
          ((and (not (eq then :latex-only)) (not (latex-unconfined-allowed-p)))
           (message "Not exported: LaTeX runs only shut in the sandbox unless you say yes"))
          (t
           ;; an Org buffer is written as LaTeX as it is; a LaTeX file is read from disk, so saved first
           (when (and (eq kind :latex) (tuatara.buffer:buffer-modified-p buffer))
             (save-buffer-with-care buffer))
           (let ((save (and (eq kind :latex) (tuatara.io:save-of buffer))))
             (if (and (eq kind :latex) (tuatara.buffer:buffer-modified-p buffer) (null save))
                 (message "Not exported: ~a has changes not saved" name)
                 (let ((scope (and (eq kind :org)
                                   (cond ((region-active-p buffer) :selection)
                                         ((prefix-argument) :subtree)))))
                   ;; a part alone: its text, and where each of its lines is in the file
                   (multiple-value-bind (part origins lines words) (and scope (org-export-part buffer scope))
                     (cond ((and scope (null part)) (message "There is no headline here to export the subtree of"))
                           ;; where the part says it is written: its headline's EXPORT_FILE_NAME too
                           ((export-target-refused-p buffer '("tex" "pdf") (or part text)))
                           (t (let ((job (make-pdf-export buffer (tuatara.io:native (tuatara.io:expand-path file)) kind
                                                          then save (and (eq kind :org)
                                                                         (tuatara.buffer:buffer-text buffer))
                                                          enhanced)))
                                (setf (pdf-export-after job) after)
                                (when part
                                  (setf (pdf-export-rope job) part (pdf-export-origins job) origins
                                        (pdf-export-lines job) (coerce lines 'list) (pdf-export-part job) words))
                                (tuatara.runtime:start-job-thread
                                 job (tuatara.runtime:carry-settings *latex-carried-settings* #'export-work)
                                 :name "tuatara export"
                                 :details (format nil "~(~a~), then ~(~a~)~:[~;, enhanced~]" kind then enhanced))
                                (message "~a ~@[~a of ~]~a to ~a..." (export-doing kind) words name
                                         (if (eq then :latex-only) "LaTeX" "PDF")))))))))))))

(define-command org-export-latex-pdf ()
  "Make the Org file of the current buffer into a PDF beside it, in the
background: the plain export, the note set as standard LaTeX sets it, with
no template or layout of Tuatara's own. The buffer is written as
LaTeX first, as it is in the buffer, beside its file (or where its
#+export_file_name says), with its #+setupfile, #+latex_header, #+options
and the rest; its code blocks are never run. The LaTeX is made into a PDF by
xelatex, or by lualatex when xelatex fails (*latex-engines*), run as often as
its cross-references need. Their working files are kept apart, so the
directory gets only the .tex and the .pdf. When no program makes it,
*LaTeX errors* shows what they said, each error at the line of your file it
comes from, \\<latex-errors-mode>\\[list-visit] there showing that line. A
view of the PDF already open is read again; the PDF is not shown otherwise:
\\[org-export-latex-pdf-buffer] shows it, \\[org-export-latex-pdf-xdg] opens it
in the desktop's program. \\[org-export-latex-pdf-enhanced] makes it with
Tuatara's own improvements. With text selected, only the
lines it is on are exported, under the document's keywords; with
\\[universal-argument] first, only the subtree at point, its headline the
title (or its EXPORT_TITLE property), its EXPORT_FILE_NAME property, when it
has one, naming the file. So for every command making a PDF or LaTeX of an
Org file."
  (start-latex-export :file))

(define-command org-export-latex-pdf-buffer ()
  "Make the Org file of the current buffer into a PDF beside it, as
\\[org-export-latex-pdf] does, and show the PDF: down the right side of the
screen when it is wide enough (*split-width-threshold* columns), else alone
on it. The focus stays in the file, and making it again reads the PDF shown
again, at the same page. Where it shows is the rule for :exported buffers:
\(place-buffers :exported :right-or-alone) as standard."
  (start-latex-export :view))

(define-command org-export-latex-pdf-xdg ()
  "Make the Org file of the current buffer into a PDF beside it, as
\\[org-export-latex-pdf] does, and open the PDF in the desktop's program for
PDFs (xdg-open), or the one *open-externally-programs* gives
\"application/pdf\", asking first unless *open-externally-ask* is NIL."
  (start-latex-export :outside))

(define-command org-export-latex ()
  "Write the Org file of the current buffer as LaTeX beside it (or where its
#+export_file_name says), the plain export with no template or layout of
Tuatara's own, in the background, without making a PDF: the LaTeX file \\[org-export-latex-pdf]
makes its PDF from. No LaTeX program is needed for it."
  (start-latex-export :latex-only))

(define-command org-export-latex-enhanced ()
  "Write the Org file of the current buffer as LaTeX as
\\[org-export-latex] does, with Tuatara's own improvements (its
#+template, tables that fit, redaction): the LaTeX file
\\[org-export-latex-pdf-enhanced] makes its PDF from."
  (start-latex-export :latex-only :enhanced t))

(define-command org-export-latex-pdf-enhanced ()
  "Make the Org file of the current buffer into a PDF beside it, as
\\[org-export-latex-pdf] does, with Tuatara's own improvements: its
#+template, tables that fit and redaction (\\[org-export-latex-enhanced])."
  (start-latex-export :file :enhanced t))

(define-command org-export-latex-pdf-enhanced-buffer ()
  "Make the Org file of the current buffer into a PDF as
\\[org-export-latex-pdf-enhanced] does, and show it as
\\[org-export-latex-pdf-buffer] shows its PDF."
  (start-latex-export :view :enhanced t))

(define-command org-export-latex-pdf-enhanced-xdg ()
  "Make the Org file of the current buffer into a PDF as
\\[org-export-latex-pdf-enhanced] does, and open it in the desktop's program
as \\[org-export-latex-pdf-xdg] opens its PDF."
  (start-latex-export :outside :enhanced t))

(define-command export-latex-compile-pdf ()
  "Make the LaTeX file of the current buffer into a PDF beside it, in the
background, saving it first when it has changes, by xelatex, or by lualatex
when xelatex fails (*latex-engines*), or first by the one its first lines
name (\"% Intended LaTeX compiler: lualatex\", \"%!TEX program = lualatex\"),
run as often as its cross-references need. Their working files are kept
apart, so the directory gets only the .pdf. When no program makes it,
*LaTeX errors* shows what they said, each error at its line,
\\<latex-errors-mode>\\[list-visit] there showing that line. A view of the PDF
already open is read again; the PDF is not shown otherwise:
\\[export-latex-compile-pdf-buffer] shows it, \\[export-latex-compile-pdf-xdg] opens it in the
desktop's program."
  (start-latex-export :file))

(define-command export-latex-compile-pdf-buffer ()
  "Make the LaTeX file of the current buffer into a PDF beside it, as
\\[export-latex-compile-pdf] does, and show the PDF where and as
\\[org-export-latex-pdf-buffer] shows an Org file's."
  (start-latex-export :view))

(define-command export-latex-compile-pdf-xdg ()
  "Make the LaTeX file of the current buffer into a PDF beside it, as
\\[export-latex-compile-pdf] does, and open the PDF in the desktop's program
as \\[org-export-latex-pdf-xdg] opens an Org file's."
  (start-latex-export :outside))

(define-command export-latex-template-copy ()
  "Copy a built-in template of the enhanced export, asked for with completion
\(article, its own look, or org, the plain export's document), into your
own templates, ~/.config/tuatara/latex-templates/NAME.tex, and visit it there
to change: from then on #+template: NAME takes your copy. A copy of yours
already there is visited as it is, never written over. In a template,
{{{KEY}}} is where the value of a note's #+KEY: line goes; the template's
first lines say which it takes. A template of your own for another kind of
document (a report) can start from one of these, kept there or anywhere a
note's #+template: names by its path."
  (copy-chosen-own-file "Copy the export template: "
                        (loop for (name . text) in *built-in-latex-templates*
                              collect (make-candidate name :value (list nil (concatenate 'string name ".tex") text)))
                        (list (latex-templates-directory))
                        "export template"))

;;; Word documents and web pages

(define-record (note-export (:include tuatara.runtime:job) (:copier nil))
  "An Org note being written as a document for BUFFER, a job: TEXT the note's,
FILE its file; THEN :FILE or :OUTSIDE (opened in the desktop's program after);
PATH, the file written; NOTES, what else to say of it."
  (text nil :documentation "The text of the Org note NOTE-EXPORT writes as a document.")
  (file nil :documentation "The file of the note.")
  (then nil :documentation "What is done after: :FILE or :OUTSIDE (opened in the desktop's program).")
  (path nil :documentation "The path of the file written.")
  (notes nil :documentation "What else to say of NOTE-EXPORT."))

(defmethod tuatara.runtime:job-holds-queue-p ((job note-export))
  "Writing a note as a document holds back no other work on its buffer: it only reads the note."
  nil)

(defmethod tuatara.runtime:job-take-in ((job note-export))
  "Say where the document went and what else there is to say of it; open
it in the desktop's program when asked."
  (case (note-export-status job)
    (:done (let ((path (note-export-path job)))
             (message "Wrote ~a~{; ~a~}" (abbreviated-path path) (note-export-notes job))
             (when (eq (note-export-then job) :outside) (open-externally-file path :also '(:docx)))))
    (t (message "~a" (tuatara.runtime:job-ending-words job)))))

(define-record (docx-export (:include note-export) (:constructor make-docx-export (buffer text file then))
                            (:copier nil))
  "An Org note being written as a Word document (NOTE-EXPORT).")

(tuatara.runtime:define-job-words docx-export
    :doing "Writing the Word document"
    :description (format nil "Writing ~a as a Word document" (tuatara.io:native-file-name (docx-export-file job)))
    :verb "write the Word document"
    :left-words "no Word document written")

(define-record (html-export (:include note-export) (:constructor make-html-export (buffer text file then))
                            (:copier nil))
  "An Org note being written as a web page (NOTE-EXPORT).")

(setf (documentation 'html-export-p 'function) "True when OBJECT is an HTML-EXPORT.")

(tuatara.runtime:define-job-words html-export
    :doing "Writing the web page"
    :description (format nil "Writing ~a as a web page" (tuatara.io:native-file-name (html-export-file job)))
    :verb "write the web page"
    :left-words "no web page written")

(defun start-note-export (then kind type words make write)
  "Start writing the current buffer's Org note as a document of TYPE, the ending
of its file, beside it (or where #+EXPORT_FILE_NAME says), then do THEN with it;
or say why not. KIND is the job's type, WORDS its name in a sentence (a Word
document), MAKE makes the job from the buffer, note, file and THEN, and WRITE
takes the note and file and gives the contents, the file #+EXPORT_FILE_NAME
names or NIL, and what else to say."
  (let* ((buffer (current-buffer))
         (name (tuatara.buffer:buffer-name buffer))
         (file (tuatara.buffer:buffer-file buffer))
         (org (eq (export-kind buffer) :org))
         (text (and org (tuatara.buffer:buffer-string buffer))))
    (cond ((export-refused-p buffer text
                             :wrong-kind (and (not org)
                                              (format nil "~a is no Org note: nothing to write as ~a" name words))
                             :running (and (find-if (lambda (job) (typep job kind)) (jobs-of buffer :running t))
                                           (format nil "~a is being written as ~a already" name words))
                             :redactions t))
          ((export-target-refused-p buffer (list type) text))
          (t
           (let ((job (funcall make buffer text (tuatara.io:native (tuatara.io:expand-path file)) then)))
             (tuatara.runtime:start-job-thread
              job (lambda (job)
                    (multiple-value-bind (contents named notes)
                        (funcall write (note-export-text job) (note-export-file job))
                      (let ((path (or named (replaced-type (note-export-file job) type))))
                        (check-export-directory path)
                        (tuatara.io:replace-file path contents)
                        (setf (note-export-path job) path (note-export-notes job) notes))))
              :name (format nil "tuatara ~a export" type))
             (message "Writing ~a as ~a..." name words))))))

(defun start-docx-export (then)
  "Start writing the current buffer's Org note as a Word document beside it
\(or where #+EXPORT_FILE_NAME says), then do THEN with it; or say why not."
  (start-note-export then 'docx-export "docx" "a Word document" #'make-docx-export
                     (lambda (text file) (org-docx-document text :file file))))

(define-command org-export-docx ()
  "Write the Org note of this buffer as a Word document (.docx) beside it, or
where #+EXPORT_FILE_NAME says (its .docx): read as the enhanced PDF export
reads it, its :EXPORT: drawers' settings and all, what redact and protect hide
a black bar, never its text, in the file; in the background."
  (start-docx-export :file))

(define-command org-export-docx-xdg ()
  "Write the Org note of this buffer as a Word document, as \\[org-export-docx]
does, and open it in the desktop's program for Word documents."
  (start-docx-export :outside))

(define-command org-export-html ()
  "Write the Org note of this buffer as a web page (.html) beside it, or where
#+EXPORT_FILE_NAME says (its .html), one file with its stylesheet inside, in the
Ao theme's colours, light or dark as the reader's system prefers: read as the
enhanced PDF export reads it, what redact and protect hide a black bar, never
its text. Every character of the note escaped and raw HTML left out, so a note
from anywhere cannot make the page do anything. In the background."
  (start-note-export :file 'html-export "html" "a web page" #'make-html-export
                     (lambda (text file) (org-html-document text :file file))))

(define-major-mode latex-errors-mode special-mode
  "*LaTeX errors*: what the programs making a PDF said when none made it, or
when the programs making its bibliography or index failed, each error at the
line of your file it comes from, under the name of the program that met it.
\\<latex-errors-mode>\\[list-visit] on an error shows that line in another
window, \\[list-visit-other] the list staying selected; \\[list-next-entry] and
\\[list-previous-entry] go from one error to the next; from anywhere,
\\[next-place] and \\[previous-place] show the next error's line and the one
before; \\[search-filter-regex] narrows the list to the errors whose words
match; \\`q' closes."
  :lighter "LaTeX errors" :interactive nil)

(defmethod entry-place ((mode latex-errors-mode) entry)
  "The file and line the error ENTRY comes from (ERROR-PLACE); NIL for a
line that shows no error, or an error LaTeX said nothing of where it is."
  (destructuring-bind (kind &optional file line) entry
    (and (eq kind :error) file (values file line))))

(defun install-latex-export ()
  "Bind the keys of *LaTeX errors*; offer the commands making PDFs of
Org files in Org buffers, those of LaTeX files in LaTeX buffers; and make
the standard rule of where exported PDFs are shown: down the right side of
the screen when it is wide enough, else alone on it; and offer the checking
and marking of redactions (INSTALL-REDACTIONS), and the showing of a line in
the PDF and back (INSTALL-PDF-SYNC)."
  (entry-list-keys 'latex-errors-mode "*LaTeX errors*")
  (commands-for-mode 'org-mode "Org files" 'org-export-latex 'org-export-latex-enhanced 'org-export-latex-pdf
                     'org-export-latex-pdf-buffer 'org-export-latex-pdf-xdg
                     'org-export-latex-pdf-enhanced 'org-export-latex-pdf-enhanced-buffer
                     'org-export-latex-pdf-enhanced-xdg)
  (commands-for-mode 'latex-mode "LaTeX files" 'export-latex-compile-pdf 'export-latex-compile-pdf-buffer
                     'export-latex-compile-pdf-xdg)
  (commands-for-mode 'org-mode "Org files" 'org-insert-export-drawer 'org-export-docx 'org-export-docx-xdg
                     'org-export-html)
  (place-buffers :exported :right-or-alone)
  (install-redactions)
  (install-pdf-sync))
