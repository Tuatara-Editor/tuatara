;;;; lisp/org/tangle.lisp --- Code files written from an Org file's source blocks, as org-babel-tangle does.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; A source block with :tangle FILE (on its #+begin_src line, in a
;;;; :header-args: property of a subtree, or in #+PROPERTY: header-args for
;;;; the whole file) has its code written to FILE, the blocks for one file
;;;; one after another. tangle-code-files lists every file it would write,
;;;; new, changed or the same, and those outside the Org file's directory, and
;;;; writes them only when told to; the Org file decides where they go.
;;;; Nothing in a block is run: <<name()>>, which Org would run, is left as it is.

(in-package #:tuatara.defaults)

(defparameter *code-file-extensions*
  '(("emacs-lisp" . "el") ("elisp" . "el") ("lisp" . "lisp") ("python" . "py") ("sh" . "sh") ("shell" . "sh")
    ("bash" . "sh") ("zsh" . "sh") ("fish" . "fish") ("rust" . "rs") ("go" . "go") ("C" . "c") ("c" . "c")
    ("cpp" . "cpp") ("C++" . "cpp") ("js" . "js") ("javascript" . "js") ("typescript" . "ts") ("ruby" . "rb")
    ("perl" . "pl") ("lua" . "lua") ("haskell" . "hs") ("latex" . "tex") ("java" . "java") ("awk" . "awk")
    ("sql" . "sql") ("sqlite" . "sql") ("html" . "html") ("css" . "css") ("toml" . "toml") ("yaml" . "yaml")
    ("json" . "json") ("conf" . "conf") ("text" . "txt") ("org" . "org") ("makefile" . "mk") ("scheme" . "scm")
    ("clojure" . "clj") ("ocaml" . "ml") ("R" . "R") ("julia" . "jl") ("nix" . "nix") ("zig" . "zig"))
  "The end of a file's name :tangle yes gives the code of each language: the
Org file's name with it (notes.el); a language not here, its own name.")

(define-setting *code-files-make-directories* nil
  "Whether tangle-code-files makes the directories a file goes in.
- nil: only for blocks with :mkdirp yes, so that an Org file writes the same
  everywhere.
- t: for every block, as :mkdirp yes does; a block's :mkdirp no still says
  not to."
  :values boolean)

(defun org-block-header-arguments (tree block)
  "The header arguments of BLOCK, a source block of TREE, merged: its own over
its language's (header-args:LANGUAGE) over all languages' (header-args), each
as the subtrees above it and the file give them, over the defaults (:tangle
no, :padline yes, :noweb no), with :mkdirp as *CODE-FILES-MAKE-DIRECTORIES* says."
  (let ((language (org-property block :language))
        (merged (list (cons "tangle" "no")
                      (cons "padline" "yes")
                      (cons "noweb" "no")
                      (cons "mkdirp" (if *code-files-make-directories* "yes" "no")))))
    (flet ((over (pairs)
             (loop for (key . value) in pairs
                   do (setf merged (cons (cons key value) (remove key merged :key #'car :test #'string=))))))
      (over (org-header-arguments (org-inherited-property tree block "header-args")))
      (when language
        (over (org-header-arguments (org-inherited-property tree block (format nil "header-args:~a" language)))))
      (over (org-header-arguments (org-property block :parameters))))
    merged))

(defun org-named-blocks (tree)
  "A table from each #+NAME: of TREE's source blocks to the block."
  (let ((table (make-hash-table :test 'equal)))
    (org-walk-tree tree (lambda (node)
                          (when (and (eq (org-node-type node) 'src-block) (org-property node :name))
                            (setf (gethash (org-property node :name) table) node))))
    table))

(defparameter *noweb-limit* (* 16 1024 1024)
  "The most characters noweb references may grow a block's code to; past it
the references left are left as they are written.")

(defun org-expand-noweb (code names &optional (depth 0) (within '()) (budget (list *noweb-limit*)))
  "CODE with each <<name>> on its own in a line put in place by the code of the
block of that #+NAME: in NAMES, every line of it with what came before the
reference on its line. <<name()>>, which would run the block, names not known, a
block's reference to itself or to a block it is within (WITHIN), and references
past *NOWEB-LIMIT* (BUDGET) left as they are."
  (if (> depth 20)
      code
      (cl-ppcre:regex-replace-all
       "(?m)^(.*?)<<([^<>()\\n]+)>>(.*)$" code
       (lambda (match before name after)
         (let* ((key (string-trim " " name))
                (block (gethash key names)))
           (if (or (null block) (member key within :test #'string=) (minusp (car budget)))
               match
               (let* ((lines (uiop:split-string (org-expand-noweb (org-trim-code (org-block-code block)) names
                                                                   (1+ depth) (cons key within) budget)
                                                :separator '(#\Newline)))
                      (text (format nil "~{~a~^~%~}~a"
                                    (mapcar (lambda (line) (concatenate 'string before line)) lines) after)))
                 (decf (car budget) (length text))
                 (if (minusp (car budget)) match text)))))
       :simple-calls t)))

(defun org-commented-p (node)
  "True when NODE is under a COMMENT headline, which Org writes nothing of."
  (loop for n = (org-node-parent node) then (org-node-parent n)
        while n thereis (and (eq (org-node-type n) 'headline) (org-property n :commentedp))))

(define-record (code-file (:conc-name code-file-))
  "A file tangle-code-files writes: PATH (native), TEXT, the BLOCKS written to
it (PIECES, the pieces of its text newest first, while they are gathered), MODE
to give it (or NIL), MAKE-DIRECTORIES true for :mkdirp, and STATE
\(:new, :changed, :same or :cannot), with why it cannot be written. REAL, where
writing it lands, its links followed, when that is elsewhere. STAMP, what was
there when it was looked at (NIL for nothing), so that a file put in its place
meanwhile is not written over."
  (path nil :documentation "The native path of a CODE-FILE.")
  (text nil :documentation "The text of a CODE-FILE.")
  (blocks 0 :documentation "How many blocks have been written to a CODE-FILE.")
  (pieces nil :documentation "The pieces of the text of a CODE-FILE, newest first, while they are gathered.")
  (mode nil :documentation "The mode to give a CODE-FILE, or NIL.")
  (make-directories nil :documentation "True when a CODE-FILE makes its directories, for :mkdirp.")
  (state nil :documentation "The state of a CODE-FILE: :NEW, :CHANGED, :SAME or :CANNOT.")
  (reason nil :documentation "Why a CODE-FILE cannot be written, or NIL.")
  (real nil :documentation "Where writing a CODE-FILE lands, its links followed, when that is elsewhere, or NIL.")
  (stamp nil
         :documentation "What was there when a CODE-FILE was looked at, NIL for nothing, so that a file put in its
place meanwhile is not written over."))

(defun org-tangle-mode (value)
  "The file mode :tangle-mode VALUE (o755, #o755, 755) gives, or NIL; a mode written as Lisp is not run.
Only the permissions are taken: setuid, setgid and sticky are left out."
  (multiple-value-bind (match groups) (cl-ppcre:scan-to-strings "^(?:#?o)?([0-7]{3,4})$" (string-trim " " value))
    (and match (logand #o777 (parse-integer (aref groups 0) :radix 8)))))

(defparameter *org-noweb-expanding* '("yes" "tangle" "strip-tangle" "no-export" "strip-export")
  "The values of a block's :noweb under which its <<name>> references are put
in place as it is written to its file.")

(defun org-block-target (node arguments base)
  "The file the source block NODE writes to as its header ARGUMENTS say, as
written: its :tangle, or for :tangle yes BASE, the Org file's name without its
extension, with the extension of its language (*CODE-FILE-EXTENSIONS*); NIL
when it writes none."
  (let ((tangle (cdr (assoc "tangle" arguments :test #'string=))))
    (cond ((member tangle '("no" "") :test #'string=) nil)
          ((string/= tangle "yes") tangle)
          (base (let ((language (or (org-property node :language) "")))
                  (format nil "~a.~a" base (or (cdr (assoc language *code-file-extensions* :test #'string=))
                                               language)))))))

(defun org-block-tangled-code (node arguments names)
  "The code the source block NODE writes, as its header ARGUMENTS say: its
<<name>> references put in place from NAMES (ORG-EXPAND-NOWEB) under
*ORG-NOWEB-EXPANDING*, trimmed (ORG-TRIM-CODE)."
  (let ((code (org-block-code node)))
    (org-trim-code (if (member (cdr (assoc "noweb" arguments :test #'string=)) *org-noweb-expanding* :test #'string=)
                       (org-expand-noweb code names 0 (let ((own (org-property node :name))) (and own (list own))))
                       code))))

(defun code-file-add-block (file code arguments)
  "Add CODE, a source block's, to the pieces of FILE, a CODE-FILE, as the
block's header ARGUMENTS say: after a blank line unless :padline no, the first
after its :shebang; the file made executable by a shebang or given its
:tangle-mode, and its directories made for :mkdirp yes."
  (flet ((argument (name) (cdr (assoc name arguments :test #'string=))))
    (let ((shebang (argument "shebang"))
          (mode (argument "tangle-mode")))
      (cond ((zerop (code-file-blocks file))
             (when (plusp (length shebang)) (push (format nil "~a~%" shebang) (code-file-pieces file))))
            ((string/= (argument "padline") "no") (push (string #\Newline) (code-file-pieces file))))
      (push code (code-file-pieces file))
      (push (string #\Newline) (code-file-pieces file))
      (incf (code-file-blocks file))
      (when (plusp (length shebang))
        (setf (code-file-mode file) (or (code-file-mode file) :executable)))
      (when mode
        (setf (code-file-mode file) (or (org-tangle-mode mode) (code-file-mode file))))
      (when (string= (argument "mkdirp") "yes")
        (setf (code-file-make-directories file) t)))))

(defun org-code-files (tree org-file)
  "The files TREE's source blocks write, as CODE-FILE objects in the order of
their first block, ORG-FILE (a native path, or NIL) the Org file the blocks
are in, relative :tangle files taken from its directory."
  (let ((files '()) (names (org-named-blocks tree))
        (directory (if org-file
                       (tuatara.io:parent-path org-file)
                       (tuatara.io:native (tuatara.io:default-directory))))
        (base (and org-file (tuatara.io:native-file-stem org-file)))
        (*org-file-properties-kept* (cons tree (make-hash-table :test 'equal))))
    (org-walk-tree tree
                   (lambda (node)
                     (when (and (eq (org-node-type node) 'src-block) (not (org-commented-p node)))
                       (let* ((arguments (org-block-header-arguments tree node))
                              (target (org-block-target node arguments base)))
                         (when target
                           (let* ((path (tuatara.io:clean-path
                                         (tuatara.io:native (tuatara.io:expand-path target :directory directory))))
                                  (file (or (find path files :key #'code-file-path :test #'string=)
                                            (car (push (make-code-file :path path :text nil) files)))))
                             (code-file-add-block file (org-block-tangled-code node arguments names) arguments)))))))
    ;; each file's text joined once, its pieces let go
    (dolist (file files)
      (setf (code-file-text file) (with-output-to-string (out)
                                    (dolist (piece (reverse (code-file-pieces file)))
                                      (write-string piece out)))
            (code-file-pieces file) nil))
    (nreverse files)))

(defun code-file-place (file)
  "Where writing FILE (a CODE-FILE) lands, as CHECK-CODE-FILE found it: the
file its links lead to, or its own path."
  (or (code-file-real file) (code-file-path file)))

(defun check-code-file (file)
  "Find out what writing FILE would do: STATE :new, :changed or :same, or :cannot with REASON."
  (let* ((path (code-file-path file))
         (directory (tuatara.io:parent-path path))
         ;; where writing it lands: a link in its path, the file's own or a directory's, followed
         (real (or (ignoring-errors-reported ("finding where ~a leads" path) (tuatara.io:real-path path))
                   (let* ((resolved (or (ignoring-errors-reported ("finding where ~a leads" path)
                                          (tuatara.io:native (tuatara.io:resolve-links path)))
                                        path))
                          (place (ignoring-errors-reported ("finding where ~a leads" resolved)
                                   (tuatara.io:real-path (tuatara.io:parent-path resolved)))))
                     (and place (tuatara.io:path-join place (tuatara.io:native-file-name resolved))))))
         (stamp (progn (setf (code-file-real file) (and real (string/= real path) real))
                       (tuatara.io:path-stamp (code-file-place file) :follow nil :failure nil))))
    (setf (code-file-stamp file) stamp)
    (cond ((and stamp (not (eq (tuatara.io:stamp-kind stamp) :regular)))
           (setf (code-file-state file) :cannot
                 (code-file-reason file) (format nil "it is ~a" (tuatara.io:kind-words (tuatara.io:stamp-kind stamp)))))
          (stamp
           (let ((old (ignoring-errors-reported ("reading ~a to compare" (code-file-place file))
                        (tuatara.io:file-octets (code-file-place file)))))
             (setf (code-file-state file)
                   (if (equalp old (tuatara.text:string-octets (code-file-text file))) :same :changed))))
          ((or (tuatara.io:path-stamp directory :failure nil) (code-file-make-directories file))
           (setf (code-file-state file) :new))
          (t (setf (code-file-state file) :cannot
                   (code-file-reason file) "its directory does not exist (:mkdirp yes makes it)")))
    file))

(define-major-mode code-files-mode special-mode
  "The files tangle-code-files would write from an Org file's source blocks,
listed while it asks whether to write them: each new, changed or the same
(not written again), those it cannot write with why, and those outside the
Org file's directory marked."
  :lighter "Code files" :interactive nil)

(defun code-file-inside-p (file home)
  "True when FILE (a CODE-FILE) lands in the directory HOME (native, ending in /),
by its name and by where its links lead."
  (and (uiop:string-prefix-p home (code-file-path file))
       (uiop:string-prefix-p (or (ignoring-errors-reported ("finding where ~a leads" home) (tuatara.io:real-path home))
                                 home)
                             (or (code-file-real file) (code-file-path file)))))

(defun code-file-words (file)
  "FILE (a CODE-FILE) as what is said of it written: its path, and where it
really landed when a link led elsewhere (out.sh → ~/.bashrc)."
  (format nil "~a~@[ → ~a~]" (abbreviated-path (code-file-path file))
          (and (code-file-real file) (abbreviated-path (code-file-real file)))))

(defun code-files-listing (files org-file)
  "The text listing FILES (CODE-FILE objects) for tangle-code-files, ORG-FILE the
Org file's native path or NIL: what writing each does, its blocks, and its
path, from the Org file's directory when it is in it."
  (let ((home (and org-file (tuatara.io:parent-path org-file))))
    (with-output-to-string (out)
      (format out "Code files from ~a~%~%" (if org-file (abbreviated-path org-file) "this buffer"))
      (dolist (file files)
        (let* ((path (code-file-path file))
               (inside (and home (code-file-inside-p file home))))
          (format out "  ~9a ~22a ~a~@[  → ~a~]~:[  outside the Org file's directory~;~]~%"
                  (ecase (code-file-state file) (:new "new") (:changed "changed") (:same "the same") (:cannot "cannot"))
                  (if (eq (code-file-state file) :cannot)
                      ""
                      (format nil "~d block~:p~a~:[~;, directory made~]" (code-file-blocks file)
                              (let ((mode (code-file-mode file)))
                                (cond ((null mode) "")
                                      ((eq mode :executable) ", executable")
                                      (t (format nil ", mode ~3,'0o" mode))))
                              (and (eq (code-file-state file) :new)
                                   (not (tuatara.io:path-stamp (tuatara.io:parent-path path) :failure nil)))))
                  (if inside (subseq path (length home)) (abbreviated-path path))
                  (and (code-file-real file) (abbreviated-path (code-file-real file)))
                  (or inside (null home)))
          (when (eq (code-file-state file) :cannot)
            (format out "            ~a~%" (code-file-reason file))))))))

(defun tangle-write-code-file (file)
  "Write FILE (a CODE-FILE), its directories made first with :mkdirp, its mode
given; NIL when done, else why not. It is written where CHECK-CODE-FILE found
it lands, into a new file renamed into place: a symbolic link put there since,
or a file put there or changed since, is refused, never followed or written
over."
  (handler-case
      (reporting-errors ("writing ~a" (code-file-path file))
        (let ((path (code-file-path file)))
          (when (code-file-make-directories file)
            (let ((directories '()))
              (loop for directory = (string-right-trim "/" (tuatara.io:parent-path path))
                      then (string-right-trim "/" (tuatara.io:parent-path directory))
                    while (and (plusp (length directory)) (not (tuatara.io:path-stamp directory :failure nil)))
                    do (push directory directories))
              (dolist (directory directories) (tuatara.io:make-directory directory))))
          (let ((mode (code-file-mode file)))
            (tuatara.io:write-text-file (tuatara.text:make-rope (code-file-text file)) (code-file-place file)
                                        :expected (code-file-stamp file) :follow-links nil
                                        :mode (and mode (lambda (now)
                                                          (if (eq mode :executable)
                                                              (logior (logand now #o777) #o111)
                                                              mode)))))
          nil))
    (error (e) (format nil "~a" e))))

(define-command tangle-code-files ()
  "Write the code files of this Org file's source blocks (\\`C-c C-v t'), each
block with :tangle FILE (on its #+begin_src line, in a subtree's property
:header-args: or :header-args:LANGUAGE:, or in #+PROPERTY: header-args for the
file) goes to FILE. The file is relative to the Org file's directory, the blocks
for one file in order, a blank line between them unless :padline no. :tangle yes
the Org file's name with the language's ending (*CODE-FILE-EXTENSIONS*); :mkdirp
yes makes the directories a file goes in when they are not there (for every block
with *CODE-FILES-MAKE-DIRECTORIES*); :shebang \"#!/bin/sh\" begins the file and
makes it executable, :tangle-mode o755 gives it that mode; :noweb yes puts in
the code <<name>> stands for. First it lists every file, new, changed, the same
or not to be written, and those outside the Org file's directory, and writes the
new and changed ones only when you say so. Nothing in a block is ever run."
  (let ((buffer (current-buffer)))
    (if (not (typep (buffer-major-mode buffer) 'org-mode))
        (message "tangle-code-files writes the code of an Org file's source blocks: this is no Org buffer")
        (let ((tree (org-parse-tree (org-parse-now buffer))))
          (let* ((org-file (let ((file (tuatara.buffer:buffer-file buffer))) (and file (tuatara.io:native file))))
                 (files (mapcar #'check-code-file (org-code-files tree org-file)))
                 (to-write (remove-if-not (lambda (f) (member (code-file-state f) '(:new :changed))) files)))
            (cond
              ((null files) (message "No source block here has :tangle: nothing to write"))
              ((and (tuatara.io:buffer-plaintext-p buffer)
                    (not (yes-or-no-p* (format nil "~a is encrypted, but the code files it writes are not; go on?"
                                               (tuatara.buffer:buffer-name buffer)))))
               (message "No code files written"))
              (t
               (let ((listing (tuatara.buffer:make-buffer (tuatara.buffer:unique-buffer-name "*Code files*"))))
                 (set-major-mode listing 'code-files-mode)
                 (set-made-text listing (code-files-listing files org-file))
                 (setf (tuatara.buffer:buffer-point listing) 0)
                 (show-buffer listing)
                 (unwind-protect
                      (if (null to-write)
                          (message "Nothing to write: ~:[every file is as it would be written~;~
                                    the files listed cannot be written~]"
                                   (find :cannot files :key #'code-file-state))
                          (if (yes-or-no-p* (format nil "Write ~d file~:p~@[ (~d outside the Org file's directory)~]?"
                                                    (length to-write)
                                                    (let* ((home (and org-file (tuatara.io:parent-path org-file)))
                                                           (outside (and home
                                                                         (remove-if (lambda (f)
                                                                                      (code-file-inside-p f home))
                                                                                    to-write))))
                                                      (and outside (length outside)))))
                              (let ((written '()) (failed '()))
                                (dolist (file to-write)
                                  (let ((why (tangle-write-code-file file)))
                                    (if why
                                        (push (format nil "~a: ~a" (abbreviated-path (code-file-path file)) why) failed)
                                        (push file written))))
                                (if failed
                                    (message "Wrote ~d of ~d (~{~a~^, ~}); not written: ~{~a~^; ~}"
                                             (length written)
                                             (length to-write)
                                             (mapcar #'code-file-words (reverse written)) (reverse failed))
                                    (message "Wrote ~d file~:p: ~{~a~^, ~}" (length to-write)
                                             (mapcar #'code-file-words to-write))))
                              (message "Nothing written")))
                   (tuatara.buffer:kill-buffer listing))))))))))

(defun install-code-files ()
  "Bind \\`C-c C-v t' in Org buffers to tangle-code-files, under \\`C-c C-v'
\(code files)."
  (let ((keymap (mode-keymap 'org-mode)))
    (define-key keymap "C-c C-v t" 'tangle-code-files)
    (label-keys keymap '(("C-c C-v" "code files")))))
