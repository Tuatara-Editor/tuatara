;;;; lisp/org/tangle.lisp --- Code files written from an Org file's source blocks, as Org's org-babel-tangle writes them.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; A source block with :tangle FILE (on its #+begin_src line, in a
;;;; :header-args: property of a subtree, or in #+PROPERTY: header-args for
;;;; the whole file) has its code written to FILE, the blocks for one file
;;;; one after another. tangle-code-files lists every file it would write,
;;;; new, changed or the same, and those outside the Org file's folder, and
;;;; writes them only when told to: the Org file decides where they go.
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

(defvar *code-files-make-folders* nil
  "NIL for tangle-code-files to make the folders a file goes in only for blocks
with :mkdirp yes, so that an Org file writes the same everywhere; true to make
them for every block, as :mkdirp yes does (a block's :mkdirp no still says not
to).")

(defun org-keyword-arguments (text)
  "The settings TEXT gives as :KEY VALUE (:tangle ~/.bashrc :mkdirp yes), in
order, as an alist of (KEY . VALUE), KEY as written without its colon, VALUE
without the spaces around it; a key with no value has \"\"."
  (loop for part in (cl-ppcre:split "(?:^|[ \\t]+)(?=:[A-Za-z][-A-Za-z0-9_]*(?:[ \\t]|$))" (or text ""))
        for setting = (string-trim " 	" part)
        when (and (plusp (length setting)) (char= (char setting 0) #\:))
          collect (let ((space (position-if (lambda (c) (member c '(#\Space #\Tab))) setting)))
                    (cons (subseq setting 1 space) (if space (string-trim " 	" (subseq setting space)) "")))))

(defun org-header-arguments (text)
  "The header arguments of TEXT (:tangle ~/.bashrc :mkdirp yes) as an alist of
(KEY . VALUE) (ORG-KEYWORD-ARGUMENTS), KEY in lower case, VALUE without the
double quotes around it."
  (loop for (key . value) in (org-keyword-arguments text)
        collect (cons (string-downcase key)
                      (if (and (>= (length value) 2) (char= (char value 0) #\") (char= (char value (1- (length value))) #\"))
                          (subseq value 1 (1- (length value)))
                          value))))

(defun org-property-with (value name key new)
  "VALUE, the property NAME's so far, after a property KEY of value NEW: NEW
when KEY is NAME, NEW added to VALUE after a space when KEY is NAME+, else
VALUE as it was."
  (cond ((string-equal key name) new)
        ((and (= (length key) (1+ (length name))) (char= (char key (length name)) #\+) (string-equal key name :end1 (length name)))
         (if value (format nil "~a ~a" value new) new))
        (t value)))

(defun org-file-properties (tree name)
  "The value the file gives property NAME by #+PROPERTY: lines (NAME+ adding to it), or NIL."
  (let ((value nil))
    (labels ((walk (node)
               (when (and (eq (org-node-type node) 'keyword) (string= (org-property node :key) "PROPERTY"))
                 (let* ((text (string-trim " 	" (org-property node :value)))
                        (space (position-if (lambda (c) (member c '(#\Space #\Tab))) text))
                        (key (subseq text 0 space))
                        (rest (if space (string-trim " 	" (subseq text space)) "")))
                   (setf value (org-property-with value name key rest))))
               (when (member (org-node-type node) '(org-data section))
                 (mapc #'walk (org-node-children node)))))
      (walk tree))
    value))

(defun org-headline-properties (headline)
  "The properties of HEADLINE's property drawer, as an alist of (KEY . VALUE), in order."
  (let ((section (find 'section (org-node-children headline) :key #'org-node-type)))
    (and section
         (let ((drawer (find 'property-drawer (org-node-children section) :key #'org-node-type)))
           (and drawer
                (loop for node in (org-node-children drawer)
                      when (eq (org-node-type node) 'node-property)
                        collect (cons (org-property node :key) (or (org-property node :value) ""))))))))

(defun org-headline-property (headline key)
  "The value of the property KEY in HEADLINE's property drawer, or NIL when it
has none or an empty one."
  (let ((value (cdr (assoc key (org-headline-properties headline) :test #'string-equal))))
    (and (plusp (length value)) value)))

(defun org-inherited-property (tree node name)
  "The value property NAME has at NODE, inherited: the file's (#+PROPERTY:),
then each headline above NODE's from the outermost, NAME replacing what is
there and NAME+ adding to it; NIL when none."
  (let ((value (org-file-properties tree name))
        (headlines (loop for n = (org-node-parent node) then (org-node-parent n)
                         while n when (eq (org-node-type n) 'headline) collect n)))
    (dolist (headline (reverse headlines) value)
      (loop for (key . v) in (org-headline-properties headline)
            do (setf value (org-property-with value name key v))))))

(defun org-block-header-arguments (tree block)
  "The header arguments of BLOCK, a source block of TREE, merged: its own over
its language's (header-args:LANGUAGE) over all languages' (header-args), each
as the subtrees above it and the file give them, over the defaults (:tangle
no, :padline yes, :noweb no), with :mkdirp as *CODE-FILES-MAKE-FOLDERS* says."
  (let ((language (org-property block :language))
        (merged (list (cons "tangle" "no") (cons "padline" "yes") (cons "noweb" "no") (cons "mkdirp" (if *code-files-make-folders* "yes" "no")))))
    (flet ((over (pairs) (loop for (key . value) in pairs
                               do (setf merged (cons (cons key value) (remove key merged :key #'car :test #'string=))))))
      (over (org-header-arguments (org-inherited-property tree block "header-args")))
      (when language
        (over (org-header-arguments (org-inherited-property tree block (format nil "header-args:~a" language)))))
      (over (org-header-arguments (org-property block :parameters))))
    merged))

(defun org-remove-common-indentation (text)
  "TEXT with the white space all its lines that are not blank begin with taken
away (a tab counting to the next eighth column)."
  (let* ((lines (uiop:split-string text :separator '(#\Newline)))
         (indent (loop for line in lines
                       unless (string= (string-trim " 	" line) "")
                         minimize (loop for c across line
                                        with column = 0
                                        do (case c (#\Space (incf column)) (#\Tab (setf column (tuatara.display:next-tab-stop column 8))) (t (return column)))
                                        finally (return column)))))
    (if (or (null indent) (zerop indent))
        text
        (format nil "~{~a~^~%~}"
                (mapcar (lambda (line)
                          (let ((column 0) (i 0))
                            (loop while (and (< i (length line)) (< column indent))
                                  do (case (char line i)
                                       (#\Space (incf column)) (#\Tab (setf column (tuatara.display:next-tab-stop column 8)))
                                       (t (return)))
                                     (incf i))
                            (subseq line i)))
                        lines)))))

(defun org-block-code (block)
  "The code of BLOCK, a source block, as it is tangled and exported: its lines
unescaped and their common indentation taken away (`org-remove-common-indentation')."
  (org-remove-common-indentation (or (org-property block :value) "")))

(defun org-trim-code (code)
  "CODE without the spaces, tabs and blank lines at its start and end, as a block's code is written to its file."
  (string-trim '(#\Space #\Tab #\Newline #\Return) code))

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
  "CODE with each <<name>> on its own in a line put in place by the code of
the block of that #+NAME: in NAMES, every line of it with what came before the
reference on its line; <<name()>>, which would run the block, names not known,
a block's reference to itself or to a block it is within (WITHIN), and
references past *NOWEB-LIMIT* (BUDGET) left as they are."
  (if (> depth 20)
      code
      (cl-ppcre:regex-replace-all
       "(?m)^(.*?)<<([^<>()\\n]+)>>(.*)$" code
       (lambda (match before name after)
         (let* ((key (string-trim " " name))
                (block (gethash key names)))
           (if (or (null block) (member key within :test #'string=) (minusp (car budget)))
               match
               (let* ((lines (uiop:split-string (org-expand-noweb (org-trim-code (org-block-code block)) names (1+ depth)
                                                                   (cons key within) budget)
                                                :separator '(#\Newline)))
                      (text (format nil "~{~a~^~%~}~a" (mapcar (lambda (line) (concatenate 'string before line)) lines) after)))
                 (decf (car budget) (length text))
                 (if (minusp (car budget)) match text)))))
       :simple-calls t)))

(defun org-commented-p (node)
  "True when NODE is under a COMMENT headline, which Org writes nothing of."
  (loop for n = (org-node-parent node) then (org-node-parent n)
        while n thereis (and (eq (org-node-type n) 'headline) (org-property n :commentedp))))

(defstruct (code-file (:conc-name code-file-))
  "A file tangle-code-files writes: PATH (native), TEXT, the BLOCKS written to
it, MODE to give it (or NIL), MAKE-DIRECTORIES true for :mkdirp, and STATE
(:new, :changed, :same or :cannot), with why it cannot be written; REAL,
where writing it lands, its links followed, when that is elsewhere; STAMP,
what was there when it was looked at (NIL for nothing), so that a file put
in its place meanwhile is not written over."
  path text (blocks 0) mode make-directories state reason real stamp)

(defun org-tangle-mode (value)
  "The file mode :tangle-mode VALUE (o755, #o755, 755) gives, or NIL; a mode written as Lisp is not run.
Only the permissions are taken: setuid, setgid and sticky are left out."
  (multiple-value-bind (match groups) (cl-ppcre:scan-to-strings "^(?:#?o)?([0-7]{3,4})$" (string-trim " " value))
    (and match (logand #o777 (parse-integer (aref groups 0) :radix 8)))))

(defun org-code-files (tree org-file)
  "The files TREE's source blocks write, as CODE-FILE objects in the order of
their first block, ORG-FILE (a native path, or NIL) the Org file the blocks
are in, relative :tangle files taken from its folder."
  (let ((files '()) (names (org-named-blocks tree))
        (directory (if org-file
                       (tuatara.io:parent-path org-file)
                       (tuatara.io:native (tuatara.io:default-directory))))
        (base (and org-file (tuatara.io:native-file-stem org-file))))
    (flet ((visit (node)
             (when (and (eq (org-node-type node) 'src-block) (not (org-commented-p node)))
               (let* ((arguments (org-block-header-arguments tree node))
                      (tangle (cdr (assoc "tangle" arguments :test #'string=))))
                 (unless (member tangle '("no" "") :test #'string=)
                   (let* ((language (or (org-property node :language) ""))
                          (target (if (string= tangle "yes")
                                      (and base (format nil "~a.~a" base (or (cdr (assoc language *code-file-extensions* :test #'string=))
                                                                             language)))
                                      tangle)))
                     (when target
                       (let* ((path (tuatara.io:clean-path (tuatara.io:native (tuatara.io:expand-path target :directory directory))))
                              (file (or (find path files :key #'code-file-path :test #'string=)
                                        (car (push (make-code-file :path path :text nil) files))))
                              (code (org-block-code node))
                              (code (org-trim-code (if (member (cdr (assoc "noweb" arguments :test #'string=)) '("yes" "tangle" "strip-tangle" "no-export" "strip-export") :test #'string=)
                                        (org-expand-noweb code names 0 (let ((own (org-property node :name))) (and own (list own))))
                                        code)))
                              (shebang (cdr (assoc "shebang" arguments :test #'string=)))
                              (padline (not (string= (cdr (assoc "padline" arguments :test #'string=)) "no"))))
                         (setf (code-file-text file)
                               (concatenate 'string
                                            (cond ((null (code-file-text file)) (if (plusp (length shebang)) (format nil "~a~%" shebang) ""))
                                                  (padline (format nil "~a~%" (code-file-text file)))
                                                  (t (code-file-text file)))
                                            code (string #\Newline)))
                         (incf (code-file-blocks file))
                         (when (plusp (length shebang)) (setf (code-file-mode file) (or (code-file-mode file) :executable)))
                         (let ((mode (cdr (assoc "tangle-mode" arguments :test #'string=))))
                           (when mode (setf (code-file-mode file) (or (org-tangle-mode mode) (code-file-mode file)))))
                         (when (string= (cdr (assoc "mkdirp" arguments :test #'string=)) "yes")
                           (setf (code-file-make-directories file) t))))))))))
      (org-walk-tree tree #'visit))
    (nreverse files)))

(defun code-file-place (file)
  "Where writing FILE (a CODE-FILE) lands, as CHECK-CODE-FILE found it: the
file its links lead to, or its own path."
  (or (code-file-real file) (code-file-path file)))

(defun check-code-file (file)
  "Find out what writing FILE would do: STATE :new, :changed or :same, or :cannot with REASON."
  (let* ((path (code-file-path file))
         (folder (tuatara.io:parent-path path))
         ;; where writing it lands: a link in its path, the file's own or a folder's, followed
         (real (or (ignore-errors (tuatara.io:real-path path))
                   (let* ((resolved (or (ignore-errors (tuatara.io:native (tuatara.io:resolve-links path))) path))
                          (place (ignore-errors (tuatara.io:real-path (tuatara.io:parent-path resolved)))))
                     (and place (tuatara.io:path-join place (tuatara.io:native-file-name resolved))))))
         (stamp (progn (setf (code-file-real file) (and real (string/= real path) real))
                       (ignore-errors (tuatara.io:path-stamp (code-file-place file) :follow nil)))))
    (setf (code-file-stamp file) stamp)
    (cond ((and stamp (not (eq (tuatara.io:stamp-kind stamp) :regular)))
           (setf (code-file-state file) :cannot
                 (code-file-reason file) (format nil "it is ~a" (tuatara.io:kind-words (tuatara.io:stamp-kind stamp)))))
          (stamp
           (let ((old (ignore-errors (tuatara.io:file-octets (code-file-place file)))))
             (setf (code-file-state file) (if (equalp old (tuatara.text:string-octets (code-file-text file))) :same :changed))))
          ((or (ignore-errors (tuatara.io:path-stamp folder)) (code-file-make-directories file))
           (setf (code-file-state file) :new))
          (t (setf (code-file-state file) :cannot
                   (code-file-reason file) "its folder does not exist (:mkdirp yes makes it)")))
    file))

(define-major-mode code-files-mode special-mode
  "The files tangle-code-files would write from an Org file's source blocks,
listed while it asks whether to write them: each new, changed or the same
(not written again), those it cannot write with why, and those outside the
Org file's folder marked."
  :lighter "Code files" :interactive nil)

(defun code-file-inside-p (file home)
  "True when FILE (a CODE-FILE) lands in the folder HOME (native, ending in /),
by its name and by where its links lead."
  (and (uiop:string-prefix-p home (code-file-path file))
       (uiop:string-prefix-p (or (ignore-errors (tuatara.io:real-path home)) home)
                             (or (code-file-real file) (code-file-path file)))))

(defun code-file-words (file)
  "FILE (a CODE-FILE) as what is said of it written: its path, and where it
really landed when a link led elsewhere (out.sh → ~/.bashrc)."
  (format nil "~a~@[ → ~a~]" (abbreviated-path (code-file-path file))
          (and (code-file-real file) (abbreviated-path (code-file-real file)))))

(defun code-files-listing (files org-file)
  "The text listing FILES (CODE-FILE objects) for tangle-code-files, ORG-FILE the
Org file's native path or NIL: what writing each does, its blocks, and its
path, from the Org file's folder when it is in it."
  (let ((home (and org-file (tuatara.io:parent-path org-file))))
    (with-output-to-string (out)
      (format out "Code files from ~a~%~%" (if org-file (abbreviated-path org-file) "this buffer"))
      (dolist (file files)
        (let* ((path (code-file-path file))
               (inside (and home (code-file-inside-p file home))))
          (format out "  ~9a ~22a ~a~@[  → ~a~]~:[  outside the Org file's folder~;~]~%"
                  (ecase (code-file-state file) (:new "new") (:changed "changed") (:same "the same") (:cannot "cannot"))
                  (if (eq (code-file-state file) :cannot)
                      ""
                      (format nil "~d block~:p~a~:[~;, folder made~]" (code-file-blocks file)
                              (let ((mode (code-file-mode file)))
                                (cond ((null mode) "")
                                      ((eq mode :executable) ", executable")
                                      (t (format nil ", mode ~3,'0o" mode))))
                              (and (eq (code-file-state file) :new) (not (ignore-errors (tuatara.io:path-stamp (tuatara.io:parent-path path)))))))
                  (if inside (subseq path (length home)) (abbreviated-path path))
                  (and (code-file-real file) (abbreviated-path (code-file-real file)))
                  (or inside (null home)))
          (when (eq (code-file-state file) :cannot)
            (format out "            ~a~%" (code-file-reason file))))))))

(defun tangle-write-code-file (file)
  "Write FILE (a CODE-FILE), its folders made first with :mkdirp, its mode
given; NIL when done, else why not. It is written where CHECK-CODE-FILE found
it lands, into a new file renamed into place: a symbolic link put there since,
or a file put there or changed since, is refused, never followed or written
over."
  (handler-case
      (let ((path (code-file-path file)))
        (when (code-file-make-directories file)
          (let ((folders '()))
            (loop for folder = (string-right-trim "/" (tuatara.io:parent-path path))
                    then (string-right-trim "/" (tuatara.io:parent-path folder))
                  while (and (plusp (length folder)) (not (ignore-errors (tuatara.io:path-stamp folder))))
                  do (push folder folders))
            (dolist (folder folders) (tuatara.io:make-directory folder))))
        (let ((mode (code-file-mode file)))
          (tuatara.io:write-text-file (tuatara.text:make-rope (code-file-text file)) (code-file-place file)
                                      :expected (code-file-stamp file) :follow-links nil
                                      :mode (and mode (lambda (now) (if (eq mode :executable) (logior (logand now #o777) #o111) mode)))))
        nil)
    (error (e) (format nil "~a" e))))

(define-command tangle-code-files ()
  "Write the code files of this Org file's source blocks (\\`C-c C-v t'): each block with :tangle FILE (on its
#+begin_src line, in a subtree's :header-args: or :header-args:LANGUAGE:
property, or in #+PROPERTY: header-args for the file) goes to FILE, relative
to the Org file's folder, the blocks for one file in order, a blank line
between them unless :padline no; :tangle yes, the Org file's name with the
language's ending (*CODE-FILE-EXTENSIONS*); :mkdirp yes makes the folders a
file goes in when they are not there (for every block with
*CODE-FILES-MAKE-FOLDERS*);
:shebang \"#!/bin/sh\" begins the file and makes it executable, :tangle-mode
o755 gives it that mode; :noweb yes puts in the code <<name>> stands for.
First it lists every file, new, changed, the same or not to be written, and
those outside the Org file's folder, and writes the new and changed ones only
when you say so. Nothing in a block is ever run."
  (let ((buffer (current-buffer)))
    (if (not (typep (buffer-major-mode buffer) 'org-mode))
        (message "tangle-code-files writes the code of an Org file's source blocks: this is no Org buffer")
        (let ((tree (org-tree-now buffer)))
          (let* ((org-file (let ((file (tuatara.buffer:buffer-file buffer))) (and file (tuatara.io:native file))))
                 (files (mapcar #'check-code-file (org-code-files tree org-file)))
                 (to-write (remove-if-not (lambda (f) (member (code-file-state f) '(:new :changed))) files)))
            (cond
              ((null files) (message "No source block here has :tangle: nothing to write"))
              ((and (tuatara.buffer:buffer-local buffer :plaintext)
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
                          (message "Nothing to write: ~:[every file is as it would be written~;the files listed cannot be written~]"
                                   (find :cannot files :key #'code-file-state))
                          (if (yes-or-no-p* (format nil "Write ~d file~:p~@[ (~d outside the Org file's folder)~]?"
                                                    (length to-write)
                                                    (let ((home (and org-file (tuatara.io:parent-path org-file))))
                                                      (and home (let ((n (count-if-not (lambda (f) (code-file-inside-p f home)) to-write)))
                                                                  (and (plusp n) n))))))
                              (let ((written '()) (failed '()))
                                (dolist (file to-write)
                                  (let ((why (tangle-write-code-file file)))
                                    (if why
                                        (push (format nil "~a: ~a" (abbreviated-path (code-file-path file)) why) failed)
                                        (push file written))))
                                (if failed
                                    (message "Wrote ~d of ~d (~{~a~^, ~}); not written: ~{~a~^; ~}" (length written) (length to-write)
                                             (mapcar #'code-file-words (reverse written)) (reverse failed))
                                    (message "Wrote ~d file~:p: ~{~a~^, ~}" (length to-write)
                                             (mapcar #'code-file-words to-write))))
                              (message "Nothing written")))
                   (tuatara.buffer:kill-buffer listing))))))))))

(defun install-code-files ()
  "Bind \\`C-c C-v t' and \\`C-c C-v C-t' in Org buffers to tangle-code-files,
under \\`C-c C-v' (code files)."
  (let ((keymap (mode-keymap 'org-mode)))
    (define-key keymap "C-c C-v t" 'tangle-code-files)
    (define-key keymap "C-c C-v C-t" 'tangle-code-files)
    (label-keys keymap '(("C-c C-v" "code files")))))
