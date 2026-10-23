;;;; lisp/packages.lisp --- Packages from anywhere: a folder of a git repository, or of your disk.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; A package is a folder of Lisp files, NAME/NAME.lisp its entry (load.lisp
;;;; too), in a git repository anyone hosts, one package a repository or a
;;;; folder each, or in a folder of your own disk. There is no index: the init
;;;; file names where each comes from,
;;;;
;;;;   (package-source "mine" "https://example.org/my-packages.git")
;;;;   (load-package "example" :source "mine")
;;;;
;;;; and the first time Tuatara fetches only what it needs, in the background:
;;;; which commit the version asked for is (the repository's newest tag by date
;;;; unless said), then that commit alone and the package's folder alone. It is
;;;; kept in your data folder and loaded from there at every start, never
;;;; fetched again until M-x package-update. Before a package is first compiled
;;;; it asks, its source offered to read; then its files are compiled into the
;;;; cache once a version and loaded. A package has a Lisp package of its own
;;;; that uses TUATARA, the names lisp/api.lisp lists, and makes its settings
;;;; with DEFINE-PACKAGE-SETTING.

(in-package #:tuatara.defaults)

(defvar *package-ask-before-loading* t
  "True to be asked before a package is first compiled and loaded: one fetched
the first time, and again when an update brings another commit; one of a
folder of yours the first time only, what you change in it later being yours.
y loads it, n leaves it, v shows its source first. NIL to load them without
asking.")

(defvar *package-sources* '()
  "The places packages come from, each (NAME . WHERE): WHERE a git
repository's address or a folder of your disk (PACKAGE-SOURCE sets them).")

(defvar *packages-kept* 500
  "How many fetched packages' versions are remembered.")

(defun package-line (entry)
  "The line of the packages file for ENTRY, (KEY COMMIT TAG ACCEPTED), COMMIT
being a dash for a folder of your disk."
  (destructuring-bind (key commit tag accepted) entry
    (format nil "~a~c~a~c~a~c~:[no~;yes~]" (escape-path key) #\Tab commit #\Tab (escape-path (or tag "")) #\Tab accepted)))

(defun parse-package-line (line)
  "The entry LINE of the packages file writes, (KEY COMMIT TAG ACCEPTED), or
NIL for a line that makes no sense."
  (let ((fields (uiop:split-string line :separator '(#\Tab))))
    (when (= 4 (length fields))
      (destructuring-bind (key commit tag accepted) fields
        (and (plusp (length key)) (cl-ppcre:scan "^(?:[0-9a-f]{7,64}|-)$" commit)
             (list (unescape-path key) commit (let ((tag (unescape-path tag))) (and (plusp (length tag)) tag))
                   (string= accepted "yes")))))))

(define-state-list *fetched-packages* "packages" 'parse-package-line 'package-line '*packages-kept*
  :header (format nil "# The packages fetched, the latest first: the package, its source and the~%~
                       # version asked for; the commit fetched; its tag; whether it was let load.~%")
  :path-of #'first)

(defun fetched-package (key)
  "What is remembered of the package KEY names (PACKAGE-KEY), (KEY COMMIT TAG
ACCEPTED), or NIL."
  (state-list-find *fetched-packages* key))

(defun tuatara-user:package-source (name where)
  "Name WHERE, a git repository's address or a folder of your disk (~/ for
your home folder), as the source NAME, for load-package's :source:
\(package-source \"mine\" \"https://example.org/my-packages.git\")."
  (unless (and (stringp name) (plusp (length name)))
    (error "package-source takes a name and an address or a folder: ~s ~s" name where))
  (check-package-where where)
  (setf *package-sources* (acons name where (remove name *package-sources* :key #'car :test #'string=)))
  where)

(defun package-where (source)
  "Where SOURCE, a source's name (PACKAGE-SOURCE) or itself an address or a
folder, is."
  (or (cdr (assoc source *package-sources* :test #'string=)) source))

(defun local-source-p (where)
  "True when WHERE is a folder of your disk rather than a repository's address."
  (and (plusp (length where)) (find (char where 0) "/~")))

(defun check-package-where (where)
  "Signal an error unless WHERE, a package's source, is an address or a folder
named whole (/ or ~/ first): one named from the folder Tuatara starts in would
be another folder at each start, and one that starts with - would be read by
git as an option."
  (unless (and (stringp where) (plusp (length where)))
    (error "A package's source is an address or a folder: ~s" where))
  (when (char= (char where 0) #\-)
    (error "A package's source cannot start with -: ~s" where))
  (when (char= (char where 0) #\.)
    (error "A package's folder is named whole, from / or ~~/: ~s" where))
  (when (cl-ppcre:scan "^[a-z][a-z0-9+.-]*://[^/@]*@" where)
    (error "A package's address cannot hold a name or a password (@): ~s" where)))

(defun package-key (name where version)
  "What a package called NAME from WHERE, VERSION asked for, is remembered by."
  (format nil "~a ~a ~a" name where (or version "release")))

(defvar *packages-directory* nil
  "The folder Tuatara keeps the packages it fetches in, in place of
~/.local/share/tuatara/packages/ ($XDG_DATA_HOME/tuatara/packages/); NIL for
that standard place. As *CACHE-DIRECTORY*: made by you, and while it is not
there no package is fetched or loaded from it.")

(defun packages-directory ()
  "Where fetched packages are kept: *PACKAGES-DIRECTORY* when set
\(TUATARA.IO:CHOSEN-DIRECTORY), else tuatara/packages/ in your data folder."
  (tuatara.io:native (tuatara.io:chosen-directory '*packages-directory*
                                                  (merge-pathnames "tuatara/packages/" (tuatara.io:data-home)))))

(defun package-kept-folder (where name)
  "The folder the package NAME of the repository at WHERE is kept in, a folder
for each commit of it fetched."
  (format nil "~a~a/~a/" (packages-directory) (tuatara.io:path-file-name where) name))

(defun package-folder-for (where name commit)
  "The folder a commit of the repository at WHERE is kept in for the package NAME."
  (format nil "~a~a/" (package-kept-folder where name) commit))

;;; Declaring packages: load-package

(defvar *package-folder* nil
  "The folder of the package being loaded, for LOAD-PACKAGE-FILE.")

(defvar *loaded-packages* '()
  "The names of the packages loaded this session.")

(defvar *fetching-packages* '()
  "The names of the packages being fetched.")

(defvar *package-failures* '()
  "Why packages were not fetched or loaded this session, each (NAME . WORDS).")

(defvar *refused-packages* '()
  "The names of the packages refused this session for using names this
Tuatara lacks; why is in *PACKAGE-FAILURES*.")

(defvar *declared-packages* '()
  "The packages load-package names this session, each (NAME SOURCE VERSION
FILE FOLDER); the first named first.")

(defun declared-package (name)
  "The package called NAME as load-package named it this session, (NAME SOURCE
VERSION FILE FOLDER), or NIL."
  (find name *declared-packages* :key #'first :test #'string=))

(defvar *package-configs* '()
  "The :config forms of each package named, each (NAME . FORMS), evaluated once
it has loaded.")

(defvar *deferred-packages* '()
  "The names of the packages named to be loaded later (:defer, :commands,
:modes) that are not loaded yet.")

(defvar *packages-waiting* '()
  "The packages waiting for another to load first (:after), each (OTHER NAME
. FUNCTION), FUNCTION loading NAME, the one waiting.")

(defvar *commands-after-load* '()
  "Commands run before their package had loaded, waiting to run once it has,
each (NAME COMMAND . FUNCTION): FUNCTION runs COMMAND of the package NAME.")

(defvar *start-package-times* nil
  "While the init file is read, the packages loaded meanwhile, each (NAME .
MILLISECONDS), after :START; NIL otherwise.")

(defparameter *load-package-keywords* '(:source :version :file :folder :disabled :if :defer :after :commands :modes :init :config)
  "What load-package takes after a package's name, each followed by its value
or, for :commands, :modes, :init and :config, by any number of them.")

(defun load-package-options (options)
  "OPTIONS, what load-package was given after the name, as a property list of
each keyword and the list of what followed it; an error naming one it does
not take, or a second value after one that takes one: another load-package
there is said to be inside this one, a parenthesis left open before it."
  (let ((parsed '()) (key nil) (items '()))
    (flet ((close-key ()
             (when key
               (when (and (rest items) (not (member key '(:commands :modes :init :config))))
                 (let ((extra (first (last items 2))))
                   (if (and (consp extra) (symbolp (first extra)) (string-equal (first extra) "LOAD-PACKAGE"))
                       (error "load-package ~s is inside this one, after ~(~s~): a parenthesis is left open before it"
                              (second extra) key)
                       (error "load-package's ~(~s~) takes one value, not also ~s" key extra))))
               (setf (getf parsed key) (reverse items)))))
      (dolist (item options)
        (if (and (keywordp item) (or (member item *load-package-keywords*) (null key) (not (member key '(:init :config)))))
            (progn
              (unless (member item *load-package-keywords*)
                (error "load-package takes ~{~(~s~)~^, ~}, not ~(~s~)" *load-package-keywords* item))
              (close-key)
              (setf key item items '()))
            (progn
              (unless key (error "load-package takes a keyword after the name, not ~s" item))
              (push item items))))
      (close-key))
    parsed))

(defmacro tuatara-user:load-package (name &rest options)
  "Load the package NAME (a string: \"example\"), or set up a part of
Tuatara, the settings and keys of it together. What follows the name, each
keyword with its value, or for the last four with any number of them:

:source  where it comes from: a source's name (package-source), a git
         repository's address or a folder of your disk. Without it, NAME is
         \"tuatara\", a mode's name (\"org\" for org-mode) or a name of
         your own for a group of settings (\"writing\"), which load nothing
         and run :init and :config at once. A name a letter or two from a
         package's or a mode's is taken for a mistyping of it: an error,
         saying which.
:version the tag, branch or commit fetched: the repository's newest tag by
         date when not given or \"release\", its newest commit for \"latest\"
         or when not given and it has no tag yet.
:folder  its folder in the repository or source folder, when not NAME.
:file    its first file, when not NAME.lisp or load.lisp.
:disabled t to leave the whole block out, as if it were not there.
:if      a form: the block is left out when it gives NIL, as on another
         machine, (:if (string= (machine-instance) \"laptop\")).
:defer   t to load it once the editor has started rather than while the init
         file is read.
:after   another package's or group's name: loaded, or for a group its
         :config run, only once that one has. There is no :before: A
         before B is B :after A.
:commands commands of it, loaded when one of them is first run (\\[execute-extended-command] offers
         them meanwhile, a key may be bound to them).
:modes   modes, loaded when a buffer is first in one of them (\"csv\" for csv-mode).
:init    forms evaluated at once, before it loads.
:config  forms evaluated once it has loaded: its settings and keys (bind-key
         with :mode for keys of a mode).

For a group of settings, \"tuatara\" or a mode's block, nothing loads: :init
runs at once, and :defer, :after, :commands and :modes say when its :config
runs, at once when none is given.

  (load-package \"example\" :source \"mine\" :commands example-run
    :config
    (setf *example-arguments* \"-H\")
    (bind-key \"C-c r\" 'example-run :mode 'csv))

A package of a repository is fetched in the background the first time, and
asked about before it is compiled and loaded (*package-ask-before-loading*);
it is loaded from where it is kept at every start after. Each package loaded
is said in *Messages* with the time it took, and how many at the end of the
init file; one that fails is said, the rest of the init file read all the
same. One whose code uses a function or variable this Tuatara lacks is
refused, naming them, and what of it loaded is taken out
\(LOAD-PACKAGE-FILE). Returns the file loaded, or NIL when it is loaded
later or nothing is."
  (let ((parsed (load-package-options options)))
    (flet ((one (key) (first (getf parsed key))))
      `(declare-package ,name :left-out ,(cond ((one :disabled) :disabled)
                                               ((getf parsed :if) `(and (not ,(one :if)) :if)))
                              :source ,(one :source) :version ,(one :version) :file ,(one :file)
                              :folder ,(one :folder) :defer ,(one :defer) :after ,(one :after)
                              :commands ',(getf parsed :commands) :modes ',(getf parsed :modes)
                              :init ',(getf parsed :init) :config ',(getf parsed :config)))))

(defun run-package-forms (name what forms)
  "Evaluate FORMS, the :init or :config (WHAT) of the package NAME, in the init
file's package, each by itself, the compiler's warnings unsaid (a setting
named before its package defines it): one that fails is said, with why, and
the others still evaluated."
  (let ((*package* (find-package :tuatara-user)))
    (dolist (form forms)
      (handler-case (handler-bind ((warning #'muffle-warning)) (eval form))
        (error (e)
          (message "~a's ~(~s~): ~a failed: ~a" name what (let ((*print-length* 4) (*print-level* 2)) (prin1-to-string form)) e))))))

(defun package-loaded-p (name)
  "True when the package NAME has loaded this session."
  (member name *loaded-packages* :test #'string=))

(defun package-loaded (name)
  "What follows the package NAME's loading: it counts as loaded, its :config is
evaluated, the packages waiting for it (:after) load, and the commands of it
run before it had loaded run (*COMMANDS-AFTER-LOAD*)."
  (pushnew name *loaded-packages* :test #'string=)
  (run-package-forms name :config (cdr (assoc name *package-configs* :test #'string=)))
  (let ((waiting (remove name *packages-waiting* :key #'car :test-not #'string=)))
    (setf *packages-waiting* (remove name *packages-waiting* :key #'car :test #'string=))
    (dolist (entry (reverse waiting)) (funcall (cddr entry))))
  (let ((commands (remove name *commands-after-load* :key #'first :test-not #'string=)))
    (setf *commands-after-load* (remove name *commands-after-load* :key #'first :test #'string=))
    (dolist (entry (reverse commands)) (defer-action (cddr entry)))))

(defun loading-package (name where load)
  "Load the package NAME by calling LOAD, then what follows (PACKAGE-LOADED),
timed: said in *Messages*, WHERE being where it is loaded from, with the
milliseconds it took; a failure said on the echo row too, the package not
counted as loaded. Returns what LOAD returned, or NIL when it failed."
  (let ((start (get-internal-real-time)))
    (handler-case
        (multiple-value-prog1 (funcall load)
          (package-loaded name)
          (let ((ms (round (tuatara.runtime:milliseconds-since start))))
            (when *start-package-times* (push (cons name ms) (cdr *start-package-times*)))
            (log-message (format nil "Loading package ~a from ~a...done (~d ms)" name where ms))))
      (error (e)
        (push (cons name (princ-to-string e)) *package-failures*)
        (if (typep e 'package-lacks-names)
            (progn (pushnew name *refused-packages* :test #'string=)
                   (message "~a" e))
            (message "Loading package ~a...failed: ~a" name e))
        nil))))

(defun package-command-stub (command name start)
  "Make COMMAND, a command of the package NAME not loaded yet, one that calls
START to load it and then runs the command the package has of that name: at
once when the package loads at once, else once it has (asked about first, or
fetched), or saying why it could not load; a command of that name there is
already is left as it is."
  (unless (find-command (symbol-name command))
    (let (stub)
      (labels ((real ()
                 (let ((other (loop for s being the hash-keys of *commands*
                                    when (and (not (eq s command)) (string= (symbol-name s) (symbol-name command)))
                                      return s)))
                   (cond (other (gethash other *commands*))
                         ((not (eq (symbol-function command) stub)) (symbol-function command)))))
               (run-real ()
                 (let ((real (real)))
                   (when real
                     (setf (symbol-function command) real
                           (gethash command *commands*) real)
                     (funcall real)
                     t))))
        (setf stub (lambda ()
                     (funcall start)
                     (let ((failure (cdr (assoc name *package-failures* :test #'string=))))
                       (cond ((run-real))
                             (failure (message "~(~a~) is of the package ~a, which could not be loaded: ~a" command name failure))
                             (t (setf *commands-after-load*
                                      (cons (list* name command #'run-real) (remove command *commands-after-load* :key #'second)))
                                (message "~(~a~) runs once the package ~a has loaded~:[~; (being fetched)~]"
                                         command name (member name *fetching-packages* :test #'string=)))))))
        (setf (symbol-function command) stub
              (documentation command 'function)
              (format nil "A command of the package ~a: running it loads the package (its load-package's :commands), then runs the package's own." name)
              (gethash command *commands*) stub)))))

(defun arrange-package-load (name load &key defer after commands modes)
  "Have LOAD, a function loading the package NAME, called once: at once; with
AFTER, once the package AFTER has loaded; with COMMANDS or MODES, when one of
those commands is first run or a buffer is first in one of those modes; with
DEFER alone, once the editor has started."
  (let* ((started nil)
         (start (lambda ()
                  (unless started
                    (setf started t
                          *deferred-packages* (remove name *deferred-packages* :test #'string=))
                    (if (and after (not (package-loaded-p after)))
                        (push (list* after name load) *packages-waiting*)
                        (funcall load))))))
    (cond ((or commands modes)
           (pushnew name *deferred-packages* :test #'string=)
           (dolist (command commands) (package-command-stub command name start))
           (dolist (mode modes)
             (add-mode-hook (mode-named mode) (lambda (buffer) (declare (ignore buffer)) (funcall start)))))
          (defer
           (pushnew name *deferred-packages* :test #'string=)
           (defer-action start))
          (t (funcall start)))))

(defun edit-distance (a b)
  "How many letters must be put in, taken out, changed or swapped with the one
beside them to make the string A into B (Damerau's distance, each letter
changed once at most), case ignored."
  (let ((d (make-array (list (1+ (length a)) (1+ (length b))))))
    (dotimes (i (1+ (length a))) (setf (aref d i 0) i))
    (dotimes (j (1+ (length b))) (setf (aref d 0 j) j))
    (loop for i from 1 to (length a)
          do (loop for j from 1 to (length b)
                   do (let ((cost (if (char-equal (char a (1- i)) (char b (1- j))) 0 1)))
                        (setf (aref d i j) (min (1+ (aref d (1- i) j)) (1+ (aref d i (1- j))) (+ (aref d (1- i) (1- j)) cost)))
                        (when (and (> i 1) (> j 1)
                                   (char-equal (char a (1- i)) (char b (- j 2)))
                                   (char-equal (char a (- i 2)) (char b (1- j))))
                          (setf (aref d i j) (min (aref d i j) (1+ (aref d (- i 2) (- j 2)))))))))
    (aref d (length a) (length b))))

(defun known-package-name-near (name)
  "The name of a package, of Tuatara itself or of a mode NAME is a letter or
two away from, as a mistyping of it would be, or NIL: one letter for a name
of four letters or fewer, two for a longer one."
  (let ((most (if (<= (length name) 4) 1 2)))
    (find-if (lambda (known) (<= 1 (edit-distance name known) most))
             (append (mapcar #'first *declared-packages*) (list "tuatara")
                     (mapcar (lambda (mode) (let ((text (string-downcase (symbol-name mode))))
                                              (if (uiop:string-suffix-p text "-mode") (subseq text 0 (- (length text) 5)) text)))
                             (mode-names))))))

(defun declare-package (name &key left-out source version file folder defer after commands modes init config)
  "Load the package NAME as LOAD-PACKAGE says, each keyword as it takes it,
INIT and CONFIG the forms after :init and :config; LEFT-OUT :disabled or :if
when the block is left out, said in *Messages*. Returns the file loaded, or
NIL when it is loaded later or nothing is."
  (when left-out
    (log-message (format nil "~a is left out (~(~s~))" name left-out))
    (return-from declare-package nil))
  (unless (and (stringp name) (cl-ppcre:scan "^[a-z0-9][a-z0-9-]*$" name))
    (error "A package's name is small letters, digits and -: ~s" name))
  (when (and after (not (and (stringp after) (cl-ppcre:scan "^[a-z0-9][a-z0-9-]*$" after))))
    (error "A package's :after is another package's name: ~s" after))
  (dolist (command commands)
    (unless (symbolp command) (error "A package's :commands are commands' names: ~s" command)))
  (setf modes (mapcar (lambda (mode) (if (stringp mode) (intern (string-upcase mode) :keyword) mode)) modes))
  (mapc #'mode-named modes)
  (setf *package-configs* (acons name config (remove name *package-configs* :key #'car :test #'string=)))
  (flet ((declared (entry)
           (setf *declared-packages* (append (remove name *declared-packages* :key #'first :test #'string=)
                                             (list entry))))
         (arrange (load)
           (let ((result nil))
             ;; a package still being fetched loads later: no file yet
             (arrange-package-load name (lambda () (let ((loaded (funcall load)))
                                                     (setf result (and (or (stringp loaded) (pathnamep loaded)) loaded))))
                                   :defer defer :after after :commands commands :modes modes)
             result)))
    (cond (source
           (run-package-forms name :init init)
           (unless (and (stringp source) (plusp (length source)))
             (error "A package's :source is a source's name, an address or a folder: ~s" source))
           (check-package-where (package-where source))
           (when (and version (not (and (stringp version) (cl-ppcre:scan "^[^\\s~^:?*\\[\\\\]+$" version))))
             (error "A package's :version is a tag, a branch or a commit: ~s" version))
           (when (and file (not (and (stringp file) (cl-ppcre:scan "^[A-Za-z0-9._-]+\\.lisp$" file))))
             (error "A package's :file is a .lisp file of its folder: ~s" file))
           (when (and folder (not (and (stringp folder) (cl-ppcre:scan "^[A-Za-z0-9_][A-Za-z0-9._-]*$" folder))))
             (error "A package's :folder is the name of a folder in its repository or source folder: ~s" folder))
           (declared (list name source version file folder))
           (arrange (lambda () (load-package-from name source version file))))
          (t
           (let ((mode (and (string/= name "tuatara") (ignore-errors (mode-named (intern (string-upcase name) :keyword)))))
                 (near nil))
             (unless (or mode (string= name "tuatara"))
               (setf near (known-package-name-near name))
               (when near
                 (error "No package or mode called ~a: did you mean ~a?" name near)))
             (run-package-forms name :init init)
             (arrange (lambda ()
                        (let ((start (get-internal-real-time)))
                          (package-loaded name)
                          (log-message (format nil "Settings ~a (~a) set in ~d ms" name
                                               (cond (mode (string-downcase (symbol-name mode)))
                                                     ((string= name "tuatara") "Tuatara itself")
                                                     (t "your own group"))
                                               (round (tuatara.runtime:milliseconds-since start)))))
                        nil)))))))

(defun call-with-start-packages (read)
  "Call READ, which reads the init file, the packages it loads meanwhile timed;
then say in *Messages* how many loaded and in how long, how many wait to be
loaded later, and which wait for a package no load-package names."
  (let ((*start-package-times* (list :start)))
    (multiple-value-prog1 (funcall read)
      (let ((times (rest *start-package-times*)))
        (when (or times *deferred-packages* *fetching-packages*)
          (log-message (format nil "Packages: ~d loaded in ~d ms~[~:;, ~:*~d to load later~]~[~:;, ~:*~d being fetched~]"
                               (length times) (reduce #'+ times :key #'cdr)
                               (length *deferred-packages*) (length *fetching-packages*))))
        (dolist (waiting *packages-waiting*)
          (unless (or (declared-package (car waiting))
                      (member (car waiting) *deferred-packages* :test #'string=))
            (message "~a waits for ~a (:after), which no load-package before it or after it names" (second waiting) (car waiting))))))))

;;; Loading what is there


(defvar *tuatara-identity* nil
  "Which Tuatara compiles packages, once known (TUATARA-IDENTITY).")

(defun tuatara-identity ()
  "Which Tuatara this is, as far as compiled packages go: the moment the saved
executable was built, or, run from its sources, the newest of them; another
one compiles packages anew, as what they were compiled against may have
changed."
  (or *tuatara-identity*
      (setf *tuatara-identity*
            (format nil "~:[sources-~;built-~]~d" tuatara:*built-at*
                    (or tuatara:*built-at*
                        (let ((top (asdf:system-source-directory :tuatara)))
                          (loop for file in (append (directory (merge-pathnames "src/**/*.lisp" top))
                                                    (directory (merge-pathnames "lisp/**/*.lisp" top)))
                                maximize (or (file-write-date file) 0))))))))

(defun package-cache ()
  "The folder packages are compiled into for this Tuatara and this SBCL; those
of other ones are deleted the first time it is asked for."
  (let* ((sbcl (format nil "~apackages/~a/" (tuatara.io:native (tuatara.io:cache-directory))
                       (tuatara.io:percent-encoded (lisp-implementation-version) :safe "._-")))
         (here (format nil "~a~a/" sbcl (tuatara-identity))))
    (unless (probe-file here)
      (dolist (other (append (ignore-errors (uiop:subdirectories sbcl)) (ignore-errors (uiop:directory-files sbcl))))
        (delete-package-folder (namestring other))))
    here))

(defun package-fasl (file)
  "Where the compiled FILE of a package is kept: in the cache, by which
Tuatara and which SBCL compile it (PACKAGE-CACHE), in folders as the file's
own are, fetched/ for a package fetched and local/ for one of a folder of yours."
  (let* ((native (tuatara.io:native file))
         (fetched (tuatara.io:native (packages-directory)))
         (inside (if (uiop:string-prefix-p fetched native)
                     (concatenate 'string "fetched/" (subseq native (length fetched)))
                     (concatenate 'string "local" native))))
    (format nil "~a~a.fasl" (package-cache) inside)))

(defun tuatara-user:package-file (name)
  "The file NAME (\"helper.sh\") of the package being loaded: in its
folder, or the folder of the file being loaded when no package is. A package
names its own files so, as where it is loaded from is the cache's."
  (merge-pathnames name (or *package-folder*
                            (and *load-truename* (make-pathname :name nil :type nil :version nil :defaults *load-truename*))
                            *default-pathname-defaults*)))

(defun compile-failure-words (report)
  "Why compiling a file failed, on one line, from REPORT, what the compiler
wrote: the error it caught (a reader error: an unclosed parenthesis, and the
line its form starts on); NIL when it names none."
  (let ((at (search "caught ERROR:" report)))
    (when at
      (let* ((text (subseq report (+ at (length "caught ERROR:"))
                           (or (search "compilation unit" report :start2 at) (length report))))
             (text (cl-ppcre:regex-replace-all "(?m)^;" text ""))
             (text (cl-ppcre:regex-replace-all "#<[^>]*>" text "the file"))
             (text (string-trim " " (cl-ppcre:regex-replace-all "\\s+" text " "))))
        (and (plusp (length text)) (subseq text 0 (min 240 (length text))))))))

;;; Whether the names a package uses are all there

(defun undefined-name (warning)
  "The function or variable WARNING, one of the compiler's, says is defined
nowhere, as two values: :FUNCTION or :VARIABLE, and its name; NIL for any
other warning."
  (when (typep warning 'simple-condition)
    (let ((tail (member-if (lambda (x) (member x '(:function :variable))) (simple-condition-format-arguments warning))))
      (when (and (rest tail) (search "undefined" (princ-to-string warning)))
        (values (first tail) (second tail))))))

(defun needed-name (kind name)
  "The name NAME of the KIND (:FUNCTION or :VARIABLE) a package's code uses
without its definition, as kept beside its compiled file: (KIND PACKAGE
SYMBOL), the names of its symbol's home package and of the symbol, KIND
:SETF-FUNCTION for (setf NAME); NIL for a symbol in no package."
  (let* ((setf-p (consp name))
         (symbol (if setf-p (second name) name))
         (home (and (symbolp symbol) (symbol-package symbol))))
    (and home (list (if setf-p :setf-function kind) (package-name home) (symbol-name symbol)))))

(defun needed-name-p (thing)
  "True when THING is a name as NEEDED-NAME makes it."
  (and (consp thing) (member (first thing) '(:function :setf-function :variable))
       (consp (rest thing)) (stringp (second thing))
       (consp (cddr thing)) (stringp (third thing)) (null (cdddr thing))))

(defun needed-name-symbol (needed)
  "The symbol NEEDED, as NEEDED-NAME makes it, names now; NIL when its
package or the symbol is not there."
  (let ((home (find-package (second needed))))
    (and home (find-symbol (third needed) home))))

(defun needed-name-present-p (needed)
  "True when the function or variable NEEDED (NEEDED-NAME) is defined now."
  (let ((symbol (needed-name-symbol needed)))
    (and symbol
         (ecase (first needed)
           (:function (fboundp symbol))
           (:setf-function (fboundp (list 'setf symbol)))
           (:variable (or (boundp symbol)
                          (member (sb-int:info :variable :kind symbol) '(:special :global :constant :macro))))))))

(defun beside-fasl (fasl type)
  "The file of TYPE kept beside the compiled file FASL: \"needs\", the names
its code uses without defining them (NEEDED-NAME); \"refused\", for the first
file of a package refused for names this Tuatara lacks, its files as they
were then and those names, so that it is refused again before any of it
loads (REFUSE-AS-BEFORE)."
  (concatenate 'string (subseq fasl 0 (- (length fasl) (length "fasl"))) type))

(defun write-kept-list (file list)
  "Write LIST, of keywords, strings, numbers and lists of them, to FILE, which
only you may read."
  (tuatara.io:write-private-file file (with-standard-io-syntax
                                       (let ((*print-readably* nil)) (prin1-to-string list)))))

(defun read-kept-list (file)
  "The list WRITE-KEPT-LIST wrote to FILE, nothing in it evaluated, and true
as a second value; NIL when there is none or it cannot be read."
  (let ((text (ignore-errors (and (probe-file file) (tuatara.io:file-text file)))))
    (when text
      (handler-case (with-standard-io-syntax
                      (let* ((*read-eval* nil)
                             (*package* (find-package :common-lisp))
                             (list (read-from-string text)))
                        (and (listp list) (values list t))))
        (error () nil)))))

(defvar *package-check* nil
  "While a package loads, what is gathered to see that every name its code
uses is there (a PACKAGE-CHECK); NIL otherwise.")

(defvar *loading-package-name* nil
  "The name of the package its load-package is loading, whose names are
checked (LOAD-PACKAGE-FILE); NIL when a file is loaded by itself.")

(defstruct (package-check (:constructor make-package-check (name refusal)) (:copier nil))
  "What is gathered while the package NAME loads: the Lisp packages there
before it, the names its files use without defining them (NEEDS), and the
files loaded, each (FILE . WRITE-DATE) (SOURCES); REFUSAL is the file its
refusal is kept in (BESIDE-FASL)."
  (name "" :read-only t)
  (refusal nil :read-only t)
  (before (list-all-packages) :read-only t)
  (needs '())
  (sources '()))

(setf (documentation 'package-check-name 'function) "The name of the package being checked."
      (documentation 'package-check-refusal 'function) "The file the package's refusal is kept in."
      (documentation 'package-check-before 'function) "The Lisp packages there before the package began to load."
      (documentation 'package-check-needs 'function) "The names the package's files use without defining them."
      (documentation 'package-check-sources 'function) "The package's files loaded, each (FILE . WRITE-DATE).")

(define-condition package-lacks-names (error)
  ((name :initarg :name :reader package-lacks-names-name)
   (names :initarg :names :reader package-lacks-names-names))
  (:report (lambda (condition stream)
             (format stream "~a needs names this Tuatara lacks: ~{~a~^, ~}; update the package (~a) or Tuatara"
                     (package-lacks-names-name condition) (package-lacks-names-names condition)
                     (substitute-command-keys "\\[package-update]"))))
  (:documentation "Signalled when the package NAME uses functions or variables, NAMES in
words, that this Tuatara does not have: it is not loaded."))

(setf (documentation 'package-lacks-names-name 'function) "The name of the package refused."
      (documentation 'package-lacks-names-names 'function) "The names it uses that are not there, in words.")

(defun package-check-own (check)
  "The Lisp packages of the package CHECK is about: those made since it began
to load, and the one of its name when it had loaded before."
  (let ((own (set-difference (list-all-packages) (package-check-before check)))
        (named (find-package (string-upcase (package-check-name check)))))
    (when (and named (package-loaded-p (package-check-name check)))
      (pushnew named own))
    own))

(defun outside-name-p (needed check own)
  "True when NEEDED (NEEDED-NAME) is a name of a Lisp package that was there
before the package CHECK is about began to load, and not one of OWN, its own
\(PACKAGE-CHECK-OWN): none of its files to come can define it."
  (let ((home (find-package (second needed))))
    (and home
         (member home (package-check-before check))
         (not (member home own)))))

(defun needed-name-words (needed own)
  "NEEDED (NEEDED-NAME) in words: its symbol's name, after its package's name
when that is not one of OWN, the package's own Lisp packages."
  (let* ((home (find-package (second needed)))
         (name (string-downcase (third needed)))
         (word (if (and home (member home own))
                   name
                   (format nil "~(~a~)~:[::~;:~]~a" (second needed)
                           (and home (eq (nth-value 1 (find-symbol (third needed) home)) :external))
                           name))))
    (if (eq (first needed) :setf-function) (format nil "(setf ~a)" word) word)))

(defun forget-package-code (own)
  "Take out of Tuatara what the code of the Lisp packages OWN put there, and
delete them: their commands and modes, the keys bound to their commands, and
their functions on the lists of functions Tuatara calls (*FILE-VIEWS* and the
rest). Faces and settings they define stay, doing nothing."
  (when own
    (flet ((theirs-p (thing)
             (let ((name (if (functionp thing) (nth-value 2 (function-lambda-expression thing)) thing)))
               (and name (symbolp name) (member (symbol-package name) own)))))
      (dolist (command (loop for name being the hash-keys of tuatara.command:*commands* collect name))
        (when (theirs-p command) (tuatara.command:forget-command command)))
      (dolist (command (loop for name being the hash-keys of *mode-commands* collect name))
        (when (theirs-p command) (remhash command *mode-commands*)))
      (dolist (mode (mode-names))
        (if (theirs-p mode)
            (forget-major-mode mode)
            (unbind-commands-if #'theirs-p (mode-keymap mode))))
      (when *global-keymap* (unbind-commands-if #'theirs-p *global-keymap*))
      (do-external-symbols (name :tuatara)
        (when (and (boundp name) (not (constantp name)))
          (let ((value (symbol-value name)))
            (when (and (consp value) (ignore-errors (list-length value)) (some #'theirs-p value))
              (setf (symbol-value name) (remove-if #'theirs-p value)))))))
    (dolist (package own)
      (dolist (user (package-used-by-list package))
        (unuse-package package user))
      (delete-package package))))

(defun refuse-package (check needed)
  "Refuse the package CHECK is about, for the names NEEDED (NEEDED-NAME) it
uses that are not there: its files and those names kept to refuse it again
before it loads (BESIDE-FASL), what of it loaded taken out
\(FORGET-PACKAGE-CODE), and a PACKAGE-LACKS-NAMES error signalled."
  (let* ((own (package-check-own check))
         (words (sort (remove-duplicates (mapcar (lambda (n) (needed-name-words n own)) needed) :test #'string=)
                      #'string<)))
    (when (and (package-check-refusal check) (package-check-sources check))
      (ignore-errors (write-kept-list (package-check-refusal check)
                                      (list :sources (package-check-sources check) :needs needed))))
    (tuatara.runtime:debug-report :package "~a refused, its code using names this Tuatara lacks: ~{~a~^, ~}"
                                  (package-check-name check) words)
    (forget-package-code own)
    (setf *loaded-packages* (remove (package-check-name check) *loaded-packages* :test #'string=))
    (error 'package-lacks-names :name (package-check-name check) :names words)))

(defun refuse-as-before (check)
  "Refuse at once the package CHECK is about when it was refused for names
this Tuatara lacks (BESIDE-FASL), its files are as they were then and those
names are not there still; else forget that it was."
  (let ((file (package-check-refusal check)))
    (multiple-value-bind (kept read) (read-kept-list file)
      (when read
        (let* ((needs (ignore-errors (getf kept :needs)))
               (sources (ignore-errors (getf kept :sources)))
               (missing (and (listp needs) (remove-if #'needed-name-present-p (remove-if-not #'needed-name-p needs))))
               (kept-at (or (ignore-errors (file-write-date file)) 0)))
          ;; file times are whole seconds: a file of the refusal's second may have changed after it
          (if (and missing (consp sources) (ignore-errors (list-length sources))
                   (every (lambda (source)
                            (and (consp source) (stringp (car source)) (integerp (cdr source))
                                 (eql (ignore-errors (file-write-date (car source))) (cdr source))
                                 (< (cdr source) kept-at)))
                          sources))
              (refuse-package check missing)
              (ignore-errors (delete-file file))))))))

(defun compile-package-file (file fasl)
  "Compile FILE, of a package, into FASL, and keep beside it the names its
code uses without defining them (BESIDE-FASL), which it returns. Its
other warnings are said in *Messages*, the first of them; a file that cannot
be compiled at all is an error saying why (an unclosed parenthesis, where)."
  (let ((*package* (find-package :tuatara-user))
        (said '())
        (needed '())
        (report (make-string-output-stream)))
    (let ((output (handler-bind ((warning (lambda (w)
                                            (multiple-value-bind (kind name) (undefined-name w)
                                              (cond (kind (let ((entry (needed-name kind name)))
                                                            (when entry (pushnew entry needed :test #'equal))))
                                                    ((not (typep w 'style-warning)) (push (princ-to-string w) said))))
                                            (muffle-warning w))))
                    (let ((*error-output* report))
                      ;; a compilation unit of its own: what it finds undefined is said at its end
                      (with-compilation-unit (:override t)
                        (compile-file file :output-file fasl :verbose nil :print nil))))))
      (unless output
        (error "~a could not be compiled~@[: ~a~]" (abbreviated-path (tuatara.io:native file))
               (or (compile-failure-words (get-output-stream-string report)) (first said))))
      (write-kept-list (beside-fasl fasl "needs") needed)
      (when said
        (log-message (format nil "~a compiled with ~d warning~:p: ~a" (abbreviated-path (tuatara.io:native file))
                             (length said) (first (last said)))))
      needed)))

(defun load-checked-file (file check)
  "Compile FILE when it has changed since (COMPILE-PACKAGE-FILE) and load it.
With CHECK, the names its code uses without defining them are added to it,
and it is refused before it loads when one of them is another's and not
there (OUTSIDE-NAME-P)."
  (let ((fasl (package-fasl file)))
    (tuatara.io:ensure-private-directory (directory-namestring fasl))
    (let ((needed (multiple-value-bind (kept read) (read-kept-list (beside-fasl fasl "needs"))
                    ;; file times are whole seconds: a fasl of the same second as its file may
                    ;; be older than the file's last change, and is made again
                    (if (and read (ignore-errors (every #'needed-name-p kept))
                             (probe-file fasl) (> (file-write-date fasl) (file-write-date file)))
                        kept
                        (compile-package-file file fasl)))))
      (when check
        (push (cons (namestring file) (file-write-date file)) (package-check-sources check))
        (when needed
          (let* ((own (package-check-own check))
                 (missing (remove-if (lambda (n) (or (not (outside-name-p n check own)) (needed-name-present-p n))) needed)))
            (when missing (refuse-package check missing))))
        (setf (package-check-needs check) (union needed (package-check-needs check) :test #'equal)))
      (let ((*package* (find-package :tuatara-user))
            (*package-folder* (make-pathname :name nil :type nil :version nil :defaults file)))
        (load fasl)))))

(defun tuatara-user:load-package-file (name)
  "Compile, when it has changed since, and load the file NAME (\"example.lisp\")
of the package being loaded, or of the folder of the file being loaded: a
package's entry loads its other files so. Compiled into the cache once, a
package loads in a few milliseconds at the next start; meanwhile
PACKAGE-FILE names its files from its own folder. Its warnings are said in
*Messages*, the first of them; a file that cannot be compiled at all is an
error saying why (an unclosed parenthesis, where).

Every function and variable a package's code uses must be there once all of
its files have loaded, Tuatara's above all: a package load-package loads
that uses a name this Tuatara lacks is refused, with an error naming them,
and what of it loaded is taken out again. A name of Tuatara's or of another
library is looked for before the file using it loads; a package refused is
refused again before any of it loads, until its files or Tuatara change."
  (let ((file (tuatara-user:package-file name)))
    (unless (probe-file file) (error "The package has no file ~a" (abbreviated-path (tuatara.io:native file))))
    (if (or *package-check* (not *loading-package-name*))
        (load-checked-file file *package-check*)
        (let* ((check (make-package-check *loading-package-name* (beside-fasl (package-fasl file) "refused")))
               (*package-check* check))
          (refuse-as-before check)
          (load-checked-file file check)
          (let ((missing (remove-if #'needed-name-present-p (package-check-needs check))))
            (when missing (refuse-package check missing)))))
    file))

(defun claim-package-setting (name owner)
  "The symbol of the setting NAME (a string) of the package whose Lisp package
is OWNER: in TUATARA-USER, exported, so that an init file names it as it
names Tuatara's own, and made OWNER's in place of a symbol of that name of
its own. An error when NAME does not begin with OWNER's name (*NAME-), or is
already a setting of Tuatara or of another package."
  (let ((prefix (format nil "*~a-" (package-name owner))))
    (unless (and (> (length name) (length prefix)) (string= prefix name :end2 (length prefix)))
      (error "A setting of the package ~(~a~) is named ~(~a~)...*, not ~(~a~)" (package-name owner) prefix name))
    (multiple-value-bind (symbol status) (find-symbol name :tuatara-user)
      (when (and status (not (eq status :internal))
                 (not (equal (get symbol 'package-setting) (package-name owner))))
        (error "~(~a~) is already a setting of ~a" name
               (let ((other (get symbol 'package-setting))) (if other (format nil "the package ~(~a~)" other) "Tuatara"))))
      (let ((symbol (or symbol (intern name :tuatara-user)))
            (own (find-symbol name owner)))
        (setf (get symbol 'package-setting) (package-name owner))
        (export symbol :tuatara-user)
        (unless (eq own symbol)
          (when (and own (eq (symbol-package own) owner)) (unintern own owner))
          (import symbol owner))
        symbol))))

(defmacro define-package-setting (name value documentation)
  "Define NAME, a setting of the package being loaded, with VALUE unless an
init file set it first, and DOCUMENTATION: in TUATARA-USER, where an init file
sets it by that name, and NAME in the package's own code
\(CLAIM-PACKAGE-SETTING). Its name begins with the package's (*facematch-).
It comes before the package's code that reads it, at the top of its file: a
name read before it is defined is the package's own, not the setting."
  (let ((owner (package-name *package*)))
    `(progn
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (claim-package-setting ,(symbol-name name) (find-package ,owner)))
       (defvar ,(claim-package-setting (symbol-name name) *package*) ,value ,documentation))))

(defun package-entry (folder name file)
  "The file a package called NAME in FOLDER is loaded by: FILE when given,
else NAME.lisp, else load.lisp; NIL when there is none."
  (some (lambda (entry) (and entry (probe-file (merge-pathnames entry folder)) entry))
        (list file (concatenate 'string name ".lisp") "load.lisp")))

(defun load-package-folder (folder name file)
  "Load the package called NAME in FOLDER by its entry (PACKAGE-ENTRY), its
files compiled, then what follows (LOADING-PACKAGE): its :config, the
packages waiting for it, the time it took said. Returns its entry's file, or
NIL when it failed."
  (loading-package name (abbreviated-path (tuatara.io:native folder))
                   (lambda ()
                     (let ((entry (or (package-entry folder name file)
                                      (error "The package ~a has no ~:[~a.lisp or load.lisp~;~:*~a~] in ~a" name file name
                                             (abbreviated-path (tuatara.io:native folder))))))
                       (let ((*package-folder* folder) (*loading-package-name* name) (*package-check* nil))
                         (tuatara-user:load-package-file entry))))))

(defun package-question (name where folder then)
  "Ask whether to compile and load the package NAME from WHERE, kept in
FOLDER: y calls THEN, v lists its folder, opened safely, n leaves it;
THEN at once with *PACKAGE-ASK-BEFORE-LOADING* NIL."
  (if (not *package-ask-before-loading*)
      (funcall then)
      (loop
        (let ((answer (string-trim " " (read-string (format nil "Compile and load the package ~a from ~a? (y, n, or v to view its source) "
                                                            name (abbreviated-path where))))))
          (cond ((string-equal answer "y") (return (funcall then)))
                ((string-equal answer "v")
                 (rimu-show-directory (tuatara.io:native folder) :safe t :root (tuatara.io:native folder) :back (current-buffer))
                 (return (message (substitute-command-keys "~a is shown; load-package asks again at the next start, \\[package-update] now" :for-format t)
                                  name)))
                ((string-equal answer "n") (return (message "The package ~a is not loaded" name))))))))

;;; Fetching

(defun git-for-packages ()
  "The git program packages are fetched with, or an error saying it is missing."
  (or (tuatara.io:find-program "git") (tuatara.io:program-missing "git" :why "packages are fetched with it")))

(defun package-git-environment ()
  "What git is given when it fetches packages: what it is given to push and
pull now (GIT-NETWORK-ENVIRONMENT), and besides, symbolic links in a package
checked out as small files of their target's name, never leading outside it,
and git-lfs's filters, which would ask other servers, left out."
  (append (git-network-environment '("core.symlinks" "false") '("filter.lfs.smudge" "") '("filter.lfs.process" "")
                                   '("filter.lfs.required" "false"))
          (list "GIT_LFS_SKIP_SMUDGE=1")))

(defun package-git (directory &rest arguments)
  "Run git with ARGUMENTS in DIRECTORY for a package's fetching: what it
printed, as text; an error with what it said when it fails. A package's own
scratch repository may fetch the files a sparse checkout needs from its
address as it goes."
  (multiple-value-bind (octets ok too-large said)
      (let ((*git-safety* (remove "--no-lazy-fetch" *git-safety* :test #'string=)))
        (run-git-program (git-for-packages) directory arguments :environment (package-git-environment) :errors t))
    (declare (ignore too-large))
    (unless ok
      (error "~a" (git-failure-words (first arguments) (or said ""))))
    (if octets (tuatara.text:octets-string octets) "")))

(defun package-commit-name (text where)
  "TEXT, a commit's name git printed for the repository at WHERE, checked to be
one (hexadecimal, 7 to 64 digits); an error otherwise."
  (let ((name (string-trim '(#\Newline #\Space #\Tab) text)))
    (unless (cl-ppcre:scan "^[0-9a-f]{7,64}$" name)
      (error "~a gave no commit's name: ~s" where (subseq name 0 (min 80 (length name)))))
    name))

(defun package-commit (directory where version)
  "The commit, and its tag or NIL, VERSION of the repository at WHERE is,
asking it from DIRECTORY, a repository of no commit made for it: \"latest\",
its main branch's newest commit; \"release\", its newest tag by date, its
name never read; NIL, the same, or its newest commit when it has no tag; else
that tag, branch or commit as written. Every commit's name is checked to be
one (PACKAGE-COMMIT-NAME)."
  (labels ((first-word (text) (first (uiop:split-string (string-trim '(#\Newline #\Space) text) :separator '(#\Space #\Tab))))
           (newest-commit ()
             (values (package-commit-name (or (first-word (package-git directory "ls-remote" where "HEAD"))
                                              (error "~a has no main branch" where))
                                          where)
                     nil))
           (newest-tag ()
             ;; the tags' commits alone, to find the newest by date
             (package-git directory "fetch" "--quiet" "--depth=1" "--filter=tree:0" where "+refs/tags/*:refs/tags/*")
             (let* ((line (string-trim '(#\Newline #\Space)
                                       (package-git directory "for-each-ref" "--sort=-creatordate" "--count=1"
                                                    "--format=%(if)%(*objectname)%(then)%(*objectname)%(else)%(objectname)%(end) %(refname:strip=2)"
                                                    "refs/tags")))
                    (space (position #\Space line)))
               (values (package-commit-name (subseq line 0 space) where) (subseq line (1+ space))))))
    (cond ((equal version "latest") (newest-commit))
          ((member version '(nil "release") :test #'equal)
           (cond ((plusp (length (string-trim '(#\Newline #\Space) (package-git directory "ls-remote" "--tags" where))))
                  (newest-tag))
                 ((null version) (newest-commit))
                 (t (error "~a has no tagged version; :version \"latest\" takes its newest commit" where))))
          (t
           (let* ((listed (package-git directory "ls-remote" where
                                       (format nil "refs/tags/~a" version) (format nil "refs/tags/~a^{}" version)
                                       (format nil "refs/heads/~a" version)))
                  (lines (tuatara.text:split-words listed '(#\Newline)))
                  (peeled (find-if (lambda (line) (search "^{}" line)) lines)))
             (cond (lines (values (package-commit-name (first-word (or peeled (first lines))) where)
                                  (and (search "refs/tags/" (or peeled (first lines))) version)))
                   ((cl-ppcre:scan "^[0-9a-f]{40}(?:[0-9a-f]{24})?$" version) (values version nil))
                   ((cl-ppcre:scan "^[0-9a-f]{7,64}$" version)
                    (error "~a has no tag or branch called ~a, and a commit is fetched by its whole name, ~
                            40 hexadecimal digits (git log shows it)" where version))
                   (t (error "~a has no tag, branch or commit called ~a" where version))))))))

(defun delete-package-folder (folder)
  "Delete FOLDER, with all in it, or a file, when it is inside the folders
packages are kept or compiled in, nothing it holds followed when it is a link (DELETE-TREE);
an error for any other folder."
  (let ((native (tuatara.io:native folder)))
    (unless (or (tuatara.io:path-within-p native (tuatara.io:native (packages-directory)))
                (tuatara.io:path-within-p native (format nil "~apackages/" (tuatara.io:native (tuatara.io:cache-directory)))))
      (error "~a is no folder of the packages': not deleted" (abbreviated-path native)))
    (tuatara.io:delete-tree native)))

(defparameter *package-most-octets* (* 512 1024 1024)
  "The most octets a package may take on disk while it is fetched, what git
keeps of its history and its files together: a fetch that grows past it is
ended and its files deleted.")

(define-condition package-too-large (error)
  ((where :initarg :where :reader package-too-large-where))
  (:report (lambda (condition stream)
             (format stream "~a takes more than ~a: the package was not fetched"
                     (package-too-large-where condition) (tuatara.io:octets-words *package-most-octets*))))
  (:documentation "Signalled when a package fetched from WHERE grows past *PACKAGE-MOST-OCTETS*."))

(setf (documentation 'package-too-large-where 'function) "Where the package too large was fetched from.")

(defun call-within-package-size (folder where thunk)
  "Call THUNK, which fetches into FOLDER on the thread of a job, and look at
how much FOLDER takes every second meanwhile: past *PACKAGE-MOST-OCTETS*, the
program the job runs is ended and PACKAGE-TOO-LARGE signalled, also when
THUNK has ended by then."
  (let* ((job tuatara.runtime:*job*) (most *package-most-octets*)
         (too-large nil)
         (too-large-p (lambda () (> (or (rimu-tree-size folder) 0) most)))
         (timer (sb-ext:make-timer (lambda ()
                                     (when (and (not too-large) (funcall too-large-p))
                                       (setf too-large t)
                                       (let ((process (and job (tuatara.runtime:job-process job))))
                                         (when process (ignore-errors (sb-ext:process-kill process 15))))))
                                   :thread t)))
    (flet ((refuse () (error 'package-too-large :where where)))
      (sb-ext:schedule-timer timer 1 :repeat-interval 1)
      (multiple-value-prog1
          (unwind-protect (handler-bind ((error (lambda (e) (declare (ignore e)) (when too-large (refuse)))))
                            (funcall thunk))
            (sb-ext:unschedule-timer timer))
        (when (or too-large (funcall too-large-p)) (refuse))))))

(defun fetch-package (name where version)
  "Fetch the package called NAME, VERSION of the repository at WHERE: that
commit alone, and its folder (PACKAGE-REPOSITORY-FOLDER) alone when it has
one, else the whole of it; a repository that gives no file by itself after its folders, that commit
whole; never more than *PACKAGE-MOST-OCTETS* (CALL-WITHIN-PACKAGE-SIZE), finding
the commit as well as checking it out.
Returns the folder it is kept in, its commit and its tag."
  (let* ((scratch (format nil "~a.fetching-~a/" (packages-directory) (tuatara.io:random-name)))
         (probe (format nil "~aprobe/" scratch))
         (inside-name (package-repository-folder name)))
    (tuatara.io:ensure-private-directory probe)
    (labels ((check-out (files commit tag filter)
               (call-within-package-size scratch where (lambda () (check-out-within files commit tag filter))))
             (check-out-within (files commit tag filter)
               (tuatara.io:ensure-private-directory files)
               (package-git files "init" "--quiet")
               (apply #'package-git files "fetch" "--quiet" "--depth=1"
                      (append (and filter '("--filter=blob:none"))
                              (list where (if tag (format nil "refs/tags/~a" tag) commit))))
               (let ((inside (string-trim '(#\Newline #\Space)
                                          (package-git files "ls-tree" "-d" "--name-only" commit inside-name))))
                 (when (string= inside inside-name)
                   (package-git files "sparse-checkout" "set" "--no-cone" (format nil "/~a/" inside-name))))
               (package-git files "checkout" "--quiet" commit)))
      (unwind-protect
           (progn
             (multiple-value-bind (commit tag)
                 (call-within-package-size scratch where
                                           (lambda ()
                                             (package-git probe "init" "--quiet")
                                             (package-commit probe where version)))
               (let ((folder (package-folder-for where name commit))
                     (files (format nil "~afiles/" scratch)))
                 (unless (probe-file folder)
                   (handler-case (check-out files commit tag t)
                     (package-too-large (e) (error e))
                     (error ()
                       (delete-package-folder files)
                       (setf files (format nil "~awhole/" scratch))
                       (check-out files commit tag nil)))
                   (tuatara.io:ensure-private-directory (directory-namestring (string-right-trim "/" folder)))
                   ;; a fetch of the same commit beside this one may have kept it first
                   (handler-case (sb-posix:rename (string-right-trim "/" files) (string-right-trim "/" folder))
                     (sb-posix:syscall-error (e) (unless (probe-file folder) (error e)))))
                 (values folder commit tag))))
        (when (probe-file scratch)
          (delete-package-folder scratch))))))

(defun package-repository-folder (name)
  "The folder the package NAME is in within its repository or source folder:
its load-package's :folder, else NAME."
  (or (fifth (declared-package name)) name))

(defun package-folder-in (folder name)
  "The folder of the package NAME within FOLDER, a repository's or a source
folder: its own folder there (PACKAGE-REPOSITORY-FOLDER) when it has one, else
FOLDER itself."
  (let ((inside (format nil "~a~a/" folder (package-repository-folder name))))
    (if (probe-file inside) inside folder)))

(defun package-folder-of (name where entry)
  "The folder the package NAME from WHERE is loaded from (PACKAGE-FOLDER-IN):
within WHERE when it is a folder of your disk, else within the commit ENTRY,
what is remembered of it, says was fetched; NIL when none was."
  (cond ((local-source-p where)
         (package-folder-in (directory-name where) name))
        ((and entry (string/= (second entry) "-"))
         (package-folder-in (package-folder-for where name (second entry)) name))))

(defstruct (package-fetch (:include tuatara.runtime:job) (:constructor make-package-fetch (name where version file key))
                          (:copier nil))
  "The package NAME being fetched from WHERE, VERSION asked for, FILE its
entry when given, KEY what it is remembered by; once done, its FOLDER,
COMMIT and TAG."
  name where version file key folder commit tag)

(setf (documentation 'package-fetch-p 'function) "True when OBJECT is a PACKAGE-FETCH.")

(tuatara.runtime:define-job-words package-fetch
  :doing "Fetching a package"
  :verb "fetch the package"
  :description (format nil "Fetching the package ~a" (package-fetch-name job))
  :left-words "the package not fetched"
  :at-exit :quiet)

(defmethod tuatara.runtime:job-holds-queue-p ((job package-fetch))
  "NIL: the jobs of a buffer queued behind it need not wait for a package's fetching."
  nil)

(defun start-package-fetch (name where version file key)
  "Fetch the package NAME (FETCH-PACKAGE) in the background, then ask and
load it (PACKAGE-FETCHED)."
  (log-message (format nil "Fetching the package ~a from ~a" name where))
  (pushnew name *fetching-packages* :test #'string=)
  (setf *package-failures* (remove name *package-failures* :key #'car :test #'string=))
  (tuatara.runtime:start-job-thread
   (make-package-fetch name where version file key)
   (lambda (job)
     (multiple-value-bind (folder commit tag) (fetch-package name where version)
       (setf (package-fetch-folder job) folder (package-fetch-commit job) commit (package-fetch-tag job) tag)))
   :name "tuatara package fetch"))

(defmethod tuatara.runtime:job-take-in ((job package-fetch))
  "Ask about and load the package fetched, or say why it was not."
  (setf *fetching-packages* (remove (package-fetch-name job) *fetching-packages* :test #'string=))
  (case (package-fetch-status job)
    (:done (let ((job job))
             (defer-action (lambda () (package-fetched job)))))
    (:failed (let ((why (or (tuatara.runtime:job-error job) "it ended unexpectedly")))
               (push (cons (package-fetch-name job) why) *package-failures*)
               (message "The package ~a was not fetched: ~a" (package-fetch-name job) why)))))

(defun package-fetched (job)
  "Load the package JOB fetched: at once when that commit was let load
before, said to be up to date when it is loaded already; else asked about
first. Its commit is remembered either way."
  (let* ((name (package-fetch-name job)) (key (package-fetch-key job))
         (commit (package-fetch-commit job)) (tag (package-fetch-tag job))
         (folder (package-folder-in (package-fetch-folder job) name))
         (had (fetched-package key))
         (load (lambda ()
                 (state-list-put *fetched-packages* (list (list key commit tag t)))
                 (load-package-folder folder name (package-fetch-file job))
                 (when (package-loaded-p name)
                   (message "The package ~a~@[ ~a~] is loaded" name tag)))))
    (cond ((and had (fourth had) (string= (second had) commit))
           (if (package-loaded-p name)
               (message "The package ~a is up to date~@[ (~a)~]" name tag)
               (funcall load)))
          (t (state-list-put *fetched-packages* (list (list key commit tag nil)))
             (package-question name (package-fetch-where job) folder load)))))

;;; load-package with a source

(defun load-or-ask (name where folder file accept)
  "Load the package NAME from FOLDER, of WHERE, once let: at once when
*PACKAGE-ASK-BEFORE-LOADING* is NIL, else after asking (PACKAGE-QUESTION),
once the editor has started; ACCEPT, a function, remembers that it was let."
  (flet ((load-it () (funcall accept) (load-package-folder folder name file)))
    (if *package-ask-before-loading*
        (defer-action (lambda () (package-question name where folder #'load-it)))
        (load-it))))

(defun load-package-from (name source version file)
  "Load the package NAME from SOURCE (LOAD-PACKAGE): a folder of your disk at
once, asked about the first time; one fetched before from where it was kept;
else fetched in the background first."
  (let* ((where (package-where source))
         (key (package-key name where version))
         (entry (fetched-package key))
         (folder (package-folder-of name where entry)))
    (cond ((not (or (local-source-p where) (and folder (probe-file folder))))
           (start-package-fetch name where version file key))
          ((not (eq :directory (tuatara.io:path-kind (tuatara.io:native folder))))
           (message "The package ~a is not loaded: its folder ~a is not there" name (abbreviated-path (tuatara.io:native folder))))
          ((and entry (fourth entry))
           (load-package-folder folder name file))
          (t (load-or-ask name where folder file
                          (lambda () (state-list-put *fetched-packages*
                                                     (list (list key (if entry (second entry) "-") (third entry) t)))))))))


;;; The package manager

(define-command package-update ()
  "Fetch again each package load-package fetched (not the folders of your
disk), in the background: the version it asks for looked up anew (the newest
tag, the newest commit, or the tag, branch or commit named), and fetched and
loaded when it is another commit than the one there, asked about first
\(*PACKAGE-ASK-BEFORE-LOADING*); the others are said to be up to date."
  (update-packages (mapcar #'first *declared-packages*)))

(defun update-packages (names)
  "Fetch again the packages called NAMES that come from a repository, as
\\[package-update] does; say so when none of them does."
  (let ((fetched (remove-if-not (lambda (p) (and (member (first p) names :test #'string=)
                                                 (second p)
                                                 (not (local-source-p (package-where (second p))))))
                                *declared-packages*)))
    (if (null fetched)
        (message "No package here is fetched from a repository")
        (loop for (name source version file) in fetched
              for where = (package-where source)
              do (start-package-fetch name where version file (package-key name where version))))))

(define-major-mode packages-mode marked-list-mode
  "The package manager (\\[package-manager]): a line each package the init file
loads from a source, as the buffer list (\\[buffers]) has its buffers.
\\<support>\\[next-line] and \\[previous-line] move, \\<packages-mode>\\[list-visit] or \\`RET' lists the folder of
the package on the line in rimu, \\[list-visit-other] in another window, \\[quit-window] or
\\`h' returns; \\[list-mark-entry] marks a package (>), \\[list-mark-for-deletion] marks it for deletion (D),
\\[list-unmark-entry] unmarks it, \\[list-unmark-all] unmarks them all, \\[list-toggle-marks] swaps marked and unmarked; \\[packages-update-marked] fetches the
marked ones again (the one on the line when none is), \\[list-delete-marked] deletes what
is kept of them after asking, \\[list-execute] of those marked for deletion;
\\[revert-buffer] lists them again, marks kept, as the list does by itself while it
is shown. The README of the package on the line is shown beside the list,
\\[list-toggle-preview] turning that off and on."
  :lighter "Packages" :interactive nil)

(defvar *packages-keymap* (mode-keymap 'packages-mode)
  "Keys of the package manager (packages-mode's keymap).")

(defparameter *packages-columns* '((" M" :width 2) ("Name" :width 18) ("Version" :width 16) ("Commit" :width 8)
                                   ("State" :width 12) ("Source"))
  "The columns of the package manager, as TABLE-HEADING-PIECES takes them: the
mark (> or D), the name, the version (its tag), the commit, what is become of
it this session, and where it comes from.")

(defparameter *packages-source-width* 20
  "The columns of a package's source the package manager keeps in view: its
README goes beside the list only when the list has room for its other
columns and this much of the source, else below it.")

(defun package-state (name source version)
  "What is known of the package NAME from SOURCE, VERSION asked for: the
entry remembered (KEY COMMIT TAG ACCEPTED) and where it is kept, as two values."
  (let* ((where (package-where source))
         (entry (fetched-package (package-key name where version))))
    (values entry (package-folder-of name where entry))))

(defun package-condition (name entry)
  "What is become of the package NAME this session, ENTRY what is remembered
of it: loaded, fetching, refused (it uses names this Tuatara lacks), failed,
waiting (for another, :after), later (to load on a command, a mode or once
started), declined (not let load), kept or not fetched."
  (cond ((package-loaded-p name) "loaded")
        ((member name *fetching-packages* :test #'string=) "fetching")
        ((member name *refused-packages* :test #'string=) "refused")
        ((assoc name *package-failures* :test #'string=) "failed")
        ((find name *packages-waiting* :key #'second :test #'string=) "waiting")
        ((member name *deferred-packages* :test #'string=) "later")
        ((and entry (not (fourth entry))) "declined")
        (entry "kept")
        (t "not fetched")))

(defun package-name-face (mark condition)
  "The face of a package's name in the package manager: that of its MARK
\(LIST-MARK-FACE) when it has one; else :list-dim for one not loaded, by its
CONDITION, none for one loaded."
  (or (list-mark-face mark)
      (unless (string= condition "loaded") :list-dim)))

(defun packages-render (list &optional (point :line))
  "Fill LIST, the package manager, with a line for each package of
*DECLARED-PACKAGES* and a summing-up line; marks kept on those still there."
  (render-table-list
   list *packages-columns* (mapcar #'first *declared-packages*)
   (lambda (name mark section)
     (declare (ignore section))
     (let ((declared (assoc name *declared-packages* :test #'string=)))
       (let* ((source (second declared)) (version (third declared))
              (entry (package-state name source version))
              (condition (package-condition name entry))
              (where (package-where source)))
         (list (list-mark-sign mark)
               (cons name (package-name-face mark condition))
               (or (third entry) version (if (local-source-p where) "folder" ""))
               (if (and entry (string/= (second entry) "-")) (subseq (second entry) 0 (min 7 (length (second entry)))) "")
               condition
               (if (local-source-p where) (abbreviated-path where) where)))))
   :after (if *declared-packages*
              (list (string #\Newline)
                    (format nil "   ~[No packages~;1 package~:;~:*~d packages~], ~d loaded~%" (length *declared-packages*)
                            (count-if (lambda (p) (package-loaded-p (first p))) *declared-packages*)))
              (list (format nil "~%   No packages: (load-package NAME) in the init file~%")))
   :point point
   :refill (lambda (list) (packages-render list :keep))))

(defun packages-key (list)
  "What the package manager LIST shows, cheaply; another key means it is out
of date (KEEP-SHOWN-CURRENT)."
  (declare (ignore list))
  (list *declared-packages* *loaded-packages* *fetching-packages* *package-failures*
        (state-list-entries *fetched-packages*)))

(keep-shown-current 'packages-mode 'packages-key)

(define-command package-manager ()
  "Manage the packages the init file loads from a source (load-package with
:source) in a list, as the buffer list (\\[buffers]) manages buffers. Each
line: its mark (> marked, D marked for deletion), name, the version kept (its
tag), its commit, what is become of it (loaded; fetching; refused, using
names this Tuatara lacks; failed; declined, not let load; kept, not loaded;
not fetched) and where it comes from; the names of those not loaded set
apart; why one was refused or failed is shown beside the list in place of
its README. \\<packages-mode>\\[list-visit] or \\`RET' lists a package's
folder in rimu, to read its source, \\[list-visit-other] in another window; \\[list-mark-entry] marks
it, \\[list-mark-for-deletion] marks it for deletion, \\[list-unmark-entry] and \\[list-unmark-all] unmark, \\[list-toggle-marks] swaps marked and unmarked, a count marking as
many; \\[packages-update-marked] fetches the marked ones again, \\[list-delete-marked] deletes what is kept of
them after asking, \\[list-execute] of those marked for deletion. The README of
the package on the line is shown beside the list, \\[list-toggle-preview] turning that
off and on. \\[package-update] fetches them all again."
  (let ((list (buffer-in-mode "*Packages*" 'packages-mode)))
    (when (eq (tuatara.buffer:buffer-local list :list-preview :unset) :unset)
      (setf (tuatara.buffer:buffer-local list :list-preview) t))
    (packages-render list 2)
    (show-buffer list)))

;;; Finding the packages the init file names

(defun load-package-form-p (form)
  "True when FORM is a load-package form loading a package from a source:
\(load-package NAME ... :source WHERE ...)."
  (and (consp form) (symbolp (first form)) (string-equal (symbol-name (first form)) "LOAD-PACKAGE")
       (consp (rest form)) (stringp (second form))
       (member :source (cddr form))
       t))

(defun swallowed-package-forms (form)
  "The load-package forms FORM, a load-package form, holds as values of its
options that take one value, as after a parenthesis left open before them,
and those they hold in turn, in order; never the forms of :init, :config,
:commands or :modes, nor more than 10,000 of FORM's parts."
  (let ((found '()) (key nil) (values 0))
    (loop for item in (cddr form)
          repeat 10000
          do (if (keywordp item)
                 (setf key item values 0)
                 (unless (member key '(:init :config :commands :modes))
                   (when (and (> (incf values) 1) (load-package-form-p item))
                     (push item found)
                     (dolist (inner (swallowed-package-forms item)) (push inner found))))))
    (nreverse found)))

(defun init-file-package-forms (file)
  "The load-package forms of the init file FILE that load a package from a
source, read and never evaluated (no #. either), in the order they stand,
each (FORM LINE INSIDE): LINE where the form holding it at the top begins,
INSIDE true when it was left inside another load-package by a parenthesis
left open (SWALLOWED-PACKAGE-FORMS). Only the init file's own top-level
forms are looked at, not code that decides whether one runs. As a second
value, where the reading stopped, (LINE . WHY), or NIL when the whole file
was read."
  (let ((found '()))
    (let* ((*package* (find-package :tuatara-user))
           (*read-eval* nil)
           (stopped (map-text-forms (tuatara.io:file-text file)
                                    (lambda (form line)
                                      (when (load-package-form-p form)
                                        (push (list form line nil) found)
                                        (dolist (inner (swallowed-package-forms form))
                                          (push (list inner line t) found)))))))
      (values (nreverse found) (and stopped (cons (car stopped) (princ-to-string (cdr stopped))))))))

(defun scan-init-file-packages (file)
  "Load each package the init file FILE names with load-package and :source
that is not loaded or on its way (INIT-FILE-PACKAGE-FORMS): its load-package
evaluated by itself, as at the start, so that it is fetched, asked about and
loaded as there. Each found, (NAME LINE INSIDE OUTCOME), OUTCOME what
became of it (PACKAGE-CONDITION) or why it failed; as a second value, where
the reading stopped, (LINE . WHY), or NIL."
  (multiple-value-bind (found stopped) (init-file-package-forms file)
    (values
     (loop for (form line inside) in found
           for name = (second form)
           collect (list name line inside
                         (let ((before (let ((declared (declared-package name)))
                                         (and declared (package-condition name (package-state name (second declared) (third declared)))))))
                           (if (member before '("loaded" "fetching" "later" "waiting") :test #'equal)
                               before
                               (handler-case
                                   (let ((*package* (find-package :tuatara-user)))
                                     (setf *package-failures* (remove name *package-failures* :key #'car :test #'string=)
                                           *refused-packages* (remove name *refused-packages* :test #'string=))
                                     (eval form)
                                     (let ((declared (declared-package name)))
                                       (if declared
                                           (package-condition name (package-state name (second declared) (third declared)))
                                           "not named")))
                                 (error (e) (format nil "failed: ~a" e)))))))
     stopped)))

(define-command package-scan ()
  "Look in the init file for the packages it loads from a source (each
load-package with :source), and load each that is not loaded or on its way,
as at the start: fetched in the background, asked about before it is
compiled, loaded. A load-package found inside another form, as after a
parenthesis left open, is loaded by itself and said; where the file could
not be read on is said, the packages after it unseen. The package manager
\(\\[package-manager]) is then shown, following them as they load; what each
became is in *Messages* (\\[view-echo-area-messages])."
  (let ((file (config-file)))
    (unless (probe-file file)
      (error "No init file at ~a" (abbreviated-path file)))
    (multiple-value-bind (results stopped) (scan-init-file-packages file)
      (loop for (name line inside outcome) in results
            do (log-message (format nil "package-scan: ~a, line ~d~:[~;, inside another form (a parenthesis left open before it?)~]: ~a"
                                    name line inside outcome)))
      (when stopped
        (log-message (format nil "package-scan: ~a could not be read on from line ~d: ~a; packages after it are not seen"
                             (abbreviated-path file) (car stopped) (cdr stopped))))
      (package-manager)
      (message "~[No packages~;1 package~:;~:*~d packages~] in ~a~[~:;, ~:*~d inside another form~]~[~:;, ~:*~d failed~]~@[, read up to line ~d~]: \\[view-echo-area-messages] says each"
               (length results) (abbreviated-path file)
               (count-if #'third results)
               (count-if (lambda (r) (uiop:string-prefix-p "failed" (fourth r))) results)
               (car stopped)))))

(defun package-on-line ()
  "The package on the current line of the package manager, (NAME SOURCE
VERSION FILE), or NIL."
  (let ((name (list-entry-at)))
    (and name (declared-package name))))

(defmethod list-entry-noun ((mode packages-mode))
  "A package."
  "package")

(defmethod visit-entry ((mode packages-mode) list name other)
  "List the folder of the package called NAME in rimu, in another window with
OTHER, opened safely, to read its source, each file previewed beside it;
\\<rimu-mode>\\[rimu-up] in the package's folder comes back to LIST."
  (let ((package (declared-package name)))
    (if (null package)
        (message "No package on this line")
        (destructuring-bind (name source version &rest more) package
          (declare (ignore more))
          (let ((folder (nth-value 1 (package-state name source version))))
            (if folder
                (rimu-show-directory (tuatara.io:native folder) :other other :safe t :root (tuatara.io:native folder) :back list)
                (message "~a is not fetched yet" name)))))))

(define-command packages-update-marked ()
  "Fetch again the marked packages (>), or with none marked the one on the
current line, as \\[package-update] does them all."
  (act-on-list (current-buffer) (lambda (list names) (update-packages names) (unmark-list list)) "package"))

(defun forget-package (name source version)
  "Delete what is kept of the package NAME from SOURCE, VERSION asked for:
what is remembered of it, its commits fetched and their files compiled; for a
folder of your disk, that it was let load and its files compiled, the folder
left as it is."
  (let* ((where (package-where source))
         (local (local-source-p where))
         (top (if local (package-folder-of name where nil) (package-kept-folder where name))))
    (state-list-drop *fetched-packages* (list (package-key name where version)))
    (dolist (folder (cons (directory-namestring (package-fasl (merge-pathnames "x" top))) (unless local (list top))))
      (when (probe-file folder)
        (delete-package-folder folder)))))

(defun packages-delete (list names)
  "Delete what is kept of the packages called NAMES after asking (FORGET-PACKAGE),
and make LIST again."
  (when (yes-or-no-p* (format nil "~a?" (list-question "Delete what Tuatara keeps of" "package" names)))
    (dolist (name names)
      (let ((package (declared-package name)))
        (when package (forget-package (first package) (second package) (third package)))))
    (unmark-list list)
    (message "~[No packages~;1 package~:;~:*~d packages~] deleted: one loaded stays so until Tuatara ends; at the next start its load-package line fetches it again, or for a folder of your disk asks again"
             (length names))))

(defmethod delete-entries ((mode packages-mode) list names mark)
  "Delete what is kept of the packages, after asking (PACKAGES-DELETE)."
  (declare (ignore mark))
  (packages-delete list names))

;;; The README of the package on the line, beside the list

(defun package-readme (folder)
  "The README of the package in FOLDER: README.org, README.md, README.txt or
README, in any case; NIL when it has none."
  (let ((files (ignore-errors (uiop:directory-files folder))))
    (loop for name in '("readme.org" "readme.md" "readme.txt" "readme")
          for file = (find name files :key #'file-namestring :test #'string-equal)
          when file return file)))

(defun package-preview-buffer (package)
  "The buffer showing what PACKAGE, (NAME SOURCE VERSION FILE), says of
itself, and true as a second value when it was opened for this: why it was
refused or failed when it was not loaded for that, else its README opened
safely, else a note that it has none or is not fetched yet."
  (destructuring-bind (name source version &rest more) package
    (declare (ignore more))
    (let* ((folder (nth-value 1 (package-state name source version)))
           (failure (and (not (package-loaded-p name)) (assoc name *package-failures* :test #'string=)))
           (readme (and folder (not failure) (package-readme folder))))
      (cond
        (failure
         (values (side-preview-note name (format nil "~a is not loaded: ~a" name (cdr failure))) t))
        (readme
         (multiple-value-bind (buffer existing) (tuatara.io:visit-file (tuatara.io:native readme) :safe t)
           (values buffer (not existing))))
        (t
         (values (side-preview-note name (format nil "~a ~:[is not fetched yet: there is nothing of it to show.~;~:*has no README in ~a.~]"
                                                 name (and folder (abbreviated-path (tuatara.io:native folder)))))
                 t))))))

(defmethod preview-entry ((mode packages-mode) list name)
  "The README of the package called NAME, or why it was not loaded, beside
the package manager (PACKAGE-PREVIEW-BUFFER)."
  (declare (ignore list))
  (let ((package (declared-package name)))
    (and package (package-preview-buffer package))))

(defmethod entry-preview-width ((mode packages-mode) list)
  "The columns of the package manager, its sources' as wide as
*PACKAGES-SOURCE-WIDTH* says."
  (declare (ignore list))
  (table-columns-width *packages-columns* *packages-source-width*))

(defun install-packages ()
  "Bind the package manager's keys, \\`P' turning the README beside it off and on."
  (entry-list-keys 'packages-mode "the package manager (\\[package-manager])")
  (define-keys *packages-keymap* '(("h" quit-window) ("f" packages-update-marked)))
  (entry-preview-keys 'packages-mode "the package manager (\\[package-manager])"))

(commands-for-mode 'packages-mode "the package manager (\\[package-manager])" 'packages-update-marked)
