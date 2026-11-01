;;;; lisp/git/look-only.lisp --- Git in a repository not trusted: commands that read, on a shadow of it.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; A repository you have not trusted (answered "look only", never asked, or
;;;; one that is, is in or holds an evidence folder) is still read by git, but
;;;; only read, and git never sees what the repository says of itself. RUN-GIT
;;;; runs there the commands of *GIT-READING-OPTIONS* alone, each with the
;;;; options listed for it, and runs them on a shadow of the repository: a
;;;; private folder holding settings Tuatara writes (the format and the
;;;; extensions git needs to read it, nothing else), copies of HEAD, the
;;;; references, their logs and, for the commands that read it, the index,
;;;; with the repository's objects reached read-only as an alternate. Its own
;;;; settings, the files they take in, its attributes, its hooks and its
;;;; grafts never reach git, and nothing in the repository is written, its
;;;; index included. Every program git could run is switched off besides
;;;; (*GIT-SETTINGS-LOOKING*), your own GIT_ variables are left out, and
;;;; nothing is fetched. A shadow is made again only when what it copies has
;;;; changed.

(in-package #:tuatara.defaults)

;;; What is run

(defparameter *git-diff-options*
  '("-p" "-u" "--patch" "-s" "--no-patch" "--raw" "--stat" "--numstat" "--shortstat" "--summary" "--name-only"
    "--name-status" "--no-color" "--color=never" "--no-ext-diff" "--no-textconv" "--binary" "--full-index"
    "-M" "-C" "-B" "-R" "--no-renames" "--find-renames" "--find-copies" "-w" "-b" "--ignore-all-space"
    "--ignore-space-change" "--ignore-blank-lines" "--function-context" "-W" "--relative" "--no-relative"
    "-z" "--exit-code" "--quiet" "--check" "--minimal" "--patience" "--histogram" "--text" "-a" "--cc"
    "--abbrev" "--no-abbrev" "--irreversible-delete" "-D" "--no-prefix"
    "--unified=" "-U#" "--diff-filter=" "--stat=" "--relative=" "--abbrev=" "--word-diff=" "--word-diff"
    "--diff-algorithm=" "--find-renames=" "--find-copies=" "-M#" "-C#" "--src-prefix=" "--dst-prefix=")
  "The options of git's differences that only shape what they show.")

(defparameter *git-log-options*
  (append *git-diff-options*
          '("--graph" "--no-merges" "--merges" "--first-parent" "--follow" "--all" "--branches" "--remotes" "--tags"
            "--oneline" "--reverse" "--date-order" "--topo-order" "--author-date-order" "--decorate" "--no-decorate"
            "--abbrev-commit" "--no-abbrev-commit" "--full-history" "--simplify-merges" "--left-right" "--boundary"
            "--cherry-pick" "--cherry-mark" "--not" "--end-of-options" "-g" "--walk-reflogs" "--no-walk" "--parents"
            "--children" "--count" "--objects" "--no-mailmap" "--no-use-mailmap" "--i-still-use-this" "-m"
            "--format=" "--pretty=" "--date=" "--max-count=" "-n#" "-#" "--skip=" "--author=" "--committer=" "--grep="
            "-S*" "-G*" "--since=" "--until=" "--after=" "--before=" "--decorate=" "--branches=" "--tags=" "--remotes="
            "--exclude=" "--min-parents=" "--max-parents=" "-i" "--regexp-ignore-case" "-E" "--extended-regexp"
            "-F" "--fixed-strings" "--all-match" "--invert-grep" "--pickaxe-regex" "--pickaxe-all"))
  "The options of git log and its kin that only shape what they show.")

(defparameter *git-reading-options*
  `(("status" :options ("--porcelain" "--porcelain=v1" "--porcelain=v2" "--branch" "-b" "-s" "--short" "--long"
                        "-z" "-u" "--untracked-files=" "--ignored" "--ignored=" "--no-renames" "--ignore-submodules="
                        "-v" "--verbose" "--show-stash" "--ahead-behind" "--no-ahead-behind" "--find-renames"))
    ("diff" :options ("--cached" "--staged" "--no-index" "--ignore-submodules=" ,@*git-diff-options*))
    ("diff-files" :options ("-r" "-c" "-q" "--ignore-submodules=" ,@*git-diff-options*))
    ("diff-index" :options ("-r" "--cached" "-m" "--ignore-submodules=" ,@*git-diff-options*))
    ("diff-tree" :options ("-r" "-t" "--root" "-c" "--no-commit-id" "--always" ,@*git-diff-options*))
    ("log" :options ,*git-log-options* :values ("--format" "--pretty" "-n" "--max-count" "--author" "--grep"))
    ("whatchanged" :options ,*git-log-options* :values ("--format" "--pretty" "-n"))
    ("show" :options ,*git-log-options* :values ("--format" "--pretty"))
    ("rev-list" :options ,*git-log-options* :values ("--format" "--pretty" "-n" "--max-count"))
    ("range-diff" :options ("--creation-factor=" "--no-dual-color" ,@*git-log-options*))
    ("cherry" :options ("-v" "--abbrev" "--abbrev="))
    ("shortlog" :options ("-s" "-n" "-e" "-c" "-w" "--summary" "--numbered" "--email" "--committer" "--group="
                          ,@*git-log-options*))
    ("blame" :options ("--porcelain" "--line-porcelain" "-p" "-l" "-t" "-s" "-e" "-w" "-M" "-C" "-M#" "-C#"
                       "--incremental" "--root" "--show-stats" "--show-name" "--show-number" "-n" "-f" "-c"
                       "--no-textconv" "--date=" "--abbrev=" "-L*" "--first-parent" "--reverse")
             :values ("-L"))
    ("ls-files" :options ("-z" "-s" "--stage" "-o" "--others" "--directory" "--no-empty-directory" "-c" "--cached"
                          "-m" "--modified" "-d" "--deleted" "-i" "--ignored" "--exclude-standard" "-u" "--unmerged"
                          "-k" "--killed" "--full-name" "--error-unmatch" "--eol" "--deduplicate" "-t" "-v" "-f"
                          "--format=" "--abbrev=" "--exclude=" "--abbrev"))
    ("ls-tree" :options ("-z" "-l" "--long" "-r" "-t" "-d" "--name-only" "--name-status" "--full-name"
                         "--full-tree" "--object-only" "--abbrev" "--abbrev=" "--format="))
    ("cat-file" :options ("-e" "-p" "-t" "-s" "--batch" "--batch-check" "--batch=" "--batch-check=" "-z" "-Z"
                          "--buffer" "--follow-symlinks" "--batch-all-objects" "--unordered"))
    ("rev-parse" :options ("--verify" "-q" "--quiet" "--absolute-git-dir" "--git-dir" "--git-common-dir"
                           "--show-toplevel" "--show-prefix" "--show-cdup" "--is-inside-work-tree" "--is-bare-repository"
                           "--is-inside-git-dir" "--abbrev-ref" "--abbrev-ref=" "--short" "--short=" "--symbolic"
                           "--symbolic-full-name" "--path-format=" "--show-object-format" "--show-object-format="
                           "--end-of-options" "--sq" "--all" "--branches" "--tags" "--remotes" "--not" "--revs-only"
                           "--no-revs" "--flags" "--no-flags")
               :values ("--git-path" "--default"))
    ("for-each-ref" :options ("--format=" "--sort=" "--count=" "--points-at=" "--merged" "--merged=" "--no-merged"
                              "--no-merged=" "--contains" "--contains=" "--no-contains" "--no-contains=" "--ignore-case"
                              "--omit-empty" "--exclude=")
                    :values ("--format" "--sort" "--count" "--points-at"))
    ("show-ref" :options ("--heads" "--tags" "--head" "-s" "--hash" "--hash=" "--verify" "-q" "--quiet" "--abbrev"
                          "--abbrev=" "-d" "--dereference" "--exists" "--branches"))
    ("describe" :options ("--always" "--tags" "--all" "--long" "--abbrev=" "--dirty" "--dirty=" "--contains"
                          "--exact-match" "--first-parent" "--match=" "--exclude=" "--candidates="))
    ("merge-base" :options ("--all" "-a" "--is-ancestor" "--fork-point" "--octopus" "--independent"))
    ("name-rev" :options ("--name-only" "--tags" "--always" "--no-undefined" "--refs=" "--exclude="))
    ("grep" :options ("-n" "--line-number" "-i" "--ignore-case" "-w" "--word-regexp" "-v" "--invert-match" "-l"
                      "--files-with-matches" "--name-only" "-L" "--files-without-match" "-c" "--count" "-h" "-H"
                      "-E" "--extended-regexp" "-F" "--fixed-strings" "-G" "--basic-regexp" "-P" "--perl-regexp"
                      "-z" "--null" "--cached" "--untracked" "--full-name" "--no-color" "--color=never" "--heading"
                      "--break" "--no-textconv" "--max-depth=" "--context=" "-A#" "-B#" "-C#" "-#" "-I" "--all-match"
                      "--column" "-o" "--only-matching" "-q" "--quiet")
            :values ("-e"))
    ("check-ignore" :options ("-v" "--verbose" "-n" "--non-matching" "-z" "--no-index"))
    ("check-attr" :options ("-a" "--all" "--cached" "-z"))
    ("count-objects" :options ("-v" "--verbose" "-H" "--human-readable"))
    ("version" :options ("--build-options"))
    ("hash-object" :options ("--no-filters" "--literally") :values ("-t"))
    ("stash list" :options ,*git-log-options* :values ("--format" "--pretty"))
    ("stash show" :options ("--include-untracked" "-u" "--only-untracked" ,@*git-diff-options*))
    ("worktree list" :options ("--porcelain" "-v" "--verbose" "-z") :plain nil)
    ("notes list" :options ())
    ("notes show" :options ())
    ("reflog" :options ,*git-log-options* :values ("--format" "--pretty") :plain nil)
    ("reflog show" :options ,*git-log-options* :values ("--format" "--pretty"))
    ("remote" :options ("-v" "--verbose") :plain nil)
    ("remote get-url" :options ("--push" "--all"))
    ("branch" :options ("--show-current" "--list" "-l" "-a" "--all" "-r" "--remotes" "-v" "-vv" "--verbose"
                        "--format=" "--sort=" "--contains=" "--no-contains=" "--merged=" "--no-merged="
                        "--points-at=" "--column" "--no-column" "--abbrev=" "-i" "--ignore-case" "--omit-empty")
              :values ("--format" "--sort" "--points-at") :listing t)
    ("tag" :options ("-l" "--list" "-n#" "-n" "--sort=" "--format=" "--contains=" "--no-contains=" "--merged="
                     "--no-merged=" "--points-at=" "--column" "--no-column" "-i" "--ignore-case" "--omit-empty")
           :values ("--format" "--sort" "--points-at") :listing t)
    ("config" :options ("--get" "--get-all" "--get-regexp" "--list" "-l" "-z" "--null" "--name-only" "--show-origin"
                        "--show-scope" "--type=" "--bool" "--int" "--path" "--default=") :config t)
    ("config get" :options ("--all" "--regexp" "-z" "--null" "--show-origin" "--show-scope" "--type=" "--default="))
    ("config list" :options ("-z" "--null" "--name-only" "--show-origin" "--show-scope")))
  "The git commands run in a repository not trusted, by their name (with the
word after it for those of several parts: stash list), each with the
options it may be given there, those only shaping what it shows. An option
is written as git takes it, alone in its word; one ending in = takes the
value written after it, in # a number, in * anything. :VALUES are options
taking the next word as their value; :PLAIN NIL, no word that is not an
option (one of several parts given none after its options); :LISTING, names
given only with -l or --list (to list those matching); :CONFIG, only git
config's forms that read (GIT-CONFIG-READING-P).")

(defparameter *git-commands-of-several-parts* '("stash" "worktree" "notes" "reflog" "remote" "config")
  "The git commands whose first word after them says what they do (stash list).")

(defparameter *git-format-words-refused* '("%G" "%(signature" "%aN" "%aE" "%aL" "%cN" "%cE" "%cL")
  "What an option's value given git in a repository not trusted may not hold:
a signature checked runs a program, and a name mapped is not the one the
commit holds.")

(defun git-option-allowed-p (word options)
  "True when WORD, an option given a git command, is one of OPTIONS as
*GIT-READING-OPTIONS* writes them: exactly, or with the value its =, # or *
allows."
  (some (lambda (option)
          (let ((end (char option (1- (length option)))) (stem (subseq option 0 (1- (length option)))))
            (case end
              (#\= (uiop:string-prefix-p option word))
              (#\# (and (> (length word) (length stem)) (uiop:string-prefix-p stem word)
                        (every #'digit-char-p (subseq word (length stem)))))
              (#\* (and (> (length word) (length stem)) (uiop:string-prefix-p stem word)))
              (t (string= option word)))))
        options))

(defun git-value-refused (value)
  "The one of *GIT-FORMAT-WORDS-REFUSED* VALUE, an option or its value, holds, or NIL."
  (find-if (lambda (refused) (search refused value)) *git-format-words-refused*))

(defun git-reading-refusal (arguments)
  "Why git is not run with ARGUMENTS in a repository not trusted, in one
sentence; NIL when they only read: no option before the command but those
of *GIT-LINE-OPTIONS-ALLOWED*; a command of *GIT-READING-OPTIONS* (with its
first word for one of several parts) given only the options listed for it,
each in a word of its own, whose values ask for no signature and no name
mapped; names to branch and tag only when listing; git config only reading."
  (let* ((words (git-command-words arguments))
         (leading (remove-if (lambda (word) (member word *git-line-options-allowed* :test #'string=))
                             (ldiff arguments words)))
         (command (first words))
         (more (rest words))
         (part (and (member command *git-commands-of-several-parts* :test #'equal)
                    more (not (uiop:string-prefix-p "-" (first more)))
                    (first more)))
         (key (if part (format nil "~a ~a" command part) command))
         (spec (cdr (assoc key *git-reading-options* :test #'equal)))
         (plain '()) (refusal nil))
    (cond (leading (format nil "git is given no option before its command here (not ~a)" (first leading)))
          ((null command) "no git command is given")
          ((null spec) (format nil "git ~a is not run here, where git only reads" key))
          (t
           (loop with options = (getf spec :options)
                 with values = (getf spec :values)
                 with rest = (if part (rest more) more)
                 with ended = nil
                 while (and rest (not refusal))
                 do (let ((word (pop rest)))
                      (cond ((and (not ended) (string= word "--")) (return))
                            ((or ended (not (uiop:string-prefix-p "-" word)) (string= word "-")) (push word plain))
                            ((member word values :test #'string=)
                             (let ((value (pop rest)))
                               (cond ((null value) (setf refusal (format nil "~a is given no value" word)))
                                     ((git-value-refused value)
                                      (setf refusal (format nil "~a in what git is given asks for more than reading" (git-value-refused value)))))))
                            ((not (git-option-allowed-p word options))
                             (setf refusal (format nil "git ~a is not given ~a here, where git only reads" key word)))
                            ((git-value-refused word)
                             (setf refusal (format nil "~a in what git is given asks for more than reading" (git-value-refused word))))
                            ((string= word "--end-of-options") (setf ended t)))))
           (cond (refusal)
                 ((and plain (not (getf spec :plain t)))
                  (format nil "git ~a is given no ~a here, where git only reads" key (first (last plain))))
                 ((and (getf spec :listing) plain
                       (notany (lambda (word) (member word '("-l" "--list") :test #'string=)) more))
                  (format nil "git ~a is given names here only to list them (-l)" command))
                 ((and (getf spec :config) (not (git-config-reading-p more)))
                  "git config only reads here"))))))

;;; The repository's own folders, read without git

(defun git-refuse-reading (top control &rest arguments)
  "Signal GIT-REFUSED: the repository at TOP is not read, why said by CONTROL
and ARGUMENTS."
  (error 'git-refused :operation "read" :path top
                      :reason (format nil "~a is not read: ~?" (abbreviated-path top) control arguments)))

(defparameter *git-small-file-most* (* 64 1024)
  "The most octets a file of git's own that names a place (.git as a file,
commondir) may hold to be read.")

(defun git-small-file (top path what)
  "The text of the file at PATH, of the repository at TOP: a regular file of
at most *GIT-SMALL-FILE-MOST* octets, examined before it is opened, so that a
named pipe or a device is never opened; WHAT names it when it cannot be."
  (let ((stamp (ignore-errors (tuatara.io:path-stamp path :follow nil))))
    (unless (and stamp (eq (tuatara.io:stamp-kind stamp) :regular))
      (git-refuse-reading top "its ~a is ~:[not there~;not a file~]" what stamp))
    (when (> (tuatara.io:stamp-size stamp) *git-small-file-most*)
      (git-refuse-reading top "its ~a is too large to be one" what))
    (string-right-trim '(#\Newline #\Return #\Space) (tuatara.text:lenient-string (tuatara.io:file-octets path)))))

(defun git-folder-from (base text)
  "The folder TEXT, as a file of git's own names it, from BASE when it is
relative, where it really is, ending in /; NIL when it is not there."
  (let ((real (ignore-errors (tuatara.io:real-path (if (uiop:string-prefix-p "/" text) text (tuatara.io:path-join base text))))))
    (and real (eq (tuatara.io:path-kind real) :directory) (tuatara.io:slash-ended real))))

(defun git-real-folders (top)
  "Git's own folder for the repository or worktree at TOP, read without git:
its .git folder, or the one its .git file names; and, as a second value, the
folder its objects and references are in (the one its commondir names, for
a worktree). Both where they really are, ending in /. GIT-REFUSED when there
is none git could read: no HEAD file there, no objects folder."
  (let* ((dot (tuatara.io:path-join (tuatara.io:native top) ".git"))
         (folder (case (tuatara.io:path-kind dot :follow nil)
                   ((:directory :symlink) (git-folder-from top ".git"))
                   (:regular
                    (let ((text (git-small-file top dot ".git file")))
                      (unless (uiop:string-prefix-p "gitdir: " text)
                        (git-refuse-reading top "its .git file names no folder"))
                      (git-folder-from top (subseq text 8)))))))
    (unless folder (git-refuse-reading top "it has no folder of git's own"))
    (let* ((commondir (tuatara.io:path-join folder "commondir"))
           (common (if (tuatara.io:path-kind commondir :follow nil)
                       (or (git-folder-from folder (git-small-file top commondir "commondir"))
                           (git-refuse-reading top "its commondir leads nowhere"))
                       folder)))
      (unless (eq (tuatara.io:path-kind (tuatara.io:path-join folder "HEAD") :follow nil) :regular)
        (git-refuse-reading top "~a holds no HEAD" (abbreviated-path folder)))
      (unless (eq (tuatara.io:path-kind (tuatara.io:path-join common "objects") :follow nil) :directory)
        (git-refuse-reading top "~a holds no objects" (abbreviated-path common)))
      (values folder common))))

(defun git-real-folder (top)
  "Git's own folder for the repository or worktree at TOP (GIT-REAL-FOLDERS),
or NIL when it has none git could read."
  (handler-case (values (git-real-folders top)) (git-refused () nil)))

;;; Its shadow

(defstruct (git-shadow (:constructor make-git-shadow (folder stamps settings)) (:copier nil))
  "A shadow of a repository not trusted, in FOLDER: STAMPS, the state of what
it was copied from (GIT-REPOSITORY-STATE); SETTINGS, the extensions it was
given, (NAME VALUE) each; INDEX-STAMPS, those of the index copied into it
last; USERS, how many runs of git use it now; RETIRED, true once a newer
shadow took its place, deleted when the last run using it is done."
  folder stamps settings (index-stamps :none) (users 0) retired)

(setf (documentation 'git-shadow-p 'function) "True when OBJECT is a GIT-SHADOW.")

(defvar *git-shadows* (make-hash-table :test 'equal)
  "The shadow of each repository not trusted git was run in, by its own
folder (GIT-REAL-FOLDERS).")

(defvar *git-shadows-lock* (sb-thread:make-mutex :name "tuatara git shadows")
  "Held while a shadow is looked at, made, taken up or let go of.")

(defparameter *git-shadow-most-entries* 200000
  "The most files and folders a repository's references and their logs may
hold for a shadow to be made of them.")

(defparameter *git-extensions-read*
  '(("objectformat" "sha1" "sha256") ("refstorage" "files" "reftable"))
  "The extensions a shadow is given as its repository has them, each with
the values it may have.")

(defparameter *git-extensions-left-out* '("partialclone" "preciousobjects" "worktreeconfig" "noop")
  "The extensions a repository may have that its shadow does without:
nothing fetched, nothing kept from being pruned, no settings of a
worktree's own.")

(defun git-shadows-directory ()
  "Where this session's shadows are made: git-shadow/PID/ in the private cache."
  (private-cache-directory (format nil "git-shadow/~d/" (sb-posix:getpid))))

(defun forget-old-git-shadows ()
  "Delete the shadows of sessions no longer running, as one that ended
without deleting its own (a crash) leaves them."
  (let ((all (private-cache-directory "git-shadow/")))
    (dolist (entry (ignore-errors (tuatara.io:directory-entries all)))
      (let ((pid (parse-integer (first entry) :junk-allowed t)))
        (when (or (null pid)
                  (and (/= pid (sb-posix:getpid)) (not (tuatara.io:path-kind (format nil "/proc/~d/" pid)))))
          (ignore-errors (tuatara.io:delete-tree (tuatara.io:path-join all (first entry)))))))))

(defun git-file-row (path)
  "What a shadow knows of the file at PATH, a link not followed: its kind,
device, inode, size, and the times of its change, in nanoseconds; NIL when
it is not there."
  (let ((stamp (ignore-errors (tuatara.io:path-stamp path :follow nil))))
    (and stamp (list (tuatara.io:stamp-kind stamp) (tuatara.io:stamp-dev stamp) (tuatara.io:stamp-ino stamp)
                     (tuatara.io:stamp-size stamp) (tuatara.io:stamp-mtime stamp) (tuatara.io:stamp-ctime stamp)))))

(defun git-tree-entries (top root)
  "Every file and folder within ROOT, a folder of the repository at TOP, as
\(PATH KIND DEV INO SIZE MTIME CTIME), PATH from ROOT, in order: what a
shadow copies of it, and is made again by when it changes. A link, or
anything but a file or a folder, is left out, never opened. NIL when ROOT
is not there."
  (let ((rows '()) (count 0))
    (labels ((walk (folder relative depth)
               (when (> depth 64) (git-refuse-reading top "~a is nested too deep" (abbreviated-path root)))
               (dolist (entry (tuatara.io:directory-entries folder :stamps t))
                 (destructuring-bind (name kind size own &optional stamp) entry
                   (declare (ignore kind size))
                   (when (and stamp (member own '(:regular :directory)))
                     (when (> (incf count) *git-shadow-most-entries*)
                       (git-refuse-reading top "~a holds too many references" (abbreviated-path root)))
                     (let ((path (concatenate 'string relative name)))
                       (push (list path own (tuatara.io:stamp-dev stamp) (tuatara.io:stamp-ino stamp)
                                   (tuatara.io:stamp-size stamp) (tuatara.io:stamp-mtime stamp) (tuatara.io:stamp-ctime stamp))
                             rows)
                       (when (eq own :directory)
                         (walk (tuatara.io:slash-ended (tuatara.io:path-join folder name))
                               (concatenate 'string path "/") (1+ depth)))))))))
      (when (eq (tuatara.io:path-kind root :follow nil) :directory)
        (walk (tuatara.io:slash-ended root) "" 0)))
    (nreverse rows)))

(defun git-repository-state (folder common)
  "What a shadow of the repository whose own folders are FOLDER and COMMON is
made again by: the state of its HEAD, settings, packed references, shallow
commits and patterns left out, of the log of HEAD, and of every file and
folder of its references, their logs and its reference table."
  (list (git-file-row (tuatara.io:path-join folder "HEAD"))
        (mapcar (lambda (name) (git-file-row (tuatara.io:path-join common name)))
                '("config" "packed-refs" "shallow" "info/exclude"))
        (git-file-row (tuatara.io:path-join folder "logs/HEAD"))
        (git-tree-entries common (tuatara.io:path-join common "refs"))
        (git-tree-entries common (tuatara.io:path-join common "logs"))
        (git-tree-entries common (tuatara.io:path-join common "reftable"))))

(defun git-copy-file (top from to what)
  "Copy the file FROM, of the repository at TOP, to TO, in a shadow, when it
is there: examined first, so that a named pipe or a device is never opened,
and refused (GIT-REFUSE-READING, WHAT naming it) when it is not a file."
  (let ((kind (tuatara.io:path-kind from :follow nil)))
    (cond ((null kind))
          ((eq kind :regular)
           (ensure-directories-exist (sb-ext:parse-native-namestring (tuatara.io:parent-path to)))
           (tuatara.io:copy-regular-file from to))
          (t (git-refuse-reading top "its ~a is not a file" what)))))

(defun git-copy-tree (top from to rows)
  "Copy into the folder TO what ROWS (GIT-TREE-ENTRIES of FROM) list of the
folder FROM, of the repository at TOP: its folders made, its files copied."
  (dolist (row rows)
    (destructuring-bind (path kind &rest stamp) row
      (declare (ignore stamp))
      (let ((target (tuatara.io:path-join to path)))
        (if (eq kind :directory)
            (ensure-directories-exist (sb-ext:parse-native-namestring (tuatara.io:slash-ended target)))
            (git-copy-file top (tuatara.io:path-join from path) target "references"))))))

(defun git-helper-output (top folder arguments)
  "What git prints with ARGUMENTS, run in FOLDER, a shadow's, with none of
your GIT_ variables and no repository found above it, for the repository at
TOP: as text when it ended well (exit 0); GIT-REFUSED else."
  (multiple-value-bind (octets ok)
      (run-git-program (tuatara.io:find-program "git") folder arguments :reading t :seconds 10
                       :environment (list (format nil "GIT_CEILING_DIRECTORIES=~a"
                                                  (string-right-trim "/" (tuatara.io:parent-path folder)))))
    (unless (and ok octets)
      (git-refuse-reading top "git could not read its settings"))
    (tuatara.text:lenient-string octets)))

(defun git-repository-settings (top common folder)
  "The extensions the repository at TOP, whose common folder is COMMON, needs
to be read, (NAME VALUE) each: read by git config from a copy of its
settings made in FOLDER, nothing they name taken in. GIT-REFUSED when it is
of a format newer than git's 1, or needs an extension that is neither one
of *GIT-EXTENSIONS-READ* nor one of *GIT-EXTENSIONS-LEFT-OUT*."
  (let ((copy (tuatara.io:path-join folder "settings-read")))
    (git-copy-file top (tuatara.io:path-join common "config") copy "settings")
    (if (not (tuatara.io:path-kind copy :follow nil))
        '()
        (let* ((text (unwind-protect (git-helper-output top folder (list "config" "--file" copy "--no-includes" "-z" "--list"))
                       (ignore-errors (delete-file (sb-ext:parse-native-namestring copy)))))
               (entries (loop for entry in (uiop:split-string text :separator (list (code-char 0)))
                              for newline = (position #\Newline entry)
                              when (plusp (length entry))
                                collect (list (string-downcase (subseq entry 0 (or newline (length entry))))
                                              (if newline (subseq entry (1+ newline)) "true"))))
               (version (parse-integer (or (second (assoc "core.repositoryformatversion" entries :test #'string=)) "0")
                                       :junk-allowed t)))
          (unless (member version '(0 1))
            (git-refuse-reading top "it is of git's format ~a, which is not read here" version))
          (loop for (key value) in entries
                when (uiop:string-prefix-p "extensions." key)
                  append (let* ((name (subseq key 11))
                                (read (assoc name *git-extensions-read* :test #'string=)))
                           (cond ((and read (member (string-downcase value) (rest read) :test #'string=))
                                  (list (list name (string-downcase value))))
                                 ((member name *git-extensions-left-out* :test #'string=) '())
                                 (t (git-refuse-reading top "it needs git's extension ~a = ~a, which is not read here"
                                                        name value)))))))))

(defun git-shadow-settings-text (settings)
  "The settings a shadow's git reads, for SETTINGS, its extensions."
  (format nil "[core]~%	repositoryformatversion = ~d~%	bare = false~%	logallrefupdates = false~%~@[[extensions]~%~{	~{~a = ~a~}~%~}~]"
          (if settings 1 0) settings))

(defun make-git-shadow-of (top folder common stamps)
  "Make a shadow of the repository at TOP, its own folders FOLDER and COMMON,
STAMPS its state now (GIT-REPOSITORY-STATE): a new private folder holding
settings of its own, and copies of HEAD, the references and their logs, a
reference table, packed references, the shallow commits and the patterns of
files left out; an empty folder for objects of its own, and an empty one to
run in."
  (let ((place (new-private-folder (git-shadows-directory))))
    (handler-bind ((error (lambda (e) (declare (ignore e)) (delete-private-folder place))))
      (let ((settings (git-repository-settings top common place)))
        (tuatara.io:write-private-file (tuatara.io:path-join place "config") (git-shadow-settings-text settings))
        (git-copy-file top (tuatara.io:path-join folder "HEAD") (tuatara.io:path-join place "HEAD") "HEAD")
        (dolist (name '("packed-refs" "shallow" "info/exclude"))
          (git-copy-file top (tuatara.io:path-join common name) (tuatara.io:path-join place name) name))
        (destructuring-bind (head files head-log refs logs reftable) stamps
          (declare (ignore head files head-log))
          (git-copy-tree top (tuatara.io:path-join common "refs/") (tuatara.io:path-join place "refs/") refs)
          (git-copy-tree top (tuatara.io:path-join common "logs/") (tuatara.io:path-join place "logs/") logs)
          (git-copy-tree top (tuatara.io:path-join common "reftable/") (tuatara.io:path-join place "reftable/") reftable))
        (git-copy-file top (tuatara.io:path-join folder "logs/HEAD") (tuatara.io:path-join place "logs/HEAD.copy") "log of HEAD")
        (when (tuatara.io:path-kind (tuatara.io:path-join place "logs/HEAD.copy") :follow nil)
          (tuatara.io:rename-path (tuatara.io:path-join place "logs/HEAD.copy") (tuatara.io:path-join place "logs/HEAD") :replace t))
        (dolist (name '("refs/" "objects/" "empty/"))
          (ensure-directories-exist (sb-ext:parse-native-namestring (tuatara.io:path-join place name))))
        (make-git-shadow place stamps settings)))))

(defun git-index-rows (folder)
  "What a shadow's copy of the index in git's own FOLDER is made again by:
the state of the index and of each shared index beside it (sharedindex.*)."
  (cons (git-file-row (tuatara.io:path-join folder "index"))
        (loop for entry in (ignore-errors (tuatara.io:directory-entries folder))
              when (uiop:string-prefix-p "sharedindex." (first entry))
                collect (cons (first entry) (git-file-row (tuatara.io:path-join folder (first entry)))))))

(defun git-copy-index (top folder shadow)
  "Copy the index of the repository at TOP, git's own FOLDER, and its shared
indexes into SHADOW when they changed since they were copied last, each to a
new name then over the one there; the copy taken away when the repository
has no index. GIT-REFUSED when one is not a file."
  (let ((rows (git-index-rows folder))
        (place (git-shadow-folder shadow)))
    (unless (equal rows (git-shadow-index-stamps shadow))
      (dolist (name (cons "index" (mapcar #'first (rest rows))))
        (let ((from (tuatara.io:path-join folder name))
              (to (tuatara.io:path-join place name))
              (fresh (tuatara.io:path-join place (format nil "~a.~a" name (tuatara.io:random-name)))))
          (if (tuatara.io:path-kind from :follow nil)
              (progn (git-copy-file top from fresh (if (string= name "index") "index" "shared index"))
                     (tuatara.io:rename-path fresh to :replace t))
              (ignore-errors (delete-file (sb-ext:parse-native-namestring to))))))
      (setf (git-shadow-index-stamps shadow) rows))))

(defparameter *git-commands-without-index*
  '("log" "show" "whatchanged" "cat-file" "ls-tree" "rev-list" "for-each-ref" "show-ref" "merge-base" "name-rev"
    "shortlog" "branch" "tag" "remote" "notes" "reflog" "worktree" "count-objects" "version" "cherry" "range-diff"
    "hash-object" "config")
  "The git commands that never read the index: no copy of it is made for them.")

(defun retire-git-shadow (shadow)
  "Mark SHADOW as replaced: deleted at once when no run of git uses it, else
when the last does."
  (setf (git-shadow-retired shadow) t)
  (when (zerop (git-shadow-users shadow))
    (delete-private-folder (git-shadow-folder shadow))))

(defun take-git-shadow (top folder common index)
  "The shadow of the repository at TOP, its own folders FOLDER and COMMON:
made now when there is none, or when what it copies changed (the one before
then retired), and taken up for one run of git; with INDEX, the index copied
into it when it changed. LET-GO-OF-GIT-SHADOW lets go of it."
  (sb-thread:with-mutex (*git-shadows-lock*)
    (let* ((stamps (git-repository-state folder common))
           (shadow (gethash folder *git-shadows*)))
      (unless (and shadow (equal stamps (git-shadow-stamps shadow)))
        (when shadow
          (remhash folder *git-shadows*)
          (retire-git-shadow shadow))
        (setf shadow (make-git-shadow-of top folder common stamps)
              (gethash folder *git-shadows*) shadow))
      (when index (git-copy-index top folder shadow))
      (incf (git-shadow-users shadow))
      shadow)))

(defun let-go-of-git-shadow (shadow)
  "One run of git is done with SHADOW; deleted when it was retired and none uses it now."
  (sb-thread:with-mutex (*git-shadows-lock*)
    (decf (git-shadow-users shadow))
    (when (and (git-shadow-retired shadow) (zerop (git-shadow-users shadow)))
      (delete-private-folder (git-shadow-folder shadow)))))

;;; Running git on it

(defparameter *git-settings-looking*
  '(("core.fsmonitor" "false") ("core.hooksPath" "/dev/null") ("core.attributesFile" "/dev/null")
    ("core.pager" "cat") ("core.sshCommand" "") ("core.askPass" "") ("core.alternateRefsCommand" "")
    ("core.untrackedCache" "false") ("core.useReplaceRefs" "false") ("credential.helper" "")
    ("gpg.program" "") ("gpg.openpgp.program" "") ("gpg.x509.program" "") ("gpg.ssh.program" "")
    ("gpg.ssh.defaultKeyCommand" "") ("diff.external" "") ("diff.submodule" "short") ("diff.ignoreSubmodules" "all")
    ("submodule.recurse" "false") ("status.submoduleSummary" "false") ("protocol.allow" "never")
    ("log.showSignature" "false") ("log.mailmap" "false") ("mailmap.file" "") ("mailmap.blob" "")
    ("safe.bareRepository" "explicit") ("gc.auto" "0") ("maintenance.auto" "false"))
  "The settings git is given in a repository not trusted, over your own,
each (KEY VALUE): no program runs, no submodule is looked into, nothing is
fetched, no name is mapped, nothing is tidied.")

(defun git-alternate-folder (top path)
  "PATH, a folder of objects, as git reads one in a list of alternates;
refused (GIT-REFUSE-READING) when it holds a colon, a quote or a newline,
which git would read as more than one."
  (when (find-if (lambda (c) (member c '(#\: #\" #\Newline))) path)
    (git-refuse-reading top "its objects are in a folder whose name git would read as more than one"))
  (string-right-trim "/" path))

(defun git-shadow-environment (top common shadow work)
  "The environment git reads the repository at TOP through SHADOW with, WORK
its working folder: the shadow as git's own folder, the repository's
objects (in COMMON) reached as an alternate, objects of its own written
only in the shadow, no grafts and no replace refs, no lock it can do
without, no protocol at all, and *GIT-SETTINGS-LOOKING* as GIT_CONFIG_COUNT
settings."
  (let ((place (git-shadow-folder shadow)))
    (append (list (format nil "GIT_DIR=~a" (string-right-trim "/" place))
                  (format nil "GIT_WORK_TREE=~a" (string-right-trim "/" work))
                  (format nil "GIT_OBJECT_DIRECTORY=~aobjects" place)
                  (format nil "GIT_ALTERNATE_OBJECT_DIRECTORIES=~a"
                          (git-alternate-folder top (tuatara.io:path-join common "objects")))
                  (format nil "GIT_INDEX_FILE=~aindex" place)
                  (format nil "GIT_GRAFT_FILE=~ano-grafts" place)
                  "GIT_NO_REPLACE_OBJECTS=1" "GIT_OPTIONAL_LOCKS=0" "GIT_ALLOW_PROTOCOL=none"
                  (format nil "GIT_CONFIG_COUNT=~d" (length *git-settings-looking*)))
            (loop for (key value) in *git-settings-looking*
                  for i from 0
                  collect (format nil "GIT_CONFIG_KEY_~d=~a" i key)
                  collect (format nil "GIT_CONFIG_VALUE_~d=~a" i value)))))

(defparameter *git-empty-trees*
  '(("sha256" . "6ef19b41225c5369f1c104d45d8d85efa9b057b53b14b4b9b939dd74decc5321"))
  "The empty tree's name in repositories of other hashes than SHA-1, by
their extensions.objectFormat; SHA-1's is 4b825dc642cb6eb9a060e54bf8d69288fbee4904.")

(defun git-empty-tree (settings)
  "The name of the empty tree in a repository whose extensions are SETTINGS."
  (or (cdr (assoc (second (assoc "objectformat" settings :test #'string=)) *git-empty-trees* :test #'equal))
      "4b825dc642cb6eb9a060e54bf8d69288fbee4904"))

(defparameter *git-commands-taking-no-programs*
  '(("status" "--ignore-submodules=all") ("diff" "--no-ext-diff" "--no-textconv" "--ignore-submodules=all")
    ("log" "--no-ext-diff" "--no-textconv") ("show" "--no-ext-diff" "--no-textconv")
    ("whatchanged" "--no-ext-diff" "--no-textconv") ("blame" "--no-textconv") ("grep" "--no-textconv")
    ("diff-files" "--no-ext-diff" "--no-textconv") ("diff-index" "--no-ext-diff" "--no-textconv")
    ("diff-tree" "--no-ext-diff" "--no-textconv"))
  "The options each git command is given in a repository not trusted, after
its name: no diff program, no text conversion, no submodule looked into.")

(defun git-reading-words (words)
  "WORDS, git's command and its own, with the options of
*GIT-COMMANDS-TAKING-NO-PROGRAMS* after the command; stash show given those
of show."
  (destructuring-bind (command &rest more) words
    (let ((extra (rest (assoc command *git-commands-taking-no-programs* :test #'string=))))
      (cond (extra (list* command (append extra more)))
            ((and (string= command "stash") (equal (first more) "show"))
             (list* command "show" (append (rest (assoc "show" *git-commands-taking-no-programs* :test #'string=)) (rest more))))
            (t words)))))

(defun git-blame-raw-words (words top here)
  "WORDS, git blame and its own, as blame is run from an empty folder, where
no mapping of names is found: the path after -- (or the last plain word)
made from TOP, HERE being the folder git was asked to run in; with no
revision given, the file as it is now given as the contents (--contents)
to blame against HEAD."
  (let* ((relative (if (and here (tuatara.io:path-within-p (tuatara.io:native here) (string-right-trim "/" top)))
                       (subseq (tuatara.io:slash-ended (tuatara.io:native here)) (length (tuatara.io:slash-ended top)))
                       ""))
         (dashes (position "--" words :test #'string=))
         (path (if dashes
                   (nth (1+ dashes) words)
                   (find-if-not (lambda (w) (uiop:string-prefix-p "-" w)) (rest words) :from-end t)))
         (before (if dashes (subseq words 0 dashes) (remove path words :count 1 :from-end t :test #'equal)))
         (revision (find-if-not (lambda (w) (uiop:string-prefix-p "-" w)) (rest before))))
    (if (null path)
        words
        (let ((whole (concatenate 'string relative path)))
          (append before
                  (and (not revision) (list "--contents" (tuatara.io:path-join top whole) "HEAD"))
                  (list "--" whole))))))

(defun call-git-reading (top directory arguments function)
  "Call FUNCTION with the arguments, the environment and the folder to run in
that git reads the repository at TOP with, not trusted, ARGUMENTS asked for
in DIRECTORY: refused first when they do not only read (GIT-READING-REFUSAL,
GIT-REFUSED signalled); then run on its shadow (TAKE-GIT-SHADOW), the index
copied for a command that reads it, with the attributes of the empty tree
and the options of GIT-READING-WORDS; blame and shortlog run from an empty
folder, as their raw forms. The shadow is let go of once FUNCTION returns."
  (let ((refusal (git-reading-refusal arguments)))
    (when refusal
      (error 'git-refused :operation (or (first (git-command-words arguments)) "") :path top
                          :reason (format nil "~a is looked at only (~a): ~a" (abbreviated-path top)
                                          (or (project-look-only-reason top) "it is not trusted") refusal))))
  (multiple-value-bind (folder common) (git-real-folders top)
    (let* ((words (git-command-words arguments))
           (raw (member (first words) '("blame" "shortlog") :test #'string=))
           (shadow (take-git-shadow top folder common
                                    (not (member (first words) *git-commands-without-index* :test #'string=)))))
      (unwind-protect
           (let ((empty (tuatara.io:path-join (git-shadow-folder shadow) "empty/"))
                 (words (if (equal (first words) "blame") (git-blame-raw-words words top directory) words)))
             (funcall function
                      (append (ldiff arguments (git-command-words arguments))
                              (list (format nil "--attr-source=~a" (git-empty-tree (git-shadow-settings shadow))))
                              (git-reading-words words))
                      (git-shadow-environment top common shadow (if raw empty top))
                      (if raw empty directory)))
        (let-go-of-git-shadow shadow)))))
