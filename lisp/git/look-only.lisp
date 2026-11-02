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

(in-package #:tuatara.git)

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

(defparameter *git-line-options-allowed* '("--no-pager" "-P" "--literal-pathspecs")
  "The options before the command \\[git-run-command] takes while evidence
folders are named, and git is given in a repository not trusted: none that
sets something or points git elsewhere.")

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
                      :reason (escape-controls (format nil "~a is not read: ~?" (abbreviated-path top) control arguments))))

(defparameter *git-small-file-most* (* 64 1024)
  "The most octets a file of git's own that names a place (.git as a file,
commondir) may hold to be read.")

(defun git-small-file (top path what)
  "The text of the file at PATH, of the repository at TOP: a regular file of
at most *GIT-SMALL-FILE-MOST* octets, examined before it is opened, so that a
named pipe or a device is never opened; WHAT names it when it cannot be."
  (let ((stamp (tuatara.io:path-stamp path :follow nil :failure nil)))
    (unless (and stamp (eq (tuatara.io:stamp-kind stamp) :regular))
      (git-refuse-reading top "its ~a is ~:[not there~;not a file~]" what stamp))
    (when (> (tuatara.io:stamp-size stamp) *git-small-file-most*)
      (git-refuse-reading top "its ~a is too large to be one" what))
    (string-right-trim '(#\Newline #\Return #\Space) (tuatara.text:lenient-string (tuatara.io:file-octets path)))))

(defun git-folder-from (base text)
  "The folder TEXT, as a file of git's own names it, from BASE when it is
relative, where it really is, ending in /; NIL when it is not there."
  (let ((real (ignoring-errors-reported ("finding the folder ~a" text)
                (tuatara.io:real-path (if (uiop:string-prefix-p "/" text) text (tuatara.io:path-join base text))))))
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
      (unless (eq (tuatara.io:path-kind (tuatara.io:path-join common "objects")) :directory)
        (git-refuse-reading top "~a holds no objects" (abbreviated-path common)))
      (values folder common))))

(defun git-real-folder (top)
  "Git's own folder for the repository or worktree at TOP (GIT-REAL-FOLDERS),
or NIL when it has none git could read."
  (handler-case (values (git-real-folders top)) (git-refused () nil)))

;;; Its shadow

(defstruct (git-shadow (:constructor make-git-shadow (folder)) (:copier nil))
  "A shadow of a repository not trusted, in FOLDER: STATE, the state of what
it was copied from (GIT-REPOSITORY-STATE); FOLDERS, the folders of its
references, whose stamps tell when one was written; SETTINGS, the extensions
it was given, (NAME VALUE) each; ELSEWHERE, the folders of objects outside
the repository its alternates name; COPIED, the octets copied into it so
far; INDEX-STAMPS and LOGS-STAMPS, the state of the index and of the logs
copied into it last, :NONE before; USERS, how many runs of git use it now;
RETIRED, true once a newer shadow took its place, deleted when the last run
using it is done."
  folder (state :none) (folders '()) settings (elsewhere '()) (copied 0)
  (index-stamps :none) (logs-stamps :none) (users 0) retired)

(setf (documentation 'git-shadow-p 'function) "True when OBJECT is a GIT-SHADOW.")

(defstruct (git-shadow-place (:constructor make-git-shadow-place ()) (:copier nil))
  "Where the shadow of one repository is kept: LOCK, held while it is looked
at, made, taken up or let go of, so that one repository's shadow being made
holds up no other; CURRENT, the shadow in use, or NIL."
  (lock (sb-thread:make-mutex :name "tuatara git shadow")) current)

(setf (documentation 'git-shadow-place-p 'function) "True when OBJECT is a GIT-SHADOW-PLACE.")

(defvar *git-shadows* (make-hash-table :test 'equal :synchronized t)
  "The GIT-SHADOW-PLACE of each repository not trusted git was run in, by its
own folder (GIT-REAL-FOLDERS).")

(defparameter *git-shadow-most-entries* 200000
  "The most files and folders a repository's references and their logs may
hold for a shadow to be made of them.")

(defparameter *git-shadow-file-most*
  '(("index" . #.(* 1024 1024 1024)) ("packed-refs" . #.(* 256 1024 1024)) ("logs" . #.(* 64 1024 1024))
    ("shallow" . #.(* 256 1024 1024)) ("references" . #.(* 64 1024)) (t . #.(* 1024 1024)))
  "The most octets a file of each kind copied into a shadow may hold: the
index (and a shared index), the packed references, the logs and the list of
shallow commits as a large repository has them, a loose reference (a hash
or a name), anything else.")

(defparameter *git-shadow-most* (* 2 1024 1024 1024)
  "The most octets a shadow may hold copied, all its files together.")

(defparameter *git-extensions-read*
  '(("objectformat" "sha1" "sha256") ("refstorage" "files" "reftable"))
  "The extensions a shadow is given as its repository has them, each with
the values it may have.")

(defparameter *git-extensions-left-out* '("partialclone" "preciousobjects" "worktreeconfig" "noop")
  "The extensions a repository may have that its shadow does without:
nothing fetched, nothing kept from being pruned, no settings of a
worktree's own.")

(defparameter *git-alternates-deepest* 5
  "How many alternates deep git follows, one naming another: as deep their
files are looked at before git is run.")

(defvar *git-shadows-folder* nil
  "This session's folder of shadows and the file descriptor (FD) of its lock,
held as long as the session runs: (FOLDER . FD), made the first time one is
wanted.")

(defvar *git-shadows-folder-lock* (sb-thread:make-mutex :name "tuatara git shadows folder")
  "Held while this session's folder of shadows is made.")

(defun git-shadows-directory ()
  "Where this session's shadows are made: a folder of its own in git-shadow/
in the private cache (MAKE-OWNED-FOLDER), made the first time one is wanted."
  (sb-thread:with-mutex (*git-shadows-folder-lock*)
    (car (or *git-shadows-folder*
             (setf *git-shadows-folder* (multiple-value-call #'cons (make-owned-folder "git-shadow/")))))))

(defun forget-old-git-shadows ()
  "Delete the shadows of sessions on this machine no longer running, as one
that ended without deleting its own (a crash) leaves them
\(FORGET-ABANDONED-FOLDERS)."
  (forget-abandoned-folders "git-shadow/" (car *git-shadows-folder*)))

(defun git-file-row (path)
  "What a shadow knows of the file or folder at PATH, a link not followed:
its kind, device, inode, size, and the times of its change, in nanoseconds;
NIL when it is not there."
  (let ((stamp (tuatara.io:path-stamp path :follow nil :failure nil)))
    (and stamp (list (tuatara.io:stamp-kind stamp) (tuatara.io:stamp-dev stamp) (tuatara.io:stamp-ino stamp)
                     (tuatara.io:stamp-size stamp) (tuatara.io:stamp-mtime stamp) (tuatara.io:stamp-ctime stamp)))))

(defun git-tree-entries (top root)
  "Every file and folder within ROOT, a folder of the repository at TOP, as
\(PATH KIND . ROW) (ROW as GIT-FILE-ROW gives it), PATH from ROOT, in
order. A link, or anything but a file or a folder, is left out, never
opened. NIL when ROOT is not there."
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
                       (push (list* path own (git-file-row (tuatara.io:path-join folder name))) rows)
                       (when (eq own :directory)
                         (walk (tuatara.io:slash-ended (tuatara.io:path-join folder name))
                               (concatenate 'string path "/") (1+ depth)))))))))
      (when (eq (tuatara.io:path-kind root :follow nil) :directory)
        (walk (tuatara.io:slash-ended root) "" 0)))
    (nreverse rows)))

(defun git-repository-state (folder common folders)
  "What a shadow of the repository whose own folders are FOLDER and COMMON is
made again by: the state of its HEAD, settings, packed references, shallow
commits, patterns left out and reference table, and of FOLDERS, the folders
of its references, which change whenever one is written (git writes a
reference to a new name, then renames it over the old)."
  (list (git-file-row (tuatara.io:path-join folder "HEAD"))
        (mapcar (lambda (name) (git-file-row (tuatara.io:path-join common name)))
                '("config" "packed-refs" "shallow" "info/exclude" "reftable/" "reftable/tables.list"
                  "objects/info/" "objects/pack/" "objects/info/alternates"))
        (mapcar #'git-file-row folders)))

(defun git-file-most (what)
  "The most octets a file copied into a shadow may hold as WHAT (*GIT-SHADOW-FILE-MOST*)."
  (cdr (or (assoc what *git-shadow-file-most* :test #'equal) (assoc t *git-shadow-file-most*))))

(defun git-copy-file (top from to what shadow)
  "Copy the file FROM, of the repository at TOP, to TO, in SHADOW, when it is
there, as WHAT (index, packed-refs, logs, references, or another of git's
own files). It is examined first, so that a named pipe or a device is never
opened; refused (GIT-REFUSE-READING) when it is not a file, holds holes (a
file of more octets than the disk gives it), is larger than such a file
may be (GIT-FILE-MOST), or would take the shadow past *GIT-SHADOW-MOST*."
  (let ((stamp (tuatara.io:path-stamp from :follow nil :failure nil)))
    (cond ((null stamp))
          ((not (eq (tuatara.io:stamp-kind stamp) :regular))
           (git-refuse-reading top "its ~a ~a is not a file" what (abbreviated-path from)))
          ((tuatara.io:file-holes-p from)
           (git-refuse-reading top "its ~a ~a has holes, more octets than the disk holds" what (abbreviated-path from)))
          ((> (tuatara.io:stamp-size stamp) (git-file-most what))
           (git-refuse-reading top "its ~a ~a is larger than any a repository holds (~a)" what (abbreviated-path from)
                               (tuatara.io:octets-words (tuatara.io:stamp-size stamp))))
          ((> (+ (git-shadow-copied shadow) (tuatara.io:stamp-size stamp)) *git-shadow-most*)
           (git-refuse-reading top "its references, logs and index together are larger than ~a"
                               (tuatara.io:octets-words *git-shadow-most*)))
          (t (incf (git-shadow-copied shadow) (tuatara.io:stamp-size stamp))
             (ensure-directories-exist (sb-ext:parse-native-namestring (tuatara.io:parent-path to)))
             (tuatara.io:copy-regular-file from to)))))

(defun git-copy-tree (top from to what shadow)
  "Copy the folder FROM, of the repository at TOP, into TO, in SHADOW: its
folders made, its files copied as WHAT (GIT-COPY-FILE), a link or anything
else left out unopened; the folders, where they are in FROM, as a list."
  (let ((folders (list (tuatara.io:slash-ended from))))
    (ensure-directories-exist (sb-ext:parse-native-namestring (tuatara.io:slash-ended to)))
    (dolist (row (git-tree-entries top from) (nreverse folders))
      (destructuring-bind (path kind &rest stamp) row
        (declare (ignore stamp))
        (let ((target (tuatara.io:path-join to path)))
          (if (eq kind :directory)
              (progn (push (tuatara.io:slash-ended (tuatara.io:path-join from path)) folders)
                     (ensure-directories-exist (sb-ext:parse-native-namestring (tuatara.io:slash-ended target))))
              (git-copy-file top (tuatara.io:path-join from path) target what shadow)))))))

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

(defun git-repository-settings (top common shadow)
  "The extensions the repository at TOP, whose common folder is COMMON, needs
to be read, (NAME VALUE) each: read by git config from a copy of its
settings made in SHADOW, nothing they name taken in. None for a repository
of git's format 0, which git reads with none; GIT-REFUSED for one of a
format newer than 1, or one needing an extension that is neither one of
*GIT-EXTENSIONS-READ* nor one of *GIT-EXTENSIONS-LEFT-OUT*."
  (let ((copy (tuatara.io:path-join (git-shadow-folder shadow) "settings-read")))
    (git-copy-file top (tuatara.io:path-join common "config") copy "settings" shadow)
    (if (not (tuatara.io:path-kind copy :follow nil))
        '()
        (let* ((text (unwind-protect (git-helper-output top (git-shadow-folder shadow)
                                                        (list "config" "--file" copy "--no-includes" "-z" "--list"))
                       (handler-case (delete-file (sb-ext:parse-native-namestring copy)) (file-error () nil))))
               (entries (loop for entry in (uiop:split-string text :separator (list (code-char 0)))
                              for newline = (position #\Newline entry)
                              when (plusp (length entry))
                                collect (list (string-downcase (subseq entry 0 (or newline (length entry))))
                                              (if newline (subseq entry (1+ newline)) "true"))))
               ;; the last of a setting given twice is the one git takes
               (entries (reverse entries))
               (version (parse-integer (or (second (assoc "core.repositoryformatversion" entries :test #'string=)) "0")
                                       :junk-allowed t)))
          (unless (member version '(0 1))
            (git-refuse-reading top "it is of git's format ~a, which is not read here" version))
          (and (eql version 1)
               (loop for (key value) in entries
                     when (uiop:string-prefix-p "extensions." key)
                       append (let* ((name (subseq key 11))
                                     (read (assoc name *git-extensions-read* :test #'string=)))
                                (cond ((and read (member (string-downcase value) (rest read) :test #'string=))
                                       (list (list name (string-downcase value))))
                                      ((member name *git-extensions-left-out* :test #'string=) '())
                                      (t (git-refuse-reading top "it needs git's extension ~a = ~a, which is not read here"
                                                             name value))))))))))

(defun git-shadow-settings-text (settings)
  "The settings a shadow's git reads, for SETTINGS, its extensions."
  (format nil "[core]~%	repositoryformatversion = ~d~%	bare = false~%	logallrefupdates = false~%~@[[extensions]~%~{	~{~a = ~a~}~%~}~]"
          (if settings 1 0) settings))

(defun git-object-folders (top common)
  "The folders of objects git reads for the repository at TOP, whose common
folder is COMMON: its own, then those its alternates name, and theirs in
turn, *GIT-ALTERNATES-DEEPEST* deep, each where it really is, ending in /.
Read without git, and every file git would open there looked at first: an
alternates file, and what the folders of packs and of git's notes on its
objects hold, must be files (a named pipe is never opened), and an alternate
must be a folder. GIT-REFUSED otherwise."
  (let ((found '()))
    (labels ((visit (folder depth)
               (let ((real (git-folder-from folder ".")))
                 (unless real (git-refuse-reading top "its objects ~a are not a folder" (abbreviated-path folder)))
                 (unless (member real found :test #'string=)
                   (push real found)
                   (dolist (inside '("pack/" "info/" "info/commit-graphs/"))
                     (dolist (entry (ignoring-errors-reported ("reading ~a~a" real inside)
                                      (tuatara.io:directory-entries (tuatara.io:path-join real inside))))
                       (unless (member (fourth entry) '(:regular :directory))
                         (git-refuse-reading top "~a~a~a is not a file" (abbreviated-path real) inside (first entry)))))
                   (let ((alternates (tuatara.io:path-join real "info/alternates")))
                     (when (tuatara.io:path-kind alternates :follow nil)
                       (when (>= depth *git-alternates-deepest*)
                         (git-refuse-reading top "its alternates name others more than ~d deep" *git-alternates-deepest*))
                       (dolist (line (uiop:split-string (git-small-file top alternates "alternates") :separator '(#\Newline)))
                         (let ((line (string-trim '(#\Space #\Return #\Tab) line)))
                           (cond ((or (zerop (length line)) (char= (char line 0) #\#)))
                                 ((char= (char line 0) #\")
                                   (git-refuse-reading top "its alternates name a folder git reads quoted"))
                                 (t (visit (if (char= (char line 0) #\/) line (tuatara.io:path-join real line))
                                           (1+ depth))))))))))))
      (visit (tuatara.io:path-join common "objects/") 0))
    (nreverse found)))

(defun git-copy-logs (top folder common shadow which)
  "Copy into SHADOW the logs of the references of the repository at TOP, its
own folders FOLDER and COMMON, WHICH of them: :STASH the stash's alone,
:ALL every one (HEAD's from FOLDER); again only when they changed since they
were copied last."
  (let* ((from (tuatara.io:path-join common (if (eq which :stash) "logs/refs/stash" "logs/")))
         (rows (if (eq which :stash) (git-file-row from) (git-tree-entries top from)))
         (key (cons which rows)))
    (unless (equal key (git-shadow-logs-stamps shadow))
      (let ((to (tuatara.io:path-join (git-shadow-folder shadow) (if (eq which :stash) "logs/refs/stash" "logs/"))))
        (if (eq which :stash)
            (progn (handler-case (delete-file (sb-ext:parse-native-namestring to)) (file-error () nil))
                   (git-copy-file top from to "logs" shadow))
            (progn (ignoring-errors-reported ("deleting the copy ~a" to) (tuatara.io:delete-tree to))
                   (git-copy-tree top from to "logs" shadow)
                   (unless (string= folder common)
                     (handler-case (delete-file (sb-ext:parse-native-namestring (tuatara.io:path-join to "HEAD")))
                       (file-error () nil))
                     (git-copy-file top (tuatara.io:path-join folder "logs/HEAD") (tuatara.io:path-join to "HEAD") "logs" shadow)))))
      (setf (git-shadow-logs-stamps shadow) key))))

(defun make-git-shadow-of (top folder common)
  "Make a shadow of the repository at TOP, its own folders FOLDER and COMMON:
a new private folder holding settings of its own, and copies of HEAD, the
references, a reference table, packed references, the shallow commits and
the patterns of files left out; an empty folder for objects of its own, and
an empty one to run in. Its objects' folders are looked at first
\(GIT-OBJECT-FOLDERS): those outside the repository are kept as ELSEWHERE."
  (let* ((place (new-private-folder (git-shadows-directory)))
         (shadow (make-git-shadow place)))
    (handler-bind ((error (lambda (e) (declare (ignore e)) (delete-private-folder place))))
      (let ((settings (git-repository-settings top common shadow))
            (own (string-right-trim "/" (tuatara.io:parent-path common))))
        (setf (git-shadow-elsewhere shadow)
              (remove-if (lambda (objects) (tuatara.io:path-within-p objects own)) (git-object-folders top common)))
        (tuatara.io:write-private-file (tuatara.io:path-join place "config") (git-shadow-settings-text settings))
        (git-copy-file top (tuatara.io:path-join folder "HEAD") (tuatara.io:path-join place "HEAD") "HEAD" shadow)
        (git-copy-file top (tuatara.io:path-join common "packed-refs") (tuatara.io:path-join place "packed-refs") "packed-refs" shadow)
        (dolist (name '("shallow" "info/exclude"))
          (git-copy-file top (tuatara.io:path-join common name) (tuatara.io:path-join place name) name shadow))
        (setf (git-shadow-folders shadow)
              (git-copy-tree top (tuatara.io:path-join common "refs/") (tuatara.io:path-join place "refs/") "references" shadow))
        (when (tuatara.io:path-kind (tuatara.io:path-join common "reftable/") :follow nil)
          (git-copy-tree top (tuatara.io:path-join common "reftable/") (tuatara.io:path-join place "reftable/") "packed-refs" shadow))
        (dolist (name '("refs/" "objects/" "empty/" "logs/"))
          (ensure-directories-exist (sb-ext:parse-native-namestring (tuatara.io:path-join place name))))
        (setf (git-shadow-settings shadow) settings
              (git-shadow-state shadow) (git-repository-state folder common (git-shadow-folders shadow)))
        shadow))))

(defun git-index-rows (folder)
  "What a shadow's copy of the index in git's own FOLDER is made again by:
the state of the index and of each shared index beside it (sharedindex.*)."
  (cons (git-file-row (tuatara.io:path-join folder "index"))
        (loop for entry in (ignoring-errors-reported ("reading ~a" folder) (tuatara.io:directory-entries folder))
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
              (progn (git-copy-file top from fresh "index" shadow)
                     (tuatara.io:rename-path fresh to :replace t))
              (handler-case (delete-file (sb-ext:parse-native-namestring to)) (file-error () nil)))))
      (setf (git-shadow-index-stamps shadow) rows))))

(defparameter *git-commands-without-index*
  '("log" "show" "whatchanged" "cat-file" "ls-tree" "rev-list" "for-each-ref" "show-ref" "merge-base" "name-rev"
    "shortlog" "branch" "tag" "remote" "notes" "reflog" "worktree" "count-objects" "version" "cherry" "range-diff"
    "hash-object" "config")
  "The git commands that never read the index: no copy of it is made for them.")

(defun git-logs-wanted (words)
  "Which logs of references git's command WORDS reads: :STASH for stash, :ALL
for reflog, or a command walking the logs (-g, --walk-reflogs) or naming a
place in one (@{...}); NIL for one that reads none."
  (let ((command (first words)))
    (cond ((equal command "stash") :stash)
          ((equal command "reflog") :all)
          ((some (lambda (word) (or (member word '("-g" "--walk-reflogs") :test #'string=) (search "@{" word))) (rest words))
           :all))))

(defun retire-git-shadow (shadow)
  "Mark SHADOW as replaced: deleted at once when no run of git uses it, else
when the last does."
  (setf (git-shadow-retired shadow) t)
  (when (zerop (git-shadow-users shadow))
    (delete-private-folder (git-shadow-folder shadow))))

(defun take-git-shadow (top folder common &key index logs)
  "The shadow of the repository at TOP, its own folders FOLDER and COMMON,
taken up for one run of git: made now when there is none, or when what it
copies changed (the one before then retired); with INDEX, the index copied
into it when it changed, with LOGS (as GIT-LOGS-WANTED says) those logs.
Only the repository's own lock is held meanwhile, so that its shadow being
made holds up no other's. LET-GO-OF-GIT-SHADOW lets go of it."
  (let ((place (or (gethash folder *git-shadows*)
                   (sb-ext:with-locked-hash-table (*git-shadows*)
                     (or (gethash folder *git-shadows*)
                         (setf (gethash folder *git-shadows*) (make-git-shadow-place)))))))
    (sb-thread:with-mutex ((git-shadow-place-lock place))
      (let ((shadow (git-shadow-place-current place)))
        (unless (and shadow (equal (git-repository-state folder common (git-shadow-folders shadow))
                                   (git-shadow-state shadow)))
          (when shadow
            (setf (git-shadow-place-current place) nil)
            (retire-git-shadow shadow))
          (setf shadow (make-git-shadow-of top folder common)
                (git-shadow-place-current place) shadow))
        (when index (git-copy-index top folder shadow))
        (when logs (git-copy-logs top folder common shadow logs))
        (incf (git-shadow-users shadow))
        (values shadow place)))))

(defun let-go-of-git-shadow (shadow place)
  "One run of git is done with SHADOW, kept in PLACE; deleted when it was
retired and none uses it now."
  (sb-thread:with-mutex ((git-shadow-place-lock place))
    (decf (git-shadow-users shadow))
    (when (and (git-shadow-retired shadow) (zerop (git-shadow-users shadow)))
      (delete-private-folder (git-shadow-folder shadow)))))

(defun git-shadow-elsewhere-of (top)
  "The folders of objects outside the repository at TOP its shadow reads, as
its alternates name them, when it has a shadow; NIL else."
  (let* ((folder (git-real-folder top))
         (place (and folder (gethash folder *git-shadows*)))
         (shadow (and place (git-shadow-place-current place))))
    (and shadow (git-shadow-elsewhere shadow))))

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

(defparameter *git-blame-options-taking-values* '("-L")
  "The options of git blame that take the next word as their value.")

(defun git-blame-raw-words (words top here)
  "WORDS, git blame and its own, as blame is run from an empty folder, where
no mapping of names is found: the path after -- (or the last plain word,
values of options left aside) made from TOP, HERE being the folder git was
asked to run in. With no revision given, the file as it is now is given as
the contents (--contents) to blame against HEAD when it is a file; a link is
blamed as HEAD has it, its own text; anything else is refused
\(GIT-REFUSE-READING)."
  (let* ((relative (if (and here (tuatara.io:path-within-p (tuatara.io:native here) (string-right-trim "/" top)))
                       (subseq (tuatara.io:slash-ended (tuatara.io:native here)) (length (tuatara.io:slash-ended top)))
                       ""))
         (dashes (position "--" words :test #'string=))
         (before (if dashes (subseq words 0 dashes) words))
         (plain (let ((value nil))
                  (loop for word in (rest before)
                        if value do (setf value nil)
                        else if (member word *git-blame-options-taking-values* :test #'string=) do (setf value t)
                        else unless (uiop:string-prefix-p "-" word) collect word)))
         (path (if dashes (nth (1+ dashes) words) (car (last plain))))
         (before (if dashes before (remove path before :count 1 :from-end t :test #'equal)))
         (revision (if dashes (first plain) (and (rest plain) (first plain)))))
    (if (null path)
        words
        (let* ((whole (concatenate 'string relative path))
               (file (tuatara.io:path-join top whole))
               (kind (tuatara.io:path-kind file :follow nil)))
          (append before
                  (cond (revision '())
                        ((eq kind :regular) (list "--contents" file "HEAD"))
                        ((eq kind :symlink) (list "HEAD"))
                        (t (git-refuse-reading top "~a is not a file to blame" whole)))
                  (list "--" whole))))))

(defun note-git-timed-out (top)
  "Note that git ran too long in the repository at TOP, not trusted, and say
so, once: it is not run there again this session until \\[git] asks."
  (unless (gethash top *git-timed-out*)
    (setf (gethash top *git-timed-out*) t)
    (message "~a" (escape-controls (format nil "git ran too long in ~a: it is not asked of it again this session, the change gutter there left out, until ~a there"
                                           (abbreviated-path top) (substitute-command-keys "\\[git]"))))))

(defun forget-git-shadow (top)
  "Have the repository at TOP read anew when git next runs there: its shadow
made again (a reference written over in place seen too), and git tried
again if it ran too long there before."
  (remhash top *git-timed-out*)
  (let* ((folder (git-real-folder top))
         (place (and folder (gethash folder *git-shadows*))))
    (when place
      (sb-thread:with-mutex ((git-shadow-place-lock place))
        (let ((shadow (git-shadow-place-current place)))
          (when shadow
            (setf (git-shadow-place-current place) nil)
            (retire-git-shadow shadow)))))))

(defun git-reads-only-p (top)
  "True when git only reads the repository at TOP, on its shadow: it is not
trusted (PROJECT-TRUSTED-P); or it is, but git's own folder its .git leads
to, or the common one, is of a repository looked at only (a folder answered
so, or within one, or an evidence folder or one holding it)."
  (or (not (project-trusted-p top))
      (multiple-value-bind (folder common) (ignoring-errors-reported ("finding git's own folder for ~a" top)
                                             (handler-case (git-real-folders top) (git-refused () nil)))
        (and folder
             (or (project-look-only-reason (tuatara.io:parent-path (string-right-trim "/" common)))
                 (project-look-only-reason folder)
                 (project-look-only-reason common))
             t))))

(defun git-folder-elsewhere (top)
  "Git's own folder for the repository at TOP when it is not TOP's .git, as
a .git file or link, or a worktree's common folder, leads elsewhere; NIL
when it is there, or there is none."
  (multiple-value-bind (folder common) (ignoring-errors-reported ("finding git's own folder for ~a" top)
                                         (handler-case (git-real-folders top) (git-refused () nil)))
    (let ((own (let ((real (real-folder top))) (and real (tuatara.io:path-join real ".git/")))))
      (and folder (not (and own (string= folder own) (string= common own)))
           (if (string= folder common) folder (format nil "~a (its common folder ~a)" folder common))))))

(defun call-git-reading (top directory arguments function)
  "Call FUNCTION with the arguments, the environment and the folder to run in
that git reads the repository at TOP with, not trusted, ARGUMENTS asked for
in DIRECTORY: refused first when they do not only read (GIT-READING-REFUSAL,
GIT-REFUSED signalled); then run on its shadow (TAKE-GIT-SHADOW), the index
copied for a command that reads it and the logs for one that reads them,
with the attributes of the empty tree and the options of GIT-READING-WORDS;
blame and shortlog run from an empty folder, as their raw forms. The shadow
is let go of once FUNCTION returns."
  (let ((refusal (git-reading-refusal arguments)))
    (when refusal
      (error 'git-refused :operation (or (first (git-command-words arguments)) "") :path top
                          :reason (escape-controls (format nil "~a is looked at only (~a): ~a" (abbreviated-path top)
                                                           (or (project-look-only-reason top) "it is not trusted") refusal)))))
  (multiple-value-bind (folder common) (git-real-folders top)
    (let ((words (git-command-words arguments)))
      (multiple-value-bind (shadow place)
          (take-git-shadow top folder common
                           :index (not (member (first words) *git-commands-without-index* :test #'string=))
                           :logs (git-logs-wanted words))
        (unwind-protect
             (let* ((raw (member (first words) '("blame" "shortlog") :test #'string=))
                    (empty (tuatara.io:path-join (git-shadow-folder shadow) "empty/"))
                    (words (if (equal (first words) "blame") (git-blame-raw-words words top directory) words)))
               (funcall function
                        (append (ldiff arguments (git-command-words arguments))
                                (list (format nil "--attr-source=~a" (git-empty-tree (git-shadow-settings shadow))))
                                (git-reading-words words))
                        (git-shadow-environment top common shadow (if raw empty top))
                        (if raw empty directory)))
          (let-go-of-git-shadow shadow place))))))
