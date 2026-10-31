;;;; lisp/git/look-only.lisp --- Git in a repository looked at only: commands that read, and none of its programs.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; A repository answered "look only" when asked whether it is trusted, or one
;;;; that is, is in or holds an evidence folder, is still read by git, but only
;;;; read. RUN-GIT runs there the commands of *GIT-READING-COMMANDS* alone,
;;;; refusing the options that write a file, start a program or ask for
;;;; signatures, and gives git a setting for every way a repository can name a
;;;; program of its own: no file-system monitor, no hooks, no pager, no filters
;;;; or diff drivers (every one its settings name, found by git config, which
;;;; reads them as git does, the files they take in too, and runs nothing), no
;;;; attributes from its tree or from a file of yours, no ssh, credential
;;;; helper or signature program, no network at all, nothing of a submodule's.
;;;; Git is given a copy of the index, for some commands that only read write
;;;; it all the same, so that nothing in the repository changes, and the
;;;; objects are shown as they are, never as replace refs would have them.

(in-package #:tuatara.defaults)

(defparameter *git-reading-commands*
  '("status" "log" "diff" "show" "blame" "ls-files" "ls-tree" "cat-file" "rev-list" "rev-parse"
    "for-each-ref" "show-ref" "describe" "merge-base" "name-rev" "shortlog" "grep" "diff-tree"
    "diff-index" "diff-files" "check-ignore" "check-attr" "count-objects" "cherry" "range-diff"
    "whatchanged" "version" "hash-object" "stash" "worktree" "notes" "reflog" "remote" "branch" "tag"
    "config")
  "The git commands run in a repository looked at only: those that read it.
Some only as they are given (GIT-READING-WORDS-P): stash list and show,
worktree list, notes list and show, reflog shown, remote listed and
get-url, branch and tag listing, config reading, hash-object without -w.")

(defparameter *git-options-refused-looking*
  '("--output" "--output-directory" "--ext-diff" "--textconv" "--filters" "--open-files-in-pager"
    "--show-signature" "--recurse-submodules" "--upload-pack" "--exec" "--git-dir" "--work-tree")
  "The options of git's commands refused in a repository looked at only, or
any shortening of them git would take: they write a file, start a program,
check a signature or reach another repository.")

(defparameter *git-settings-looking*
  '(("core.fsmonitor" "false") ("core.hooksPath" "/dev/null") ("core.attributesFile" "/dev/null")
    ("core.pager" "cat") ("core.sshCommand" "") ("core.askPass" "") ("core.alternateRefsCommand" "")
    ("core.untrackedCache" "false") ("core.splitIndex" "false") ("core.useReplaceRefs" "false")
    ("credential.helper" "") ("gpg.program" "") ("gpg.openpgp.program" "") ("gpg.x509.program" "")
    ("gpg.ssh.program" "") ("gpg.ssh.defaultKeyCommand" "") ("diff.external" "")
    ("diff.submodule" "short") ("diff.ignoreSubmodules" "all") ("submodule.recurse" "false")
    ("status.submoduleSummary" "false") ("protocol.allow" "never") ("log.showSignature" "false"))
  "The settings git is given in a repository looked at only, each (KEY
VALUE), over the repository's own and yours: no program of its runs, no
submodule is looked into, nothing is fetched.")

(defparameter *git-programs-pattern*
  "^(filter\\..+\\.(clean|smudge|process|required)|diff\\..+\\.(textconv|command)|merge\\..+\\.driver|credential\\..+\\.helper|gpg\\..+\\.program|extensions\\.objectformat)$"
  "The settings naming a program git would run for a file or a remote, as
git config --get-regexp is asked for them, and the hash a repository's
objects are named by (GIT-PROGRAMS-NAMED).")

(defparameter *git-empty-trees*
  '(("sha256" . "6ef19b41225c5369f1c104d45d8d85efa9b057b53b14b4b9b939dd74decc5321"))
  "The empty tree's name in repositories of other hashes than SHA-1, by
their extensions.objectFormat; SHA-1's is 4b825dc642cb6eb9a060e54bf8d69288fbee4904.")

(defun git-option-refused-looking (word)
  "The option of *GIT-OPTIONS-REFUSED-LOOKING* WORD, an option given to a git
command, is or shortens (--out for --output), or NIL."
  (let* ((equals (position #\= word))
         (name (subseq word 0 (or equals (length word)))))
    (and (> (length name) 2)
         (uiop:string-prefix-p "--" name)
         (find-if (lambda (refused) (uiop:string-prefix-p name refused)) *git-options-refused-looking*))))

(defun git-plain-words (words)
  "Those of WORDS not beginning with -, before a standalone --."
  (loop for word in words
        until (string= word "--")
        unless (uiop:string-prefix-p "-" word) collect word))

(defun git-reading-words-p (command more)
  "True when git COMMAND, MORE the words after it, only reads: any of
*GIT-READING-COMMANDS*, but stash only list and show, worktree only list,
notes only list and show, reflog only shown, remote only listed or asked
its address (get-url), branch and tag only listing, config only reading
\(GIT-CONFIG-READING-P), and hash-object never with -w."
  (let ((plain (git-plain-words more)))
    (flet ((given (&rest options) (some (lambda (option) (member option more :test #'string=)) options))
           (first-in (&rest words) (or (null plain) (member (first plain) words :test #'string=))))
      (cond ((not (member command *git-reading-commands* :test #'string=)) nil)
            ((string= command "stash") (and plain (first-in "list" "show")))
            ((string= command "worktree") (and plain (first-in "list")))
            ((string= command "notes") (first-in "list" "show"))
            ((string= command "reflog") (first-in "show"))
            ((string= command "remote") (first-in "get-url"))
            ((string= command "branch")
             (and (or (null plain) (given "--list" "-l"))
                  (notany (lambda (word) (or (member word '("-d" "-D" "-m" "-M" "-c" "-C" "-f" "-u" "-t") :test #'string=)
                                             (some (lambda (option) (uiop:string-prefix-p word option))
                                                   '("--delete" "--move" "--copy" "--force" "--set-upstream-to" "--unset-upstream"
                                                     "--edit-description" "--track" "--no-track" "--create-reflog"))))
                          (remove-if-not (lambda (word) (uiop:string-prefix-p "-" word)) more))))
            ((string= command "tag")
             (and (or (null plain) (given "--list" "-l"))
                  (notany (lambda (word) (or (member word '("-a" "-s" "-u" "-f" "-d" "-v" "-m" "-F" "-e") :test #'string=)
                                             (some (lambda (option) (uiop:string-prefix-p word option))
                                                   '("--annotate" "--sign" "--local-user" "--force" "--delete" "--verify"
                                                     "--message" "--file" "--edit" "--create-reflog"))))
                          (remove-if-not (lambda (word) (uiop:string-prefix-p "-" word)) more))))
            ((string= command "config") (git-config-reading-p more))
            ((string= command "hash-object") (not (given "-w")))
            (t t)))))

(defun git-reading-refusal (arguments)
  "Why git is not run with ARGUMENTS in a repository looked at only, in
words; NIL when it only reads: no option before its command but those of
*GIT-LINE-OPTIONS-ALLOWED*, a command that only reads as it is given
\(GIT-READING-WORDS-P), no option of *GIT-OPTIONS-REFUSED-LOOKING* before a
standalone --, grep never given a program to show its files in (-O), and no
signature asked of a commit (%G in a format, %(signature) of a reference)."
  (let* ((words (git-command-words arguments))
         (command (first words))
         (options (loop for word in (rest words)
                        until (string= word "--")
                        when (uiop:string-prefix-p "-" word) collect word))
         (leading (remove-if (lambda (word) (member word *git-line-options-allowed* :test #'string=))
                             (ldiff arguments words))))
    (cond (leading (format nil "git is given no option before its command here (not ~a)" (first leading)))
          ((null command) "no git command is given")
          ((not (git-reading-words-p command (rest words)))
           (format nil "git ~a~@[ ~a~] changes the repository, or reaches another, and git only reads here"
                   command (first (git-plain-words (rest words)))))
          ((some #'git-option-refused-looking options)
           (format nil "~a writes a file, runs a program or reaches elsewhere, and git only reads here"
                   (git-option-refused-looking (find-if #'git-option-refused-looking options))))
          ((and (string= command "grep") (some (lambda (word) (uiop:string-prefix-p "-O" word)) options))
           "grep -O runs a program to show the files, and git only reads here")
          ((some (lambda (word) (or (search "%G" word) (search "%(signature" word))) options)
           "a signature checked runs a program, and git only reads here"))))

(defvar *git-index-paths* (make-hash-table :test 'equal :synchronized t)
  "Where the index of each repository looked at only is, by its top, as git
said once (GIT-INDEX-PATH).")

(defun git-reading-output (git top arguments)
  "What git, the program GIT, prints with ARGUMENTS in the repository at TOP,
looked at only, as text, for the settings it is to be given: run as
reading (RUN-GIT-PROGRAM), within ten seconds; NIL when it failed."
  (multiple-value-bind (octets ok)
      (run-git-program git top arguments :environment (git-looking-environment top) :reading t :seconds 10)
    (and octets (or ok (zerop (length octets))) (tuatara.text:lenient-string octets))))

(defun git-index-path (git top)
  "Where the index of the repository at TOP is, by git, the program GIT: its
own folder's index, asked once (*GIT-INDEX-PATHS*); NIL when git could not
say."
  (or (gethash top *git-index-paths*)
      (let ((path (string-trim '(#\Newline)
                               (or (git-reading-output git top (list "rev-parse" "--path-format=absolute" "--git-path" "index")) ""))))
        (and (plusp (length path)) (setf (gethash top *git-index-paths*) path)))))

(defun git-programs-named (git top)
  "The settings of the repository at TOP, and yours, naming a program git
would run on its files (*GIT-PROGRAMS-PATTERN*), as git config reads them,
the files they take in (include.path, includeIf) followed as git follows
them; a list of (KEY VALUE). Signals GIT-REFUSED when git cannot read them:
whatever git then reads, it is not known what it would run."
  (let ((text (git-reading-output git top (list "config" "-z" "--get-regexp" *git-programs-pattern*))))
    (unless text
      (error 'git-refused :operation "config" :path top
                          :reason (format nil "the settings of ~a cannot be read, so what git would run there is not known"
                                          (abbreviated-path top))))
    (loop for entry in (uiop:split-string text :separator (list (code-char 0)))
          for newline = (position #\Newline entry)
          when (plusp (length entry))
            collect (list (subseq entry 0 (or newline (length entry)))
                          (if newline (subseq entry (1+ newline)) "")))))

(defun git-looking-environment (top &optional named)
  "What git is given to read the repository at TOP, looked at only: no lock
taken that it could leave out, no replace refs, TOP as its working folder
whatever its settings say, and every setting of *GIT-SETTINGS-LOOKING*, and
of NAMED (from GIT-PROGRAMS-NAMED) blanked, filters said not to be
required, as GIT_CONFIG_COUNT settings, which hold any name a setting may
have."
  (let ((settings (append *git-settings-looking*
                          (loop for (key) in named
                                unless (string= key "extensions.objectformat")
                                  collect (list key (if (uiop:string-suffix-p key ".required") "false" ""))))))
    (append (list "GIT_OPTIONAL_LOCKS=0" "GIT_NO_REPLACE_OBJECTS=1"
                  (format nil "GIT_WORK_TREE=~a" (string-right-trim "/" top))
                  (format nil "GIT_CONFIG_COUNT=~d" (length settings)))
            (loop for (key value) in settings
                  for i from 0
                  collect (format nil "GIT_CONFIG_KEY_~d=~a" i key)
                  collect (format nil "GIT_CONFIG_VALUE_~d=~a" i value)))))

(defun git-empty-tree (named)
  "The name of the empty tree in a repository whose settings NAMED (from
GIT-PROGRAMS-NAMED) say how its objects are named."
  (let ((format (second (assoc "extensions.objectformat" named :test #'string=))))
    (or (cdr (assoc (and format (string-downcase format)) *git-empty-trees* :test #'equal))
        "4b825dc642cb6eb9a060e54bf8d69288fbee4904")))

(defparameter *git-commands-taking-no-programs*
  '(("status" "--ignore-submodules=all") ("diff" "--no-ext-diff" "--no-textconv" "--ignore-submodules=all")
    ("log" "--no-ext-diff" "--no-textconv") ("show" "--no-ext-diff" "--no-textconv")
    ("whatchanged" "--no-ext-diff" "--no-textconv") ("blame" "--no-textconv") ("grep" "--no-textconv")
    ("diff-files" "--no-ext-diff" "--no-textconv") ("diff-index" "--no-ext-diff" "--no-textconv")
    ("diff-tree" "--no-ext-diff" "--no-textconv"))
  "The options each git command is given in a repository looked at only,
after its name: no diff program, no text conversion, no submodule looked
into.")

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

(defun call-git-reading (git top arguments function)
  "Call FUNCTION with the arguments and the environment git, the program
GIT, is run with to read the repository at TOP, looked at only, with
ARGUMENTS, refused first when they do not only read (GIT-READING-REFUSAL,
GIT-REFUSED signalled): the attributes of the empty tree, the options of
GIT-READING-WORDS, the environment of GIT-LOOKING-ENVIRONMENT with the
programs its settings name (GIT-PROGRAMS-NAMED), and a copy of the index,
made in a private folder of its own and deleted once FUNCTION returns."
  (let ((refusal (git-reading-refusal arguments)))
    (when refusal
      (error 'git-refused :operation (or (first (git-command-words arguments)) "") :path top
                          :reason (format nil "~a is looked at only (~a): ~a" (abbreviated-path top)
                                          (project-look-only-reason top) refusal))))
  (let* ((named (git-programs-named git top))
         (words (git-command-words arguments))
         (index (git-index-path git top))
         (folder (new-private-folder (private-cache-directory "git-index/"))))
    (unwind-protect
         (let ((copy (tuatara.io:path-join folder "index")))
           (when (and index (tuatara.io:path-kind index :follow nil))
             (tuatara.io:copy-to-free-name index folder "index"))
           (funcall function
                    (append (ldiff arguments words)
                            (list (format nil "--attr-source=~a" (git-empty-tree named)))
                            (git-reading-words words))
                    (cons (format nil "GIT_INDEX_FILE=~a" copy) (git-looking-environment top named))))
      (delete-private-folder folder))))
