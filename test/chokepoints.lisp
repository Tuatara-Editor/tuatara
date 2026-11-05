;;;; test/chokepoints.lisp --- Only the files meant to do a dangerous or central thing do it.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; Starting programs, reading environment variables, writing and removing
;;;; files and naming colours each have a place in the code. A new use of one
;;;; elsewhere fails here, so that it is added to the place that exists, or
;;;; to the list of exceptions below with a reason. An exception whose file no
;;;; longer does the thing fails too, so the lists stay short.

(in-package #:tuatara.tests)

(defvar *core-tokens* nil
  "The tokens of each core source file, as (RELATIVE-NAME . TOKENS), while a chokepoint test runs; NIL otherwise.")

(defun core-tokens ()
  "The tokens (TEXT-TOKENS) of each core source file, as (RELATIVE-NAME . TOKENS)."
  (or *core-tokens*
      (loop for file in (core-source-files)
            collect (cons (tree-relative-name file)
                          (text-tokens (coerce (uiop:read-file-string file :external-format :utf-8) 'simple-string))))))

(defun token-runs (tokens length)
  "The runs of LENGTH successive TOKENS, each as a list."
  (loop for rest on tokens while (nthcdr (1- length) rest) collect (subseq rest 0 length)))

(defun token-matches-p (token name)
  "True when TOKEN, as written, is the symbol called NAME with any package prefix (any number of colons) or none,
or the keyword NAME when NAME starts with a colon. A NAME that starts with \"*:\" matches only with a prefix."
  (if (uiop:string-prefix-p "*:" name)
      (and (find #\: token :start 1) (string= (token-base token) (subseq name 2)))
      (string= (token-base token) name)))

(defun files-doing (&rest patterns)
  "The files of the core where a run of tokens matches one of PATTERNS, sorted. A pattern is a list with one element
for each token of the run: a name or a list of names (TOKEN-MATCHES-P)."
  (flet ((matches (element token)
           (some (lambda (name) (token-matches-p token name)) (if (listp element) element (list element)))))
    (sort (loop for (file . tokens) in (core-tokens)
                when (some (lambda (pattern)
                             (some (lambda (run) (every #'matches pattern run)) (token-runs tokens (length pattern))))
                           patterns)
                  collect file)
          #'string<)))

(defun chokepoint-problems (found allowed)
  "Lines saying which of the files FOUND are not in ALLOWED (rows of (FILE REASON)), and which rows of ALLOWED are
not in FOUND."
  (append (loop for file in found unless (assoc file allowed :test #'string=)
                collect (format nil "~a does it, and is not allowed to" file))
          (loop for (file) in allowed unless (member file found :test #'string=)
                collect (format nil "~a is allowed to do it, and does not: remove the exception" file))))

(defmacro define-chokepoint (name (&rest patterns) allowed documentation)
  "Define the test (definitions NAME): the files of the core where a run of tokens matches one of PATTERNS
\(FILES-DOING) are the ones in ALLOWED, rows of (FILE REASON), and no others."
  `(define-test (definitions ,name)
     :description ,documentation
     (let* ((*core-tokens* (core-tokens))
            (problems (chokepoint-problems (files-doing ,@(mapcar (lambda (pattern) `',pattern) patterns)) ',allowed)))
       (false problems "~{~%  ~a~}" problems))))

(define-chokepoint programs-started
  ((("run-program" "launch-program")))
  (("src/io/sandbox.lisp" "the one place that starts programs: limits, environment and confinement")
   ("frontends/tui/client.lisp" "starts the daemon the client talks to, with its own environment")
   ("frontends/tui/ssh.lisp" "runs ssh for the remote editing connection")
   ("frontends/tui/tui.lisp" "runs the clipboard watcher the terminal listens to")
   ("lisp/files/external.lisp" "hands a file to the desktop's program for it")
   ("lisp/git/remote.lisp" "asks git once for the commands it has built in")
   ("lisp/lsp/client.lisp" "starts a language server and talks to it over pipes")
   ("lisp/terminal/emulator.lisp" "starts the shell of a terminal buffer on a pseudo-terminal")
   ("src/syntax/helper.lisp" "starts the helper that parses in its own process"))
  "Programs are started in the sandbox's file, and in the few files whose job is a program of their own.")

(define-chokepoint environment-read
  ((("getenv" "getenv-absolute-directory" "posix-getenv")))
  (("src/io/sandbox.lisp" "the environment programs get, and the variables the editor itself asks for")
   ("src/io/posix.lisp" "the folders of the state and of the runtime, from the XDG variables")
   ("src/io/operations.lisp" "the data folder, from XDG_DATA_HOME")
   ("src/dates/zones.lisp" "the time zone, from TZ and TZDIR")
   ("src/command/redisplay.lisp" "the home folder, to show paths short")
   ("lisp/api.lisp" "re-exports uiop's getenv for packages")
   ("lisp/daemon.lisp" "the runtime folder the daemon's socket is in")
   ("lisp/editing/spellcheck.lisp" "the language, from LC_ALL, LC_MESSAGES and LANG")
   ("lisp/export/export.lisp" "the home and configuration folders the export tools use")
   ("lisp/files/files.lisp" "the time zone, from TZ")
   ("lisp/files/walk.lisp" "the configuration folder, from XDG_CONFIG_HOME")
   ("lisp/packages/compile-apart.lisp" "the request a compiling child process is given")
   ("frontends/gtk/window.lisp" "the renderer and measuring switches of the window")
   ("frontends/tui/client.lisp" "the user and the ssh agent of the daemon's owner")
   ("frontends/tui/desktop.lisp" "the folder of desktop entries")
   ("frontends/tui/main.lisp" "the heap size and the switches of the built program")
   ("frontends/tui/ssh.lisp" "the user's name for ssh's % words")
   ("frontends/tui/tui.lisp" "the terminal and the desktop the editor itself runs in"))
  "Environment variables are read through the program environment in the sandbox's file, and in the files whose
variable is the editor's own (its terminal, folders, time zone and switches).")

(define-chokepoint files-written
  ((":direction" (":output" ":io"))
   (("write-string-into-file" "with-output-file"))
   (("o-wronly" "o-rdwr" "o-creat" "o-append" "o-trunc")))
  (("src/io/files.lisp" "saving a buffer: the write, the temporary copy and the lock")
   ("src/io/in-place.lisp" "writing changes into a file where it is")
   ("src/io/operations.lisp" "the file operations: copy, make, move")
   ("src/io/posix.lisp" "the descriptor-level writing every other writer is built on")
   ("src/runtime/debug.lisp" "the debug log, rotated by its own code")
   ("lisp/files/external.lisp" "the lock file of a file open in another program")
   ("frontends/tui/client.lisp" "opens /dev/null for the daemon's output")
   ("frontends/tui/main.lisp" "opens the terminal to write to it")
   ("frontends/tui/man-pages.lisp" "writes the man pages into the build folder")
   ("frontends/tui/tui.lisp" "the shared memory object of a picture")
   ("frontends/gtk/window.lisp" "the files of the window's drawing measurement"))
  "A file is written by the file layer (src/io), and in the few other files that make a file of their own kind.")

(define-chokepoint files-removed
  ((("delete-file" "rename-file" "*:rename" "*:unlink" "*:rmdir" "delete-file-if-exists" "delete-directory-tree")))
  (("src/io/files.lisp" "removing a temporary copy a save did not finish")
   ("src/io/posix.lisp" "the descriptor-level calls every other remover is built on")
   ("src/runtime/debug.lisp" "rotating the debug log")
   ("frontends/tui/tui.lisp" "removing a picture's shared memory object")
   ("lisp/files/archives.lisp" "removing the folder an archive was opened into")
   ("lisp/files/grammars.lisp" "replacing a downloaded grammar's library")
   ("lisp/git/look-only.lisp" "removing the copies a safe visit made")
   ("lisp/packages/compile-apart.lisp" "the compiled core and the request files of a compile")
   ("lisp/packages/packages.lisp" "a package's compiled files")
   ("lisp/packages/signing.lisp" "a file made to check a signature")
   ("lisp/search/indexed.lisp" "the files of a search index"))
  "A file is renamed or removed by the file layer (src/io), and in the few other files that clean up what they made.")

(defun colour-literal-p (string)
  "True when STRING, the text of a string literal, is a colour written as #rgb, #rrggbb or #rrggbbaa."
  (and (cl-ppcre:scan "^#(?:[0-9a-fA-F]{3}|[0-9a-fA-F]{6}|[0-9a-fA-F]{8})$" string) t))

(defun files-naming-colours ()
  "The files of the core, but lisp/theme.lisp, where a string literal is a colour (COLOUR-LITERAL-P). A docstring
that says \"#abcdef\" among other words is not one."
  (sort (loop for file in (core-source-files)
              for name = (tree-relative-name file)
              when (and (string/= name "lisp/theme.lisp")
                        (some #'colour-literal-p
                              (text-strings (coerce (uiop:read-file-string file :external-format :utf-8)
                                                    'simple-string))))
                collect name)
        #'string<))

(define-test (definitions colours-named-in-the-theme)
  :description "A colour is written as a string \"#rrggbb\" (or #rgb, #rrggbbaa) only in lisp/theme.lisp, where
the faces are defined."
  (let ((found (files-naming-colours)))
    (false found "Colour literals outside lisp/theme.lisp: ~{~%  ~a~}" found)))

(define-test (definitions chokepoint-names-match-with-any-prefix)
  :description "A call is found whatever its package prefix: one colon, two, or none."
  (let ((*core-tokens* (list (cons "a.lisp" (text-tokens "(sb-posix::unlink x)
                                                          (uiop/filesystem:delete-file-if-exists y)"))
                             (cons "b.lisp" (text-tokens "(cl:rename-file x y)"))
                             (cons "c.lisp" (text-tokens "(rename-file x y) (let ((unlink 1)) unlink)")))))
    (is equal '("a.lisp" "b.lisp" "c.lisp") (files-doing '(("delete-file-if-exists" "rename-file" "*:unlink"))))
    (is equal '("a.lisp") (files-doing '(("*:unlink"))))
    (is equal '("b.lisp" "c.lisp") (files-doing '(("rename-file"))))))

(define-test (definitions colour-literals-are-found-by-their-text)
  :description "A string that is a colour is found wherever it is, even after a semicolon; a docstring saying one is not."
  (true (colour-literal-p "#abc"))
  (true (colour-literal-p "#A0B1C2"))
  (true (colour-literal-p "#a0b1c2d3"))
  (false (colour-literal-p "The colour #abcdef is bright"))
  (is equal '("a ; #abcdef") (text-strings "(foo \"a ; #abcdef\") ; \"#123456\"")))
