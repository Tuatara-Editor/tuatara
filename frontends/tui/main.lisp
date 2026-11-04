;;;; frontends/tui/main.lisp --- Tuatara in the terminal: the program's entry point.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; The defaults in the terminal frontend, started by MAIN: from tools/tui.lisp
;;;; while developing, and from build/tuatara, the whole editor saved as one
;;;; executable by tools/build-image.sh, which starts in a fraction of a second.

(defpackage #:tuatara.terminal
  (:use #:cl)
  (:documentation "Tuatara in the terminal: the entry point shared by tools/tui.lisp and the saved executable.")
  (:export #:main #:image-toplevel #:*built-heap* #:*options* #:*option-help* #:*usage-forms* #:write-man-pages))

(in-package #:tuatara.terminal)

(defparameter *usage-forms*
  '("tuatara [--gui] [--frames N] [-Q] [-c INIT-FILE] [--safe] [--hex] [[+LINE[:COLUMN]] FILE...]"
    "tuatara [--gui] [-Q] [-c INIT-FILE] --diff OLD NEW"
    "tuatara [--gui] [-Q] [-c INIT-FILE] --merge LOCAL BASE REMOTE MERGED"
    "tuatara [--gui] [-Q] [-c INIT-FILE] --tutor"
    "tuatara --daemon[=NAME] [-c INIT-FILE]"
    "tuatara --client[=NAME] [--gui] [--start] [--fallback] [--frames N] [--safe] [--hex] [[+LINE[:COLUMN]] FILE...]"
    "tuatara --client[=NAME] [--gui] [--start] [--fallback] --diff OLD NEW"
    "tuatara --client[=NAME] [--gui] [--start] [--fallback] --merge LOCAL BASE REMOTE MERGED"
    "tuatara --stop[=NAME]"
    "tuatara --generate-autostart [--daemon=NAME]")
  "The ways of starting Tuatara, as --help and the manual page show them.")

(defparameter *usage-introduction*
  "Edit the FILEs in this terminal, or start in the scratch buffers without any.
Options may stand anywhere before --; --safe and --hex hold for every FILE of
the call, a +LINE for the FILE after it.
Written by Michael A Jones; free software under the GNU General Public
License, version 3 or later.
"
  "What --help says after the ways of starting, before the options.")

(defparameter *option-help*
  '((:flags (("--gui"))
     :text ("open a window of its own instead (as when started with no terminal)"))
    (:flags (("--frames" "N"))
     :text ("open N frames, from 1 to 16: with --gui each a window of its"
            "own, in the terminal screens shown one at a time (C-x 5 o goes"
            "to the next); the files named are shown one in each frame in"
            "turn, those left over open behind the first; --frames=N does as"
            "well"))
    (:flags (("-c" "INIT-FILE") ("--config" "INIT-FILE"))
     :text ("read INIT-FILE as the init file instead of"
            "~/.config/tuatara/init.lisp; --config=INIT-FILE does as well"))
    (:flags (("+LINE[:COLUMN]"))
     :text ("before a file: open it with the cursor on that line (from 1;"
            "the last line when the file has fewer) and column (from 1;"
            "the C in the mode line counts from 0), as in"
            "tuatara +120:8 main.lisp +3 notes.org"))
    (:flags (("--diff" "OLD NEW"))
     :text ("open the differences between the two files, or two folders, as"
            "M-x diff-files shows them, and exit when you quit; for git, as"
            "git config difftool.tuatara.cmd 'tuatara --diff \"$LOCAL\" \"$REMOTE\"'"))
    (:flags (("--merge" "LOCAL BASE REMOTE MERGED"))
     :text ("merge the three files into MERGED, the conflicts marked there to"
            "resolve, then save it; exits 0 when MERGED is saved with no"
            "conflict left, else 1; for git, as git config"
            "mergetool.tuatara.cmd 'tuatara --merge \"$LOCAL\" \"$BASE\""
            "\"$REMOTE\" \"$MERGED\"' and mergetool.tuatara.trustExitCode true"))
    (:flags (("--safe") ("-R"))
     :text ("open the files safely, whatever folder they are in: read only,"
            "no mode or hook acting on them, nothing written"))
    (:flags (("--hex"))
     :text ("open the files in the hex view, never first as text"))
    (:flags (("-"))
     :text ("as a file: read standard input into a buffer, *stdin*, opened"
            "safely (tuatara - < data, or ... | tuatara -); the terminal stays"
            "the keyboard"))
    (:flags (("--tutor"))
     :text ("start in the tutor"))
    (:flags (("--daemon[=NAME]"))
     :text ("run one editor in the background, with no screen, for"
            "tuatara --client to show in terminals, and return once it is"
            "ready; it reads the init file and loads the packages as Tuatara"
            "does, and runs until M-x daemon-quit or tuatara --stop, or until"
            "it is sent SIGTERM, as at logout, which keeps the changes not"
            "saved as auto-saved copies and asks nothing; NAME, default when"
            "none is given, tells several apart; its log is"
            "~/.cache/tuatara/daemon-NAME.log, its session"
            "~/.local/state/tuatara/session-daemon-NAME"))
    (:flags (("--client[=NAME]"))
     :text ("show the daemon NAME in this terminal, or with --gui in a window"
            "of its own (C-x 5 2 opens another), a frame of its own, the files"
            "named opened in it, with +LINE, --safe, --hex, - and --frames as"
            "for Tuatara itself; --diff and --merge as well, with their exit"
            "codes; C-x C-c closes this client alone, its buffers kept in the"
            "daemon, and the daemon goes on; as git's editor"
            "(GIT_EDITOR='tuatara --client') the commit is made when the"
            "message is finished; exits 1 when no daemon of that name runs, or"
            "when it ends"))
    (:flags (("--start"))
     :text ("with --client: start the daemon first when none runs"))
    (:flags (("--fallback"))
     :text ("with --client: when no daemon runs, start Tuatara as it starts"
            "without --client instead, the files opened in an editor of its"
            "own; the desktop's Tuatara (client) starts it so"))
    (:flags (("--stop[=NAME]"))
     :text ("stop the daemon NAME, asking in this terminal about the changes"
            "not saved in it, as M-x daemon-quit does; with no terminal,"
            "nothing is asked, and the changes are kept as auto-saved copies"
            "that M-x recover-file offers"))
    (:flags (("-Q") ("--no-init"))
     :text ("start without the init file and so without packages, with what"
            "Tuatara keeps between sessions (files visited lately, the"
            "session, bookmarks) kept apart in a folder that goes with the"
            "session, the real one neither read nor changed"))
    (:flags (("--generate-config"))
     :text ("write the init file Tuatara writes when there is none, every"
            "setting in it commented out, as"
            "~/.config/tuatara/generated-init.lisp, and exit; a file there"
            "already is left as it is"))
    (:flags (("--generate-autostart"))
     :text ("write the entry that has the desktop start the daemon as you log"
            "in, ~/.config/autostart/tuatara-daemon.desktop, running tuatara"
            "--daemon, and exit; with --daemon=NAME, the daemon NAME's,"
            "tuatara-daemon-NAME.desktop; a file there already is left as it"
            "is"))
    (:flags (("--help") ("-h"))
     :text ("show this and exit"))
    (:flags (("--version") ("-V"))
     :text ("show the version and exit"))
    (:flags (("--"))
     :text ("the arguments after it are all files, even when one starts with a"
            "dash or a plus"))
    (:flags (("--install-desktop")) :only :appimage
     :text ("(the AppImage) copy it to ~/.local/bin/tuatara and put Tuatara"
            "and Tuatara (client) in the desktop's applications, the icon"
            "too"))
    (:flags (("--remove-desktop")) :only :appimage
     :text ("(the AppImage) take them away again"))
    (:flags (("--dark") ("--light")) :only :appimage
     :text ("(the AppImage) show the window's title bar in the dark or the"
            "light style; with --install-desktop, for the desktop's"
            "applications"))
    (:flags (("--dynamic-space-size" "SIZE")) :only :runtime
     :text ("the most memory Tuatara's own data may take, such as 6GB; by"
            "default half the machine's memory, from 2GB to 16GB, or"
            "$TUATARA_HEAP_MB megabytes")))
  "The options, in the order --help lists them, each a property list: :FLAGS,
the ways of writing it, each (FLAG) or (FLAG ARGUMENT); :TEXT, what it does,
as lines; :ONLY, :APPIMAGE for an option only the AppImage's launcher takes or
:RUNTIME for one the program's runtime takes out of the command line before
Tuatara sees it. The manual page is made from this table too.")

(defun option-label (flags)
  "The way FLAGS, those of an entry of *OPTION-HELP*, are written in --help:
each flag with its argument, separated by commas."
  (format nil "~{~a~^, ~}" (mapcar (lambda (flag) (format nil "~{~a~^ ~}" flag)) flags)))

(defun usage-text ()
  "What --help shows: the ways of starting Tuatara, *USAGE-INTRODUCTION* and
each of *OPTION-HELP* with its label, its text beside it when the label is
short, else below it, then where the init file is."
  (with-output-to-string (out)
    (loop for form in *usage-forms*
          for head = "Usage: " then "       "
          do (format out "~a~a~%" head form))
    (write-string *usage-introduction* out)
    (dolist (entry *option-help*)
      (let ((label (option-label (getf entry :flags))))
        (if (< (length label) 11)
            (format out "  ~12a~a~%" label (first (getf entry :text)))
            (format out "  ~a~%              ~a~%" label (first (getf entry :text))))
        (dolist (line (rest (getf entry :text)))
          (format out "              ~a~%" line))))
    (format out "The init file is ~~/.config/tuatara/init.lisp, written the first time~%~
                 Tuatara starts without one.~%~
                 The manual pages tuatara(1) and tuatara-init(5) come with the packages;~%~
                 in the editor, C-h C-a introduces it.~%")))

(defparameter *usage* (usage-text)
  "What --help shows, as it is (it is no format control: ~/ starts one).")

;;; The heap, decided when the executable starts

(defvar *built-heap* nil
  "The heap, in octets, the executable was saved with (tools/build-image.lisp
sets it): a heap of another size was asked for with --dynamic-space-size.")

(defun file-first-line (path)
  "The first line of the file at PATH, or NIL when it cannot be read."
  (tuatara.runtime:ignoring-errors-reported ("reading ~a" path)
    (with-open-file (in path :if-does-not-exist nil)
      (and in (read-line in nil)))))

(defun memory-limit-kb (&key (root "/sys/fs/cgroup")
                             (group (let ((line (tuatara.runtime:ignoring-errors-reported ("reading /proc/self/cgroup")
                                                  (with-open-file (in "/proc/self/cgroup")
                                                    (loop for line = (read-line in nil)
                                                          while line
                                                          when (uiop:string-prefix-p "0::" line)
                                                            return line)))))
                                      (and line (subseq line 3)))))
  "The lowest memory limit, in kilobytes, set on GROUP (this process's control
group, as /proc/self/cgroup names it) or a group above it: memory.max under
ROOT. NIL when none is set, or they cannot be read."
  (let ((lowest nil))
    (when group
      (loop for path = (string-right-trim "/" group) then (subseq path 0 (or (position #\/ path :from-end t) 0))
            for max = (file-first-line (concatenate 'string root path "/memory.max"))
            do (when (and max (plusp (length max)) (every #'digit-char-p max))
                 (let ((kb (floor (parse-integer max) 1024)))
                   (setf lowest (if lowest (min lowest kb) kb))))
            while (plusp (length path))))
    lowest))

(defun memory-kb ()
  "The memory Tuatara may use, in kilobytes: the machine's, as /proc/meminfo
says, or less under a memory limit (MEMORY-LIMIT-KB). NIL when unknown."
  (let ((total (tuatara.runtime:ignoring-errors-reported ("reading /proc/meminfo")
                 (with-open-file (in "/proc/meminfo")
                   (loop for line = (read-line in nil)
                         while line
                         when (uiop:string-prefix-p "MemTotal:" line)
                           return (parse-integer line :start 9 :junk-allowed t)))))
        (limit (memory-limit-kb)))
    (if (and total limit) (min total limit) (or total limit))))

(defun wanted-heap-mb (&key (setting (uiop:getenv "TUATARA_HEAP_MB")) (memory (memory-kb)))
  "The heap Tuatara wants, in megabytes: SETTING ($TUATARA_HEAP_MB) when it is
a number, else half of MEMORY (kilobytes, the machine's), from 2 GB to 16 GB,
so that a replacement across a file of many gigabytes has room while the heap
cannot outgrow the memory. NIL when neither is known."
  (or (let ((mb (tuatara.text:whole-number setting))) (and mb (plusp mb) mb))
      (and memory (max 2048 (min 16384 (floor memory 2048))))))

(defun heap-to-restart-with (&key (now (sb-ext:dynamic-space-size)) (built *built-heap*)
                                  (wanted (wanted-heap-mb)) (restarted (uiop:getenv "TUATARA_HEAP_DECIDED")))
  "The heap, in megabytes, to start again with, or NIL to go on: when the heap
NOW (octets) is the one the executable was BUILT with, not one asked for, and
the WANTED one differs from it by more than a tenth, unless this start is
already the second (RESTARTED)."
  (when (and wanted built (= now built) (not restarted)
             (> (abs (- (* wanted 1024 1024) now)) (floor now 10)))
    wanted))

(defun restart-with-heap (mb)
  "Run this executable again in place of this process, with the same arguments
and a heap of MB megabytes. Returns only when that fails."
  (sb-posix:setenv "TUATARA_HEAP_DECIDED" "1" 1)
  (tuatara.io:run-program-again (list* "--dynamic-space-size" (format nil "~dMB" mb) (rest sb-ext:*posix-argv*))))

(defparameter *runtime-options-with-values* '("--dynamic-space-size" "--control-stack-size" "--tls-limit")
  "The options SBCL's runtime takes out of the command line, each with the value after it.")

(defun command-line-arguments (&key (argv sb-ext:*posix-argv*) (from "/proc/self/cmdline"))
  "The program's command line, its name first, as SB-EXT:*POSIX-ARGV* has it:
ARGV itself, or, when it is NIL because an argument is not UTF-8, read again
as octets FROM the system's record of it, each octet that is not UTF-8 kept
as its raw byte's character, so that such a file name still opens, the
runtime's options (*RUNTIME-OPTIONS-WITH-VALUES*) left out as it leaves them out."
  (or argv
      (let* ((octets (tuatara.runtime:ignoring-errors-reported ("reading ~a" from) (tuatara.io:file-octets from)))
             (parts (and octets
                         (loop with start = 0
                               for end = (position 0 octets :start start)
                               while end
                               collect (tuatara.text:octets-string octets :start start :end end)
                               do (setf start (1+ end))))))
        (loop while parts
              for part = (pop parts)
              if (member part *runtime-options-with-values* :test #'string=)
                do (pop parts)
              else collect part))))

(defun terminal-p (fd)
  "True when the file descriptor FD is a terminal."
  (eql 1 (sb-unix:unix-isatty fd)))

(defun window-wanted-p (&key (input (terminal-p 0)) (output (terminal-p 1))
                             (desktop (or (uiop:getenv "WAYLAND_DISPLAY") (uiop:getenv "DISPLAY"))))
  "True when Tuatara was started with no terminal to run in, on a desktop: from
a launcher rather than a shell. INPUT and OUTPUT say whether its input and
its output are a terminal, DESKTOP whether there is a desktop to show a window on."
  (and (not input) (not output) desktop t))

(defun keyboard-available-p (&key (input-terminal (terminal-p 0)) (tty "/dev/tty"))
  "True when Tuatara can read keys in a terminal: its input is one
\(INPUT-TERMINAL), or the terminal it was started in can be opened (TTY, the
file that is it)."
  (or input-terminal
      (let ((fd (handler-case (sb-posix:open tty sb-posix:o-rdwr) (sb-posix:syscall-error () nil))))
        (when fd (sb-posix:close fd) t))))

(defun read-keys-from-terminal ()
  "When Tuatara's input is not a terminal, as in
`find . -name \"*.org\" | xargs tuatara', read the keys from the terminal it
was started in (/dev/tty) instead; what came in on the input is not read as
keys. KEYBOARD-AVAILABLE-P says first whether there is such a terminal."
  (unless (terminal-p 0)
    (let ((fd (handler-case (sb-posix:open "/dev/tty" sb-posix:o-rdwr) (sb-posix:syscall-error () nil))))
      (when fd
        (sb-posix:dup2 fd 0)
        (sb-posix:close fd)))))

(defun bare-flag (flag)
  "FLAG, an option as *OPTION-HELP* writes it, without the [=NAME] of a value
it may be given: --daemon of --daemon[=NAME]."
  (subseq flag 0 (search "[=" flag)))

(defparameter *options*
  (loop for entry in *option-help*
        unless (getf entry :only)
          append (loop for flag in (getf entry :flags)
                       unless (char= (char (first flag) 0) #\+) collect (bare-flag (first flag))))
  "The options the program takes, as written, and +LINE before a file besides;
a long one taking a value takes it after an equals sign too (OPTION-WITH-VALUE),
and one that may be given a name, --daemon[=NAME], may take it so (NAMED-OPTION).")

(defparameter *named-options*
  (loop for entry in *option-help*
        append (loop for (flag) in (getf entry :flags)
                     when (search "[=" flag) collect (bare-flag flag)))
  "The options that may be given a name after an equals sign: --daemon,
--client and --stop.")

(defun named-option (argument)
  "When ARGUMENT is one of *NAMED-OPTIONS* given a name, --daemon=NAME, the
option and the name, as two values; NIL otherwise."
  (let ((equals (position #\= argument)))
    (when equals
      (let ((option (subseq argument 0 equals)))
        (when (member option *named-options* :test #'string=)
          (values option (subseq argument (1+ equals))))))))

(defparameter *options-with-a-value*
  (loop for entry in *option-help*
        unless (getf entry :only)
          append (loop for (flag argument) in (getf entry :flags)
                       when (and argument (uiop:string-prefix-p "--" flag) (not (find #\Space argument)))
                         collect flag))
  "The long options that take one value, which may be written --NAME=VALUE as
well as --NAME VALUE: --config, --frames.")

(defun option-with-value (argument)
  "When ARGUMENT is written --NAME=VALUE, NAME one of *OPTIONS-WITH-A-VALUE*,
the option and the value, as two values; NIL otherwise."
  (let ((equals (position #\= argument)))
    (when equals
      (let ((option (subseq argument 0 equals)))
        (when (member option *options-with-a-value* :test #'string=)
          (values option (subseq argument (1+ equals))))))))

(defun option-p (argument)
  "True when ARGUMENT is one of the program's options."
  (or (member argument *options* :test #'string=)
      (and (option-with-value argument) t)
      (and (named-option argument) t)))

(defun option-like-p (argument)
  "True when ARGUMENT is written as an option: it starts with a dash and is
more than a dash."
  (and (> (length argument) 1) (char= (char argument 0) #\-)))

(defun read-options (arguments &key (directory (uiop:getcwd)))
  "The options of ARGUMENTS, the program's (strings), as a plist: :GUI,
:FRAMES (how many --frames opens), :CONFIG (the init file -c names),
:GENERATE-CONFIG, :VERSION, :HELP,
:DIFF (the two names after --diff), :MERGE (the four after --merge), :SAFE,
:HEX, :TUTOR, :BARE (-Q), :DAEMON, :CLIENT and :STOP (each the daemon's
name, *DAEMON-DEFAULT-NAME* when none is given), :START, :FALLBACK,
:GENERATE-AUTOSTART and :FILES, the
files named; or :ERROR, what is wrong with them. Options may stand anywhere before --, among the files; --safe
and --hex hold for every file of the call, a +LINE[:COLUMN] for the file after
it, which is then (NAME LINE COLUMN) in :FILES, the column NIL when none was
given; a - (standard input) is (:STANDARD-INPUT). What starts with a dash and
is no option is refused, and so is an option or +LINE that is also a file's
name in DIRECTORY, where the program was started: write ./NAME for the file,
or -- before the files. After --, every argument is a file's name."
  (let ((options '()) (named '()))
    (flet ((given (key) (setf (getf options key) t))
           (fail (&rest words) (return-from read-options (list :error (apply #'format nil words)))))
      (loop
        (let ((argument (pop arguments)))
          (cond ((null argument) (return))
                ((string= argument "--")
                 (dolist (rest arguments) (push (list :literal rest) named))
                 (return))
                ;; a file named as an option, as * gives one in a folder holding it: never read as
                ;; one, so that no file there chooses the init file or what is done
                ((and (or (option-p argument) (option-like-p argument))
                      (tuatara.runtime:ignoring-errors-reported ("examining ~a" argument)
                        (tuatara.io:path-kind (tuatara.io:expand-path argument :directory directory) :follow nil)))
                 (fail "~a is a file here too: write ./~a, or -- before the files" argument argument))
                ;; --NAME=VALUE read as --NAME VALUE
                ((option-with-value argument)
                 (multiple-value-bind (option value) (option-with-value argument)
                   (push value arguments)
                   (push option arguments)))
                ((or (member argument *named-options* :test #'string=) (named-option argument))
                 (multiple-value-bind (option name) (named-option argument)
                   (let ((name (or name tuatara.defaults::*daemon-default-name*)))
                     (unless (tuatara.defaults::daemon-name-p name)
                       (fail "~a cannot name a daemon: letters, digits, - and _, at most 40" name))
                     (setf (getf options (intern (string-upcase (subseq (or option argument) 2)) :keyword)) name))))
                ((string= argument "--start") (given :start))
                ((string= argument "--fallback") (given :fallback))
                ((string= argument "--generate-autostart") (given :generate-autostart))
                ((member argument '("--version" "-V") :test #'string=) (given :version))
                ((member argument '("--help" "-h") :test #'string=) (given :help))
                ((string= argument "--gui") (given :gui))
                ((string= argument "--generate-config") (given :generate-config))
                ((member argument '("--safe" "-R") :test #'string=) (given :safe))
                ((string= argument "--hex") (given :hex))
                ((string= argument "--tutor") (given :tutor))
                ((string= argument "--frames")
                 (let ((count (tuatara.text:whole-number (pop arguments))))
                   (if (and count (<= 1 count tuatara.command:*most-frames*))
                       (setf (getf options :frames) count)
                       (fail "--frames needs how many frames to open after it, from 1 to ~d"
                             tuatara.command:*most-frames*))))
                ((member argument '("-Q" "--no-init") :test #'string=) (given :bare))
                ((string= argument "--merge")
                 (if (and (>= (length arguments) 4) (every #'plusp (mapcar #'length (subseq arguments 0 4))))
                     (setf (getf options :merge) (loop repeat 4 collect (pop arguments)))
                     (fail "--merge needs four files after it: LOCAL BASE REMOTE MERGED")))
                ((string= argument "--diff")
                 (if (and (second arguments) (plusp (length (first arguments))) (plusp (length (second arguments))))
                     (setf (getf options :diff) (list (pop arguments) (pop arguments)))
                     (fail "--diff needs two files after it, the old and the new")))
                ((member argument '("-c" "--config") :test #'string=)
                 (let ((path (pop arguments)))
                   (if (and path (plusp (length path)))
                       (setf (getf options :config) path)
                       (fail "~a needs the init file after it" argument))))
                ((option-like-p argument)
                 (fail "~a is not an option of tuatara (--help lists them): write ./~a, or -- before the files, for a file" argument argument))
                (t (push argument named)))))
      (setf named (nreverse named))
      (cond ((and (getf options :diff) named)
             (fail "--diff takes the two files to compare, not ~a too" (let ((n (first named))) (if (consp n) (second n) n))))
            ((and (getf options :merge) named)
             (fail "--merge takes the four files to merge, not ~a too" (let ((n (first named))) (if (consp n) (second n) n))))
            ((and (getf options :diff) (getf options :merge))
             (fail "--diff and --merge are not given together"))
            ((and (getf options :tutor) named)
             (fail "--tutor takes no files, not ~a" (let ((n (first named))) (if (consp n) (second n) n))))
            ((> (count-if (lambda (key) (getf options key)) '(:daemon :client :stop)) 1)
             (fail "--daemon, --client and --stop are given one at a time"))
            ((and (getf options :bare) (or (getf options :daemon) (getf options :client) (getf options :stop)))
             (fail "-Q is never a daemon's: it starts a fresh, bare editor of its own, which ends with it"))
            ((and (getf options :gui) (or (getf options :daemon) (getf options :stop)))
             (fail "~:[--stop~;--daemon~] opens no window: tuatara --client --gui shows a daemon in one"
                   (getf options :daemon)))
            ((and (getf options :start) (not (getf options :client)))
             (fail "--start goes with --client"))
            ((and (getf options :fallback) (not (getf options :client)))
             (fail "--fallback goes with --client"))
            ((and (getf options :fallback) (getf options :start))
             (fail "--start and --fallback are given one at a time: with no daemon running, --start starts one, ~
                    --fallback an editor of its own"))
            ((and (getf options :generate-autostart)
                  (or named (loop for (key) on options by #'cddr
                                  thereis (not (member key '(:generate-autostart :daemon))))))
             (fail "--generate-autostart takes nothing but --daemon=NAME, for a daemon of that name"))
            ((and (or (getf options :stop) (getf options :daemon))
                  (or named (getf options :diff) (getf options :merge) (getf options :tutor) (getf options :frames)
                      (getf options :safe) (getf options :hex)))
             (fail "~:[--stop~;--daemon~] takes no files: give them to tuatara --client" (getf options :daemon)))
            ((and (or (getf options :client) (getf options :stop)) (getf options :config))
             (fail "-c names the init file of the daemon: give it to tuatara --daemon"))
            ((and (getf options :client) (getf options :tutor))
             (fail "--tutor is not given to a client: M-x tuatara-tutor in it starts the tutor")))
      (let ((files (read-places named directory)))
        (if (stringp files)
            (list :error files)
            (list* :files files options))))))

(defun place-like-p (argument)
  "True when ARGUMENT looks like a place asked for: a + and then only digits
and colons, as +120, +120:8, +5: or +:3."
  (and (> (length argument) 1) (char= (char argument 0) #\+)
       (every (lambda (c) (or (digit-char-p c) (char= c #\:))) (subseq argument 1))))

(defun place-argument (argument)
  "The line and the column (NIL when none) a command-line argument such as
+120 or +120:8 asks for, as two values; NIL when ARGUMENT is not that, of
numbers of up to nine digits."
  (when (place-like-p argument)
    (let* ((colon (position #\: argument))
           (line (subseq argument 1 colon))
           (column (and colon (subseq argument (1+ colon)))))
      (flet ((number-p (text) (and (plusp (length text)) (< (length text) 10) (every #'digit-char-p text))))
        (when (and (number-p line) (or (null column) (number-p column)))
          (values (parse-integer line) (and column (parse-integer column))))))))

(defun read-places (arguments directory)
  "ARGUMENTS, the files named on the command line, each with the line and
column a +LINE[:COLUMN] before it asked for: a plain name where none was, else
\(NAME LINE COLUMN). A string says what is wrong instead: a +LINE with no file
after it, two of them before a file, or one that is also a file's name in
DIRECTORY."
  (let ((line nil) (column nil) (files '()))
    (dolist (argument arguments)
      (multiple-value-bind (asked-line asked-column) (and (stringp argument) (place-argument argument))
        (cond ((and (stringp argument) (place-like-p argument) (null asked-line))
               (return-from read-places
                 (format nil "~a is not +LINE or +LINE:COLUMN, numbers of up to nine digits: write ./~a for a file, or -- before the files"
                         argument argument)))
              ((consp argument)
               (push (if line (list (second argument) line column) (second argument)) files)
               (setf line nil column nil))
              ((and (or asked-line (string= argument "-"))
                    (tuatara.runtime:ignoring-errors-reported ("examining ~a" argument)
                      (tuatara.io:path-kind (tuatara.io:expand-path argument :directory directory) :follow nil)))
               (return-from read-places
                 (format nil "~a is a file here too: write ./~a, or -- before the files" argument argument)))
              ((string= argument "-")
               (when line
                 (return-from read-places (format nil "+~d needs a file, not standard input" line)))
               (when (find '(:standard-input) files :test #'equal)
                 (return-from read-places "- is given twice: standard input is read once"))
               (push (list :standard-input) files))
              (asked-line
               (when line
                 (return-from read-places (format nil "~a comes after +~d with no file between them" argument line)))
               (setf line asked-line column asked-column))
              (line (push (list argument line column) files) (setf line nil column nil))
              (t (push argument files)))))
    (when line
      (return-from read-places (format nil "+~d~@[:~d~] needs the file it is for after it" line column)))
    (nreverse files)))

(defun generate-config ()
  "Write the starter init file as generated-init.lisp in the configuration
folder, never over a file there; said on the standard output, or why not on
the error output. The exit code: 0 when written, 1 when not."
  (let ((path (tuatara.defaults::config-path "generated-init.lisp")))
    (multiple-value-bind (written why) (tuatara.defaults::write-starter-config path)
      (cond (written (format t "Wrote ~a~%" path) 0)
            (why (format *error-output* "tuatara: no init file written at ~a: ~a~%" path why) 1)
            (t (format *error-output* "tuatara: ~a is there already; move it away to write a new one~%" path) 1)))))

(defun shown (text)
  "TEXT, a name or a sentence with names in it, with its control characters
written out, to be put on the terminal."
  (values (tuatara.command::escape-controls text)))

(defun named-here (name)
  "The file NAME as the system names it, from where Tuatara was started."
  (tuatara.io:native (tuatara.io:expand-path name :directory (uiop:getcwd))))

(defun missing-file (names)
  "The first of NAMES, files, that is not there, or NIL."
  (find-if-not (lambda (name)
                 (tuatara.runtime:ignoring-errors-reported ("examining ~a" name)
                   (tuatara.io:path-kind (named-here name))))
               names))

(defconstant +f-dupfd-cloexec+ 1030
  "fcntl's request for a copy of a descriptor that no program started from here inherits.")

(defun startup-from (options &key (input 0))
  "What the editor is started with for OPTIONS (READ-OPTIONS), as two values:
the files to open, as RUN-EDITOR takes them (a name, or a list of it and
:SAFE and :HEX as asked, or (:STANDARD-INPUT FD) for a copy of the input kept
for - before the terminal takes its place: of INPUT, the descriptor of
standard input), the names from where Tuatara was started; and
START-FUNCTION for the rest. With -Q the editor is set to start bare first
\(TUATARA.DEFAULTS::START-BARE)."
  (when (getf options :bare)
    (tuatara.defaults::start-bare))
  (let* ((safe (getf options :safe))
         (hex (getf options :hex))
         (merge (mapcar #'named-here (getf options :merge)))
         (places '())
         (files (loop for file in (if merge (list (fourth merge)) (getf options :files))
                      collect (cond ((stringp file) (startup-file (named-here file) safe hex))
                                    ((eq (first file) :standard-input) (list :standard-input (sb-posix:fcntl input +f-dupfd-cloexec+ 3)))
                                    (t (let ((path (named-here (first file))))
                                         (push (list path (second file) (third file)) places)
                                         (startup-file path safe hex)))))))
    (values files
            (start-function :places (nreverse places) :diff (mapcar #'named-here (getf options :diff))
                            :merge merge :tutor (getf options :tutor) :bare (getf options :bare)
                            :frames (getf options :frames)))))

(defun startup-file (path safe hex)
  "PATH, a file to open, as RUN-EDITOR takes it: with SAFE and HEX as asked."
  (if (or safe hex) (list path :safe safe :hex hex) path))

(defun start-function (&key places diff merge tutor bare frames)
  "What the editor does once the files are shown, as a function of the editor,
or NIL when there is nothing to do: go to the line each of PLACES asks for,
\(PATH LINE COLUMN); show the differences of DIFF, the old and the new file;
merge MERGE, the local, base and remote file and the one the merge goes into;
open the tutor; say that it started BARE; and open FRAMES frames in all, the
files spread over them (TUATARA.DEFAULTS::OPEN-START-FRAMES)."
  (when (or places diff merge tutor bare (and frames (> frames 1)))
    (lambda (editor)
      (when places (tuatara.defaults::show-start-places places))
      (when diff
        (tuatara.defaults::compare-paths (first diff) (second diff))
        (tuatara.command:only-window (tuatara.command:current-window)))
      (when merge (tuatara.defaults::merge-files-into (first merge) (second merge) (third merge) (fourth merge)))
      (when tutor (tuatara.defaults::tuatara-tutor))
      (when bare (tuatara.defaults::say-started-bare))
      (when (and frames (> frames 1)) (tuatara.defaults::open-start-frames editor frames)))))

(defun main (arguments)
  "Run Tuatara in this terminal on the files named in ARGUMENTS (strings), or
in the scratch buffers when there are none, as its options say
\(READ-OPTIONS): --version, --help and --generate-config do what they say
instead; --diff OLD NEW shows what differs between two files; -c names the
init file; +LINE[:COLUMN] before a file opens it there; -- before the files
lets a file's name start with a dash or a plus. With --gui, or when started with no terminal on a
desktop, Tuatara opens a window of its own (RUN-INSTANCE). --daemon starts an
editor in the background (START-DAEMON), --client shows one in this terminal,
or a window with --gui, and --stop stops one (RUN-CLIENT), --client
--fallback starting an editor of its own when none runs; --generate-autostart
writes the entry that starts the daemon with the desktop (GENERATE-AUTOSTART).
Returns the exit code: 0, or what the session left (*EXIT-CODE*); 2 for
options it cannot take."
  (let* ((options (read-options arguments))
         (config (getf options :config))
         ;; named from where Tuatara was started, as the files are
         (config-path (and config (tuatara.io:expand-path config :directory (uiop:getcwd)))))
    (cond ((getf options :error)
           (format *error-output* "tuatara: ~a~%~a" (shown (getf options :error)) *usage*)
           2)
          ((getf options :version)
           (format t "~a~%" (tuatara:version-line))
           0)
          ((getf options :help)
           (write-string *usage*)
           0)
          ((getf options :generate-config)
           (generate-config))
          ((getf options :generate-autostart)
           (generate-autostart (or (getf options :daemon) tuatara.defaults::*daemon-default-name*)))
          ((getf options :daemon)
           (start-daemon (getf options :daemon) config-path))
          ((missing-file (getf options :diff))
           (format *error-output* "tuatara: ~a is not there: nothing to compare~%" (shown (missing-file (getf options :diff))))
           2)
          ((and (find '(:standard-input) (getf options :files) :test #'equal) (terminal-p 0))
           (format *error-output* "tuatara: - reads standard input, which is the terminal here: pipe something in~%")
           2)
          ((missing-file (let ((merge (getf options :merge))) (and merge (list (first merge) (third merge)))))
           (format *error-output* "tuatara: ~a is not there: nothing to merge~%"
                   (shown (missing-file (list (first (getf options :merge)) (third (getf options :merge))))))
           2)
          ((and config (not (eq (tuatara.runtime:ignoring-errors-reported ("examining ~a" config-path)
                                  (tuatara.io:path-kind config-path))
                                :regular)))
           (format *error-output* "tuatara: no init file at ~a (tuatara --generate-config writes one to start from)~%" (shown config))
           2)
          ((or (getf options :client) (getf options :stop))
           (let ((code (run-client options)))
             ;; --client --fallback with no daemon running: an editor of its own instead
             (if (eq code :fallback)
                 (run-instance options config-path)
                 code)))
          (t (run-instance options config-path)))))

(defun run-instance (options config-path)
  "Run an editor of its own, as OPTIONS say (READ-OPTIONS), CONFIG-PATH the
init file -c names, or NIL: in this terminal, or in a window of its own with
--gui or when started with no terminal on a desktop. Returns the exit code:
what the session left (*EXIT-CODE*), or 2 when there is no terminal to read
keys from and no window wanted."
  (if (and (not (or (getf options :gui) (window-wanted-p))) (not (keyboard-available-p)))
      (progn (format *error-output* "tuatara: its input is not a terminal and there is none to read keys from ~
                                     (/dev/tty): run it in a terminal, or with --gui~%")
             2)
      (progn
        (tuatara.defaults::set-collection-policy)
        (when config-path
          (setf tuatara.defaults::*config-file* (pathname config-path)))
        (multiple-value-bind (files start) (startup-from options)
          (let ((gui (or (getf options :gui) (window-wanted-p))))
            ;; a copy started to measure how the window draws (check-health): no init
            ;; file, the window's font as the one measuring has it
            (let ((measure (uiop:getenv "TUATARA_MEASURE_DRAWING")))
              (when measure
                (setf tuatara.defaults::*config-file* (merge-pathnames "no-init-file.lisp" measure)
                      tuatara-user:*gui-font* (uiop:getenv "TUATARA_MEASURE_FONT"))))
            (if gui
                (tuatara.frontend.gtk:run-in-window files #'tuatara.defaults:install-defaults :start start)
                (progn
                  (read-keys-from-terminal)
                  (tuatara.command:run-editor (tuatara.frontend.tui:make-tui-frontend)
                                              :files files
                                              :setup #'tuatara.defaults:install-defaults
                                              :start start)))
            tuatara.command:*exit-code*)))))

(defun leave-appimage-environment ()
  "Run from Tuatara's AppImage (its AppRun says so in TUATARA_APPIMAGE), give
LD_LIBRARY_PATH back as it was before AppRun added the libraries Tuatara
brings: the system looks there for Tuatara's own all the same, as it read the
path when Tuatara started, while the programs Tuatara starts (a terminal's
shell, the desktop's programs, those the sandbox shuts in) use their own."
  (when (uiop:getenv "TUATARA_APPIMAGE")
    (let ((before (uiop:getenv "TUATARA_LD_LIBRARY_PATH")))
      (if (and before (plusp (length before)))
          (sb-posix:setenv "LD_LIBRARY_PATH" before 1)
          (sb-posix:unsetenv "LD_LIBRARY_PATH")))
    (sb-posix:unsetenv "TUATARA_LD_LIBRARY_PATH")
    (sb-posix:unsetenv "TUATARA_APPIMAGE")))

(defun fresh-random-state ()
  "Give *RANDOM-STATE* a state of its own for this start, so that the random
numbers Tuatara draws differ from one start to the next rather than repeat
the state the executable was saved with."
  (setf *random-state* (make-random-state t)))

(defun fit-heap (arguments)
  "Fit this program's heap to what ARGUMENTS, its command line, start: a
client of a daemon, which paints and reads keys, keeps the heap it was saved
with and collects its garbage often (*CLIENT-NURSERY-OCTETS*), so that little
of its memory is touched; anything else is started again with the heap its
machine wants (HEAP-TO-RESTART-WITH), when that is another."
  (if (client-arguments-p arguments)
      (tuatara.defaults::set-collection-policy tuatara.defaults::*client-nursery-octets*)
      (let ((mb (heap-to-restart-with)))
        (when mb (restart-with-heap mb)))))

(defun image-toplevel ()
  "What the saved executable runs: MAIN on its command-line arguments, then exit
with its code at once, the terminal already given back. First, when this
machine wants a heap of another size than the executable was saved with, it
starts again with that one (HEAP-TO-RESTART-WITH), and its random numbers are
new (FRESH-RANDOM-STATE), its arguments read even when one is not UTF-8
\(COMMAND-LINE-ARGUMENTS); run from the AppImage, the
programs it starts are given the environment as it was before
\(LEAVE-APPIMAGE-ENVIRONMENT). Run again by the editor itself with
TUATARA_COMPILE_PACKAGE in its environment, it compiles a package apart from
it instead, its arguments never looked at
\(TUATARA.PACKAGES:COMPILE-PACKAGE-CHILD); run again by tuatara --daemon
with TUATARA_DAEMON, it is that daemon (RUN-DAEMON); run by the scripts
installing it with TUATARA_DESKTOP_ENTRIES, it writes its desktop entries
\(DESKTOP-ENTRIES-CHILD), a request it does not take said and left alone. A
client keeps the heap it was saved with (FIT-HEAP)."
  (sb-ext:disable-debugger)
  (setf sb-ext:*posix-argv* (command-line-arguments))
  (fresh-random-state)
  (setf tuatara.io:*saved-program* (tuatara.runtime:ignoring-errors-reported ("finding this program")
                                     (namestring (truename "/proc/self/exe"))))
  (fit-heap (rest sb-ext:*posix-argv*))
  (sb-posix:unsetenv "TUATARA_HEAP_DECIDED")
  ;; from now on the AppImage, which brings its libraries along, is what starts Tuatara again
  (let ((appimage (uiop:getenv "APPIMAGE")))
    (when (and appimage (uiop:getenv "TUATARA_APPIMAGE"))
      (setf tuatara.io:*saved-program* appimage)))
  (leave-appimage-environment)
  (uiop:call-image-restore-hook)
  ;; run again by the editor to compile a package apart from it: told so by the
  ;; environment the editor gave it, never by the command line
  (when (uiop:getenv "TUATARA_COMPILE_PACKAGE")
    (let ((code (tuatara.packages:compile-package-child)))
      (finish-output)
      (sb-ext:exit :code code :abort t)))
  ;; run by the scripts that install it to write its desktop entries, told so likewise; a request
  ;; not taken leaves it to start as it would
  (when (uiop:getenv "TUATARA_DESKTOP_ENTRIES")
    (let ((code (desktop-entries-child (rest sb-ext:*posix-argv*))))
      (when code
        (finish-output)
        (sb-ext:exit :code code :abort t))))
  ;; run again by tuatara --daemon to be the daemon, told so likewise
  (let ((daemon (uiop:getenv "TUATARA_DAEMON")))
    (when daemon
      (let ((code (run-daemon daemon (rest sb-ext:*posix-argv*))))
        (finish-output)
        (sb-ext:exit :code code :abort t))))
  (let ((code (main (rest sb-ext:*posix-argv*))))
    (finish-output)
    (sb-ext:exit :code code :abort t)))
