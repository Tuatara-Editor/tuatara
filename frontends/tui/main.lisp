;;;; frontends/tui/main.lisp --- Tuatara in the terminal: the program's entry point.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; The defaults in the terminal frontend, started by MAIN: from tools/tui.lisp
;;;; while developing, and from build/tuatara, the whole editor saved as one
;;;; executable by tools/build-image.sh, which starts in a fraction of a second.

(defpackage #:tuatara.terminal
  (:use #:cl)
  (:import-from #:tuatara.runtime #:define-record)
  (:import-from #:tuatara.defaults #:forwardable-path-p)
  (:documentation "Tuatara in the terminal: the entry point shared by tools/tui.lisp and the saved executable.")
  (:export #:main #:image-toplevel #:*built-heap* #:*options* #:*option-help* #:*environment-help* #:*usage-forms*
           #:write-man-pages #:write-bash-completion))

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
    "tuatara --client=HOST[:NAME] [--gui] [--start] [--frames N] [--safe] [--hex] [[+LINE[:COLUMN]] FILE...]"
    "tuatara --stop[=NAME]"
    "tuatara --stop=HOST[:NAME]"
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
  `((:flags (("--gui"))
     :text ("open a window of its own instead (as when started with no terminal)")
     :why ("Use it when the terminal is the limit: the window draws pictures and PDFs"
           "itself, shows 24-bit colour whatever TERM says, and takes the desktop's"
           "clipboard through GTK. It needs a graphical session (WAYLAND_DISPLAY or"
           "DISPLAY), which a console or an ssh login has not. How the window is drawn"
           "is the setting *gui-renderer*."))
    (:flags (("--frames" "N"))
     :text (,(format nil "open N frames, from 1 to ~d: with --gui each a window of its" tuatara.command:*most-frames*)
            "own, in the terminal screens shown one at a time (C-x 5 o goes"
            "to the next); the files named are shown one in each frame in"
            "turn, those left over open behind the first; --frames=N does as"
            "well")
     :why ("Use it to start with several views of the same buffers. With --gui they are"
           "windows you can put on different screens. In a terminal only one frame"
           "shows at a time, so more than one is for C-x 5 o; to see two views at once,"
           "split the window (C-x 2, C-x 3). The cost is small: measured once here,"
           "16 frames on a small file added about 30 ms to the first screen and no"
           "memory worth naming."))
    (:flags (("-c" "INIT-FILE") ("--config" "INIT-FILE"))
     :text ("read INIT-FILE as the init file instead of"
            "~/.config/tuatara/init.lisp; --config=INIT-FILE does as well")
     :why ("Use it to keep several setups (one for writing, one for a work machine), to"
           "try a changed init file without touching the one you rely on, or to start"
           "from a file kept elsewhere. The file is read as the usual one is, so the"
           "packages it names are loaded. A file that is not there ends the start with"
           "status 2; the usual init file is not used in its place."))
    (:flags (("+LINE[:COLUMN]"))
     :text ("before a file: open it with the cursor on that line (from 1;"
            "the last line when the file has fewer) and column (from 1;"
            "the C in the mode line counts from 0), as in"
            "tuatara +120:8 main.lisp +3 notes.org"))
    (:flags (("--diff" "OLD NEW"))
     :text ("open the differences between the two files, or two directories, as"
            "M-x diff-files shows them, and exit when you quit; for git, as"
            "git config difftool.tuatara.cmd 'tuatara --diff \"$LOCAL\" \"$REMOTE\"'"))
    (:flags (("--merge" "LOCAL BASE REMOTE MERGED"))
     :text ("merge the three files into MERGED, the conflicts marked there to"
            "resolve, then save it; exits 0 when MERGED is saved with no"
            "conflict left, else 1; for git, as git config"
            "mergetool.tuatara.cmd 'tuatara --merge \"$LOCAL\" \"$BASE\""
            "\"$REMOTE\" \"$MERGED\"' and mergetool.tuatara.trustExitCode true"))
    (:flags (("--safe") ("-R"))
     :text ("open the files safely, whatever directory they are in: read only,"
            "no mode or hook acting on them, nothing written")
     :why ("Use it to look at a file you do not trust or must not alter: a download,"
           "the contents of a disk image, evidence. Nothing in the directory acts on the"
           "file and nothing is written, so opening it leaves it as it was. What you"
           "give up is what the directory's own settings and tools would add, and saving"
           "changes."))
    (:flags (("--hex"))
     :text ("open the files in the hex view, never first as text")
     :why ("Use it for a file that is not text (a program, an image, a database), or"
           "for text whose bytes you must read as stored, where the text view could"
           "hide a stray byte or a different line ending. A byte takes more room on"
           "the screen than a character. Measured once here, a 1 GB file showed its"
           "first screen in the same time as hex or as text (about 0.35 s)."))
    (:flags (("-"))
     :text ("as a file: read standard input into a buffer, *stdin*, opened"
            "safely (tuatara - < data, or ... | tuatara -); the terminal stays"
            "the keyboard")
     :why ("Use it to look at what another program writes, in a pipe, before it goes"
           "on. The buffer is opened safely, as --safe does, so it is for reading and"
           "for taking parts of out."))
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
            "~/.local/state/tuatara/session-daemon-NAME")
     :why ("Use it for an editor that is already there when you want one: a client"
           "(tuatara --client) shows a frame in about 0.23 s, against about 0.32 s to"
           "start an editor of its own (measured once here, on a small file), and the"
           "buffers stay open when a terminal closes. Several names keep several"
           "separate sessions. The cost is a process that stays in memory until it is"
           "stopped, with whatever it has open: a large file stays held. It needs"
           "XDG_RUNTIME_DIR, where its socket is."))
    (:flags (("--client[=NAME]"))
     :text ("show the daemon NAME in this terminal, or with --gui in a window"
            "of its own (C-x 5 2 opens another), a frame of its own, the files"
            "named opened in it, with +LINE, --safe, --hex, - and --frames as"
            "for Tuatara itself; --diff and --merge as well, with their exit"
            "codes; C-x C-c closes this client alone, its buffers kept in the"
            "daemon, and the daemon goes on; as git's editor"
            "(GIT_EDITOR='tuatara --client') the commit is made when the"
            "message is finished; exits 1 when no daemon of that name runs, or"
            "when it ends")
     :why ("Use it for the quicker start a daemon gives (about 0.23 s against about"
           "0.32 s here) and one set of buffers in every terminal. In return, a change"
           "to the init file or a package reaches the daemon only when it is stopped"
           "and started again, and every client works in the one editor. As git's"
           "editor, add --start so that it works when no daemon runs."))
    (:flags (("--client=HOST[:NAME]"))
     :text ("show the daemon NAME running on the machine HOST, reached with"
            "the system's ssh, as --client shows one here: HOST as ssh takes"
            "it (build.example, me@build.example), a name from ~/.ssh/config"
            "written with a colon after it (build: or build:work); its host"
            "key must be known already; the files named are that machine's,"
            "a relative name from your home directory there; - is not taken,"
            "and pictures stay on each side, the clipboard, dropped files and"
            "your agents too unless HOST is trusted with them (M-x"
            "server-list); with --start, the daemon there is started first"
            "when none runs; --stop=HOST[:NAME] stops it")
     :why ("Use it to work on the machine that has the files, a build server or a"
           "remote box, with your own screen and keys. Tuatara must be installed there,"
           "and the system's ssh is all it needs here. The editor runs there, so files"
           "are read at the speed of its disks, and each key crosses the network: a slow"
           "link shows as lag. The clipboard, dropped files and pictures stay on each"
           "side unless HOST is trusted with them, so a copy made there does not reach"
           "this machine's clipboard by itself."))
    (:flags (("--daemon-socket[=NAME]"))
     :text ("say where the daemon NAME listens, which Tuatara this is and"
            "whether it runs, in lines a client on another machine reads"
            "over ssh, with where gpg here looks for its agent, and exit;"
            "with --start, start it first when none runs"))
    (:flags (("--receive-file" "PATH"))
     :text ("write standard input, a file dropped on the window of a client"
            "on another machine, at PATH, from your home directory unless it"
            "begins with /, for you alone and under a name nothing has yet;"
            "say where, and exit"))
    (:flags (("--start"))
     :text ("with --client: start the daemon first when none runs")
     :why ("Use it in a script or as git's editor, where there may be no daemon yet:"
           "the first call starts one and later calls find it. Without it, a client"
           "with no daemon exits 1."))
    (:flags (("--fallback"))
     :text ("with --client: when no daemon runs, start Tuatara as it starts"
            "without --client instead, the files opened in an editor of its"
            "own; the desktop's Tuatara (client) starts it so")
     :why ("Use it where an editor must always open, as in a desktop entry. When no"
           "daemon runs it costs the ordinary start (about 0.32 s here) and gives an"
           "editor of its own, not shared with later clients."))
    (:flags (("--stop[=NAME]"))
     :text ("stop the daemon NAME, asking in this terminal about the changes"
            "not saved in it, as M-x daemon-quit does; with no terminal,"
            "nothing is asked, and the changes are kept as auto-saved copies"
            "that M-x recover-file offers")
     :why ("Use it to free the memory of a daemon you no longer need, or to start it"
           "again after you change the init file or a package. With no terminal"
           "nothing is asked, so a script can stop it without waiting; what was not"
           "saved is kept as auto-saved copies."))
    (:flags (("-Q") ("--no-init"))
     :text ("start without the init file and so without packages, with what"
            "Tuatara keeps between sessions (files visited lately, the"
            "session, bookmarks) kept apart in a directory that goes with the"
            "session, the real one neither read nor changed")
     :why ("Use it when something in the init file or a package gets in the way: a"
           "setting that slows the editor or a package that fails to load. You get the"
           "editor as it ships, so you can tell whether the fault is yours. What you do"
           "meanwhile is not remembered. It is no faster to start: with the starter"
           "init file the first screen took the same time with and without it (about"
           "0.3 s, measured once)."))
    (:flags (("--generate-config"))
     :text ("write the init file Tuatara writes when there is none, every"
            "setting in it commented out, as"
            "~/.config/tuatara/generated-init.lisp, and exit; a file there"
            "already is left as it is")
     :why ("Use it for a fresh list of every setting after an update, to compare with"
           "your own init file. It never replaces a file that is there, and the list"
           "is all comments, so you copy out the few settings you want to change."))
    (:flags (("--generate-autostart"))
     :text ("write the entry that has the desktop start the daemon as you log"
            "in, ~/.config/autostart/tuatara-daemon.desktop, running tuatara"
            "--daemon, and exit; with --daemon=NAME, the daemon NAME's,"
            "tuatara-daemon-NAME.desktop; a file there already is left as it"
            "is")
     :why ("Use it for a daemon that is ready from login, so that the first terminal"
           "or window you open starts quickly. It costs the daemon's memory from the"
           "start of every session, whether or not you edit that day. Delete the file"
           "to undo it."))
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
            "too")
     :why ("Use it to start the AppImage from the application menu. The copy at"
           "~/.local/bin/tuatara does not update itself: run this again from a new"
           "AppImage."))
    (:flags (("--remove-desktop")) :only :appimage
     :text ("(the AppImage) take them away again"))
    (:flags (("--dark") ("--light")) :only :appimage
     :text ("(the AppImage) show the window's title bar in the dark or the"
            "light style; with --install-desktop, for the desktop's"
            "applications")
     :why ("Use it when the window's title bar clashes with the desktop. It changes"
           "only the title bar's style; the text and colours come from the theme."))
    (:flags (("--dynamic-space-size" "SIZE")) :only :runtime
     :text ("the most memory Tuatara's own data may take, such as 6GB; by"
            "default half the machine's memory, from 2GB to 16GB, or"
            "$TUATARA_HEAP_MB megabytes")
     :why ("Use it to set the heap for one run; $TUATARA_HEAP_MB does for every run."
           "That variable says what raising or lowering it does.")))
  "The options, in the order --help lists them, each a property list. :FLAGS,
the ways of writing it, each (FLAG) or (FLAG ARGUMENT); :TEXT, what it does,
as lines; :WHY, as lines, for an option with a choice: why someone would take
it, what that gets past and what to expect, the cost included (an empty line
starts a new paragraph; --help leaves it out, the manual page has it). :ONLY,
:APPIMAGE for an option only the AppImage's launcher takes or :RUNTIME for one
the program's runtime takes out of the command line before Tuatara sees it. The
manual page is made from this table too.")

(defparameter *environment-help*
  '((:names ("HOME")
     :text ("The home directory, where the directories below are when the variables that follow"
            "are not set."))
    (:names ("XDG_CONFIG_HOME")
     :text ("Where the configuration directory, tuatara/, is: the init file, snippets and"
            "templates; and the desktop's autostart/ directory. Not set: ~/.config.")
     :why ("Set it to keep the init file and templates in a directory you back up or share"
           "between machines, or to run a second setup beside your own (a trial, a"
           "test): Tuatara then reads and writes only there. A daemon takes the value"
           "of the call that started it."))
    (:names ("XDG_STATE_HOME")
     :text ("Where the state directory, tuatara/, is: what Tuatara remembers between sessions"
            "and the auto-saved copies. Not set: ~/.local/state.")
     :why ("Set it to keep what Tuatara remembers on another drive, or apart for a"
           "trial. The auto-saved copies are there too, so choose a drive that is"
           "mounted when you want to recover a file."))
    (:names ("XDG_DATA_HOME")
     :text ("Where the data directory, tuatara/, is: the grammars you added and the packages"
            "fetched. Not set: ~/.local/share.")
     :why ("Set it to keep fetched packages and added grammars on a drive with room, or"
           "apart for a trial."))
    (:names ("XDG_CACHE_HOME")
     :text ("Where the cache directory, tuatara/, is: files made again when they are gone, and"
            "the logs. Not set: ~/.cache.")
     :why ("Set it to a fast or temporary drive, as nothing there is lost for good: the"
           "next start makes again what it needs (compiled packages, previews), and"
           "takes longer for it. The debug log, stderr.log and the daemons' logs are"
           "there too, so a fresh directory starts them empty."))
    (:names ("XDG_RUNTIME_DIR")
     :text ("Where a directory named by a setting is kept in memory while it is not there, as"
            "when its drive is not mounted; and where a daemon's socket is, in tuatara/,"
            "for you alone. Not set: no daemon starts, and no client finds one.")
     :why ("Your login sets it. Set it by hand only where there is none (a container, a"
           "cron job), to a directory only you can enter, or no daemon will start there."))
    (:names ("NO_COLOR")
     :text ("When set and not empty, tuatara uses no colours in the terminal, unless the"
            "setting *tui-colours* says how many.")
     :why ("Set it for a terminal or a screen where colour gets in the way. What colour"
           "told (a changed line, an error, the current match) is then shown by the"
           "plain theme in their place."))
    (:names ("TERM")
     :text ("The terminal's type, whose description says how many colours it shows.")
     :why ("Your terminal sets it. Change it for one run when a host does not know your"
           "terminal's name (over ssh, say): xterm-256color is understood nearly"
           "everywhere, at the price of the colours and keys that terminal alone has."))
    (:names ("COLORTERM")
     :text ("When it is truecolor or 24bit, the terminal shows 24-bit colour and the"
            "theme's colours are sent exactly.")
     :why ("ssh does not pass it on, so over ssh the theme is drawn with as many colours"
           "as TERM's description gives, 256 at most. Set it to truecolor on the far"
           "side when your terminal shows 24-bit colour, or set *tui-colours* once."
           "If the terminal does not show it, the colours come out wrong. A client of"
           "a daemon brings its own value."))
    (:names ("TERMINFO" "TERMINFO_DIRS")
     :text ("Where the terminal's description is looked for, besides the usual directories.")
     :why ("Set one when a terminal's description is installed in your home directory and"
           "not in the system's; the terminal is otherwise taken for one the usual"
           "directories describe."))
    (:names ("WAYLAND_DISPLAY" "DISPLAY")
     :text ("With neither input nor output a terminal, a desktop to show a window on.")
     :why ("The desktop sets them. Set one by hand to open the window on another"
           "display, or over ssh -X, where each frame crosses the network."))
    (:names ("GSK_RENDERER")
     :text ("How the window is drawn, as GTK names it: gl, vulkan or cairo. It goes before"
            "the setting *gui-renderer*.")
     :why ("Set it when the window draws slowly or wrongly with your graphics driver: gl"
           "and vulkan draw on the graphics card, cairo on the processor alone, which"
           "is usually slower for a large window but works where the drivers do not. C-h"
           "check-health in the window measures them."))
    (:names ("TUATARA_HEAP_MB")
     :text ("The most memory, in megabytes, tuatara's data may take. Not set: half the"
            "machine's memory, from 2 GB to 16 GB. --dynamic-space-size overrides it.")
     :why ("The heap holds what the editor builds in memory: text you have typed or"
           "pasted and the undo history, the places of a split or many cursors,"
           "search indexes, pictures. Files you open are mapped, not held in it."
           "Measured here: a 1 GB file shows its first screen in about 0.35 s with"
           "any heap, and a replacement across it ran in the background in about"
           "0.4 s with a heap of 256 MB as with 2 GB, because large replacements,"
           "sorts and counts are written to files in pieces."
           ""
           "Raise it for work held in memory. With *split-limit* or *cursors-limit*"
           "raised, a split or many cursors plan 768 bytes a place out of a quarter"
           "of the free heap: about 700,000 places at 2 GB and 5 million at 16 GB"
           "(from the code). A bigger heap costs memory only as it is used, but it"
           "lets live data grow, and a full collection copies live data while the"
           "editor stands still: measured, 0.1 to 0.2 s with 300 to 600 MB live,"
           "1.6 s with 1.1 GB. Collections come after each 256 MB of new data"
           "whatever the heap, so the size does not set the pauses; the live data"
           "does."
           ""
           "Lower it on a small or shared machine: the editor started and showed a"
           "256 MB file with a heap of 64 MB. The limits above shrink with it."
           ""
           "When the heap runs out in a command, the command stops with a message"
           "and the editor goes on (measured at 512 MB: stopped at about 480 MB in"
           "use, and keys still worked). Running out in the middle of a collection"
           "cannot be recovered and ends the editor, which is why work planned in"
           "memory keeps to a quarter of what is free. Changes not saved are then"
           "in the auto-saved copies that M-x recover-file offers."))
    (:names ("LC_ALL" "LC_MESSAGES" "LANG")
     :text ("The language spelling is checked in, when no setting says.")
     :why ("Set one to check spelling in a language other than the system's for one"
           "run; the setting *spellcheck-languages* does it for every run and"
           "goes before these."))
    (:names ("TZ")
     :text ("The time zone, when set; else the system's.")
     :why ("Set it to have the editor use another zone for one run, as when you travel"
           "and keep the machine's clock as it is, Europe/Stockholm or"
           "Pacific/Auckland. Times stored in files do not change."))
    (:names ("SHELL")
     :text ("The shell commands typed in the editor run with. Not set: /bin/sh.")
     :why ("Set it to run typed commands with a shell whose aliases or syntax you use."
           "A daemon takes the one of the call that started it."))
    (:names ("GIT_EDITOR" "EDITOR" "VISUAL")
     :text ("Not read by tuatara. Set to tuatara (or git's core.editor to it), git starts"
            "it to write a commit message; see EXAMPLES.")))
  "The environment variables the program reads, in the order tuatara(1) lists
them, each a property list: :NAMES, the variables it is for; :TEXT, what they
do, as lines; :WHY, as lines, when there is a choice: why someone would
change it, what that gets past, and what to expect, the cost included (an empty
line starts a new paragraph). The manual page is made from this table.")

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

(defun wanted-heap-mb (&key (setting (tuatara.io:own-variable "TUATARA_HEAP_MB")) (memory (memory-kb)))
  "The heap Tuatara wants, in megabytes: SETTING ($TUATARA_HEAP_MB) when it is
a number, else half of MEMORY (kilobytes, the machine's), from 2 GB to 16 GB,
so that work held in memory has room while the heap cannot outgrow the memory. NIL when neither is known."
  (or (let ((mb (tuatara.text:whole-number setting))) (and mb (plusp mb) mb))
      (and memory (max 2048 (min 16384 (floor memory 2048))))))

(defun heap-to-restart-with (&key (now (sb-ext:dynamic-space-size)) (built *built-heap*)
                                  (wanted (wanted-heap-mb))
                                  (restarted (tuatara.io:own-variable "TUATARA_HEAP_DECIDED")))
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
  (tuatara.io:run-program-again (rest sb-ext:*posix-argv*) :heap mb))

(defparameter *runtime-options-with-values* '("--dynamic-space-size" "--control-stack-size" "--tls-limit")
  "The options SBCL's runtime takes out of the command line, each with the value after it.")

(defun command-line-arguments (&key (argv sb-ext:*posix-argv*) (from "/proc/self/cmdline"))
  "The program's command line, its name first, as SB-EXT:*POSIX-ARGV* has it.
This is ARGV itself. When ARGV is NIL because an argument is not UTF-8, the
line is read again as octets FROM the system's record of it, each octet that
is not UTF-8 kept as its raw byte's character, so that such a file name
still opens. The runtime's options (*RUNTIME-OPTIONS-WITH-VALUES*) are left
out, as the runtime leaves them out."
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

(defun window-wanted-p (&key (input (terminal-p 0)) (output (terminal-p 1)) (desktop (tuatara.io:desktop-p)))
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
        (when fd (tuatara.runtime:close-fd-quietly fd "the terminal") t))))

(defun read-keys-from-terminal ()
  "When Tuatara's input is not a terminal, as in
`find . -name \"*.org\" | xargs tuatara', read the keys from the terminal it
was started in (/dev/tty) instead; what came in on the input is not read as
keys. KEYBOARD-AVAILABLE-P says first whether there is such a terminal."
  (unless (terminal-p 0)
    (let ((fd (handler-case (sb-posix:open "/dev/tty" sb-posix:o-rdwr) (sb-posix:syscall-error () nil))))
      (when fd
        (sb-posix:dup2 fd 0)
        (tuatara.runtime:close-fd-quietly fd "the terminal")))))

(defun bare-flag (flag)
  "FLAG, an option as *OPTION-HELP* writes it, without the [=NAME] or =HOST
of a value it may or must be given: --daemon of --daemon[=NAME], --client of
--client=HOST[:NAME]."
  (subseq flag 0 (or (search "[=" flag) (position #\= flag))))

(defparameter *options*
  (remove-duplicates
   (loop for entry in *option-help*
         unless (getf entry :only)
           append (loop for flag in (getf entry :flags)
                        unless (char= (char (first flag) 0) #\+) collect (bare-flag (first flag))))
   :test #'string= :from-end t)
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

(defun remote-daemon (value)
  "When VALUE, given to --client= or --stop=, names a daemon on another
machine, HOST:NAME, HOST: (the default daemon there), or a HOST with a dot
or an @ in it, the host and the daemon's name, or NIL for the default one,
as two values; NIL when VALUE is a daemon's name here. A host that cannot be
one (HOST-REFUSAL) gives NIL, NIL and why, as a third value."
  (when (find-if (lambda (c) (find c ":.@")) value)
    (let* ((colon (position #\: value :from-end t))
           (host (subseq value 0 colon))
           (name (and colon (< (1+ colon) (length value)) (subseq value (1+ colon))))
           (why (tuatara.defaults:host-refusal host)))
      (if why (values nil nil why) (values host name)))))

(defun file-here-p (argument directory)
  "True when ARGUMENT, as written on the command line, names something in
DIRECTORY, a link itself and not what it points to."
  (tuatara.runtime:ignoring-errors-reported ("examining ~a" argument)
    (tuatara.io:path-kind (tuatara.io:expand-path argument :directory directory) :follow nil)))

(defun file-here-words (argument)
  "What to say when ARGUMENT looks like an option, or a place, but is also the
name of a file in the directory: how to write it as a file."
  (format nil "~a is a file here too: write ./~a, or -- before the files" argument argument))

(defun first-name (named)
  "The first of NAMED, the names READ-OPTIONS collected, as it was written on the command line."
  (let ((name (first named)))
    (if (consp name) (second name) name)))

(defun read-options (arguments &key (directory (uiop:getcwd)))
  "The options of ARGUMENTS, the program's (strings), as a plist: :GUI,
:FRAMES (how many --frames opens), :CONFIG (the init file -c names),
:GENERATE-CONFIG, :VERSION, :HELP, :DIFF (the two names after --diff), :MERGE
\(the four after --merge), :SAFE, :HEX, :TUTOR and :BARE (-Q).
The daemon's options follow. :DAEMON, :CLIENT and :STOP are each the daemon's
name, *DAEMON-DEFAULT-NAME* when none is given. :HOST is the machine a
--client or --stop daemon runs on (REMOTE-DAEMON). :DAEMON-SOCKET is a
daemon's name. :START, :FALLBACK and :GENERATE-AUTOSTART come last, and
:FILES, the files named.
The plist holds :ERROR instead, what is wrong with them.
Options may stand anywhere before --, among the files. --safe and --hex hold
for every file of the call. A +LINE[:COLUMN] holds for the file after it,
which is then (NAME LINE COLUMN) in :FILES, the column NIL when none was
given. A - (standard input) is (:STANDARD-INPUT). What starts with a dash and
is no option is refused. So is an option or +LINE that is also a file's name
in DIRECTORY, where the program was started: write ./NAME for the file, or --
before the files. After --, every argument is a file's name."
  (let ((options '()) (named '()))
    (flet ((given (key) (setf (getf options key) t))
           (fail (&rest words) (return-from read-options (list :error (apply #'format nil words)))))
      (loop
        (let ((argument (pop arguments)))
          (cond ((null argument) (return))
                ((string= argument "--")
                 (dolist (rest arguments) (push (list :literal rest) named))
                 (return))
                ;; a file named as an option, as * gives one in a directory holding it: never read as
                ;; one, so that no file there chooses the init file or what is done
                ((and (or (option-p argument) (option-like-p argument)) (file-here-p argument directory))
                 (fail "~a" (file-here-words argument)))
                ;; --NAME=VALUE read as --NAME VALUE
                ((option-with-value argument)
                 (multiple-value-bind (option value) (option-with-value argument)
                   (push value arguments)
                   (push option arguments)))
                ((or (member argument *named-options* :test #'string=) (named-option argument))
                 (multiple-value-bind (option name) (named-option argument)
                   (let ((option (or option argument)))
                     (multiple-value-bind (host daemon why)
                         (and name (member option '("--client" "--stop") :test #'string=) (remote-daemon name))
                       (when why (fail "~a" why))
                       (let ((name (or daemon (and (not host) name) tuatara.defaults:*daemon-default-name*)))
                         (unless (tuatara.defaults:daemon-name-p name)
                           (fail "~a cannot name a daemon: letters, digits, - and _, at most 40" name))
                         (when host (setf (getf options :host) host))
                         (setf (getf options (intern (string-upcase (subseq option 2)) :keyword)) name))))))
                ((string= argument "--start") (given :start))
                ((string= argument "--fallback") (given :fallback))
                ((string= argument "--generate-autostart") (given :generate-autostart))
                ((member argument '("--version" "-V") :test #'string=) (given :version))
                ((member argument '("--help" "-h") :test #'string=) (given :help))
                ((string= argument "--gui") (given :gui))
                ((string= argument "--generate-config") (given :generate-config))
                ((string= argument "--receive-file")
                 (let ((path (pop arguments)))
                   (if (plusp (length path))
                       (setf (getf options :receive-file) path)
                       (fail "--receive-file needs the path to write after it"))))
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
                 (fail "~a is not an option of tuatara (--help lists them): write ./~a, or -- before the files, ~
                        for a file"
                       argument argument))
                (t (push argument named)))))
      (setf named (nreverse named))
      (cond ((and (getf options :diff) named)
             (fail "--diff takes the two files to compare, not ~a too" (first-name named)))
            ((and (getf options :merge) named)
             (fail "--merge takes the four files to merge, not ~a too" (first-name named)))
            ((and (getf options :diff) (getf options :merge))
             (fail "--diff and --merge are not given together"))
            ((and (getf options :tutor) named)
             (fail "--tutor takes no files, not ~a" (first-name named)))
            ((> (count-if (lambda (key) (getf options key)) '(:daemon :client :stop :daemon-socket)) 1)
             (fail "--daemon, --client, --stop and --daemon-socket are given one at a time"))
            ((and (getf options :bare) (or (getf options :daemon) (getf options :client) (getf options :stop)))
             (fail "-Q is never a daemon's: it starts a fresh, bare editor of its own, which ends with it"))
            ((and (getf options :gui) (or (getf options :daemon) (getf options :stop)))
             (fail "~:[--stop~;--daemon~] opens no window: tuatara --client --gui shows a daemon in one"
                   (getf options :daemon)))
            ((and (getf options :start) (not (or (getf options :client) (getf options :daemon-socket))))
             (fail "--start goes with --client"))
            ((and (getf options :host) (member "-" named :test #'equal))
             (fail "- is not read by a client on another machine: standard input cannot go over ssh"))
            ((and (getf options :host) (getf options :fallback))
             (fail "--fallback goes with a daemon on this machine: with a host, the files named are ~
                    that machine's"))
            ((and (getf options :daemon-socket)
                  (or named (loop for (key) on options by #'cddr
                                  thereis (not (member key '(:daemon-socket :start))))))
             (fail "--daemon-socket takes nothing but --start"))
            ((and (getf options :receive-file)
                  (or named (loop for (key) on options by #'cddr thereis (not (eq key :receive-file)))))
             (fail "--receive-file takes nothing else"))
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
  "True when ARGUMENT looks like a place asked for, a + and then only digits
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
                 (format nil "~a is not +LINE or +LINE:COLUMN, numbers of up to nine digits: write ./~a for a file, ~
                          or -- before the files"
                         argument argument)))
              ((consp argument)
               (push (if line (list (second argument) line column) (second argument)) files)
               (setf line nil column nil))
              ((and (or asked-line (string= argument "-")) (file-here-p argument directory))
               (return-from read-places (file-here-words argument)))
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
directory, never over a file there; said on the standard output, or why not on
the error output. The exit code: 0 when written, 1 when not."
  (let ((path (tuatara.defaults:config-path "generated-init.lisp")))
    (multiple-value-bind (written why) (tuatara.defaults:write-starter-config path)
      (cond (written (format t "Wrote ~a~%" path) 0)
            (why (format *error-output* "tuatara: no init file written at ~a: ~a~%" path why) 1)
            (t (format *error-output* "tuatara: ~a is there already; move it away to write a new one~%" path) 1)))))

(defun shown (text)
  "TEXT, a name or a sentence with names in it, with its control characters
written out, to be put on the terminal."
  (values (tuatara.command:escape-controls text)))

(defun named-here (name)
  "The file NAME as the system names it, from where Tuatara was started."
  (tuatara.io:native (tuatara.io:expand-path name :directory (uiop:getcwd))))

(defun missing-file (names)
  "The first of NAMES, files, that is not there, or NIL."
  (find-if-not (lambda (name)
                 (tuatara.runtime:ignoring-errors-reported ("examining ~a" name)
                   (tuatara.io:path-kind (named-here name))))
               names))

(defun missing-file-exit (names purpose)
  "When one of NAMES, files, is not there, say so on the error output, PURPOSE
being what there is nothing of to do (\"compare\", \"merge\"), and return 2, the
exit code; else NIL."
  (let ((missing (missing-file names)))
    (when missing
      (format *error-output* "tuatara: ~a is not there: nothing to ~a~%" (shown missing) purpose)
      2)))

(defun startup-from (options &key (input 0))
  "What the editor is started with for OPTIONS (READ-OPTIONS), as two values.
The first is the files to open, as RUN-EDITOR takes them: a name, or a list of
it and :SAFE and :HEX as asked, or (:STANDARD-INPUT FD) for a copy of the
input kept for - before the terminal takes its place (of INPUT, the
descriptor of standard input). It also gives the names from where Tuatara
was started. The second is START-FUNCTION for the rest. With -Q the editor is
set to start bare first (TUATARA.DEFAULTS:START-BARE)."
  (when (getf options :bare)
    (tuatara.defaults:start-bare))
  (let* ((safe (getf options :safe))
         (hex (getf options :hex))
         (merge (mapcar #'named-here (getf options :merge)))
         (places '())
         (files (loop for file in (if merge (list (fourth merge)) (getf options :files))
                      collect (cond ((stringp file) (startup-file (named-here file) safe hex))
                                    ((eq (first file) :standard-input)
                                     (list :standard-input (tuatara.io:duplicate-fd input :lowest 3)))
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
or NIL when there is nothing to do. It goes to the line each of PLACES asks
for, (PATH LINE COLUMN). It shows the differences of DIFF, the old and the new
file. It merges MERGE, the local, base and remote file and the one the merge
goes into. It opens the tutor. It says that it started BARE. It opens FRAMES
frames in all, the files spread over them
\(TUATARA.DEFAULTS:OPEN-START-FRAMES)."
  (when (or places diff merge tutor bare (and frames (> frames 1)))
    (lambda (editor)
      (when places (tuatara.defaults:show-start-places places))
      (when diff
        (tuatara.defaults:compare-paths (first diff) (second diff))
        (tuatara.command:only-window (tuatara.command:current-window)))
      (when merge (tuatara.defaults:merge-files-into (first merge) (second merge) (third merge) (fourth merge)))
      (when tutor (tuatara.defaults:tuatara-tutor))
      (when bare (tuatara.defaults:say-started-bare))
      (when (and frames (> frames 1)) (tuatara.defaults:open-start-frames editor frames)))))

(defun main (arguments)
  "Run Tuatara in this terminal on the files named in ARGUMENTS (strings), or
in the scratch buffers when there are none, as its options say
\(READ-OPTIONS). --version, --help and --generate-config do what they say
instead. --diff OLD NEW shows what differs between two files. -c names the
init file. +LINE[:COLUMN] before a file opens it there. -- before the files
lets a file's name start with a dash or a plus.
With --gui, or when started with no terminal on a desktop, Tuatara opens a
window of its own (RUN-INSTANCE). --daemon starts an editor in the background
\(START-DAEMON). --client shows one in this terminal, or a window with --gui,
here or on another machine. --stop stops one (RUN-CLIENT). --daemon-socket
says where one listens (REPORT-DAEMON-SOCKET). --receive-file takes a file
dropped on such a client's window (RECEIVE-DROPPED-FILE). --client --fallback
starts an editor of its own when none runs. --generate-autostart writes the
entry that starts the daemon with the desktop (GENERATE-AUTOSTART).
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
           (generate-autostart (or (getf options :daemon) tuatara.defaults:*daemon-default-name*)))
          ((getf options :daemon)
           (start-daemon (getf options :daemon) config-path))
          ((getf options :daemon-socket)
           (report-daemon-socket (getf options :daemon-socket) (getf options :start)))
          ((getf options :receive-file)
           (receive-dropped-file (getf options :receive-file)))
          ;; the files a client on another machine names are that machine's
          ((getf options :host)
           (run-client options))
          ((missing-file-exit (getf options :diff) "compare"))
          ((and (find '(:standard-input) (getf options :files) :test #'equal) (terminal-p 0))
           (format *error-output* "tuatara: - reads standard input, which is the terminal here: pipe something in~%")
           2)
          ((let ((merge (getf options :merge)))
             (and merge (missing-file-exit (list (first merge) (third merge)) "merge"))))
          ((and config (not (eq (tuatara.runtime:ignoring-errors-reported ("examining ~a" config-path)
                                  (tuatara.io:path-kind config-path))
                                :regular)))
           (format *error-output*
                   "tuatara: no init file at ~a (tuatara --generate-config writes one to start from)~%"
                   (shown config))
           2)
          ((or (getf options :client) (getf options :stop))
           (let ((code (run-client options)))
             ;; --client --fallback with no daemon running: an editor of its own instead, with an editor's heap
             (if (eq code :fallback)
                 (progn (fit-editor-heap)
                        (run-instance options config-path))
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
        (tuatara.defaults:set-collection-policy)
        (when config-path
          (setf tuatara.defaults:*config-file* (pathname config-path)))
        (multiple-value-bind (files start) (startup-from options)
          (let ((gui (or (getf options :gui) (window-wanted-p))))
            ;; a copy started to measure how the window draws (check-health): no init
            ;; file, the window's font as the one measuring has it
            (let ((measure (tuatara.io:own-variable "TUATARA_MEASURE_DRAWING")))
              (when measure
                (setf tuatara.defaults:*config-file* (merge-pathnames "no-init-file.lisp" measure)
                      tuatara-user:*gui-font* (tuatara.io:own-variable "TUATARA_MEASURE_FONT"))))
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
  (when (tuatara.io:own-variable "TUATARA_APPIMAGE")
    (let ((before (tuatara.io:own-variable "TUATARA_LD_LIBRARY_PATH")))
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

(defun fit-editor-heap ()
  "Start this program again with the heap its machine wants for an editor
\(HEAP-TO-RESTART-WITH), when that is another than it has; else go on."
  (let ((mb (heap-to-restart-with)))
    (when mb (restart-with-heap mb))))

(defun fit-heap (arguments)
  "Fit this program's heap to what ARGUMENTS, its command line, start: a
client of a daemon, which paints and reads keys, keeps the heap it was saved
with and collects its garbage often (*CLIENT-NURSERY-OCTETS*), so that little
of its memory is touched; anything else is started again with the heap its
machine wants (FIT-EDITOR-HEAP). A client that finds no daemon and starts an
editor of its own instead (--fallback) has its heap fitted then."
  (if (client-arguments-p arguments)
      (tuatara.defaults:set-collection-policy tuatara.defaults:*client-nursery-octets*)
      (fit-editor-heap)))

(defun image-toplevel ()
  "What the saved executable runs: MAIN on its command-line arguments, then exit
with its code at once, the terminal already given back.
First, when this machine wants a heap of another size than the executable was
saved with, it starts again with that one (HEAP-TO-RESTART-WITH). Its random
numbers are new (FRESH-RANDOM-STATE), and its arguments are read even when
one is not UTF-8 (COMMAND-LINE-ARGUMENTS). Run from the AppImage, the programs
it starts are given the environment as it was before
\(LEAVE-APPIMAGE-ENVIRONMENT).
Run again by the editor itself with TUATARA_COMPILE_PACKAGE in its
environment, it compiles a package apart from it instead, its arguments never
looked at (TUATARA.PACKAGES:COMPILE-PACKAGE-CHILD). Run again by tuatara
--daemon with TUATARA_DAEMON, it is that daemon (RUN-DAEMON). Run by the
scripts installing it with TUATARA_DESKTOP_ENTRIES, it writes its desktop
entries (DESKTOP-ENTRIES-CHILD), a request it does not take said and left
alone.
A client keeps the heap it was saved with (FIT-HEAP) until it starts an editor
of its own (--fallback)."
  (sb-ext:disable-debugger)
  (setf sb-ext:*posix-argv* (command-line-arguments))
  (fresh-random-state)
  (setf tuatara.io:*saved-program* (tuatara.runtime:ignoring-errors-reported ("finding this program")
                                     (namestring (truename "/proc/self/exe"))))
  (fit-heap (rest sb-ext:*posix-argv*))
  (sb-posix:unsetenv "TUATARA_HEAP_DECIDED")
  ;; from now on the AppImage, which brings its libraries along, is what starts Tuatara again
  (let ((appimage (tuatara.io:own-variable "APPIMAGE")))
    (when (and appimage (tuatara.io:own-variable "TUATARA_APPIMAGE"))
      (setf tuatara.io:*saved-program* appimage)))
  (leave-appimage-environment)
  (uiop:call-image-restore-hook)
  ;; run again by the editor to compile a package apart from it: told so by the
  ;; environment the editor gave it, never by the command line
  (when (tuatara.io:own-variable "TUATARA_COMPILE_PACKAGE")
    (let ((code (tuatara.packages:compile-package-child)))
      (finish-output)
      (sb-ext:exit :code code :abort t)))
  ;; run by the scripts that install it to write its desktop entries, told so likewise; a request
  ;; not taken leaves it to start as it would
  (when (tuatara.io:own-variable "TUATARA_DESKTOP_ENTRIES")
    (let ((code (desktop-entries-child (rest sb-ext:*posix-argv*))))
      (when code
        (finish-output)
        (sb-ext:exit :code code :abort t))))
  ;; run again by tuatara --daemon to be the daemon, told so likewise
  (let ((daemon (tuatara.io:own-variable "TUATARA_DAEMON")))
    (when daemon
      (let ((code (run-daemon daemon (rest sb-ext:*posix-argv*))))
        (finish-output)
        (sb-ext:exit :code code :abort t))))
  (let ((code (main (rest sb-ext:*posix-argv*))))
    (finish-output)
    (sb-ext:exit :code code :abort t)))
