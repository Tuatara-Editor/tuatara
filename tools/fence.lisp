;;;; tools/fence.lisp --- Keep a program that runs the editor away from the developer's own files and settings.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; Loaded by the tests (test/fences.lisp), the fuzzers (tools/fuzz-common.lisp) and the manual builders
;;;; (tools/manual-tables.lisp, tools/manual-html.lisp): one list of what a run of the editor must not read or
;;;; write, in one place.

(require :asdf)
(require :sb-posix)

(defpackage #:tuatara.fence
  (:use #:cl)
  (:export #:fence-person-data #:*fenced-variables* #:*closed-variables*)
  (:documentation "The environment variables and the init file that lead the editor to a person's own files."))

(in-package #:tuatara.fence)

(defparameter *fenced-variables*
  '(("HOME" . "home/") ("XDG_CONFIG_HOME" . "config/") ("XDG_STATE_HOME" . "state/") ("XDG_DATA_HOME" . "data/")
    ("XDG_CACHE_HOME" . "cache/") ("GNUPGHOME" . "gnupg/") ("XDG_RUNTIME_DIR" . "runtime/") ("TMPDIR" . "tmp/")
    ("XDG_DOWNLOAD_DIR" . "downloads/"))
  "Each environment variable that leads the editor or a program it runs to a person's own files (their home, their
settings, state, data, caches and GnuPG keys, the sockets of their running daemons, their temporary files, their
downloads), with the directory of a fence it is set to.")

(defparameter *closed-variables*
  '("SSH_AUTH_SOCK" "SSH_AGENT_PID" "GPG_AGENT_INFO" "DBUS_SESSION_BUS_ADDRESS" "AT_SPI_BUS_ADDRESS" "DISPLAY"
    "WAYLAND_DISPLAY" "WAYLAND_SOCKET" "XAUTHORITY" "PULSE_SERVER" "PIPEWIRE_REMOTE" "SWAYSOCK" "I3SOCK"
    "HYPRLAND_INSTANCE_SIGNATURE" "TMUX" "KITTY_LISTEN_ON" "WEZTERM_UNIX_SOCKET" "GNOME_KEYRING_CONTROL"
    "GIT_DIR" "GIT_WORK_TREE" "GIT_INDEX_FILE" "GIT_OBJECT_DIRECTORY" "GIT_ALTERNATE_OBJECT_DIRECTORIES"
    "GIT_COMMON_DIR" "GIT_CONFIG_PARAMETERS" "GIT_CONFIG_COUNT" "GIT_AUTHOR_NAME" "GIT_AUTHOR_EMAIL"
    "GIT_COMMITTER_NAME" "GIT_COMMITTER_EMAIL" "EMAIL" "GIT_SSH_COMMAND" "GIT_SSH" "GIT_ASKPASS" "SSH_ASKPASS"
    "GIT_EXEC_PATH" "GIT_TEMPLATE_DIR")
  "Each environment variable that leads a program to a person's running agents and servers outside their runtime
directory (their ssh and GnuPG agents, session bus, display, sound, window manager, terminal multiplexer and
keyrings), or git to one of their repositories, their name and address, their programs for ssh, passwords and
git's own commands, or settings given for one run, unset in a fence. Their Kerberos
tickets are fenced apart: KRB5CCNAME names a file in the fence, as Kerberos would otherwise look in their keyring. A test that needs a display starts its own and names it.")

(defparameter *private-variables* '("GNUPGHOME" "XDG_RUNTIME_DIR")
  "The variables of *FENCED-VARIABLES* whose directory is for its owner alone, as the system's are.")

(defparameter *git-settings*
  (format nil "[user]~%~cname = Test~%~:*~cemail = test@example.invalid~%[commit]~%~:*~cgpgsign = false~%[init]~%~
               ~:*~cdefaultBranch = main~%"
          #\Tab)
  "The global git settings of a fence, in place of a person's own (their name, their signing, their aliases).")

(defun fence-pid (name)
  "The process number in NAME, a directory's, when it is that of a fence FENCE-PERSON-DATA made in the temporary
directory, as tuatara-fence-PID-RANDOM, RANDOM sixteen hexadecimal digits; NIL for any other name."
  (let* ((prefix "tuatara-fence-")
         (dash (and (uiop:string-prefix-p prefix name) (position #\- name :start (length prefix)))))
    (and dash (< (length prefix) dash)
         (every #'digit-char-p (subseq name (length prefix) dash))
         (= (length name) (+ dash 17))
         (every (lambda (c) (digit-char-p c 16)) (subseq name (1+ dash)))
         (parse-integer name :start (length prefix) :end dash))))

(defun remove-stale-fences ()
  "Remove the fences in the temporary directory whose program has ended without removing its own (killed, or
ended at once), so that none is left whatever way a run ended. Only a directory of this account's own is
removed, never a link or what one leads to."
  (dolist (entry (uiop:directory* (merge-pathnames "tuatara-fence-*" (uiop:temporary-directory))))
    (let* ((path (string-right-trim "/" (sb-ext:native-namestring entry)))
           (pid (fence-pid (subseq path (1+ (position #\/ path :from-end t)))))
           (stat (and pid (handler-case (sb-posix:lstat path) (sb-posix:syscall-error () nil)))))
      (when (and stat (= (logand (sb-posix:stat-mode stat) #o170000) #o040000)
                 (= (sb-posix:stat-uid stat) (sb-posix:getuid))
                 (handler-case (progn (sb-posix:kill pid 0) nil)
                   (sb-posix:syscall-error (e) (= (sb-posix:syscall-errno e) sb-posix:esrch))))
        (uiop:delete-directory-tree (uiop:ensure-directory-pathname path) :validate t :if-does-not-exist :ignore)))))

(defun make-fence-directory ()
  "Make a new directory for a fence in the temporary directory, for this account alone, under a name no one can
guess beforehand (FENCE-PID), and return it as a pathname. Made only when nothing is there by that name, so that
nothing planted in its place is used."
  (with-open-file (random "/dev/urandom" :element-type '(unsigned-byte 64))
    (loop (let ((path (format nil "~atuatara-fence-~d-~16,'0x" (sb-ext:native-namestring (uiop:temporary-directory))
                              (sb-posix:getpid) (read-byte random))))
            (handler-case (progn (sb-posix:mkdir path #o700)
                                 (return (uiop:ensure-directory-pathname path)))
              (sb-posix:syscall-error (e)
                (unless (= (sb-posix:syscall-errno e) sb-posix:eexist) (error e))))))))

(defun fenced-directory (place root)
  "The directory PLACE names, taken from ROOT when it is relative, with each . and .. in it worked out, so
that two spellings of one directory give equal pathnames."
  (let ((path (uiop:ensure-directory-pathname (merge-pathnames place root)))
        (parts '()))
    (dolist (part (rest (pathname-directory path)))
      (cond ((member part '(:up :back)) (pop parts))
            ((not (equal part ".")) (push part parts))))
    (make-pathname :directory (cons :absolute (reverse parts)) :name nil :type nil :version nil :defaults path)))

(defun fence-person-data (directory &key places)
  "Point every variable of *FENCED-VARIABLES* at a directory of its own under DIRECTORY, made if missing (GNUPGHOME's
private to its owner), unset those of *CLOSED-VARIABLES*, point KRB5CCNAME at a file in the fence, git at
global settings in it (*GIT-SETTINGS*) and none of the system's, and the editor's init file at a file that does
not exist, so that what runs after reads nothing of the person's, writes nothing there and reaches none of their
agents; an editor loaded already looks for its state files anew. PLACES is a list of (VARIABLE . PLACE)
overriding a variable's directory, or leaving it (a closed one, KRB5CCNAME or GIT_CONFIG_GLOBAL too) as it is
when PLACE is :KEEP. When DIRECTORY is NIL the fence is a new directory in the temporary one
\(MAKE-FENCE-DIRECTORY), removed when Lisp exits, and those other runs left behind removed first
\(REMOVE-STALE-FENCES); but in a program fenced already
\($TUATARA_FENCE names its fence, which every fence sets, and its state and settings directories are in it), as
the tests are when they run a manual builder, that fence is kept and nothing is changed. Returns the directory as a pathname."
  (let ((fenced (sb-posix:getenv "TUATARA_FENCE")))
    (when (and (null directory) fenced (probe-file fenced)
               ;; and the program is within it still, not one that only inherited the name
               (every (lambda (name)
                        (let ((place (sb-posix:getenv name)))
                          (and place (uiop:subpathp (uiop:ensure-directory-pathname place)
                                                    (uiop:ensure-directory-pathname fenced)))))
                      '("XDG_STATE_HOME" "XDG_CONFIG_HOME")))
      (return-from fence-person-data (uiop:ensure-directory-pathname fenced))))
  (unless directory (remove-stale-fences))
  (let ((root (if directory (uiop:ensure-directory-pathname directory) (make-fence-directory))))
    ;; GnuPG takes HOME/.gnupg for its usual directory and gives its agent the person's own socket in
    ;; /run/user/UID/gnupg/, which the fence does not move: a fenced run would reach their agent there
    (flet ((place (name)
             (or (cdr (assoc name places :test #'string=)) (cdr (assoc name *fenced-variables* :test #'string=)))))
      (let ((home (place "HOME")) (gnupg (place "GNUPGHOME")))
        (when (and (stringp home) (stringp gnupg)
                   (uiop:subpathp (fenced-directory gnupg root) (fenced-directory home root)))
          (error "GNUPGHOME may not be inside the fenced home: as its .gnupg, GnuPG would use the agent ~
                  socket of the person's own keys"))))
    (dolist (entry *fenced-variables*)
      (let ((place (or (cdr (assoc (car entry) places :test #'string=)) (cdr entry))))
        (unless (eq place :keep)
          (let ((where (ensure-directories-exist (merge-pathnames place root))))
            (when (member (car entry) *private-variables* :test #'string=)
              (sb-posix:chmod (namestring where) #o700))
            (when (string= (car entry) "TMPDIR") (setf uiop:*temporary-directory* where))
            (sb-posix:setenv (car entry) (string-right-trim "/" (namestring where)) 1)))))
    (dolist (name *closed-variables*)
      (unless (eq :keep (cdr (assoc name places :test #'string=)))
        (sb-posix:unsetenv name)))
    (unless (eq :keep (cdr (assoc "KRB5CCNAME" places :test #'string=)))
      (sb-posix:setenv "KRB5CCNAME" (format nil "FILE:~a" (namestring (merge-pathnames "krb5cc" root))) 1))
    (unless (eq :keep (cdr (assoc "GIT_CONFIG_GLOBAL" places :test #'string=)))
      (let ((settings (merge-pathnames "gitconfig" (ensure-directories-exist root))))
        (with-open-file (out settings :direction :output :if-exists :supersede)
          (write-string *git-settings* out))
        (sb-posix:setenv "GIT_CONFIG_GLOBAL" (namestring settings) 1)
        (sb-posix:setenv "GIT_CONFIG_NOSYSTEM" "1" 1)))
    (let ((file (and (find-package "TUATARA.DEFAULTS") (find-symbol "*CONFIG-FILE*" "TUATARA.DEFAULTS")))
          (forget (and (find-package "TUATARA.DEFAULTS") (find-symbol "FORGET-STATE-PATHS" "TUATARA.DEFAULTS"))))
      (when file (setf (symbol-value file) (merge-pathnames "no-init-file.lisp" root)))
      ;; an editor loaded already finds its state files in the fence from now on
      (when (and forget (fboundp forget)) (funcall forget)))
    (sb-posix:setenv "TUATARA_FENCE" (namestring root) 1)
    (unless directory
      (push (lambda () (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore))
            sb-ext:*exit-hooks*))
    root))
