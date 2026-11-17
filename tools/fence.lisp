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
    "HYPRLAND_INSTANCE_SIGNATURE" "TMUX" "KITTY_LISTEN_ON" "WEZTERM_UNIX_SOCKET" "GNOME_KEYRING_CONTROL")
  "Each environment variable that leads a program to a person's running agents and servers outside their runtime
directory (their ssh and GnuPG agents, session bus, display, sound, window manager, terminal multiplexer and
keyrings), unset in a fence. Their Kerberos tickets are fenced apart: KRB5CCNAME names a file in the fence, as
Kerberos would otherwise look in their keyring. A test that needs a display starts its own and names it.")

(defparameter *private-variables* '("GNUPGHOME" "XDG_RUNTIME_DIR")
  "The variables of *FENCED-VARIABLES* whose directory is for its owner alone, as the system's are.")

(defun fence-name-p (name)
  "True when NAME, a directory's, is that of a fence FENCE-PERSON-DATA made in the temporary directory, as
tuatara-fence-PID."
  (and (uiop:string-prefix-p "tuatara-fence-" name)
       (every #'digit-char-p (subseq name (length "tuatara-fence-")))
       (> (length name) (length "tuatara-fence-"))))

(defun remove-stale-fences ()
  "Remove the fences in the temporary directory whose program has ended without removing its own (killed, or
ended at once), so that none is left whatever way a run ended."
  (dolist (directory (uiop:subdirectories (uiop:temporary-directory)))
    (let ((name (car (last (pathname-directory directory)))))
      (when (and (stringp name) (fence-name-p name)
                 (handler-case (progn (sb-posix:kill (parse-integer name :start (length "tuatara-fence-")) 0) nil)
                   (sb-posix:syscall-error (e) (= (sb-posix:syscall-errno e) sb-posix:esrch))))
        (uiop:delete-directory-tree directory :validate t :if-does-not-exist :ignore)))))

(defun fence-person-data (directory &key places)
  "Point every variable of *FENCED-VARIABLES* at a directory of its own under DIRECTORY, made if missing (GNUPGHOME's
private to its owner), unset those of *CLOSED-VARIABLES*, point KRB5CCNAME at a file in the fence and the
editor's init file at a file that does not exist, so that what runs after reads nothing of the person's, writes
nothing there and reaches none of their agents; an editor loaded already looks for its state files anew.
PLACES is a list of (VARIABLE . PLACE) overriding a variable's directory, or leaving it (a closed one too) as it is
when PLACE is :KEEP. When DIRECTORY is NIL the fence is a new directory in the temporary one, removed when Lisp
exits, and those other runs left behind removed first (REMOVE-STALE-FENCES); but in a program fenced already
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
  (let ((root (uiop:ensure-directory-pathname
               (or directory (merge-pathnames (format nil "tuatara-fence-~d/" (sb-posix:getpid))
                                              (uiop:temporary-directory))))))
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
