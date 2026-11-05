;;;; frontends/tui/desktop.lisp --- The desktop's entries: Tuatara's applications, and the daemon at login.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; Every desktop entry of Tuatara is made here, each kind from its template
;;;; in data/: tuatara.desktop.in, the application; tuatara-client.desktop.in,
;;;; "Tuatara (client)", the application opening the files in a window of the
;;;; daemon, or of an editor of its own when none runs; and
;;;; tuatara-daemon.desktop.in, the daemon started as the desktop starts,
;;;; written by tuatara --generate-autostart. tools/install-desktop.sh,
;;;; tools/distro-packages.sh and tools/appimage.sh have the program write the
;;;; two application entries, asked by its environment (WRITE-APPLICATION-ENTRIES).

(in-package #:tuatara.terminal)

(defparameter *desktop-templates*
  (loop for (kind . file) in '((:application . "tuatara.desktop.in") (:client . "tuatara-client.desktop.in")
                               (:daemon . "tuatara-daemon.desktop.in"))
        collect (cons kind (tuatara.defaults::data-file-text file)))
  "The templates of the desktop entries, by kind: :APPLICATION, :CLIENT and
:DAEMON, each as its file of data/ has it, read when Tuatara is built. In each,
@EXEC@ stands for the program and the words that start it, @NAME@ for =NAME
after the option of a named daemon and @TITLE@ for its name in the title.")

(defparameter *desktop-entry-files*
  '((:application . "tuatara.desktop") (:client . "tuatara-client.desktop") (:daemon . "tuatara-daemon.desktop"))
  "The names of the files of the desktop entries, by kind; a named daemon's
entry is tuatara-daemon-NAME.desktop.")

(defun exec-argument (argument &key plain)
  "ARGUMENT as one word of a desktop entry's Exec line: in double quotes, with
\\, \", ` and $ escaped by a backslash and that backslash and every other
doubled again, as the line is read as a string first, and % written %%, so
that it is no field code; with PLAIN, a word of letters, digits and - _ = .
alone as it is. Refused for a word holding a control character, which no
Exec line can hold."
  (when (some (lambda (char) (< (char-code char) 32)) argument)
    (tuatara.runtime:refuse "~a holds a control character, which a desktop entry cannot start" (shown argument)))
  (if (and plain (plusp (length argument))
           (every (lambda (char) (or (and (< (char-code char) 128) (alphanumericp char)) (find char "-_=."))) argument))
      argument
      (with-output-to-string (out)
        (write-char #\" out)
        (loop for char across argument
              do (case char
                   (#\\ (write-string "\\\\\\\\" out))
                   ((#\" #\` #\$) (write-string "\\\\" out) (write-char char out))
                   (#\% (write-string "%%" out))
                   (t (write-char char out))))
        (write-char #\" out))))

(defun desktop-entry (kind &key (program "tuatara") arguments name)
  "The text of the desktop entry of KIND (*DESKTOP-TEMPLATES*) starting
PROGRAM, tuatara as the PATH finds it or a path, with ARGUMENTS before the
template's own, each word quoted as the Exec line wants it (EXEC-ARGUMENT);
for a daemon, the one NAME names when given."
  (let ((exec (format nil "~a~{ ~a~}" (if (string= program "tuatara") program (exec-argument program))
                      (mapcar (lambda (word) (exec-argument word :plain t)) arguments)))
        (text (cdr (assoc kind *desktop-templates*))))
    (loop for (key . value) in `(("@EXEC@" . ,exec) ("@NAME@" . ,(if name (format nil "=~a" name) ""))
                                 ("@TITLE@" . ,(if name (format nil " (~a)" name) "")))
          do (setf text (cl-ppcre:regex-replace-all key text (list value))))
    text))

(defun write-application-entries (folder program &rest arguments)
  "Write the application entries, Tuatara's and \"Tuatara (client)\", into
FOLDER (made when it is not there), each starting PROGRAM with ARGUMENTS
first (DESKTOP-ENTRY), over the ones there; the paths written. What
tools/install-desktop.sh and the packages' scripts have the program do,
asked by TUATARA_DESKTOP_ENTRIES naming FOLDER in its environment."
  (let ((folder (tuatara.io:slash-ended folder)))
    (tuatara.io:ensure-directories folder)
    (loop for kind in '(:application :client)
          for path = (tuatara.io:path-join folder (cdr (assoc kind *desktop-entry-files*)))
          do (tuatara.io:replace-file path (desktop-entry kind :program program :arguments arguments) :mode #o600)
          collect path)))

(defun desktop-entries-refusal (folder arguments &key (self tuatara.io:*saved-program*))
  "Why the request to write desktop entries into FOLDER for ARGUMENTS, the
program they start and the words before the entries' own, is not taken, in
words, or NIL when it is: FOLDER must be named from the root, the program be
tuatara, or named from the root and be this program SELF (the AppImage's
path when it runs from one) or the launcher tools/tuatara-tui.sh of the
checkout SELF was built in, and every other word an option (--dark), so that
no file named on the command line becomes a program the desktop runs."
  (flet ((real (path) (and path (tuatara.runtime:ignoring-errors-reported ("finding ~a" path)
                                  (tuatara.io:real-path path)))))
    (let* ((program (first arguments))
           (real-program (and program (real program)))
           (launcher (and self (uiop:string-suffix-p self "/build/tuatara")
                          (concatenate 'string (subseq self 0 (- (length self) (length "build/tuatara")))
                                       "tools/tuatara-tui.sh"))))
      (cond ((not (and folder (plusp (length folder)) (char= (char folder 0) #\/)))
             "the folder is not named from the root")
            ((and program (string/= program "tuatara") (not (uiop:string-prefix-p "/" program)))
             (format nil "~a is not named from the root" program))
            ((not (or (equal program "tuatara")
                      (and real-program (member real-program (list (real self) (real launcher)) :test #'equal))))
             (format nil "~a is not this program" (or program "no program")))
            ((notevery (lambda (word) (cl-ppcre:scan "^--?[a-z][a-z0-9-]*$" word)) (rest arguments))
             "a word after the program is not an option")))))

(defun desktop-entries-child (arguments)
  "Write the application entries as the environment asks, TUATARA_DESKTOP_ENTRIES
naming the folder (WRITE-APPLICATION-ENTRIES), ARGUMENTS the program the
entries start and the words before the entries' own; each path written said
on the standard output. The exit code: 0 when written, 1 when not, why said
on the error output. A request not taken (DESKTOP-ENTRIES-REFUSAL) is said on
the error output and NIL returned: the program starts as it would without it."
  (let* ((folder (tuatara.io:own-variable "TUATARA_DESKTOP_ENTRIES"))
         (why (desktop-entries-refusal folder arguments)))
    (sb-posix:unsetenv "TUATARA_DESKTOP_ENTRIES")
    (if why
        (progn (format *error-output* "tuatara: TUATARA_DESKTOP_ENTRIES left alone: ~a~%" (shown why))
               nil)
        (handler-case
            (progn (dolist (path (apply #'write-application-entries folder arguments))
                     (format t "Wrote ~a~%" path))
                   0)
          (error (condition)
            (format *error-output* "tuatara: no desktop entries written: ~a~%"
                    (shown (tuatara.runtime:failure-words condition)))
            1)))))

(defun autostart-program (&key (self tuatara.io:*saved-program*) (found (tuatara.io:find-program "tuatara")))
  "How the daemon's autostart entry names this program, SELF: tuatara when
FOUND, the tuatara the PATH finds, is this program, else SELF's own path, the
AppImage's when it runs from one."
  (flet ((real (path) (and path (tuatara.runtime:ignoring-errors-reported ("finding ~a" path)
                                  (tuatara.io:real-path path)))))
    (if (and found self (equal (real found) (real self)))
        "tuatara"
        (or self "tuatara"))))

(defun autostart-path (name)
  "Where the autostart entry of the daemon NAME is written: tuatara-daemon.desktop,
or tuatara-daemon-NAME.desktop for a daemon of another name than the
default, in the desktop's autostart folder, ~/.config/autostart/ (autostart/
in XDG_CONFIG_HOME when it is set)."
  (namestring (uiop:xdg-config-home (if (string= name tuatara.defaults::*daemon-default-name*)
                                        "autostart/tuatara-daemon.desktop"
                                        (format nil "autostart/tuatara-daemon-~a.desktop" name)))))

(defun generate-autostart (name)
  "Write the autostart entry of the daemon NAME (AUTOSTART-PATH), so that the
desktop starts it with tuatara --daemon as you log in, naming this program as
AUTOSTART-PROGRAM does; never over a file there. Said on the standard output,
or why not on the error output. The exit code: 0 when written, 1 when not."
  (let ((path (autostart-path name))
        (named (and (string/= name tuatara.defaults::*daemon-default-name*) name)))
    (handler-case
        (progn (tuatara.io:ensure-directories (tuatara.io:parent-path path))
               (tuatara.io:write-new-file path (desktop-entry :daemon :program (autostart-program) :name named)
                                          :mode #o600)
               (format t "Wrote ~a: the desktop starts the daemon~@[ ~a~] as you log in~%" path named)
               0)
      (error (condition)
        (if (tuatara.io:path-kind path :follow nil)
            (format *error-output* "tuatara: ~a is there already; move it away to write a new one~%" path)
            (format *error-output* "tuatara: no autostart entry written at ~a: ~a~%"
                    path (shown (tuatara.runtime:failure-words condition))))
        1))))
