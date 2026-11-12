;;;; lisp/lsp/tools.lisp --- Which programs serve and format each language, and which of them are here.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; A language server is a program of its own, installed apart from Tuatara,
;;;; that knows a language: what is wrong in a file, what may come next, where
;;;; a name is defined. A formatter lays code out as the language's own tools
;;;; would. Which of them go with which language comes from what Helix knows
;;;; (usual-tools.lisp, made from its languages.toml), with what marks a
;;;; language's project; *LANGUAGE-SERVERS* and *LANGUAGE-FORMATTERS* change
;;;; it for a language. M-x check-health says which are installed.

(in-package #:tuatara.lsp)

(defvar *known-language-servers* (make-hash-table :test 'equal)
  "The language servers known, by name: each a plist of :COMMAND (the program
and its arguments), :OPTIONS (what it is told at its start, JSON text) and
:ENVIRONMENT (NAME=VALUE strings added to the program's).")

(defvar *known-language-tools* (make-hash-table :test 'equalp)
  "What is known of each language's tools, by the language's name: a plist of
:SERVERS (the names of the servers that serve it, in order), :ONLY and
:EXCEPT (each (SERVER . FEATURES): the features of the language a server
alone serves, or leaves to others), :FORMATTER (the program and its
arguments), :ROOTS (the names of files that mark the top of its project) and
:LANGUAGE-ID, the name language servers know it by when it is not its own.")

(defun know-language-servers (servers)
  "Make SERVERS the language servers known: each (NAME COMMAND &key OPTIONS
ENVIRONMENT), COMMAND a list of the program and its arguments."
  (clrhash *known-language-servers*)
  (loop for (name command . more) in servers
        do (setf (gethash name *known-language-servers*) (list* :command command more))))

(defun know-language-tools (languages)
  "Make LANGUAGES what is known of each language's tools: each (NAME &key
SERVERS ONLY EXCEPT FORMATTER ROOTS LANGUAGE-ID), as *KNOWN-LANGUAGE-TOOLS*
keeps them; :ONLY and :EXCEPT may come more than once."
  (clrhash *known-language-tools*)
  (loop for (name . plist) in languages
        do (setf (gethash name *known-language-tools*)
                 (list :servers (getf plist :servers)
                       :only (loop for (key value) on plist by #'cddr when (eq key :only) collect value)
                       :except (loop for (key value) on plist by #'cddr when (eq key :except) collect value)
                       :formatter (getf plist :formatter)
                       :roots (getf plist :roots)
                       :language-id (getf plist :language-id)))))

(define-setting *language-servers* '()
  "The language servers of languages you choose yourself, in place of the
language's usual ones. All those installed run together, each for what it
can do.
- a list of (LANGUAGE SERVER...): LANGUAGE a symbol (python, rust), each
  SERVER a known server's name (\"basedpyright\") or a list of a program and
  its arguments ((\"my-server\" \"--stdio\")).
- (LANGUAGE) alone: no server for that language.
- nil: every language keeps its usual servers, as standard."
  :values (list-of "(LANGUAGE SERVER...)"))

(define-setting *language-formatters* '()
  "The formatters of languages you choose yourself, in place of the language's
usual one. The code is given on its standard input and taken back laid out
from its standard output.
- a list of (LANGUAGE PROGRAM ARGUMENT...): that program, with those
  arguments, formats that language.
- (LANGUAGE) alone: no formatter for that language, the language server or
  Tuatara's own indentation laying it out instead.
- nil: every language keeps its usual formatter, as standard."
  :values (list-of "(LANGUAGE PROGRAM ARGUMENT...)"))

(defun language-key (language)
  "The name of LANGUAGE (a language, or a symbol or string naming one) in
lower case, as its tools are known by: python, rust."
  (string-downcase (string (if (language-p language) (language-name language) language))))

(defun language-tools (language)
  "What is known of LANGUAGE's tools (a language, or a symbol or string naming
one), as *KNOWN-LANGUAGE-TOOLS* keeps it, or NIL."
  (gethash (language-key language) *known-language-tools*))

(defun setting-entry (setting language)
  "The entry of LANGUAGE in SETTING, an alist of a language's symbol and what
is chosen for it, compared by name; NIL when it has none."
  (let ((name (language-key language)))
    (assoc name setting :test (lambda (a b) (and (symbolp b) (string-equal a (symbol-name b)))))))

(define-setting *lsp-languages* t
  "The languages whose language servers are used. A language left out has no
server, its buffers and its source blocks in notes none, \\[lsp] saying so.
- t: every language with a server installed, as standard.
- a list of languages, '(rust go python): those alone.
- :not and languages, '(:not markdown latex): every one but those."
  :values (or (member t) (list-of "a language's name, or :not first")))

(defun lsp-language-on-p (language)
  "True when *LSP-LANGUAGES* lets LANGUAGE's servers be used."
  (let ((setting *lsp-languages*))
    (flet ((named (list) (setting-entry (mapcar #'list list) language)))
      (cond ((not (listp setting)) t)
            ((eq (first setting) :not) (not (named (rest setting))))
            (t (and (named setting) t))))))

(defun language-server-commands (language)
  "The language servers of LANGUAGE, as *LANGUAGE-SERVERS* chooses them, else
the language's usual ones: each a plist of :NAME, :COMMAND (the program and its
arguments), :OPTIONS and :ENVIRONMENT, in order; the names of known servers
not known (none of a program's) left out."
  (let ((chosen (setting-entry *language-servers* language)))
    (loop for server in (if chosen (rest chosen) (getf (language-tools language) :servers))
          for known = (and (stringp server) (gethash server *known-language-servers*))
          when (consp server)
            collect (list :name (first server) :command server)
          when known
            collect (list* :name server known))))

(defparameter *usual-formatters-beside-servers*
  '(("rust" "rustfmt" "--edition" "2021")
    ("python" "ruff" "format" "-")
    ("c" "clang-format")
    ("cpp" "clang-format")
    ("bash" "shfmt"))
  "The usual formatters of the languages whose known tools leave laying code
out to a language server: each (LANGUAGE PROGRAM ARGUMENT...), the code given
on its standard input and taken back laid out from its standard output.")

(defun language-formatter-command (language)
  "The formatter of LANGUAGE, the program and its arguments, as
*LANGUAGE-FORMATTERS* chooses it, else the language's usual one
\(*USUAL-FORMATTERS-BESIDE-SERVERS* for those whose known tools name none);
NIL for none."
  (let ((chosen (setting-entry *language-formatters* language)))
    (if chosen
        (rest chosen)
        (or (getf (language-tools language) :formatter)
            (rest (assoc (language-key language) *usual-formatters-beside-servers* :test #'string=))))))

(defun language-roots (language)
  "The names of the files that mark the top of a project in LANGUAGE (Cargo.toml for Rust)."
  (getf (language-tools language) :roots))

(defparameter *brought-language-servers-folder* ".local/share/tuatara/language-servers/"
  "Where, from the home folder, the language servers brought to this machine
by \\[server-bring-language-server] are kept: a folder of the editor's own,
looked in after the PATH (BROUGHT-LANGUAGE-SERVER), so that nothing of yours
is hidden and the commands ssh runs need no PATH of their own.")

(defun language-server-program-name-p (name)
  "True when NAME can name a language server's program in that folder: one
to 64 ASCII letters, digits, ., _ and -, not starting with . or -."
  (and (stringp name) (<= 1 (length name) 64)
       (not (find (char name 0) ".-"))
       (every (lambda (c) (and (< (char-code c) 128) (or (alphanumericp c) (find c "._-")))) name)))

(defun brought-language-server (name)
  "The full name of the program NAME in *BROUGHT-LANGUAGE-SERVERS-FOLDER*, or
NIL when it is not there, or that folder or the file is a link, another's, or
writable by anyone else."
  (when (language-server-program-name-p name)
    (let* ((folder (tuatara.io:path-join (tuatara.io:home-directory) *brought-language-servers-folder*))
           (path (tuatara.io:path-join folder name)))
      (flet ((yours (path kind)
               (let ((stamp (tuatara.io:path-stamp path :follow nil :failure nil)))
                 (and stamp (eq kind (tuatara.io:stamp-kind stamp)) (= (tuatara.io:stamp-uid stamp) (sb-posix:getuid))
                      (not (logtest (tuatara.io:stamp-mode stamp) #o022))))))
        (and (yours folder :directory) (yours path :regular) (tuatara.io:find-program path))))))

(defun installed-program (command)
  "The program of COMMAND (a list of a program and its arguments) as found on
the PATH, else among the language servers brought to this machine, its full
name, or NIL when it is not installed."
  (and command (or (tuatara.io:find-program (first command)) (brought-language-server (first command)))))

(defun language-server-programs-here ()
  "The language servers known that are installed on this machine, not counting
those brought to it, each (PROGRAM . FULL-NAME), sorted by program, once each."
  (let ((programs '()))
    (maphash (lambda (name spec)
               (declare (ignore name))
               (let ((program (first (getf spec :command))))
                 (when (and (language-server-program-name-p program) (not (assoc program programs :test #'string=)))
                   (let ((path (tuatara.io:find-program program)))
                     (when path (push (cons program path) programs))))))
             *known-language-servers*)
    (sort programs #'string< :key #'car)))

(defun language-tools-words (language)
  "Two values saying what LANGUAGE's tools are here, for \\[check-health]: its
formatter (its program's name, ✓ after one installed, NIL for none), and its
servers likewise, joined by commas, brought after one brought here by
\\[server-bring-language-server]."
  (flet ((word (name command)
           (let ((found (installed-program command)))
             (format nil "~a~:[~; ✓~]~:[~; brought~]" name found
                     (and found (search *brought-language-servers-folder* found))))))
    (let ((formatter (language-formatter-command language)))
      (values (and formatter (word (tuatara.io:native-file-name (first formatter)) formatter))
              (let ((servers (language-server-commands language)))
                (and servers (format nil "~{~a~^, ~}"
                                     (mapcar (lambda (s) (word (getf s :name) (getf s :command))) servers))))))))

(defun language-server-id (language)
  "The name language servers know LANGUAGE by: rust, python, typescriptreact."
  (or (getf (language-tools language) :language-id) (language-key language)))

(defun language-extension (language)
  "The ending a file of LANGUAGE has (py for Python): the shortest its modes
are chosen by, else the name servers know it by."
  (or (first (sort (loop for mode in (language-modes language) append (copy-list (tuatara.modes:mode-file-rules mode)))
                   #'< :key #'length))
      (language-server-id language)))
