;;;; lisp/files/trust.lisp --- Projects you trust to run their own programs, and those only looked at.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; A project's language servers run its code (a build script, a plugin, a
;;;; macro), git runs the programs a repository names for itself (hooks,
;;;; filters), and a build (M-x compile) runs its own code. None happens by
;;;; itself for a project you have not said you trust: once asked and kept
;;;; between sessions, or in *LSP-TRUSTED-PROJECTS*. One question, the same
;;;; words whatever asks, says all a yes allows, and offers a third answer,
;;;; look only: git reads the project, nothing of it runs, and it is not
;;;; asked again. A project that is, or holds, an evidence folder is looked
;;;; at only, never asked of. A project is known by where it really is,
;;;; every link followed, so that a link from a trusted folder to another
;;;; lends it no trust.

(in-package #:tuatara.defaults)

(defvar *lsp-trusted-projects* '()
  "Folders whose projects you trust to run their own programs: their language
servers start by themselves as their files are opened, git runs the
programs they name for themselves (hooks, filters), and \\[compile] runs their
build commands without asking. Besides those trusted when
asked (kept between sessions). Names of folders, ~ for your home folder.")

(defvar *lsp-trusted-kept* 500
  "How many projects trusted when asked are remembered, and how many looked
at only.")

(define-state-list *lsp-trusted* "lsp-trusted" 'parse-absolute-path-line 'escape-path '*lsp-trusted-kept* :trust t
  :line-key 'parse-absolute-path-line)

(defun parse-look-only-line (line)
  "The entry a line of the list of projects looked at only makes: the path a
line beginning with / names (UNESCAPE-PATH); NIL for a line of its heading
\(beginning with #); any other line kept as it is, (:KEPT LINE), written
back unchanged and naming no project, for a later version may write such
lines."
  (cond ((uiop:string-prefix-p "#" line) nil)
        ((uiop:string-prefix-p "/" line) (unescape-path line))
        (t (list :kept line))))

(defun look-only-line (entry)
  "The line of the list of projects looked at only for ENTRY: a path
\(ESCAPE-PATH), or a line kept as it was read."
  (if (stringp entry) (escape-path entry) (second entry)))

(define-state-list *look-only* "look-only" 'parse-look-only-line 'look-only-line '*lsp-trusted-kept*
  :header (format nil "# tuatara look-only 1~%~
                       # Projects looked at only, never trusted to run their own programs: git~%~
                       # reads them and runs nothing they name. One folder a line, where it~%~
                       # really is (\\\\, \\n and \\r stand for \\, a newline and a return).~%"))

(defvar *trust-changed-functions* '()
  "Functions of a project's folder, called when whether it is trusted, or
looked at only, is answered anew (\\[trust-project]): what shows it says so
again.")

(defun real-folder (path)
  "Where the folder PATH really is, every link followed and . and .. taken out,
ending in /; NIL when it is not there."
  (let ((real (ignore-errors (tuatara.io:real-path (tuatara.io:native (tuatara.io:expand-path path))))))
    (and real (tuatara.io:slash-ended real))))

(defvar *trusted-folders-seen* '()
  "Where the folders of *LSP-TRUSTED-PROJECTS* really are, as the setting was
last looked at: (FOLDERS . REAL), FOLDERS the setting's folders as written and
REAL where each really is, NIL for one not there then.")

(defun trusted-setting-folders ()
  "Where the folders of *LSP-TRUSTED-PROJECTS* really are, those there: each
looked up once while the setting stays the same, one not there looked for
again each time."
  (let ((folders (setting-list '*lsp-trusted-projects* *lsp-trusted-projects*))
        (seen *trusted-folders-seen*))
    (unless (equal folders (car seen))
      (setf seen (cons (copy-list folders) (make-list (length folders)))
            *trusted-folders-seen* seen))
    (loop for folder in folders
          for cell on (cdr seen)
          for real = (or (car cell) (setf (car cell) (real-folder folder)))
          when real collect real)))

(defun project-trusted-p (root)
  "True when the project at ROOT may run its own programs: it, where it really
is, was trusted when asked (kept by where it really was then), or is in one
of *LSP-TRUSTED-PROJECTS* (where they really are); never one that is, or
holds, an evidence folder (FOLDER-HOLDS-EVIDENCE-P), whose programs could
write there."
  (let ((real (real-folder root)))
    (and real
         (or (member real (state-list-entries *lsp-trusted*) :test #'equal)
             (some (lambda (trusted) (tuatara.io:path-within-p real trusted))
                   (trusted-setting-folders)))
         ;; asked only of a project trusted, and told from the setting, not by reading the project
         (not (folder-holds-evidence-p real)))))

(defun buffer-in-trusted-project-p (buffer)
  "True when BUFFER visits a file of a project you trust (PROJECT-TRUSTED-P):
its text is then parsed by the program shared by such files, not one of its
own. Looked up again only when the file, the projects trusted when asked,
*LSP-TRUSTED-PROJECTS* or *EVIDENCE-FOLDERS* change."
  (let* ((file (tuatara.buffer:buffer-file buffer))
         (key (list file (state-list-entries *lsp-trusted*) *lsp-trusted-projects* (evidence-generation)))
         (seen (tuatara.buffer:buffer-local buffer :trusted-project-seen)))
    (if (and seen (equal (car seen) key))
        (cdr seen)
        ;; where the file really is, every link followed: a link in a trusted
        ;; project to a file elsewhere lends it no trust
        (let* ((real (and file (ignore-errors (tuatara.io:real-path (tuatara.io:native (tuatara.io:expand-path file))))))
               (answer (and real
                            (let ((root (project-root (tuatara.io:parent-path real))))
                              (and root (project-trusted-p root) t)))))
          (setf (tuatara.buffer:buffer-local buffer :trusted-project-seen) (cons key answer))
          answer))))

(defun project-look-only-p (root)
  "True when the project at ROOT, where it really is, was answered look only
and is not trusted (PROJECT-TRUSTED-P): git reads it and runs nothing of
it, and nothing else of it runs."
  (let ((real (real-folder root)))
    (and real
         (member real (state-list-entries *look-only*) :test #'equal)
         (not (project-trusted-p root)))))

(defun project-evidence-words (root)
  "Why the project at ROOT can never be trusted, a phrase, or NIL: it is in
an evidence folder (EVIDENCE-PATH-P), or is or holds one
\(FOLDER-HOLDS-EVIDENCE-P), whose programs could write there."
  (multiple-value-bind (in entry) (evidence-path-p root)
    (if in
        (evidence-words in entry)
        (folder-holds-evidence-p root))))

(defun project-look-only-reason (root)
  "Why the project at ROOT is only looked at, never run, a phrase; NIL when
it is trusted, or not yet answered: it is, is in or holds an evidence folder
\(PROJECT-EVIDENCE-WORDS), which no trust lifts; or it was answered look
only (PROJECT-LOOK-ONLY-P)."
  (or (project-evidence-words root)
      (and (project-look-only-p root) "it is not trusted")))

(defun trust-question (real)
  "The question asked of the project at REAL, the same words whatever asks:
all a yes allows, what look only does, and that no leaves it to be asked
again."
  (format nil "Trust ~a? y: its language servers start by themselves and run its code, git runs the programs it names (hooks, filters), and its build commands run (M-x compile); l: look only, git reads it and nothing of it runs; n: not now (y, l or n) "
          (abbreviated-path real)))

(defun ask-project-trust (root &key again)
  "Whether the project at ROOT may run its own programs: :TRUSTED,
:LOOK-ONLY, or NIL when not, and as a second value why it can never be
trusted when it is evidence (PROJECT-EVIDENCE-WORDS). Trusted already, or
answered look only already, or asked now (with AGAIN, asked whatever was
answered before): y trusts it and l keeps it to looking only, each
remembered; n leaves it to be asked again, and with AGAIN forgets what was
answered. The question (TRUST-QUESTION) says all a yes allows, whatever
asks it, and only a key pressed once it is shown answers it. A project that
is, is in or holds an evidence folder is looked at only, never asked of."
  (let ((real (real-folder root))
        (evidence (project-evidence-words root)))
    (cond (evidence (values :look-only evidence))
          ((and (not again) (project-trusted-p root)) :trusted)
          ((and (not again) (project-look-only-p root)) :look-only)
          ((null real) nil)
          (t (case (read-answer-key (trust-question real) '(#\y #\l #\n) :fresh t)
               (#\y (state-list-drop *look-only* (list real))
                (state-list-put *lsp-trusted* (list real))
                :trusted)
               (#\l (state-list-drop *lsp-trusted* (list real))
                (state-list-put *look-only* (list real))
                :look-only)
               (t (when again
                    (state-list-drop *lsp-trusted* (list real))
                    (state-list-drop *look-only* (list real)))
                  nil))))))

(defun trusted-when-asked-p (root)
  "True when the project at ROOT may run its own programs: trusted already,
or asked now and trusted (ASK-PROJECT-TRUST); why not is said when it is
evidence."
  (multiple-value-bind (answer evidence) (ask-project-trust root)
    (when evidence
      (message "~a is not trusted: ~a" (abbreviated-path root) evidence))
    (eq answer :trusted)))

(define-command trust-project ()
  "Ask again whether you trust the project of the current buffer (the git
repository its file or folder is in, else its project, or the repository a
git buffer shows), whatever was answered
before: y trusts it to run its own programs (its language servers, the
programs git runs for it, its build commands); l keeps it to looking only,
git reading it and running nothing of it; n forgets the answer, to be asked
again. A project in or holding an evidence folder cannot be trusted, and a
folder of *LSP-TRUSTED-PROJECTS* stays trusted whatever is answered."
  (let* ((buffer (current-buffer))
         (root (or (tuatara.buffer:buffer-local buffer 'git-top)
                   (let ((directory (buffer-directory buffer)))
                     (and directory (or (ignore-errors (repository-top directory)) (project-root directory) directory))))))
    (if (null root)
        (message "~a is in no project" (tuatara.buffer:buffer-name buffer))
        (multiple-value-bind (answer evidence) (ask-project-trust root :again t)
          (dolist (function *trust-changed-functions*) (funcall function root))
          (message "~a ~a" (abbreviated-path root)
                   (cond (evidence (format nil "cannot be trusted: ~a" evidence))
                         ((eq answer :trusted) "is trusted")
                         ((project-trusted-p root) "is trusted all the same: it is in *lsp-trusted-projects*")
                         ((eq answer :look-only) "is looked at only")
                         (t "will be asked about again")))))))
