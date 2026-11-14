;;;; lisp/files/trust.lisp --- Projects you trust to run their own programs, and those only looked at.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; A project's language servers run its code (a build script, a plugin, a
;;;; macro), git runs the programs a repository names for itself (hooks,
;;;; filters), and a build (M-x compile) runs its own code. None happens by
;;;; itself for a project you have not said you trust: once asked and kept
;;;; between sessions, or in *TRUSTED-PROJECTS*. One question, the same
;;;; words whatever asks, says all a yes allows, and offers a third answer,
;;;; look only: git reads the project, nothing of it runs, and it is not
;;;; asked again. A project that is, or holds, an evidence directory is looked
;;;; at only, never asked of. A project is known by where it really is,
;;;; every link followed, so that a link from a trusted directory to another
;;;; lends it no trust.

(in-package #:tuatara.defaults)

(define-setting *trusted-projects* '()
  "Directories whose projects you trust to run their own programs.
Their language servers start by themselves as their files are opened, git runs
the programs they name for themselves (hooks, filters), and \\[compile] runs
their build commands without asking. These are besides the directories trusted when
asked (kept between sessions).
- nil: none, as standard.
- a list of directories' names, ~ for your home directory."
  :values (list-of directory))

(defvar *trusted-kept* 500
  "How many projects trusted when asked are remembered, and how many looked
at only.")

(define-state-list *trusted* "trusted" 'parse-absolute-path-line 'escape-path '*trusted-kept* :trust t
  :line-key 'parse-absolute-path-line :formerly "lsp-trusted")

(define-state-list *look-only* "look-only" 'parse-absolute-path-line 'escape-path '*trusted-kept*
  :header (format nil "# Projects looked at only, never trusted to run their own programs: git~%~
                       # reads them and runs nothing they name. One directory a line, where it~%~
                       # really is (\\\\, \\n and \\r stand for \\, a newline and a return).~%")
  ;; not a list of trust: a later line naming a directory, whatever fields follow, is read, and keeps it looked at only
  :line-key 'parse-absolute-path-line)

(defvar *trust-changed-functions* '()
  "Functions of a project's directory, called when whether it is trusted, or
looked at only, is answered anew (\\[trust-project]): what shows it says so
again.")

(defun real-directory (path)
  "Where the directory PATH really is, every link followed and . and .. taken out,
ending in /; NIL when it is not there."
  (let ((real (ignoring-errors-reported ("finding where ~a is" path)
                (tuatara.io:real-path (tuatara.io:native (tuatara.io:expand-path path))))))
    (and real (tuatara.io:slash-ended real))))

(defvar *trusted-directories-seen* '()
  "Where the directories of *TRUSTED-PROJECTS* really are, as the setting was
last looked at: (DIRECTORIES . REAL), DIRECTORIES the setting's directories as written and
REAL where each really is, NIL for one not there then.")

(defun trusted-setting-directories ()
  "Where the directories of *TRUSTED-PROJECTS* really are, those there: each
looked up once while the setting stays the same, one not there looked for
again each time."
  (let ((directories (setting-list '*trusted-projects* *trusted-projects*))
        (seen *trusted-directories-seen*))
    (unless (equal directories (car seen))
      (setf seen (cons (copy-list directories) (make-list (length directories)))
            *trusted-directories-seen* seen))
    (loop for directory in directories
          for cell on (cdr seen)
          for real = (or (car cell) (setf (car cell) (real-directory directory)))
          when real collect real)))

(defun project-trusted-p (root)
  "True when the project at ROOT may run its own programs. That holds when it,
where it really is, was trusted when asked (kept by where it really was then),
or is in one of *TRUSTED-PROJECTS* (where they really are). It never
holds for one that is, is in or holds an evidence directory
\(DIRECTORY-HOLDS-EVIDENCE-P), whose programs could write there, nor for one
within a directory answered look only (PROJECT-LOOK-ONLY-P). The answer that runs
less wins. While the project is still being looked through for evidence
directories it is not trusted, with :PENDING as a second value: the answer
is to be asked again."
  (let ((real (real-directory root)))
    (and real
         (or (member real (state-list-entries *trusted*) :test #'equal)
             (some (lambda (trusted) (tuatara.io:path-within-p real trusted))
                   (trusted-setting-directories)))
         (not (real-directory-look-only-p real))
         ;; asked only of a project trusted, and told from the setting, not by reading the project
         (not (evidence-path-p real))
         (multiple-value-bind (holds looking) (directory-holds-evidence-p real)
           (if looking (values nil :pending) (not holds))))))

(defun trusted-project-key (file)
  "What an answer of BUFFER-IN-TRUSTED-PROJECT-P about FILE depends on, as it
stands now: a vector of FILE, the generations of the projects trusted and of
those looked at only (STATE-LIST-GENERATION), *TRUSTED-PROJECTS* and the
generation of *EVIDENCE-DIRECTORIES*."
  (vector file (state-list-generation *trusted*) (state-list-generation *look-only*) *trusted-projects*
          (evidence-generation)))

(defun trusted-project-key-current-p (key file)
  "True when KEY, made by TRUSTED-PROJECT-KEY, is what it would be now for
FILE, which allocates nothing."
  (and (equal (svref key 0) file)
       (eql (svref key 1) (state-list-generation *trusted*))
       (eql (svref key 2) (state-list-generation *look-only*))
       (equal (svref key 3) *trusted-projects*)
       (eql (svref key 4) (evidence-generation))))

(defun buffer-in-trusted-project-p (buffer)
  "True when BUFFER visits a file of a project you trust (PROJECT-TRUSTED-P):
its text is then parsed by the program shared by such files, not one of its
own. Looked up again only when the file, the projects trusted or looked at
only when asked, *TRUSTED-PROJECTS* or *EVIDENCE-DIRECTORIES* change, or
while the project is still being looked through for evidence directories."
  (let* ((file (tuatara.buffer:buffer-file buffer))
         (seen (tuatara.buffer:buffer-local buffer :trusted-project-seen)))
    (if (and seen (trusted-project-key-current-p (car seen) file))
        (cdr seen)
        ;; where the file really is, every link followed: a link in a trusted
        ;; project to a file elsewhere lends it no trust
        (let* ((real (and file (ignoring-errors-reported ("finding where ~a is" file)
                                 (tuatara.io:real-path (tuatara.io:native (tuatara.io:expand-path file))))))
               (root (and real (project-root (tuatara.io:parent-path real)))))
          (multiple-value-bind (answer pending) (and root (project-trusted-p root))
            (unless pending
              (setf (tuatara.buffer:buffer-local buffer :trusted-project-seen)
                    (cons (trusted-project-key file) (and answer t))))
            (and answer t))))))

(defmethod tuatara.syntax:host-trusted-text-p ((host defaults-host) buffer)
  "True when BUFFER visits a file of a project you trust (BUFFER-IN-TRUSTED-PROJECT-P),
or a copy of a file on a trusted server (\\[server-list])."
  (or (buffer-in-trusted-project-p buffer) (remote-text-trusted-p buffer)))

(defun project-look-only-p (root)
  "True when the project at ROOT, where it really is, is or is within a
directory answered look only: git reads it and runs nothing of it, and
nothing else of it runs, whatever trusts it or a directory holding it."
  (let ((real (real-directory root)))
    (and real (real-directory-look-only-p real))))

(defun real-directory-look-only-p (real)
  "True when REAL, a directory where it really is, is or is within a directory
answered look only."
  (and (some (lambda (directory) (tuatara.io:path-within-p real directory))
             (state-list-entries *look-only*))
       t))

(defun project-evidence-words (root)
  "Why the project at ROOT can never be trusted, a phrase, or NIL: it is in
an evidence directory (EVIDENCE-PATH-P), or is or holds one
\(DIRECTORY-HOLDS-EVIDENCE-P), whose programs could write there. :PENDING as
a second value while it is still being looked through for one."
  (multiple-value-bind (in entry) (evidence-path-p root)
    (if in
        (evidence-words in entry)
        (multiple-value-bind (words looking) (directory-holds-evidence-p root)
          (values words (and looking :pending))))))

(defun project-look-only-reason (root)
  "Why the project at ROOT is only looked at, never run, a phrase; NIL when
it is trusted, or not yet answered: it is, is in or holds an evidence directory
\(PROJECT-EVIDENCE-WORDS), which no trust lifts; or it was answered look
only (PROJECT-LOOK-ONLY-P). :PENDING as a second value while it is still
being looked through for evidence directories: the answer is to be asked again."
  (multiple-value-bind (words pending) (project-evidence-words root)
    (cond (words (values words pending))
          ((project-look-only-p root) "it is not trusted"))))

(defun trust-question (real)
  "The question asked of the project at REAL, the same words whatever asks:
all a yes allows, what look only does, and that no leaves it to be asked
again."
  (format nil "Trust ~a? y: its language servers start by themselves and run its code, git runs the ~
               programs it names (hooks, filters), and its build commands run (M-x compile); l: look only, ~
               git reads it and nothing of it runs; n: not now (y, l or n) "
          (abbreviated-path real)))

(defun ask-project-trust (root &key again)
  "Whether the project at ROOT may run its own programs: :TRUSTED,
:LOOK-ONLY, or NIL when not, and as a second value why it can never be
trusted when it is evidence (PROJECT-EVIDENCE-WORDS). It is either trusted
already, answered look only already, or asked now (with AGAIN, asked whatever
was answered before). The keys are these: y, then yes typed (trusting cannot
be taken back once its programs ran), trusts it, unless a directory holding it is
looked at only. l keeps it to looking only, each remembered. n leaves it to be
asked again, and with AGAIN forgets what was answered. The question
\(TRUST-QUESTION) says all a yes allows, whatever asks it, and only a key
pressed once it is shown answers it. A project that is, is in or holds an
evidence directory, or is within a directory answered look only, is looked at only,
never asked of (asked again with AGAIN, it stays so)."
  (let ((real (real-directory root))
        (evidence (project-evidence-words root)))
    (cond (evidence (values :look-only evidence))
          ((and (not again) (project-trusted-p root)) :trusted)
          ((and (not again) (project-look-only-p root)) :look-only)
          ((null real) nil)
          (t (case (read-answer-key (trust-question real) '(#\y #\l #\n) :fresh t)
               (#\y (when (yes-or-no-p* (format nil "Trust ~a to run its own programs?" (abbreviated-path real)))
                      (state-list-drop *look-only* (list real))
                      (state-list-put *trusted* (list real))
                      (if (project-look-only-p real) :look-only :trusted)))
               (#\l (state-list-drop *trusted* (list real))
                (state-list-put *look-only* (list real))
                :look-only)
               (t (when again
                    (state-list-drop *trusted* (list real))
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
repository its file or directory is in, else its project, or the repository a
git buffer shows), whatever was answered before. The key y trusts it to run
its own programs (its language servers, the programs git runs for it, its
build commands). The key l keeps it to looking only, git reading it and
running nothing of it. The key n forgets the answer, to be asked again. A
project in or holding an evidence directory cannot be trusted, and a directory of
*TRUSTED-PROJECTS* stays trusted whatever is answered."
  (let* ((buffer (current-buffer))
         (root (or (tuatara.buffer:buffer-local buffer :git-top)
                   (let ((directory (buffer-directory buffer)))
                     (and directory (or (ignoring-errors-reported ("finding the repository of ~a" directory)
                                           (tuatara.git:repository-top directory))
                                         (project-root directory) directory))))))
    (if (null root)
        (message "~a is in no project" (tuatara.buffer:buffer-name buffer))
        (multiple-value-bind (answer evidence) (ask-project-trust root :again t)
          (run-hook *trust-changed-functions* root)
          (message "~a ~a" (abbreviated-path root)
                   (cond (evidence (format nil "cannot be trusted: ~a" evidence))
                         ((eq answer :trusted) "is trusted")
                         ((project-trusted-p root) "is trusted all the same: it is in *trusted-projects*")
                         ((and (eq answer :look-only)
                               (not (member (real-directory root) (state-list-entries *look-only*) :test #'equal)))
                          "is looked at only all the same: a directory holding it is")
                         ((eq answer :look-only) "is looked at only")
                         (t "will be asked about again")))))))
