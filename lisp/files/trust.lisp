;;;; lisp/files/trust.lisp --- Files from outside, and projects trusted to run their own programs or looked at only.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; A file is untrusted when it is in an evidence directory or in one of
;;;; *UNTRUSTED-DIRECTORIES* (your downloads, as standard): it opens safely,
;;;; the desktop's programs get a copy of it after asking, its export takes
;;;; files from its own directory only, and its project runs nothing of its
;;;; own until trusted when asked. UNTRUSTED-PATH-P alone decides it. What
;;;; Tuatara copies, moves or unpacks from an untrusted file is marked as come
;;;; from outside (MARK-FROM-OUTSIDE), and stays untrusted wherever it goes.
;;;;
;;;; A project's language servers run its code (a build script, a plugin, a
;;;; macro), git runs the programs a repository names for itself (hooks,
;;;; filters), and a build (M-x compile) runs its own code. That happens by
;;;; itself for a project that is trusted (UNTRUSTED-PATH-P), and for an
;;;; untrusted one you trusted when asked, kept between sessions. One
;;;; question, the same words whatever asks, says all a yes allows, and
;;;; offers a third answer,
;;;; look only: git reads the project, nothing of it runs, and it is not
;;;; asked again. A project that is, or holds, an evidence directory is looked
;;;; at only, never asked of. A project is known by where it really is,
;;;; every link followed, so that a link from a trusted directory to another
;;;; lends it no trust.

(in-package #:tuatara.defaults)

;;; Files from outside

(define-setting *untrusted-directories* '(:downloads :removable-media :temporary)
  "The directories whose files came from outside and are not trusted, besides
the evidence directories (*EVIDENCE-DIRECTORIES*), whose files never are, and
the copies of outside files Tuatara keeps in its cache (*UNTRUSTED-CACHE*). A
file marked as come from outside (the extended attribute user.xdg.origin.url,
which web browsers leave on what they download, and Tuatara on what it
copies, moves or unpacks from an untrusted file, or from another machine not
trusted), or in a directory so marked, is untrusted wherever it is, and so is
a project holding one at its top. A file is in a directory
when where it really is, links followed, is that directory or below it. An
untrusted file:
  opens safely however it is opened, as \\[find-file-safely] opens a file:
  read-only, with no major mode, hooks or version control, nothing written
  beside it (\\[end-safe-visit] opens one as usual after all); a PDF, a
  picture, an Office document or a mailbox is still shown as such, read in
  the sandbox (*SANDBOXED-FILE-VIEWS*), unless it is evidence;
  is opened in the desktop's program (\\[open-externally]) only after asking,
  from a copy (*OPEN-EXTERNALLY-KINDS*), or itself, answered e;
  as a note, has its export take setup files, pictures and templates only from
  its own directory and the directories in it, and your templates' directories;
  in a project, runs none of the project's own programs (language servers,
  git's hooks and filters, build commands) until it is trusted when asked
  \(\\[trust-project]).
Every other file is trusted: it opens as usual, is opened in the desktop's
program where it is, its export takes files from anywhere, and its project
runs its own programs by itself. Clone or download what others made into one
of these directories.
- (:downloads :removable-media :temporary), as standard: your download
  directory (the one XDG_DOWNLOAD_DIR names in user-dirs.dirs, else
  ~/Downloads/), the places drives and disk images are mounted (/run/media/,
  /media/, /mnt/), and the temporary directories (/tmp/, /var/tmp/), where
  browsers and mail programs put what they open.
- a list of directories, ~ standing for your home directory, and those
  keywords: '(:downloads :removable-media :temporary \"~/Clones/\").
- '(\"/\"): every file untrusted.
- nil: only the files in evidence directories, the cache's copies and those
  marked as come from outside untrusted."
  :values (list-of (or (member :downloads :removable-media :temporary) directory)))

(defparameter *untrusted-places*
  '((:removable-media "/run/media/" "/media/" "/mnt/") (:temporary "/tmp/" "/var/tmp/"))
  "The directories each keyword of *UNTRUSTED-DIRECTORIES* but :downloads stands for.")

(defparameter *untrusted-cache* '("opened/" "remote/" "sandbox-copies/")
  "The directories of Tuatara's cache that hold copies of files from outside:
those opened in the desktop's programs, read from other machines, and given
to programs shut in. Their files are always untrusted (UNTRUSTED-PATH-P), as
are those of the kinds of entry of the cache defined untrusted
\(DEFINE-CACHE-KIND).")

(defun untrusted-directory-names ()
  "The directories of *UNTRUSTED-DIRECTORIES* as native names, ~ expanded and
each keyword standing for its directories (*UNTRUSTED-PLACES*, :downloads for
TUATARA.IO:DOWNLOAD-DIRECTORY), as written, links not followed."
  (loop for directory in *untrusted-directories*
        append (cond ((eq directory :downloads) (list (tuatara.io:download-directory)))
                     ((keywordp directory) (rest (assoc directory *untrusted-places*)))
                     (t (list (tuatara.io:native (tuatara.io:expand-path directory)))))))

(defun untrusted-directory-of (path)
  "The directory of *UNTRUSTED-DIRECTORIES* that the file or directory at PATH
is, or is in or below, by the names as given, by the file a link leads to, or
by where their links lead (PATH-WITHIN-P, REALLY-WITHIN-P), as a native name, or
NIL; T when *UNTRUSTED-DIRECTORIES* cannot be read, every file then counting as
in one. Where a link leads is looked for only when there are directories, and
never for evidence (EVIDENCE-PATH-P, its last link followed with care), whose
links are not read."
  (handler-case
      (reporting-errors ("looking for ~a in the untrusted directories" path)
        (let ((directories (untrusted-directory-names)))
          (when directories
            (let* ((path (or (ignoring-errors-reported ("expanding ~a" path)
                               (tuatara.io:native (tuatara.io:expand-path path)))
                             path))
                   (follow (not (evidence-path-p path :follow t)))
                   (leads-to (or (and follow
                                      (ignoring-errors-reported ("following the links of ~a" path)
                                        (tuatara.io:real-path-so-far path)))
                                 path)))
              (find-if (lambda (directory)
                         (or (tuatara.io:path-within-p path directory)
                             (and follow
                                  (or (tuatara.io:path-within-p leads-to directory)
                                      (tuatara.io:really-within-p path directory)))))
                       directories)))))
    (error () t)))

(defun untrusted-cache-p (path)
  "True when the file at PATH, links followed, is in one of the directories of
Tuatara's cache that hold copies of files from outside (*UNTRUSTED-CACHE*) or
in those of a kind of entry defined untrusted (DEFINE-CACHE-KIND), told by
its name as given before any link is read."
  (let ((directories (let ((cache (tuatara.io:native (tuatara.io:cache-directory))))
                       (mapcar (lambda (name) (tuatara.io:path-join cache name))
                               (append *untrusted-cache* (untrusted-cache-kind-directories))))))
    ;; by the names as given first: the links on the way are read only when those do not tell
    (or (some (lambda (directory) (tuatara.io:path-within-p path directory)) directories)
        (some (lambda (directory) (tuatara.io:really-within-p path directory)) directories))))

(defparameter *origin-mark* "user.xdg.origin.url"
  "The extended attribute that marks a file or directory as come from outside:
web browsers leave it on what they download, and Tuatara on what it makes
from an untrusted file (MARK-FROM-OUTSIDE). Its value says where it came from.")

(define-state-list *from-outside* "from-outside" 'parse-absolute-path-line 'escape-path nil :class :kept
  :header (format nil "# Files and directories made from untrusted ones where no mark could be~%~
                       # written on them: they, and all they hold, stay untrusted. One a line,~%~
                       # where it really is (\\\\, \\n and \\r stand for \\, a newline and a return).~%")
  :line-key 'parse-absolute-path-line)

(defvar *marks-made* (list 0)
  "How many times something was marked as come from outside this session
\(MARK-FROM-OUTSIDE), in a cons: what keeps an answer of whether a project is
trusted asks again once it changes.")

(defun origin-marked-place (path)
  "The file or directory at PATH, or the directory holding it nearest to it,
that is marked as come from outside, as named from PATH: it carries
*ORIGIN-MARK*, or it, or where it really is, was made from an untrusted file
where no mark could be written (*FROM-OUTSIDE*). NIL when none is. Each mark
is read from what a name leads to, links followed by the system, so that
no link is read here unless something is in *FROM-OUTSIDE*; reading the marks
changes no time of a file."
  (let ((path (string-right-trim "/" path))
        (kept (state-list-entries *from-outside*)))
    (flet ((kept-p (place)
             (let ((real (ignoring-errors-reported ("finding where ~a is" place)
                           (tuatara.io:real-path-so-far place))))
               (some (lambda (entry)
                       (or (tuatara.io:path-within-p (tuatara.io:slash-ended place) entry)
                           (and real (tuatara.io:path-within-p (tuatara.io:slash-ended real) entry))))
                     kept))))
      (or (and kept (kept-p (if (string= path "") "/" path)) path)
          (loop for place = path then (string-right-trim "/" (tuatara.io:parent-path place))
                for named = (if (string= place "") "/" place)
                when (ignoring-errors-reported ("reading the marks of ~a" named)
                       (tuatara.io:path-xattr-p named *origin-mark* :follow t))
                  return named
                until (string= place ""))))))

(defun origin-marked-entry (directory)
  "The first thing at the top of DIRECTORY, by name, that is marked as come
from outside (*ORIGIN-MARK*, links not followed), or NIL: a project unpacked or
copied, piece by piece, into a directory of your own."
  (loop for (name) in (ignoring-errors-reported ("listing ~a" directory) (tuatara.io:directory-entries directory))
        for path = (tuatara.io:path-join directory name)
        when (ignoring-errors-reported ("reading the marks of ~a" path)
               (tuatara.io:path-xattr-p path *origin-mark*))
          return path))

(defun source-untrusted-p (from)
  "True when what is made from FROM, a file or directory here or on another
machine (/ssh:HOST:PATH), is untrusted: FROM is untrusted here
\(UNTRUSTED-PATH-P), or is on a machine not trusted (\\[server-list]) or in
evidence there."
  (if (remote-name-p from)
      (multiple-value-bind (host path) (remote-name-parts from)
        (or (not (remote-trusted-p host))
            (and (remote-evidence-words host path) t)))
      (untrusted-path-p from)))

(defun mark-from-outside (path origin &key evidence)
  "Mark PATH, a file or directory here just made from ORIGIN, an untrusted file
or directory here or on another machine, as come from outside: it carries
*ORIGIN-MARK* with ORIGIN as a file: address (as written for another
machine's), so that it, all it holds and every copy of it Tuatara makes stay
untrusted (UNTRUSTED-PATH-P) wherever they are moved. PATH is remembered in
*FROM-OUTSIDE* in place of the mark, every one kept, where the file system
keeps no extended attributes, where PATH is a link, and where PATH is evidence
or may be another name of it (EVIDENCE-VISIT-REASON, EVIDENCE-INODE-P), or
EVIDENCE is true (PATH is the evidence file ORIGIN itself, moved): evidence is
never written on, its times left as they are. Nothing is done for PATH on
another machine or not there."
  (unless (or (remote-name-p path) (null (tuatara.io:path-kind path :follow nil)))
    (let* ((path (string-right-trim "/" (tuatara.io:native path)))
           (stamp (tuatara.io:path-stamp path :follow nil)))
      (unless (and (not evidence)
                   (not (evidence-visit-reason path))
                   (not (and stamp (evidence-inode-p stamp)))
                   (ignoring-errors-reported ("marking ~a as come from outside" path)
                     (tuatara.io:set-path-xattr path *origin-mark*
                                                (if (remote-name-p origin) origin (format nil "file://~a" origin)))))
        (state-list-put *from-outside* (list (tuatara.io:slash-ended
                                              (or (ignoring-errors-reported ("finding where ~a is" path)
                                                    (tuatara.io:real-path-so-far path))
                                                  path)))))
      (sb-ext:atomic-incf (car *marks-made*)))))

(defmacro keeping-untrusted ((from to) &body body)
  "Run BODY, which makes TO, a file or directory, from FROM, by a copy, a move
or another way; when FROM was untrusted before it ran (SOURCE-UNTRUSTED-P), TO
is marked as come from outside (MARK-FROM-OUTSIDE) once BODY is over, whether
it ended or failed, when TO is there. When FROM was evidence and TO is the
same file (a hard link, a move within one drive), nothing is written on it:
it is remembered instead. BODY's values."
  (let ((origin (gensym "ORIGIN")) (made (gensym "MADE")) (evidence (gensym "EVIDENCE")))
    `(let* ((,origin (and (source-untrusted-p ,from) ,from))
            (,made ,to)
            (,evidence (and ,origin (not (remote-name-p ,origin)) (evidence-visit-reason ,origin)
                            (tuatara.io:path-stamp ,origin :follow nil))))
       (unwind-protect (progn ,@body)
         (when ,origin
           (mark-from-outside ,made ,origin
                              :evidence (and ,evidence (tuatara.io:same-stamped-file-p
                                                        ,evidence (tuatara.io:path-stamp ,made :follow nil)))))))))

(defun untrusted-reason (path &optional stamp)
  "Why the file or directory at PATH came from outside and is not trusted, a
phrase, or NIL when it is trusted: it is evidence (EVIDENCE-VISIT-REASON,
links followed), or, given its STAMP, has another name that may be in an
evidence directory (EVIDENCE-INODE-P); it is in one of *UNTRUSTED-DIRECTORIES*
\(UNTRUSTED-DIRECTORY-OF), in the cache's copies of outside files
\(UNTRUSTED-CACHE-P), or it or a directory holding it is marked as come from
outside (ORIGIN-MARKED-PLACE). When *UNTRUSTED-DIRECTORIES* cannot be read,
every file is untrusted, and the reason says so."
  (let* ((path (or (ignoring-errors-reported ("expanding ~a" path) (tuatara.io:native (tuatara.io:expand-path path)))
                   path))
         (directory nil)
         (marked nil)
         (local (not (remote-name-p path))))
    ;; evidence first: what follows reads the links on the way, which would change their access times
    (cond ((evidence-visit-reason path))
          ((and stamp (evidence-inode-p stamp))
           "it has more than one name, and one may be in an evidence directory (*evidence-directories*)")
          ((eq (setf directory (untrusted-directory-of path)) t) "*untrusted-directories* could not be read")
          (directory (format nil "it is in ~a (*untrusted-directories*)" directory))
          ((and local (untrusted-cache-p path)) "it is a copy of a file from outside, in Tuatara's cache")
          ((and local (setf marked (origin-marked-place path)))
           (if (string= (string-right-trim "/" path) marked)
               "it is marked as come from outside (user.xdg.origin.url)"
               (format nil "~a, holding it, is marked as come from outside (user.xdg.origin.url)" marked))))))

(defun untrusted-path-p (path)
  "True when the file or directory at PATH came from outside and is not
trusted (UNTRUSTED-REASON gives why). What opens safely, what the desktop's
programs get a copy of, where a note's export takes files from and which
projects run their own programs are all told by it alone."
  (and (untrusted-reason path) t))

;;; Projects

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

(defun project-trusted-p (root)
  "True when the project at ROOT may run its own programs. That holds when it,
where it really is, is trusted (UNTRUSTED-PATH-P: in no evidence directory and
none of *UNTRUSTED-DIRECTORIES*, and neither it nor a directory holding it
nor a thing at its top marked as come from outside), or was trusted when asked (kept by where it
really was then). It never holds for one that is, is in or holds an evidence directory
\(DIRECTORY-HOLDS-EVIDENCE-P), whose programs could write there, nor for one
within a directory answered look only (PROJECT-LOOK-ONLY-P). The answer that runs
less wins. While the project is still being looked through for evidence
directories it is not trusted, with :PENDING as a second value: the answer
is to be asked again."
  (let ((real (real-directory root)))
    (and real
         (or (member real (state-list-entries *trusted*) :test #'equal)
             (not (or (untrusted-path-p real) (origin-marked-entry real))))
         (not (real-directory-look-only-p real))
         ;; asked only of a project trusted, and told from the setting, not by reading the project
         (not (evidence-path-p real))
         (multiple-value-bind (holds looking) (directory-holds-evidence-p real)
           (if looking (values nil :pending) (not holds))))))

(defun trusted-project-key (file)
  "What an answer of BUFFER-IN-TRUSTED-PROJECT-P about FILE depends on, as it
stands now: a vector of FILE, the generations of the projects trusted and of
those looked at only (STATE-LIST-GENERATION), *UNTRUSTED-DIRECTORIES*, the
generation of *EVIDENCE-DIRECTORIES*, the download directory as last read and
how many marks of files come from outside were made (*MARKS-MADE*)."
  (vector file (state-list-generation *trusted*) (state-list-generation *look-only*) *untrusted-directories*
          (evidence-generation) (tuatara.io:download-directory) (car *marks-made*)))

(defun trusted-project-key-current-p (key file)
  "True when KEY, made by TRUSTED-PROJECT-KEY, is what it would be now for
FILE, which allocates nothing."
  (and (equal (svref key 0) file)
       (eql (svref key 1) (state-list-generation *trusted*))
       (eql (svref key 2) (state-list-generation *look-only*))
       (equal (svref key 3) *untrusted-directories*)
       (eql (svref key 4) (evidence-generation))
       (eq (svref key 5) (tuatara.io:known-download-directory))
       (eql (svref key 6) (car *marks-made*))))

(defun buffer-in-trusted-project-p (buffer)
  "True when BUFFER visits a file of a project you trust (PROJECT-TRUSTED-P):
its text is then parsed by the program shared by such files, not one of its
own. Looked up again only when the file, the projects trusted or looked at
only when asked, *UNTRUSTED-DIRECTORIES* or *EVIDENCE-DIRECTORIES* change, a
file is marked as come from outside, or
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

(defun forget-project-trust (root)
  "Forget what was answered of the project at ROOT, trusted or looked at only,
so that it is asked about again, and tell those who keep answers of it
\(*TRUST-CHANGED-FUNCTIONS*): its language servers, git and M-x compile run
nothing of it until it is trusted again. A project outside the untrusted
directories (UNTRUSTED-PATH-P) stays trusted."
  (let ((real (or (real-directory root) root)))
    (state-list-drop *trusted* (list real))
    (state-list-drop *look-only* (list real))
    (run-hook *trust-changed-functions* root)))

(defun ask-project-trust (root &key again)
  "Whether the project at ROOT may run its own programs: :TRUSTED,
:LOOK-ONLY, or NIL when not, and as a second value why it can never be
trusted when it is evidence (PROJECT-EVIDENCE-WORDS). It is either trusted
already, answered look only already, or asked now (with AGAIN, asked whatever
was answered before). The keys are these: y, then yes typed (trusting cannot
be taken back once its programs ran), trusts it, unless a directory holding
it is looked at only. l keeps it to looking only, each remembered. n leaves
it to be asked again, and with AGAIN forgets what was answered
\(FORGET-PROJECT-TRUST). The question (TRUST-QUESTION) says all a yes allows,
whatever asks it, and only a key pressed once it is shown answers it. A
project that is, is in or holds an evidence directory, or is within a
directory answered look only, is looked at only, never asked of (asked again
with AGAIN, it stays so)."
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
               (t (when again (forget-project-trust real))
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
project in or holding an evidence directory cannot be trusted, and a project
outside the untrusted directories (*UNTRUSTED-DIRECTORIES*) stays trusted unless
answered l."
  (let* ((buffer (current-buffer))
         (root (or (tuatara.buffer:buffer-local buffer :git-top)
                   (let ((directory (buffer-directory buffer)))
                     (and directory (or (ignoring-errors-reported ("finding the repository of ~a" directory)
                                           (tuatara.git:repository-top directory))
                                         (project-root directory) directory))))))
    (if (null root)
        (message "~a is in no project" (tuatara.buffer:buffer-name buffer))
        (multiple-value-bind (answer evidence) (ask-project-trust root :again t)
          ;; an answer forgotten has told them already
          (when answer (run-hook *trust-changed-functions* root))
          (message "~a ~a" (abbreviated-path root)
                   (cond (evidence (format nil "cannot be trusted: ~a" evidence))
                         ((eq answer :trusted) "is trusted")
                         ((project-trusted-p root) "is trusted all the same: it is outside *untrusted-directories*")
                         ((and (eq answer :look-only)
                               (not (member (real-directory root) (state-list-entries *look-only*) :test #'equal)))
                          "is looked at only all the same: a directory holding it is")
                         ((eq answer :look-only) "is looked at only")
                         (t "will be asked about again")))))))
