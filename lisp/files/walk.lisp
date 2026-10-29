;;;; lisp/files/walk.lisp --- Walking the files under a folder as git leaves them out, and finding the folder a project is in.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; FILES-UNDER walks a folder on the thread calling it, leaving out what
;;;; the .gitignore files on the way, the repository's own excludes and
;;;; yours leave out; MARKED-DIRECTORY finds the folder a project is in by
;;;; the files that mark its top (.git, Cargo.toml, *.sln); GLOB-MATCH
;;;; matches a name against a glob, as shells and .gitignore files write
;;;; them. Searches, the questions that offer files and the language servers
;;;; use them.

(in-package #:tuatara.defaults)

;;; Files on disk

(defun backup-name-p (name)
  "True for the names of editors' backups and auto-save files: ending with ~,
or #name#."
  (or (char= (char name (1- (length name))) #\~)
      (and (> (length name) 1) (char= (char name 0) #\#) (char= (char name (1- (length name))) #\#))))

;;; Names matched against globs

(defun glob-class (glob start &optional function)
  "Read the class of characters GLOB begins at START, its [: ! or ^ after the
[ makes it the characters not in it, a ] first in it stands for itself (so
that []] and [!]] hold a ]), \\ makes the next character only itself, and
a-z is a range. FUNCTION, when given, is called with the first and the last
character of each range it holds, a character alone as both. Returns whether
it is negated and the position of its ], as two values; NIL when it is not
closed, and the [ is then only itself."
  (let* ((n (length glob))
         (i (1+ start))
         (negated (and (< i n) (member (char glob i) '(#\! #\^)) t)))
    (when negated (incf i))
    (flet ((next ()
             ;; the character at I, \ taking the one after it as it is
             (if (and (char= (char glob i) #\\) (< (1+ i) n))
                 (prog1 (char glob (1+ i)) (incf i 2))
                 (prog1 (char glob i) (incf i)))))
      (loop with first = t
            while (< i n)
            do (when (and (char= (char glob i) #\]) (not first))
                 (return-from glob-class (values negated i)))
               (let ((low (next)))
                 (if (and (< (1+ i) n) (char= (char glob i) #\-) (char/= (char glob (1+ i)) #\]))
                     (progn (incf i)
                            (let ((high (next)))
                              (when function (funcall function low high))))
                     (when function (funcall function low low))))
               (setf first nil))
      nil)))

(defun glob-step (glob p char test)
  "Where GLOB goes on after its part at P, ? or a class ([abc], [a-z], [!abc]
or [^abc] those not in it, GLOB-CLASS) or \\ and a character or any other
character, when that part takes CHAR, compared by TEST; NIL when it does not."
  (let ((c (char glob p)))
    (cond ((char= c #\?) (1+ p))
          ((and (char= c #\\) (< (1+ p) (length glob)))
           (and (funcall test (char glob (1+ p)) char) (+ p 2)))
          ((char= c #\[)
           (let ((in nil)
                 (fold (not (eq test #'char=))))
             (flet ((take (low high)
                      (when (if (char= low high)
                                (funcall test low char)
                                (if fold
                                    (char<= (char-downcase low) (char-downcase char) (char-downcase high))
                                    (char<= low char high)))
                        (setf in t))))
               (declare (dynamic-extent #'take))
               (multiple-value-bind (negated end) (glob-class glob p #'take)
                 (if end
                     (and (if negated (not in) in) (1+ end))
                     (and (funcall test c char) (1+ p)))))))
          (t (and (funcall test c char) (1+ p))))))

(defun glob-alternatives (glob &key (limit 256))
  "The globs without braces that GLOB stands for, in order: {a,b} in it
either a or b, groups inside groups too, one left open closed at the end, and
a , or } outside a group only itself. NIL when there would be more than LIMIT
of them."
  (let ((i 0) (n (length glob)))
    (labels ((unit-end (i)
               ;; past the part of GLOB at I: a class, \ and a character, or one character
               (cond ((and (char= (char glob i) #\\) (< (1+ i) n)) (+ i 2))
                     ((char= (char glob i) #\[)
                      (let ((end (nth-value 1 (glob-class glob i))))
                        (if end (1+ end) (1+ i))))
                     (t (1+ i))))
             (joined (heads tails)
               (when (> (* (length heads) (length tails)) limit)
                 (return-from glob-alternatives nil))
               (loop for head in heads
                     nconc (loop for tail in tails collect (concatenate 'string head tail))))
             (run (inside)
               ;; from I to the end, or to the , or } that ends the group the run is INSIDE
               (let ((found (list "")))
                 (loop while (< i n)
                       do (let ((c (char glob i)))
                            (cond ((and inside (find c ",}")) (return))
                                  ((char= c #\{) (incf i) (setf found (joined found (group))))
                                  (t (let ((start i))
                                       (loop do (setf i (unit-end i))
                                             while (and (< i n) (not (find (char glob i) (if inside "{,}" "{")))))
                                       (setf found (joined found (list (subseq glob start i)))))))))
                 found))
             (group ()
               (let ((choices '()))
                 (loop (setf choices (append choices (run t)))
                       (when (> (length choices) limit)
                         (return-from glob-alternatives nil))
                       (cond ((>= i n) (return choices))
                             ((char= (char glob i) #\}) (incf i) (return choices))
                             (t (incf i)))))))
      (run nil))))

(defun glob-star (glob p)
  "How many characters the star of GLOB at P is: 3 for **/, 2 for **, 1 for *,
0 when there is none there."
  (let ((n (length glob)))
    (cond ((char/= (char glob p) #\*) 0)
          ((not (and (< (1+ p) n) (char= (char glob (1+ p)) #\*))) 1)
          ((and (< (+ p 2) n) (char= (char glob (+ p 2)) #\/)) 3)
          (t 2))))

(defun glob-match (pattern name &key (test #'char=) braces (start 0))
  "True when NAME, from START, is all a name or path that PATTERN stands for:
* any characters but /, ** any characters, / too (**/ any number of folders,
none too), ? any one character but /, [abc] one of those ([a-z] a range of
them, [!abc] or [^abc] one not among them, GLOB-CLASS), \\ the next character
only itself, the rest as written, compared by TEST (CHAR-EQUAL for any case).
With BRACES, {a,b} stands for either a or b (GLOB-ALTERNATIVES). It never
steps back, so that its time grows with the length of PATTERN times that of
NAME at most, whatever the two hold."
  (if braces
      (some (lambda (glob) (glob-match glob name :test test :start start))
            (glob-alternatives pattern))
      (let* ((pl (length pattern))
             (stars (make-array pl :element-type '(unsigned-byte 2)))
             (now (make-array (1+ pl) :element-type 'bit :initial-element 0))
             (next (make-array (1+ pl) :element-type 'bit :initial-element 0)))
        (declare (dynamic-extent stars now next) (fixnum pl))
        ;; the plain characters PATTERN ends with, after its last / (which **/ may stand for), must
        ;; end NAME: most names fail there at once
        (let ((plain (loop for i from (1- pl) downto 0 until (find (char pattern i) "*?[]\\/") count t)))
          (unless (and (<= plain (- (length name) start))
                       (loop for k from 1 to plain
                             always (funcall test (char pattern (- pl k)) (char name (- (length name) k)))))
            (return-from glob-match nil))
          (when (= plain pl)
            (return-from glob-match (= pl (- (length name) start)))))
        (dotimes (p pl) (setf (aref stars p) (glob-star pattern p)))
        (flet ((close-over-stars (set)
                 ;; a star may stand for nothing: a place before it is a place after it too
                 (declare (simple-bit-vector set))
                 (loop for p = (position 1 set) then (position 1 set :start (1+ p))
                       while (and p (< p pl))
                       do (let ((k (aref stars p)))
                            (when (plusp k) (setf (sbit set (+ p k)) 1))))))
          (setf (sbit now 0) 1)
          (close-over-stars now)
          (loop for n from start below (length name)
                for c = (char name n)
                do (fill next 0)
                   (loop for p = (position 1 now) then (position 1 now :start (1+ p))
                         while (and p (< p pl))
                         do (case (aref stars p)
                              (3 (setf (sbit next p) 1)
                               (when (char= c #\/) (setf (sbit next (+ p 3)) 1)))
                              (2 (setf (sbit next p) 1))
                              (1 (unless (char= c #\/) (setf (sbit next p) 1)))
                              (t (unless (and (char= c #\/) (char= (char pattern p) #\?))
                                   (let ((q (glob-step pattern p c test)))
                                     (when q (setf (sbit next q) 1)))))))
                   (close-over-stars next)
                   (rotatef now next)
                   (unless (find 1 now) (return-from glob-match nil)))
          (= 1 (sbit now pl))))))

;;; What .gitignore files leave out

(defun gitignore-rule (line)
  "The rule a line of a .gitignore file makes, as a list (GLOB
DIRECTORIES-ONLY NEGATED ANCHORED), GLOB read by GLOB-MATCH, or NIL for a blank line or a comment: a
pattern ending with / matches directories only, one beginning with ! takes
back what the lines before it left out, and one with a / before its end is of
paths from the .gitignore's directory rather than of names at any depth."
  (let ((text (string-right-trim " " line)))
    (unless (or (zerop (length text)) (char= (char text 0) #\#))
      (let* ((negated (char= (char text 0) #\!))
             (text (if negated (subseq text 1) text))
             (directories-only (and (plusp (length text)) (char= (char text (1- (length text))) #\/)))
             (text (string-right-trim "/" text))
             (anchored (find #\/ text))
             (text (if (and anchored (plusp (length text)) (char= (char text 0) #\/)) (subseq text 1) text)))
        (when (plusp (length text))
          (list text directories-only negated anchored))))))

(defun gitignore-rules (path base)
  "The rules of the file of git's patterns at PATH (a .gitignore), for the
paths under the directory BASE (a native name ending with a slash), each
(RULE . BASE); NIL when it cannot be read."
  (let ((octets (read-text-file path)))
    (when octets
      (loop for line in (uiop:split-string (tuatara.text:lenient-string octets)
                                           :separator '(#\Newline #\Return))
            for rule = (gitignore-rule line)
            when rule collect (cons rule base)))))

(defun gitignored-p (rules name path directory)
  "True when RULES (GITIGNORE-RULES, the outer files' first) leave out the entry
NAME at the native PATH, a DIRECTORY or not: the last rule that matches it
says."
  (let ((ignored nil))
    (dolist (entry rules ignored)
      (destructuring-bind ((glob directories-only negated anchored) . base) entry
        (when (and (or directory (not directories-only))
                   (if anchored
                       (and (> (length path) (length base)) (string= base path :end2 (length base))
                            (glob-match glob path :start (length base)))
                       (glob-match glob name)))
          (setf ignored (not negated)))))))

(defvar *git-excludes-file* :git
  "The file of patterns git leaves out of every repository, which a search in
one leaves out too: :GIT for the one your git configuration names
(core.excludesFile, else ~/.config/git/ignore), a path, or NIL for none.")

(defun git-excludes-file ()
  "The native name of the file *GIT-EXCLUDES-FILE* says, or NIL. Git's
configuration is only read, never run: core.excludesFile of its [core] section,
~/.gitconfig counting over $XDG_CONFIG_HOME/git/config as it does for git."
  (let ((setting *git-excludes-file*))
    (cond ((null setting) nil)
          ((not (eq setting :git)) (tuatara.io:native (tuatara.io:expand-path setting)))
          (t
           (let* ((xdg (let ((env (uiop:getenv "XDG_CONFIG_HOME")))
                         (if (and env (plusp (length env))) (directory-name env) (directory-name "~/.config/"))))
                  (named nil))
             (dolist (file (list (concatenate 'string xdg "git/config") (concatenate 'string (directory-name "~/") ".gitconfig")))
               (let ((octets (read-text-file file)))
                 (when octets
                   (let ((section nil))
                     (dolist (line (uiop:split-string (tuatara.text:lenient-string octets)
                                                      :separator '(#\Newline #\Return)))
                       (let ((text (string-trim '(#\Space #\Tab) line)))
                         (cond ((and (plusp (length text)) (char= (char text 0) #\[))
                                (setf section (string-downcase (string-trim "[] " text))))
                               ((and (equal section "core")
                                     (let ((equals (position #\= text)))
                                       (and equals (string-equal "excludesfile" (string-trim '(#\Space #\Tab) (subseq text 0 equals))))))
                                (setf named (string-trim '(#\Space #\Tab #\") (subseq text (1+ (position #\= text)))))))))))))
             (tuatara.io:native (tuatara.io:expand-path (or named (concatenate 'string xdg "git/ignore")))))))))

(defun marked-directory (directory markers &key below-home)
  "The nearest directory from DIRECTORY up holding a file or directory named
one of MARKERS, as a native name ending with a slash, or NIL; a marker with
* or ? in it stands for every name it matches (GLOB-MATCH: *.sln). With
BELOW-HOME, never the home directory (by any of its names), one above it or /."
  (let ((homes (and below-home (tuatara.io:home-directory-names))))
    (loop for dir = (directory-name directory) then (and (string/= dir "/") (tuatara.io:parent-path dir))
          while dir
          until (and below-home
                     (or (string= dir "/")
                         (some (lambda (home) (tuatara.io:path-within-p home dir)) homes)))
          when (let ((names :unread))
                 (some (lambda (marker)
                         (if (find-if (lambda (c) (find c "*?")) marker)
                             (progn
                               (when (eq names :unread)
                                 (setf names (mapcar #'first (tuatara.io:directory-entries dir))))
                               (some (lambda (name) (glob-match marker name)) names))
                             (ignore-errors (tuatara.io:path-stamp (concatenate 'string dir marker) :follow nil))))
                       markers))
            return dir)))

(defun outer-gitignore-rules (root)
  "The rules git applies to the files under ROOT from outside it: when ROOT is
in a repository, those of your excludes file (GIT-EXCLUDES-FILE), of the
repository's .git/info/exclude and of the .gitignore files of the directories
from the repository's top down to ROOT's parent, the outer ones first."
  (let ((top (repository-top root)) (root (directory-name root)))
    (when top
      (let ((global (git-excludes-file))
            (above '()))
        ;; the directories from the top down to ROOT's parent: ROOT's own
        ;; .gitignore is read by the walk
        (unless (string= top root)
          (push top above)
          (loop for slash = (position #\/ root :start (length top)) then (position #\/ root :start (1+ slash))
                while (and slash (< (1+ slash) (length root)))
                do (push (subseq root 0 (1+ slash)) above)))
        (append (and global (gitignore-rules global top))
                (gitignore-rules (concatenate 'string top ".git/info/exclude") top)
                (loop for dir in (nreverse above)
                      append (gitignore-rules (concatenate 'string dir ".gitignore") dir)))))))

(defun walk-directory (root function &key list breadth-first max-depth same-device follow-links cancel limit)
  "Go through the folder ROOT (a native name ending in /) and the folders
under it, ROOT first, calling FUNCTION with three arguments: on entering a
folder, the folder (a native name ending in /), its path from ROOT (\"\" for
ROOT, else ending in /) and NIL; then, for each entry of it by name, the
folder, its path from ROOT and the entry, a list as TUATARA.IO:DIRECTORY-ENTRIES
gives it with stamps: (NAME KIND SIZE OWN STAMP). A folder among the entries
is entered when FUNCTION returns true for it: depth first, just after its
entry, or with BREADTH-FIRST once every folder nearer ROOT has been gone
through. A link to a folder is entered only with FOLLOW-LINKS; with
MAX-DEPTH, no folder more than MAX-DEPTH below ROOT is; with SAME-DEVICE,
no folder of another file system than ROOT's (a drive mounted below it,
/proc), nor one that could not be examined. LIST, a function of a folder,
gives its entries in place of TUATARA.IO:DIRECTORY-ENTRIES (NIL for a folder
that cannot be read); an entry of only (NAME KIND) counts as no link. CANCEL,
a cons, ends the walk when its car turns true, looked at as each folder is
entered; LIMIT ends it once that many folders were entered. True when every
folder wanted was gone through, NIL when CANCEL or LIMIT ended it."
  (let ((device (and same-device (let ((stamp (ignore-errors (tuatara.io:path-stamp root))))
                                   (and stamp (tuatara.io:stamp-dev stamp)))))
        (entered 0)
        (waiting '()) (last nil))
    (labels ((entries (directory)
               (if list
                   (funcall list directory)
                   (ignore-errors (tuatara.io:directory-entries directory :stamps t))))
             (enter (directory relative depth)
               (when (or (and cancel (car cancel)) (and limit (>= entered limit)))
                 (return-from walk-directory nil))
               (incf entered)
               (funcall function directory relative nil)
               (dolist (entry (entries directory))
                 (destructuring-bind (name kind &optional size (own kind) stamp) entry
                   (declare (ignore size))
                   (when (and (funcall function directory relative entry)
                              (eq kind :directory)
                              (or follow-links (eq own kind))
                              (or (null max-depth) (< depth max-depth))
                              (or (not same-device) (and stamp (eql device (tuatara.io:stamp-dev stamp)))))
                     (let ((folder (list (concatenate 'string directory name "/") (concatenate 'string relative name "/") (1+ depth))))
                       (cond ((not breadth-first) (apply #'enter folder))
                             (last (setf (cdr last) (list folder) last (cdr last)))
                             (t (setf waiting (list folder) last waiting)))))))))
      (enter (directory-name root) "" 0)
      (loop while waiting
            do (let ((folder (pop waiting)))
                 (unless waiting (setf last nil))
                 (apply #'enter folder))))
    t))

(defun files-under (root &key (limit 100000) gitignore each cancel directories hidden)
  "The regular files in the directory ROOT and its subdirectories, as a list of
(PATH . RELATIVE), RELATIVE the path from ROOT, by name within each directory:
hidden files and directories (names starting with a dot) and backups left
out, links to directories not followed, and with GITIGNORE what git leaves
out: the .gitignore files there and, in a repository, those above ROOT, the
repository's .git/info/exclude and your excludes file (OUTER-GITIGNORE-RULES). At most LIMIT of them; a second
value is true when there were more. With DIRECTORIES, the subdirectories in
place of the files, each before what is in it, both its paths ending in /;
with DIRECTORIES :ALSO, both. With HIDDEN, names starting with a dot are
walked too, each directory then read from the disk.
With EACH, a function, it is called with the path and the relative path of
each as it is found, and none are listed; CANCEL, a cons, stops the walk when
its car turns true. Each directory is listed as DIRECTORY-LISTING lists it:
from the index of a directory kept read, else from the disk."
  (let ((found '()) (count 0) (rules-of (make-hash-table :test 'equal)) (root (directory-name root)))
    (setf (gethash root rules-of) (and gitignore (outer-gitignore-rules root)))
    (flet ((give (path inside)
             (when (>= count limit)
               (return-from files-under (values (nreverse found) t)))
             (incf count)
             (if each
                 (funcall each path inside)
                 (push (cons path inside) found)))
           (listing (directory)
             (declare (notinline index-directory-entries index-directory-rules))
             (let ((listed (directory-listing directory :rules gitignore :hidden hidden)))
               (when (and gitignore (index-directory-rules listed))
                 (setf (gethash directory rules-of) (append (gethash directory rules-of) (index-directory-rules listed))))
               (index-directory-entries listed))))
      (walk-directory root
                      (lambda (directory relative entry)
                        (when entry
                          (destructuring-bind (name kind &rest rest) entry
                            (declare (ignore rest))
                            (let ((path (concatenate 'string directory name))
                                  (inside (concatenate 'string relative name))
                                  (rules (gethash directory rules-of)))
                              (unless (and rules (gitignored-p rules name path (eq kind :directory)))
                                (if (eq kind :directory)
                                    (let ((path (concatenate 'string path "/")) (inside (concatenate 'string inside "/")))
                                      (when gitignore (setf (gethash path rules-of) rules))
                                      (when directories (give path inside))
                                      t)
                                    (progn (unless (eq directories t) (give path inside))
                                           nil)))))))
                      :list #'listing :cancel cancel))
    (values (nreverse found) nil)))
