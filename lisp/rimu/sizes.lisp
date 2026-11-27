;;;; lisp/rimu/sizes.lisp --- Rimu's sizes column: each entry's size, a directory's with all it holds.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; S in a rimu list shows before each name how much it takes: a file its
;;;; size, a directory the size of everything in it and under it, as Emacs's
;;;; dired-du shows it. A file's size shows at once. The directories are walked
;;;; one after another on one thread in the background, every entry counted,
;;;; hidden ones too (unlike the walks that look for files to read, which leave
;;;; them out), a file with several names once, links never followed; each
;;;; directory not counted yet shows a faint … until its walk ends, and then
;;;; its size, a few times a second at most. In a list of a directory on
;;;; another machine the walk reads each directory below through the list's
;;;; SFTP connection (REMOTE-TREE-SIZE), nothing run there. A walk that could
;;;; not read a directory below shows what it counted, and that size is not
;;;; kept.
;;;;
;;;; The sizes found are kept in memory, by directory, so that a list shown
;;;; again has something to show at once: a size kept shows faint, as what the
;;;; directory took when it was last counted, and the directory is walked
;;;; again all the same; the size the walk finds takes its place, in the face
;;;; of sizes. Nothing but a walk tells of a change deep under a directory, so
;;;; a size kept is never shown as what the directory takes now. Sizes kept
;;;; are forgotten for a directory and those above it when rimu sees a change
;;;; there or a file under it is saved (RIMU-FORGET-SIZES), and for everything
;;;; under a list read again with g.

(in-package #:tuatara.defaults)

(alexandria:define-constant +rimu-size-unknown+ '("…" . :list-dim) :test #'equal
  :documentation "The sizes column's text for a directory whose size is being worked out.")

(defparameter *rimu-sizes-kept* 20000
  "The most sizes of directories the sizes column keeps in memory
\(RIMU-KEEP-SIZE), the one kept longest dropped first: a whole number from 0
to a million. With any other value none is kept.")

(defvar *rimu-sizes-lock* (sb-thread:make-mutex :name "rimu sizes")
  "Held while the sizes kept (*RIMU-SIZES*) and the walks under way
\(*RIMU-SIZES-WALKED*) are looked at or changed, which the walks' threads and
the editor's do.")

(defvar *rimu-sizes* (make-hash-table :test 'equal)
  "The sizes of directories the sizes column found, by the directory's native
name ending in / (on another machine /ssh:HOST:/path/): each (OCTETS INO
MTIME SLOT), INO and MTIME those of its stamp then, SLOT its place in
*RIMU-SIZES-ORDER*.")

(defvar *rimu-sizes-order* (vector)
  "The names of the directories whose sizes are kept, in a ring of
*RIMU-SIZES-KEPT* places, each new one in the place after the last: the one it
takes the place of is dropped.")

(defvar *rimu-sizes-next* 0
  "The place in *RIMU-SIZES-ORDER* the next size kept takes.")

(defvar *rimu-sizes-walked* '()
  "The walks of a directory's size under way (RIMU-WALKED-SIZE), each
\(DIRECTORY . FORGOTTEN): DIRECTORY its native name ending in /, FORGOTTEN true
once sizes were forgotten at it, under it or above it (RIMU-FORGET-SIZES), as
the size it finds may then be of the directory before the change.")

(defun rimu-known-size (directory stamp)
  "The octets kept for DIRECTORY, a native name ending in /, when its STAMP is
as it was when they were found; else NIL. What lies deeper may have changed
since: the octets are what DIRECTORY took then."
  (sb-thread:with-mutex (*rimu-sizes-lock*)
    (let ((kept (gethash directory *rimu-sizes*)))
      (and kept
           (eql (second kept) (tuatara.io:stamp-ino stamp))
           (eql (third kept) (tuatara.io:stamp-mtime stamp))
           (first kept)))))

(defun rimu-keep-size (directory stamp octets &optional walked)
  "Keep OCTETS as the size of DIRECTORY, a native name ending in /, with its
STAMP, unless WALKED, the walk that found them (an entry of
*RIMU-SIZES-WALKED*), saw sizes forgotten there meanwhile. Once
*RIMU-SIZES-KEPT* are kept, the one kept longest is dropped. Takes constant
time."
  (sb-thread:with-mutex (*rimu-sizes-lock*)
    (unless (cdr walked)
      (let ((most (if (typep *rimu-sizes-kept* '(integer 0 1000000)) *rimu-sizes-kept* 0)))
        (unless (= (length *rimu-sizes-order*) most)
          (setf *rimu-sizes-order* (make-array most :initial-element nil)
                *rimu-sizes-next* 0)
          (clrhash *rimu-sizes*)))
      (let ((order *rimu-sizes-order*))
        (when (plusp (length order))
          (let* ((slot *rimu-sizes-next*)
                 (old (svref order slot)))
            ;; the directory in that place is dropped, unless it was kept again since in another
            (when (and old (eql slot (fourth (gethash old *rimu-sizes*))))
              (remhash old *rimu-sizes*))
            (setf (svref order slot) directory
                  (gethash directory *rimu-sizes*)
                  (list octets (tuatara.io:stamp-ino stamp) (tuatara.io:stamp-mtime stamp) slot)
                  *rimu-sizes-next* (mod (1+ slot) (length order)))))))))

(defun rimu-walked-size (directory stamp walk)
  "The size of DIRECTORY, a native name ending in /, as WALK finds it: a
function of no arguments returning the octets, or something else when it has
none, and true when it read every directory below. The octets of such a walk
are kept with STAMP (RIMU-KEEP-SIZE), unless sizes were forgotten at
DIRECTORY, under it or above it while it was walked: a change elsewhere takes
nothing from it. Returns what WALK returned first."
  (let ((walked (cons directory nil)))
    (sb-thread:with-mutex (*rimu-sizes-lock*)
      (push walked *rimu-sizes-walked*))
    (unwind-protect
         (multiple-value-bind (octets whole) (funcall walk)
           (when (and (integerp octets) whole)
             (rimu-keep-size directory stamp octets walked))
           octets)
      (sb-thread:with-mutex (*rimu-sizes-lock*)
        (setf *rimu-sizes-walked* (delete walked *rimu-sizes-walked* :test #'eq))))))

(defun rimu-forget-sizes (paths &key below)
  "Forget the sizes kept of the directories PATHS (native names, here or on
another machine) are or name, and of every directory above them on the same
machine; with BELOW, of every directory under them too. A walk under way of a
directory that any of PATHS is, is under or is above does not keep what it
finds (RIMU-WALKED-SIZE). Takes time in proportion to the length of PATHS and
how deep they lie, and with BELOW to the sizes kept and how deep those lie as
well, however many PATHS there are."
  (sb-thread:with-mutex (*rimu-sizes-lock*)
    (let ((directories (mapcar (lambda (path) (concatenate 'string (string-right-trim "/" path) "/")) paths)))
      (dolist (walked *rimu-sizes-walked*)
        (when (some (lambda (directory)
                      (or (uiop:string-prefix-p directory (car walked)) (uiop:string-prefix-p (car walked) directory)))
                    directories)
          (setf (cdr walked) t)))
      (when (plusp (hash-table-count *rimu-sizes*))
        (dolist (directory directories)
          ;; the directory itself and each above it: each name up to a /, from the first / of its own machine
          (loop for end = (position #\/ directory :start (or (remote-prefix-length directory) 0))
                  then (position #\/ directory :start (1+ end))
                while end
                do (remhash (subseq directory 0 (1+ end)) *rimu-sizes*)))
        (when below
          ;; one pass over the sizes kept: each kept name's own beginnings looked up among PATHS
          (let ((under (make-hash-table :test 'equal)))
            (dolist (directory directories)
              (setf (gethash directory under) t))
            (loop for key in (loop for key being the hash-keys of *rimu-sizes*
                                   when (loop for end = (position #\/ key) then (position #\/ key :start (1+ end))
                                              while end
                                                thereis (gethash (subseq key 0 (1+ end)) under))
                                     collect key)
                  do (remhash key *rimu-sizes*))))))))

(defun rimu-forget-saved (buffer)
  "After-save function: forget the sizes kept of the directories above the
file BUFFER was saved to (RIMU-FORGET-SIZES), and, when it is a copy of a file
on another machine, of those above the file there."
  (let ((file (tuatara.buffer:buffer-file buffer))
        (there (buffer-remote-name buffer)))
    (rimu-forget-sizes (append (and file (list (tuatara.io:native file))) (and there (list there))))))

(defun rimu-sizes-p (buffer)
  "True when the rimu list BUFFER shows the sizes column
\(\\<rimu-mode>\\[rimu-toggle-sizes]): never in a list opened safely, as summing
them reads every directory below, nor in one of a directory as a commit left
it, whose directories are not on the disk."
  (rimu-column-allowed-p buffer 'rimu-sizes 'rimu-toggle-sizes))

(defun rimu-size-listing-p (buffer directory)
  "True when BUFFER, a rimu list, lists DIRECTORY, or is about to: one of the
home directory of another machine (/ssh:HOST:) takes the name its server gives
that directory once it is read."
  (let ((listed (rimu-list-directory buffer)))
    (or (equal directory listed)
        (and (remote-name-p listed) (remote-name-p directory)
             (= (remote-prefix-length listed) (length listed))
             (uiop:string-prefix-p listed directory)))))

(defun rimu-size-wanted-p (directory entries)
  "True when the sizes of ENTRIES of DIRECTORY are still wanted: a rimu list
showing the sizes column holds them, or has read them and is laying them out,
or is reading DIRECTORY just now, to show them once that is done. A list
laying out other entries wants them no longer."
  (some (lambda (buffer)
          (and (rimu-list-p buffer)
               (rimu-sizes-p buffer)
               (rimu-list-awaits-p buffer entries (rimu-size-listing-p buffer directory))))
        tuatara.buffer:*buffers*))

(defun rimu-size-text (octets &optional kept)
  "The sizes column's text for OCTETS: in the face of sizes, or with KEPT,
for a size kept from an earlier walk and not counted again yet, faint."
  (cons (rimu-size-words octets) (if kept :list-dim :list-size)))

(defun rimu-size-walked-p (entry)
  "True when the size of ENTRY takes a walk: it is a directory other than .
and its parent."
  (let ((stamp (tuatara.io:listed-stamp entry)))
    (and stamp
         (eq (tuatara.io:stamp-kind stamp) :directory)
         (tuatara.io:entry-name-p (tuatara.io:listed-name entry)))))

(defun rimu-size-cell (directory entry)
  "The sizes column's text for ENTRY of DIRECTORY before any walk: a file's
size from its stamp; a directory's size kept from an earlier walk
\(RIMU-KNOWN-SIZE), faint as it may no longer hold, else +RIMU-SIZE-UNKNOWN+;
NIL for anything else."
  (let ((stamp (tuatara.io:listed-stamp entry))
        (name (tuatara.io:listed-name entry)))
    (cond ((rimu-size-walked-p entry)
           (let ((known (rimu-known-size (concatenate 'string directory name "/") stamp)))
             (if known (rimu-size-text known t) +rimu-size-unknown+)))
          ((and stamp (eq (tuatara.io:stamp-kind stamp) :regular) (tuatara.io:entry-name-p name))
           (rimu-size-text (tuatara.io:stamp-size stamp))))))

(defun rimu-size-cells (directory entries)
  "The sizes column's texts for ENTRIES of DIRECTORY: each file's size; for
each directory the size kept from an earlier walk, faint, or a faint … when
none is. The directories are then walked one after another on one thread in
the background (TUATARA.IO:TREE-SIZE, or REMOTE-TREE-SIZE on another machine),
and each one's size takes the place of the faint text as its walk ends, given
to the lists by RIMU-COLUMN-READY a few times a second at most and only when a
size was found since the last give, and kept when the walk read every
directory below (RIMU-WALKED-SIZE), as either walk says with its second value.
The walk ends once no list shows those entries with the column, nor is about
to; what it found is then forgotten by the lists, to be worked out when the
column shows again. On another machine it ends too when the connection is
lost or does not answer in time, the sizes not yet found left empty. Returns
in time in proportion to the number of ENTRIES; each give copies the texts, in
that time too."
  (let ((shown (map 'simple-vector (lambda (entry) (rimu-size-cell directory entry)) entries)))
    (if (notany #'rimu-size-walked-p entries)
        shown
        (rimu-column-in-background
         :sizes entries "tuatara rimu sizes column"
         (lambda (names give)
           (declare (ignore names))
           (let ((cells (copy-seq shown))
                 (news nil)
                 (lost nil))
             (labels ((tell ()
                        ;; a size found since the last give goes to the lists once the interval allows
                        (when (and news (funcall give cells))
                          (setf news nil)))
                      (wanted ()
                        (tell)
                        (rimu-size-wanted-p directory entries))
                      (walk (path)
                        ;; the octets, NIL when the walk was ended as no longer wanted, or :NONE when it failed; and
                        ;; true when every directory below was read
                        ;; whole when the walk says so: a directory that could not be read, or on another
                        ;; machine past the limits of the walk, adds nothing, and such a sum is shown but not kept
                        (if (remote-name-p path)
                            (handler-case (multiple-value-bind (host there) (remote-name-parts path)
                                            (remote-tree-size host (tuatara.io:slash-ended there) :wanted #'wanted))
                              (tuatara.io:file-system-error () (setf lost t) :none))
                            (handler-case (tuatara.io:tree-size path :wanted #'wanted)
                              (tuatara.io:file-system-error () :none)))))
               (dotimes (i (length entries) cells)
                 (let ((entry (aref entries i)))
                   (when (rimu-size-walked-p entry)
                     (if lost
                         (setf (svref cells i) nil)
                         (let ((path (concatenate 'string directory (tuatara.io:listed-name entry) "/")))
                           (unless (rimu-size-wanted-p directory entries)
                             (return nil))
                           (let ((size (rimu-walked-size path (tuatara.io:listed-stamp entry)
                                                         (lambda () (walk path)))))
                             (when (null size)
                               (return nil))
                             (setf (svref cells i) (and (integerp size) (rimu-size-text size))
                                   news t)
                             (tell))))))))))
         :shown shown))))

(define-command rimu-toggle-sizes ()
  "Show or hide, before each name in this list, how much it takes: a file
its size, a directory the size of everything in it and under it, hidden
files too, directories of another drive mounted in it left out, worked out in
the background while the list shows it, a faint … before each directory until
its size is known. A directory counted before shows the size it had then,
faint, until it is counted again: every directory is counted each time the
list is read and each time the column is shown, and its size then shows in
the face of sizes. *RIMU-DIRECTORY-SIZES* says whether new lists show the
column. In a list of another machine each directory is read through its
connection, and as SFTP tells of no file's identity nor drive, a file with
several names is counted once for each and a drive mounted below is counted
in."
  (let* ((buffer (current-buffer))
         (listing (rimu-list-listing buffer)))
    ;; sizes found before the column was hidden are counted again
    (when (and listing (not (tuatara.buffer:buffer-local buffer 'rimu-sizes)))
      (remhash :sizes (rimu-listing-cells listing))))
  (rimu-toggle 'rimu-sizes "Sizes shown" "Sizes hidden"))

(defun install-rimu-sizes ()
  "Make the sizes column, and bind S to show or hide it; refuse it in a list
of a directory not on the disk, unless its source works it out itself; have
each save forget the sizes kept above the file saved."
  (rimu-set-column :sizes 'rimu-size-cells :place :before :align :right :shown 'rimu-sizes-p
                   :documentation "How much the entry takes: a directory with everything under it; on another
machine a file of several names counted once for each, a drive mounted below counted in.")
  (define-key *rimu-keymap* "S" 'rimu-toggle-sizes)
  (pushnew 'rimu-toggle-sizes *rimu-disk-commands*)
  (pushnew 'rimu-forget-saved *after-save-functions*))
