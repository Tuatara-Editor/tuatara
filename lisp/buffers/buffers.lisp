;;;; lisp/buffers/buffers.lisp --- The buffer list: mark, kill, save, visit.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; M-x buffers (C-x C-b) shows every live buffer in *Buffers*, a read-only
;;;; support buffer with one line per buffer and its own keys; C-x b
;;;; are the quick selector with completion. The list looks as Emacs's ibuffer
;;;; does: the buffers used most recently first, in a section for each project
;;;; and one for those in none, their marks, names, sizes, modes and files in
;;;; columns under titles, a buffer's name in a face for what it is, a line
;;;; summing them up at the end. C-x p k kills a project's buffers.

(in-package #:tuatara.defaults)

(define-major-mode buffers-mode sectioned-list-mode
  "The buffer list: one line per live buffer, in a section for each project.
\\<support>\\[next-line] and \\[previous-line] move, \\<buffers-mode>\\[list-visit] or \\`RET' opens the buffer
on the line (on a heading, folds or unfolds its section; on that of a client that ended,
takes its buffers up into this client), \\[list-visit-other] in another
window, \\[quit-window] returns; \\[list-toggle-fold] folds or unfolds the section, \\[list-toggle-all-folds] all of them; \\[list-mark-entry] marks a buffer (>),
\\[list-mark-for-deletion] marks it for deletion (D; on the heading of a client that ended, every
buffer of its folder), \\[list-unmark-entry] unmarks it,
\\[list-unmark-all] unmarks them all, \\[list-toggle-marks] swaps marked and unmarked; \\[buffers-save] saves the marked buffers,
\\[list-delete-marked] kills them after asking (on a heading, every buffer of
that project or client's folder), \\[list-execute] kills those marked for deletion; \\[revert-buffer] lists
them again, as the list does by itself while it is shown, marks kept. The modal keys that move, select and copy
work as in any support buffer; \\<normal>\\[start-selection] and
\\[select-lines] select."
  :lighter "Buffers" :interactive nil)

(defvar *buffers-keymap* (mode-keymap 'buffers-mode)
  "Keys of the *Buffers* list (buffers-mode's keymap).")

(defparameter *buffers-title-lines* 2
  "The lines of *Buffers* above its first buffer: the column titles and the
dashes under them.")

(defun buffers-in-order (&optional list-buffer)
  "The live buffers but LIST-BUFFER, those used most recently first; those
never shown after, the one made last first (BUFFERS-IN-USE-ORDER)."
  (remove list-buffer (buffers-in-use-order)))

(defun buffer-name-face (buffer mark)
  "The face of BUFFER's name in the buffer list: that of its MARK
\(LIST-MARK-FACE) when it has one; else :list-dim for help, :list-special
for a name in stars (*Messages*), :list-read-only for a buffer that is
read-only; none for any other."
  (or (list-mark-face mark)
      (cond ((typep (buffer-major-mode buffer) 'help-mode) :list-dim)
            ((uiop:string-prefix-p "*" (tuatara.buffer:buffer-name buffer)) :list-special)
            ((buffer-read-only-p buffer) :list-read-only))))

(defparameter *buffers-columns*
  '((" MR" :width 4) ("Name" :width 18) ("Size" :width 9 :right t :elide nil) ("Mode" :width 16) ("Filename/Process"))
  "The columns of *Buffers*, as TABLE-HEADING-PIECES takes them: the marks (>
marked or D, then * modified, % read-only), the name, the size, the major mode and the file
\(or the picture shown), or the folder a list of one (rimu's) lists.")

(defparameter *no-project* "No project"
  "The title of the buffer list's section for the buffers in no project.")

(defun buffer-project (buffer &optional roots)
  "The root of the project BUFFER's file or listed folder is in (PROJECT-ROOT),
or :NONE for a buffer of neither or one in no project. It is looked for once
while the buffer stands in the same folder, and kept with the buffer; ROOTS,
a table by folder, shares what is found among the buffers of one list."
  (let ((directory (or (buffer-given-directory buffer)
                       (let ((file (tuatara.buffer:buffer-file buffer)))
                         (and file (tuatara.io:parent-path file)))))
        (kept (tuatara.buffer:buffer-local buffer 'buffer-project)))
    (cond ((null directory) :none)
          ((equal (car kept) directory) (cdr kept))
          (t (let ((root (or (and roots (gethash directory roots)) (project-root directory) :none)))
               (when roots (setf (gethash directory roots) root))
               (setf (tuatara.buffer:buffer-local buffer 'buffer-project) (cons directory root))
               root)))))

(defun client-folders-p ()
  "True when the buffer list shows a folder for each client: in an editor
running in the background, whose clients each have buffers of their own."
  (and *editor* (editor-idle-frontend *editor*) t))

(defun buffer-folder (buffer client)
  "The folder of the buffer list BUFFER is in, for CLIENT: CLIENT when it is
CLIENT's, else the first client's whose it is (CLIENT-SEES-BUFFER-P), or
:EVERYONE when it is no one's."
  (let ((clients (buffer-clients buffer)))
    (cond ((null clients) :everyone)
          ((member client clients) client)
          (t (first clients)))))

(defun ended-folder-p (section)
  "True when SECTION of the buffer list is the folder of a client that ended."
  (and (client-p section) (client-ended section) t))

(defun buffer-sections (buffers)
  "BUFFERS, those used most recently first, in sections, each (SECTION .
BUFFERS): one for each project, SECTION its root, the project of the buffer
used last first; then one for those in none, SECTION :NONE. Where each client
has buffers of its own (CLIENT-FOLDERS-P), a folder for each client instead,
SECTION the client, the one working in the frame first, the others in the
order they came, then those of clients that ended, the last to end first;
then one for the buffers of no one's, SECTION :EVERYONE."
  (if (client-folders-p)
      (let ((client (current-client)))
        (group-sections buffers (lambda (b) (buffer-folder b client))
                        :first client :last :everyone
                        :order (lambda (a b)
                                 (cond ((and (client-ended a) (client-ended b)) (ended-later-p a b))
                                       ((client-ended a) nil)
                                       ((client-ended b) t)
                                       (t (< (client-number a) (client-number b)))))))
      (let ((roots (make-hash-table :test 'equal)))
        (group-sections buffers (lambda (b) (buffer-project b roots)) :last :none))))

(defparameter *every-client-folder* "Every client's"
  "The title of the buffer list's folder of the buffers every client is offered.")

(defvar *client-kept-without-buffers* :first
  "Which clients of a daemon stay when their last buffer is closed: :FIRST,
the one opened first of those running; :LAST, only the last one left; :ALL,
every one; NIL, none. The others end as \\[save-buffers-kill-editor] would end
them. In the buffer list \\<buffers-mode>\\[buffers-keep-client] makes another
client the one that stays, until it ends.")

(defvar *client-chosen-to-keep* nil
  "The client \\<buffers-mode>\\[buffers-keep-client] made the one that stays when its
last buffer is closed; NIL when none has been chosen.")

(defun client-kept-p (editor client)
  "True when CLIENT, running in EDITOR, stays when its last buffer is closed:
*CLIENT-KEPT-WITHOUT-BUFFERS* says, a client chosen with
\\<buffers-mode>\\[buffers-keep-client] and still running taking the place of the
one it names."
  (let* ((running (remove-if-not (lambda (other) (and (client-remote other) (not (client-ended other))))
                                 (editor-clients editor)))
         (chosen (find *client-chosen-to-keep* running)))
    (and (member client running)
         (cond ((eq *client-kept-without-buffers* :all) t)
               (chosen (eq client chosen))
               ((eq *client-kept-without-buffers* :first) (eq client (first running)))
               ((eq *client-kept-without-buffers* :last) (null (rest running)))))))

(defun buffer-section-title (section &optional sections)
  "What the buffer list calls SECTION: a project's folder name, or where it
is when another of SECTIONS (each (SECTION . BUFFERS)) has a folder of that
name; *NO-PROJECT* for :NONE; a client's folder by when and where it began
\(CLIENT-FOLDER-WORDS), one that ended after saying so, the one working in the
frame called this client; *EVERY-CLIENT-FOLDER* for :EVERYONE."
  (flet ((name (root) (tuatara.io:native-file-name root)))
    (cond ((eq section :none) *no-project*)
          ((eq section :everyone) *every-client-folder*)
          ((client-p section)
           (format nil "~:[~;This client: ~]~a" (eq section (current-client)) (client-folder-words section)))
          ((find-if (lambda (other) (and (not (eq other :none)) (not (equal other section))
                                         (equal (name other) (name section))))
                    sections :key #'car)
           (abbreviated-path section))
          (t (name section)))))

(defun buffer-section-note (section)
  "The words after the heading of SECTION in the buffer list: \"(persistent)\"
for the client that stays when its last buffer is closed (CLIENT-KEPT-P), else
NIL."
  (and (client-p section) (client-kept-p *editor* section) "(persistent)"))

(defun buffer-cells (b mark)
  "The cells of the line of the buffer B, marked MARK, in the buffer list."
  (list (format nil "~a~:[ ~;*~]~:[ ~;%~]" (list-mark-sign mark)
                (tuatara.buffer:buffer-modified-p b) (buffer-read-only-p b))
        (cons (tuatara.buffer:buffer-name b) (buffer-name-face b mark))
        (format nil "~:d" (tuatara.buffer:buffer-size b))
        (tuatara.modes:mode-lighter (type-of (buffer-major-mode b)))
        (let ((where (or (buffer-listed-file b) (buffer-given-directory b))))
          (if where (abbreviated-path where) ""))))

(defun buffers-render (list-buffer &optional (point :line))
  "Fill LIST-BUFFER with the live buffers (see BUFFERS) in a section for each
project and one for those in none (BUFFER-SECTIONS, SECTIONED-PIECES), then a
line summing them up; point staying on its buffer's line, or on its line when
that buffer is gone, at its start, or with POINT :keep at its column; a mark
on a buffer killed meanwhile is dropped."
  (let* ((buffers (buffers-in-order list-buffer))
         (files (count-if #'buffer-listed-file buffers))
         (sections (buffer-sections buffers)))
    (render-table-list
     list-buffer *buffers-columns* sections
     (lambda (b mark section) (declare (ignore section)) (buffer-cells b mark))
     :sections t
     :section-title (lambda (section) (buffer-section-title section sections))
     :section-note #'buffer-section-note
     :section-face (lambda (section) (and (ended-folder-p section) :list-dim))
     :after (cons (string #\Newline)
                  (table-row-pieces *buffers-columns*
                                    (list "" (format nil "~[No buffers~;1 buffer~:;~:*~d buffers~]" (length buffers))
                                          (format nil "~:d" (reduce #'+ buffers :key #'tuatara.buffer:buffer-size))
                                          ""
                                          (if (client-folders-p)
                                              (format nil "~[No files~;1 file~:;~:*~d files~], ~[no folders~;1 folder~:;~:*~d folders~]"
                                                      files (length sections))
                                              (format nil "~[No files~;1 file~:;~:*~d files~], ~[no projects~;1 project~:;~:*~d projects~]"
                                                      files (count :none sections :key #'car :test-not #'eq))))))
     :point point :first-line *buffers-title-lines*
     :refill (lambda (list-buffer) (buffers-render list-buffer :keep)))))

(defun buffers-key (list-buffer)
  "What *Buffers* LIST-BUFFER shows, cheaply: each live buffer in its order,
with its name, whether it is modified and read-only, its size, its mode, its
file and the folder it lists, and the clients whose folders they are in and
which of them ended or are kept; another key means the list is out of date
\(KEEP-SHOWN-CURRENT)."
  (cons (and (client-folders-p)
             (mapcar (lambda (client) (list client (client-ended client) (client-kept-p *editor* client)))
                     (editor-clients *editor*)))
        (mapcar (lambda (b)
                  (list b (tuatara.buffer:buffer-name b) (tuatara.buffer:buffer-modified-p b)
                        (buffer-read-only-p b) (tuatara.buffer:buffer-size b)
                        (type-of (buffer-major-mode b)) (tuatara.buffer:buffer-file b)
                        (buffer-given-directory b) (buffer-clients b)))
                (buffers-in-order list-buffer))))

(keep-shown-current 'buffers-mode 'buffers-key)

(define-command buffers ()
  "List every live buffer in *Buffers*, those used most recently first, in a
section for each project (a folder holding .git or .project,
*PROJECT-ROOT-MARKERS*: its files, the folders of it listed and the buffers
standing in them), the project of the buffer used last first, then one for
those in none; each heading says the project's folder name and how many
buffers it holds. In an editor running in the background, a folder for each
client instead: this client's open, the other clients' folded, then those of
clients that ended, the last to end first, each under a faint heading saying
when it ended; then those of the buffers every client is offered (*Messages*);
a buffer opened from another folder becomes this client's too; where they are shows in the last column, a rimu
list's folder there too. \\<buffers-mode>\\[list-visit] on the heading of a client that ended takes its buffers up into
this client, as starting tuatara --client with no files does for the last to end,
and \\[list-mark-for-deletion] there marks every buffer of its folder for deletion;
\\[list-toggle-fold] folds or unfolds a section,
\\[list-toggle-all-folds] all of them; \\[list-delete-marked] on a heading kills the buffers of the project or client's folder, as
\\[kill-project-buffers] does from any of them. Each line shows a buffer's marks (> marked, D marked for
deletion, * modified, % read-only), name, size, major mode and file; names in
stars (*Messages*), read-only buffers, help and marked names each set apart
in a face of their own; the number of buffers, their size and files summed
up at the end. \\<support>\\[next-line] and
\\[previous-line] move, \\<buffers-mode>\\[list-visit] or \\`RET' opens the
buffer on the line, \\[list-visit-other] in another window, \\[quit-window] returns; \\[list-mark-entry] marks the
buffer on the line, \\[list-mark-for-deletion] marks it for deletion,
\\[list-unmark-entry] unmarks it, \\[list-unmark-all] unmarks them all, \\[list-toggle-marks] swaps marked and unmarked, a count
marking as many;
\\[buffers-keep-client] makes the client whose folder the cursor is in the persistent one;
\\[buffers-save] saves the marked buffers (the one on the line when none is),
\\[list-delete-marked] kills them after asking, \\[list-execute] kills those
marked for deletion; \\[revert-buffer] lists them again, marks kept, as the
list does by itself while it is shown. \\<support>\\[beginning-of-buffer],
\\`g e', \\[start-selection], \\[copy-selection-or-char] and the other modal keys that read
and select work as in any support buffer."
  (let ((list (buffer-in-mode "*Buffers*" 'buffers-mode)))
    (show-buffer list)
    ;; the other clients' folders shown folded, this client's open
    (when (client-folders-p)
      (setf (tuatara.buffer:buffer-local list 'list-folded)
            (remove (current-client) (remove-if-not #'client-remote (editor-clients *editor*)))))
    (buffers-render list (1+ *buffers-title-lines*))))

(defmethod list-entry-noun ((mode buffers-mode))
  "A buffer."
  "buffer")

(defmethod visit-entry ((mode buffers-mode) list b other)
  "Show the buffer B, or with OTHER show it in another window and go there, the
list staying in its own; one killed meanwhile is said so, the list made again."
  (cond ((not (live-buffer-p b))
         (buffers-render list)
         (message "~a has been killed" (tuatara.buffer:buffer-name b)))
        (other (pop-to-buffer b :other t))
        (t (show-buffer b))))

(defun kill-buffers-asked (targets question &key modified-only)
  "Kill the buffers TARGETS after QUESTION, a string, says what, asked with
yes or no (modified ones said), or with MODIFIED-ONLY only when one of them
is modified; each lets go only when a question of its own lets it
\(KILL-BUFFER-ALLOWED-P). The buffer list, when it is open, is made again;
how many were killed is said."
  (let ((modified (count-if #'tuatara.buffer:buffer-modified-p targets)))
    (when (or (and modified-only (zerop modified))
              (yes-or-no-p* (format nil "~a~[~;, 1 modified~:;, ~:*~d modified~]?" question modified)))
      (dolist (b targets) (when (kill-buffer-allowed-p b) (tuatara.buffer:kill-buffer b)))
      (refill-open-list "*Buffers*" 'buffers-mode)
      (let ((killed (count-if-not #'live-buffer-p targets)))
        (message "Killed ~d buffer~:p~[~:;, ~:*~d kept~]" killed (- (length targets) killed))))))

(defun kill-listed-buffers (targets &key modified-only)
  "Kill the buffers TARGETS of the buffer list after asking, as
KILL-BUFFERS-ASKED does, with MODIFIED-ONLY."
  (kill-buffers-asked targets (list-question "Kill" "buffer" targets :name-of #'tuatara.buffer:buffer-name)
                      :modified-only modified-only))

(defmethod visit-section ((mode buffers-mode) list section other)
  "On the heading of the folder of a client that ended, take its buffers up
into this client (TAKE-UP-ENDED-FOLDER); on another heading fold or unfold
the section."
  (cond ((ended-folder-p section)
         (take-up-ended-folder *editor* (current-client) section)
         (buffers-render list)
         (message "Took up the buffers of client ~d" (client-number section)))
        (t (call-next-method))))

(defmethod mark-entries ((mode buffers-mode) list mark count)
  "On the heading of the folder of a client that ended, marking for deletion
\(D) marks every buffer of the folder; the cursor moves down as it does
anywhere."
  (let ((section (list-section-at list)))
    (when (and (eq mark :deletion) (ended-folder-p section) (eq (first (list-entry-at list)) :section))
      (dolist (b (cdr (assoc section (buffer-sections (buffers-in-order list)))))
        (set-entry-mark list b :deletion)))
    (call-next-method)))

(defmethod delete-entries ((mode buffers-mode) list targets mark)
  "Kill the buffers TARGETS: those marked for deletion (D) asking first only
when one of them is modified, else after asking."
  (declare (ignore list))
  (kill-listed-buffers targets :modified-only (eq mark :deletion)))

(define-command buffers-keep-client ()
  "Make the client whose folder the cursor is in, in the buffer list, the one
that stays when its last buffer is closed, in place of the one
*CLIENT-KEPT-WITHOUT-BUFFERS* names, until it ends. Its heading says
\"(persistent)\". With that setting :ALL every client stays, and there is
nothing to choose."
  (let* ((list (current-buffer))
         (section (list-section-at list)))
    (cond ((not (and (client-p section) (not (client-ended section))))
           (message "The cursor is not in the folder of a running client"))
          ((eq *client-kept-without-buffers* :all)
           (message "Every client stays: *client-kept-without-buffers* is :all"))
          (t (setf *client-chosen-to-keep* section)
             (buffers-render list)
             (message "Client ~d stays when its last buffer is closed" (client-number section))))))

(define-command buffers-save ()
  "Save the marked buffers (>) that have a file, or with none marked the
buffer on the current line."
  (act-on-list (current-buffer)
               (lambda (list targets)
                 (let ((files (remove-if-not #'tuatara.buffer:buffer-file targets)))
                   (if (null files)
                       (message "~:[~a has~;~*None of them has~] no file" (rest targets) (tuatara.buffer:buffer-name (first targets)))
                       (progn (save-buffers-saying files (and (rest files) "Saved ~d buffers"))
                              (buffers-render list)))))
               "buffer"))

(defun project-buffers (root)
  "The live buffers of the project at ROOT (BUFFER-PROJECT), those used most
recently first, of those offered to the client working in the frame
\(CLIENT-BUFFERS); with ROOT :NONE, those in no project."
  (let ((roots (make-hash-table :test 'equal)))
    (remove-if-not (lambda (b) (equal (buffer-project b roots) root)) (client-buffers (buffers-in-order)))))

(defun kill-project-buffers-asked (root &optional except)
  "Kill the buffers of the project at ROOT (PROJECT-BUFFERS), or of the folder
of ROOT when it is a client (BUFFER-SECTIONS), but EXCEPT, after asking, the
project or folder named as the buffer list names it. The buffers in no
project (ROOT :NONE) are not killed together: *Messages* and the scratch
buffers are among them."
  (let ((targets (and (not (eq root :none))
                      (remove except (if (client-p root)
                                         (cdr (assoc root (buffer-sections (buffers-in-order))))
                                         (project-buffers root))))))
    (cond ((eq root :none)
           (message "The buffers in no project are not killed together: mark them (m), then D"))
          ((null targets) (message "No buffer on this line"))
          (t (kill-buffers-asked
              targets (list-question "Kill" "buffer" targets
                                     :name-of #'tuatara.buffer:buffer-name
                                     :after (format nil "of ~a" (buffer-section-title root (buffer-sections (buffers-in-order))))))))))

(defmethod delete-list-entries ((mode buffers-mode) list mark)
  "On a section's heading with no buffer marked, \\<buffers-mode>\\[list-delete-marked] kills every buffer of
that project or client's folder, after asking."
  (let ((entry (list-entry-at list)))
    (if (and (eq mark :marked) (eq (first entry) :section) (null (list-marked list :marked)))
        (kill-project-buffers-asked (second entry) list)
        (call-next-method))))

(define-command kill-project-buffers ()
  "Kill every buffer of the project the current buffer is in (a folder above
it holding one of *PROJECT-ROOT-MARKERS*, .git or .project), its files, its
folders listed (\\[rimu]), and the buffers standing in them; after asking,
saying how many are modified, each modified one asked about as it goes. In
*Buffers*, the project of the section the cursor is in."
  (let* ((buffer (current-buffer))
         (root (if (typep (buffer-major-mode buffer) 'buffers-mode)
                   (list-section-at buffer)
                   (buffer-project buffer))))
    (if (or (null root) (eq root :none))
        (message "~a is in no project" (tuatara.buffer:buffer-name buffer))
        (kill-project-buffers-asked root))))

(defun install-buffers ()
  "Bind the *Buffers* keys and the global entry points."
  (entry-list-keys 'buffers-mode "the buffer list (\\[buffers])")
  (define-keys *buffers-keymap* '(("s" buffers-save) ("K" buffers-keep-client) ("h" nil)))
  (define-keys *global-keymap* '(("C-x C-b" buffers) ("C-x p k" kill-project-buffers))))

(commands-for-mode 'buffers-mode "the buffer list (\\[buffers])" 'buffers-save)
