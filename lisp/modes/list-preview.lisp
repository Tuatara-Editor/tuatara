;;;; lisp/modes/list-preview.lisp --- The entry on a list's current line, shown in a window beside the list.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; A list with a preview shows the entry on its current line in a window
;;;; beside it, on its right when there is room, else below it, and the
;;;; preview follows the cursor from line to line; it goes when the cursor
;;;; goes anywhere but the list and the preview. Rimu previews files this
;;;; way (lisp/rimu/preview.lisp, on SIDE-PREVIEW-FOLLOW), and any other list
;;;; does it by a method of PREVIEW-ENTRY for its major mode: the package
;;;; manager shows a package's README, a package's list what it lists. P
;;;; turns a list's preview off and on (list-toggle-preview).

(in-package #:tuatara.defaults)

(defstruct (side-preview (:constructor make-side-preview (list-window &optional (list-width 40))) (:copier nil))
  "What a list shows of the entry on its current line in a window beside it
(rimu's preview, the package manager's README): LIST-WINDOW, the window
showing the list; LIST-WIDTH, the columns the list needs to show whole;
WINDOW, the one beside it, showing BUFFER; OWN, true when BUFFER was made for
the preview and goes with it; SHOWN, what is shown or being made ready."
  list-window (list-width 40) window buffer own shown)

(setf (documentation 'side-preview-p 'function) "True when OBJECT is a SIDE-PREVIEW.")

(defun side-preview-follow (editor place &key list-p make close held follow inside)
  "After each command, have the preview held in the variable PLACE (a
SIDE-PREVIEW, or NIL for none) follow the list in EDITOR's selected window:
LIST-P, a function of a buffer, is true of a list with a preview; MAKE, a
function of the window showing it, makes its preview, kept in PLACE; FOLLOW,
a function of the preview and the list, has it follow the list's current
line; CLOSE, a function of no arguments, takes the preview away, PLACE NIL
after. The preview is taken away when its list's window is gone, made again
for a list in another window, left alone while the cursor is in it (INSIDE,
a function of the preview, called then when given) and taken away when the
cursor is anywhere else. HELD, when given, a function of the preview called
first, may take it away or end what keeps it, and is true while the preview
is to stay as it is this time."
  (let ((state (symbol-value place)))
    (when (and state (not (member (side-preview-list-window state) (editor-windows editor))))
      (funcall close)
      (setf state nil))
    (when (and state held (funcall held state))
      (return-from side-preview-follow))
    (let* ((state (symbol-value place))
           (window (editor-window editor))
           (buffer (and window (tuatara.display:window-buffer window))))
      (cond ((and buffer (funcall list-p buffer))
             (when (and state (not (eq (side-preview-list-window state) window)))
               (funcall close)
               (setf state nil))
             (funcall follow (or state (setf (symbol-value place) (funcall make window))) buffer))
            ((and state (eq window (side-preview-window state)))
             (when inside (funcall inside state)))
            (state (funcall close))))))

(defun side-preview-make-window (list-window buffer list-width)
  "A window for a preview beside LIST-WINDOW, showing BUFFER: on its right
when half of LIST-WINDOW is still LIST-WIDTH columns wide, the columns the
list needs, else below it; NIL when there is no room."
  (let ((window (ignore-errors
                 (split-window list-window (if (>= (floor (tuatara.display:window-width list-window) 2) list-width) :right :below)
                               :buffer buffer))))
    (when window
      (setf (tuatara.display:window-quit-restore window) :delete))
    window))

(defun side-preview-show (state buffer own)
  "Show BUFFER in the window of the preview STATE (a SIDE-PREVIEW), made beside
its list when there is none; OWN when BUFFER was made for the preview, to go
with it. The buffer the preview showed before goes when it was made for it,
one made for it again of the same entry taking back its name (not NAME<2>).
With no room for a window, nothing is shown. True when BUFFER is shown."
  (let ((window (side-preview-window state))
        (old (and (side-preview-own state) (side-preview-buffer state))))
    (unless (and window (member window (editor-windows)))
      (setf window (side-preview-make-window (side-preview-list-window state) buffer (side-preview-list-width state))))
    (when (and (null window) own)
      (tuatara.buffer:kill-buffer buffer))
    (when window
      (tuatara.display:set-window-buffer window buffer :remember nil))
    (setf (side-preview-window state) window
          (side-preview-buffer state) (and window buffer)
          (side-preview-own state) (and window own))
    (when (and old (not (eq old buffer)) (live-buffer-p old))
      (let ((name (tuatara.buffer:buffer-name old)))
        (tuatara.buffer:kill-buffer old)
        (when (and window own (eql 0 (search (format nil "~a<" name) (tuatara.buffer:buffer-name buffer))))
          (setf (tuatara.buffer:buffer-name buffer) name))))
    (and window t)))

(defun side-preview-close (state)
  "Take away the preview STATE (a SIDE-PREVIEW): its window, but when it is the
only one, and its buffer when that was made for it."
  (let ((window (side-preview-window state))
        (buffer (and (side-preview-own state) (side-preview-buffer state))))
    (when (and window *editor* (member window (editor-windows)) (tuatara.display:area-parent window))
      (remove-window window))
    (when (and buffer (live-buffer-p buffer))
      (tuatara.buffer:kill-buffer buffer))))

(defun side-preview-note (name text)
  "A buffer for the preview of NAME saying TEXT, made for the preview."
  (let ((buffer (tuatara.buffer:make-buffer (tuatara.buffer:unique-buffer-name (format nil "~a (preview)" (escape-controls name)))
                                            :text (format nil "~a~%" text))))
    (setf (tuatara.buffer:buffer-local buffer :read-only) t)
    buffer))

;;; The preview of any list

(defgeneric preview-entry (mode list entry)
  (:documentation "The buffer showing ENTRY of LIST, a list in MODE, in the
preview beside the list, and true as a second value when that buffer was
made for the preview, to go with it as the preview moves on or closes; NIL
for no preview of ENTRY. Called on the editor's thread as the cursor comes
to ENTRY's line, so a buffer whose text takes time to make is shown at once
and filled in the background (READING-BUFFER). In a list in sections, ENTRY
is a member of a section (ENTRY-MEMBER).")
  (:method (mode list entry)
    "A list whose entries have no preview."
    (declare (ignore mode list entry))
    nil))

(defgeneric entry-preview-width (mode list)
  (:documentation "The columns LIST, a list in MODE, needs to show whole: its
preview goes on its right while half of its window is that wide, else
below it.")
  (:method (mode list)
    "Forty columns."
    (declare (ignore mode list))
    40))

(defvar *entry-preview* nil
  "The preview beside the list in the selected window, a SIDE-PREVIEW whose
SHOWN is the entry shown; NIL when there is none.")

(defvar *previewing-modes* (make-hash-table :test 'eq)
  "For each class of major mode asked about, whether its lists have a method
of PREVIEW-ENTRY of their own, :YES or :NO.")

(defun entry-preview-list-p (buffer)
  "True when BUFFER is a list whose entries are previewed by PREVIEW-ENTRY,
with its preview on (\\[list-toggle-preview])."
  (and (tuatara.buffer:buffer-local buffer :list-preview)
       (let ((mode (buffer-major-mode buffer)))
         (eq :yes (or (gethash (class-of mode) *previewing-modes*)
                      (setf (gethash (class-of mode) *previewing-modes*)
                            (if (rest (compute-applicable-methods #'preview-entry (list mode buffer nil))) :yes :no)))))))

(defun entry-preview-again ()
  "Have the preview beside a list show the entry on its line anew after this
command, as what it showed may have changed: deleted, made again."
  (when *entry-preview*
    (setf (side-preview-shown *entry-preview*) nil)))

(defun close-entry-preview ()
  "Take away the preview beside a list: its window, and its buffer when that
was made for it."
  (let ((preview (shiftf *entry-preview* nil)))
    (when preview
      (side-preview-close preview))))

(defun entry-preview-follow (editor)
  "After each command: while the selected window shows a list with its
preview on (ENTRY-PREVIEW-LIST-P), show beside it the entry on its current
line (PREVIEW-ENTRY); on a line with no entry, or one with no preview, there
is none; the cursor gone into it leaves it as it is; anywhere else, it goes
\(SIDE-PREVIEW-FOLLOW)."
  (side-preview-follow
   editor '*entry-preview*
   :list-p #'entry-preview-list-p
   :make (lambda (window)
           (let ((list (tuatara.display:window-buffer window)))
             (make-side-preview window (entry-preview-width (buffer-major-mode list) list))))
   :close #'close-entry-preview
   :follow (lambda (preview list)
             (let* ((mode (buffer-major-mode list))
                    (entry (list-entry-at list))
                    (entry (if (typep mode 'sectioned-list-mode) (entry-member entry) entry)))
               (cond ((null entry) (close-entry-preview))
                     ((equal entry (side-preview-shown preview)))
                     (t (multiple-value-bind (buffer own) (preview-entry mode list entry)
                          (if buffer
                              (setf (side-preview-shown preview) (and (side-preview-show preview buffer own) entry))
                              (close-entry-preview)))))))))

(define-command list-toggle-preview ()
  "Turn the preview beside this list on or off: the entry on the current line
shown in a window beside the list, on its right when there is room, else
below it, following the cursor from line to line. It closes, its buffers
with it, when the cursor goes anywhere but the list and the preview. A list
that has a preview says in a setting of its own whether new lists have it
on (*RIMU-PREVIEW* for rimu's). With a count, on when it is positive, off when
it is zero or less."
  (let* ((list (current-buffer))
         (on (switched-by-count (tuatara.buffer:buffer-local list :list-preview))))
    (setf (tuatara.buffer:buffer-local list :list-preview) on)
    (message "Preview ~:[off~;on~]" on)))

(defun entry-preview-keys (mode where)
  "Give the lists of major MODE, WHERE in words for those elsewhere, the key
of a list with a preview: \\`P' turns it on and off (\\[list-toggle-preview])."
  (define-key (mode-keymap mode) "P" 'list-toggle-preview)
  (commands-for-mode mode where 'list-toggle-preview))

(defun install-entry-previews (editor)
  "Have EDITOR show beside a list with a preview the entry on its current line."
  (pushnew 'entry-preview-follow (editor-post-command-functions editor)))
