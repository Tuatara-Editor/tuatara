;;;; lisp/modes/list-kept-current.lisp --- Lists made again, kept current while shown, left; going from entry to entry.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.defaults)

;;; Made again, kept current, left

(defun show-list-from-thread (buffer placeholder work fill &key settings)
  "Fill BUFFER, a list, from what WORK finds on a thread of its own, so that a
drive not answering holds nothing up: FILL, a function of what WORK returns,
is called on the editor's thread once it ends. WORK is called with a function
of no arguments that is true while what it finds is still wanted, to give up
early once it is not. A BUFFER still empty says PLACEHOLDER, a line of text,
meanwhile. Of listings of BUFFER that overlap, only the one begun last fills
it; the others are let go, as they are once BUFFER is killed. SETTINGS, names
of settings, keep their values here on the thread. An error in WORK is said
and counts as finding NIL."
  (when (zerop (tuatara.buffer:buffer-size buffer))
    (render-pieces buffer (list (format nil "~a~%" placeholder)) :point :start))
  (let ((listing (setf (tuatara.buffer:buffer-local buffer 'list-listing)
                       (1+ (tuatara.buffer:buffer-local buffer 'list-listing 0))))
        (name (tuatara.buffer:buffer-name buffer)))
    (flet ((wanted ()
             (and (live-buffer-p buffer) (eql listing (tuatara.buffer:buffer-local buffer 'list-listing)))))
      (tuatara.runtime:make-guarded-thread
       (tuatara.runtime:carry-settings
        settings
        (lambda ()
          (let ((found (ignoring-errors-reported ("listing ~a" name) (funcall work #'wanted))))
            (defer-action (lambda () (when (wanted) (funcall fill found)))))))
       :name (format nil "tuatara ~a listed" name)))))

(defun refill-list (buffer)
  "Make BUFFER again as \\[revert-buffer] makes it (its :REFILL), when it says how."
  (let ((refill (tuatara.buffer:buffer-local buffer :refill)))
    (when refill (funcall refill buffer))))

(defun redraw-list (buffer)
  "Draw BUFFER again, its marks and folds as they are now: from what it holds
\(its REDRAW, RENDER-PIECES), else as \\[revert-buffer] makes it (REFILL-LIST)."
  (let ((redraw (tuatara.buffer:buffer-local buffer 'list-redraw)))
    (if redraw (funcall redraw buffer) (refill-list buffer))))

(defun keep-shown-current (mode key &optional follow)
  "Keep the lists of major mode MODE current while a window shows them. KEY, a
function of such a list, gives cheaply what it is made from, equal keys
meaning nothing it shows has changed. When KEY gives other than it gave when
the list was last filled (SET-MADE-TEXT), the list is made again. It is made
by FOLLOW, a function of the list and a window showing it (the selected one
when that is one), when given, else as \\[revert-buffer] makes it (its
:REFILL)."
  (setf *shown-lists* (append (remove mode *shown-lists* :key #'first) (list (list mode key follow))))
  mode)

(defun refill-open-list (name mode)
  "Make the list called NAME again, when there is one in major MODE, as
\\[revert-buffer] makes it (its :REFILL): what it shows has changed."
  (let ((buffer (tuatara.buffer:find-buffer name)))
    (when (and buffer (typep (buffer-major-mode buffer) mode))
      (refill-list buffer))))

(defvar *shown-lists-seen* (make-array 8 :adjustable t :fill-pointer 0)
  "The buffers KEEP-SHOWN-LISTS-CURRENT has looked at so far in its tick,
emptied after it: the vector is kept from one tick to the next so that a tick
makes nothing.")

(defun keep-shown-lists-current (editor)
  "Tick function: make again each list kept current (KEEP-SHOWN-CURRENT) that a
window of EDITOR shows and whose key has changed since it was filled; a list
whose key is the same is left as it is, as is one a waiting question is about
\(BUFFER-HELD-BY-QUESTION-P). Wants no tick of its own."
  (let ((seen *shown-lists-seen*))
    (setf (fill-pointer seen) 0)
    (do-frames-windows (window editor)
      (let ((buffer (and window (tuatara.display:window-buffer window))))
        (when (and buffer (not (find buffer seen)) (not (buffer-held-by-question-p buffer)))
          (vector-push-extend buffer seen)
          (let ((kind (shown-list-kind buffer)))
            (when (and kind (not (equal (funcall (second kind) buffer)
                                        (tuatara.buffer:buffer-local buffer 'list-key))))
              (if (third kind)
                  (funcall (third kind) buffer window)
                  (refill-list buffer)))))))
    (fill seen nil)
    (setf (fill-pointer seen) 0))
  nil)

(defgeneric mode-before-quit (mode buffer)
  (:documentation "What \\[quit-window] does first in a window showing BUFFER, of
major mode MODE, before it puts the window back: nothing, unless MODE has
something of its own to end (a rimu list's preview and quick edit).")
  (:method (mode buffer) (declare (ignore mode buffer)) nil))

(defparameter *list-free-keys* '("w" "b" "W" "B" "v" "%")
  "The keys of the modal layer's NORMAL state that move by word or expression
and select but by lines, which do nothing in a list that does not bind them
itself: a list moves a line at a time and acts on whole entries, \\`V'
selecting lines and \\`J' and \\`K' growing that selection.")

(defun free-list-keys (keymap)
  "Make *LIST-FREE-KEYS* do nothing in KEYMAP, a list's, but those it binds itself."
  (dolist (keys *list-free-keys*)
    (unless (lookup-key keymap (tuatara.frontend:parse-key-sequence keys) :parents nil)
      (define-key keymap keys :undefined))))

(defun install-special-buffers ()
  "Give every buffer a command makes (special-mode and those made from it) the
same keys to leave it and to make it again: \\`q' quit-window, \\`r' and
\\`g r' revert-buffer, and text made in a mode of its own (VIEW-MADE-TEXT) its
\\`q'. Give every list whose entries are marked (marked-list-mode) the same
keys to mark and delete them and no moves by word nor selections but by lines
\(FREE-LIST-KEYS). Give every list in sections (sectioned-list-mode) the same
keys to fold them. Make the lists kept current again as what they show changes
\(KEEP-SHOWN-LISTS-CURRENT)."
  (define-keys (mode-keymap 'special-mode) '(("q" quit-window) ("r" revert-buffer) ("g r" revert-buffer)))
  (define-key *made-text-keymap* "q" 'quit-window)
  (define-keys (mode-keymap 'marked-list-mode) '(("m" list-mark-entry) ("d" list-mark-for-deletion)
                                                 ("u" list-unmark-entry) ("U" list-unmark-all) ("t" list-toggle-marks)
                                                 ("x" list-execute) ("D" list-delete-marked)))
  (free-list-keys (mode-keymap 'marked-list-mode))
  (commands-for-mode 'marked-list-mode "a list whose entries are marked, as the buffer list (\\[buffer-manager])"
                     'list-mark-entry 'list-mark-for-deletion 'list-unmark-entry 'list-unmark-all 'list-toggle-marks
                     'list-execute 'list-delete-marked)
  (define-keys (mode-keymap 'sectioned-list-mode) '(("TAB" list-toggle-fold) ("S-TAB" list-toggle-all-folds)))
  (commands-for-mode 'sectioned-list-mode "a list in sections, as the buffer list (\\[buffer-manager])"
                     'list-toggle-fold 'list-toggle-all-folds)
  (pushnew 'keep-shown-lists-current *tick-functions*))

;;; Going from entry to entry

(defun entry-start-p (entries index)
  "True when line INDEX of ENTRIES, a list's (RENDER-PIECES), begins an entry:
it shows one, and not the one the line before it shows, an entry being shown
on several lines in a row by one object."
  (let ((entry (elt entries index)))
    (and entry (or (zerop index) (not (eq entry (elt entries (1- index))))))))

(defun list-step-entry (list direction &key (test (constantly t)))
  "Put point of LIST, a list made by RENDER-PIECES, at the start of the first
line of the next entry after the one on the current line (DIRECTION 1), or
of the one before it (-1), passing over those TEST, a function of an entry,
is false of. Returns that entry, or NIL, point left where it is, when there
is none that way."
  (let* ((entries (tuatara.buffer:buffer-local list 'list-entries #()))
         (first (tuatara.buffer:buffer-local list 'list-first-line 0))
         (here (- (tuatara.buffer:buffer-line-number list (tuatara.buffer:buffer-point list)) first))
         (count (length entries))
         (current (and (< -1 here count) (elt entries here))))
    (loop for index = (if (plusp direction) (max 0 (1+ here)) (min (1- count) (1- here))) then (+ index direction)
          while (< -1 index count)
          do (let ((entry (elt entries index)))
               (when (and (entry-start-p entries index) (not (eq entry current)) (funcall test entry))
                 (setf (tuatara.buffer:buffer-point list) (tuatara.buffer:buffer-goto-line list (+ first index)))
                 (return entry))))))

(defun list-heading-p (list entry)
  "True when ENTRY of LIST heads a section rather than standing for something:
a section's line, (:SECTION NAME), or one the list's search filtering says is
a heading (SET-SEARCH-FILTERING)."
  (let ((filtering (list-filtering list)))
    (if filtering
        (heading-entry-p filtering entry)
        (and (consp entry) (eq (first entry) :section)))))

(defgeneric list-entry-stepped (mode list entry)
  (:documentation "What a list of major mode MODE does once \\[list-next-entry] or
\\[list-previous-entry] has put point on ENTRY of LIST: nothing, unless the
mode shows the entry somewhere (a definition in the window the list was
asked from).")
  (:method (mode list entry)
    (declare (ignore mode list entry))
    nil))

(defun list-step-to-entry (direction)
  "Go to the next entry of the current list, or with DIRECTION -1 the one
before, passing over the headings of its sections (LIST-STEP-ENTRY), and do
what the list does then (LIST-ENTRY-STEPPED); say so when there is none."
  (let* ((list (current-buffer))
         (entry (list-step-entry list direction :test (lambda (entry) (not (list-heading-p list entry))))))
    (if entry
        (list-entry-stepped (buffer-major-mode list) list entry)
        (message (if (plusp direction) "No more below" "No more above")))))

(define-command list-next-entry ()
  "Go to the next entry of the list, the start of its first line, passing
over the headings of its sections."
  (list-step-to-entry 1))

(define-command list-previous-entry ()
  "Go to the entry of the list before the one on the current line, the start
of its first line, passing over the headings of its sections."
  (list-step-to-entry -1))

(defvar *list-kinds* (make-hash-table :test 'eq)
  "The kind (LIST-KIND) each major mode that makes a list declared, by its name.")

(defun list-kind (mode)
  "The kind of list the major mode named MODE declared (DECLARE-LIST-KIND), or
NIL when it declared none."
  (gethash mode *list-kinds*))

(defun declare-list-kind (mode kind)
  "Say that the buffers of major mode MODE, a symbol, are lists of KIND, and
give them the keys of it. \\`q' leaves every kind. The kinds:
:ENTRIES, a list whose entries are opened (\\`RET', \\`l' and \\`C-f'
\(\\[list-visit]), \\`o' in another window (\\[list-visit-other]), \\`n' and
\\`p' to the next entry and the one before); :MANAGER, the same for a
list of things you keep and act on (buffers, bookmarks, packages, the cache),
in sections as a rule, \\`h' and \\`C-b' folding the section of the heading
\(\\[list-fold-heading]); :DRILL-DOWN, :ENTRIES for a list entered from
another view, \\`h' and \\`C-b' going back to it (\\[quit-window]); :PLAIN,
a list with no entries to open, and :TEXT, text to read, which add nothing:
\\`h', \\`j', \\`k' and \\`l' only move there. A list never binds \\`h' by
itself: it names its kind."
  (check-type kind (member :manager :entries :drill-down :plain :text))
  (setf (gethash mode *list-kinds*) kind)
  (let ((keymap (mode-keymap mode)))
    (unless (member kind '(:plain :text))
      (define-keys keymap '(("RET" list-visit) ("l" list-visit) ("C-f" list-visit) ("o" list-visit-other)
                            ("n" list-next-entry) ("p" list-previous-entry))))
    (case kind
      (:manager (define-keys keymap '(("h" list-fold-heading) ("C-b" list-fold-heading))))
      (:drill-down (define-keys keymap '(("h" quit-window) ("C-b" quit-window)))))))

(defun entry-list-keys (mode where &key back kind)
  "Give the lists of major MODE, WHERE in words for those elsewhere, the keys
of a list whose entries are opened (DECLARE-LIST-KIND): :MANAGER when MODE is
made from SECTIONED-LIST-MODE, :DRILL-DOWN with BACK true for a list entered
from another view, else :ENTRIES, or the KIND given; the commands run there
\(COMMANDS-FOR-MODE).
\\`h' and \\`C-b' only move unless the kind says more."
  (declare-list-kind mode (cond (kind kind)
                                ((subtypep mode 'sectioned-list-mode) :manager)
                                (back :drill-down)
                                (t :entries)))
  (commands-for-mode mode where 'list-visit 'list-visit-other 'list-next-entry 'list-previous-entry))
