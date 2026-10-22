;;;; lisp/modes/git-commit.lisp --- Git's commit messages, when the editor is the one git runs.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.defaults)

(define-major-mode git-commit-mode text-mode
  "The messages git asks the editor for: a commit's (COMMIT_EDITMSG), a
merge's, a tag's, a squash's. Lines are filled to 72 columns, as git
recommends for a message's body (*GIT-COMMIT-FILL-COLUMN*); a line beginning
with # is a comment, which git leaves out."
  :lighter "Git commit"
  :comment "#"
  :file-names ("COMMIT_EDITMSG" "MERGE_MSG" "TAG_EDITMSG" "SQUASH_MSG" "EDIT_DESCRIPTION" "NOTES_EDITMSG"))

(defvar *git-commit-fill-column* 72
  "The column a git message's lines are filled to: 72, as git recommends for
the body; the summary on the first line is best kept to 50.")

(defmethod mode-enter ((mode git-commit-mode) buffer)
  "Fill BUFFER's message at *GIT-COMMIT-FILL-COLUMN*; with no message written
yet, begin in INSERT state, for typing it at once."
  (call-next-method)
  (setf (tuatara.buffer:buffer-local buffer :fill-column) *git-commit-fill-column*)
  (when (and *editor* (member (tuatara.buffer:buffer-char-at buffer 0) '(nil #\Newline)))
    (set-modal-state :insert buffer)))
