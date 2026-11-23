;;;; lisp/lsp/lsp.lisp --- Language servers and formatters put to work in the editor.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; Loaded after the rest of lisp/lsp/: the one place the parts are installed,
;;;; formatting, the servers' ticks and hooks, what they find, their features.

(in-package #:tuatara.lsp)

(defun install-diagnostics-place ()
  "Show the list of what the language servers found in a window across the
bottom, 30% of the height, kept for it and selected."
  (place-buffers 'diagnostics-mode :bottom :size 0.3 :keep t :select t))

(defun install-lsp (editor)
  "Have buffers formatted before they are saved, as *FORMAT-ON-SAVE* says;
files opened in their projects' language servers, kept in step, what the
servers find shown, and the servers ended as the editor stops; and what the
servers know of where the cursor rests shown in EDITOR, with code's keys
under \\`C-c'."
  (install-format)
  (pushnew 'lsp-tick *tick-functions*)
  (setf tuatara.buffer:*find-file-functions*
        (append (remove 'lsp-visit tuatara.buffer:*find-file-functions*) (list 'lsp-visit)))
  (pushnew 'lsp-killed tuatara.buffer:*kill-buffer-functions*)
  (pushnew 'lsp-saved *after-save-functions*)
  (pushnew 'lsp-stop-all *exit-functions*)
  (pushnew 'lsp-outside-work *outside-work-functions*)
  (install-diagnostics)
  (install-semantic-tokens)
  (install-lsp-watching)
  (pushnew 'lsp-selection-ranges *selection-ranges-functions*)
  (install-org-blocks)
  (install-lsp-features editor))
