;;;; src/buffer/package.lisp --- Package definition for tuatara.buffer.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(defpackage #:tuatara.buffer
  (:use #:cl #:alexandria)
  (:documentation "Buffer objects: rope plus anchors plus buffer-local variables, change hooks, modification counters, versions and undo.")
  (:export #:buffer #:*buffers* #:make-buffer #:find-buffer #:live-buffer-p #:kill-buffer #:buffer-name #:buffer-version
           #:buffer-text #:buffer-size #:buffer-string #:buffer-char-at #:buffer-modified-p #:buffer-file
           #:buffer-point #:buffer-mark #:buffer-mark-active-p #:buffer-region
           #:buffer-edit #:*hex-view-resizing* #:hex-view-refuses-p #:buffer-insert #:buffer-delete #:buffer-replace #:buffer-undo #:buffer-redo
           #:buffer-undo-stack #:buffer-redo-stack #:buffer-change-functions #:buffer-swap-text #:buffer-change-where-begun
           #:merge-undo-groups #:undo-back-to
           #:make-undo-place #:forget-undo-place #:buffer-text-at #:span-after-batch
           #:buffer-forward-char #:buffer-line-start #:buffer-line-end #:buffer-line-text #:buffer-line-number #:buffer-line-count
           #:buffer-indexed-p #:buffer-adopt-text #:ensure-indexed #:*index-wait-function*
           #:buffer-splice #:buffer-splice-spans #:buffer-slice #:buffer-lent-p #:*large-edit-octets*
           #:buffer-scan-line #:*line-scan-limit* #:buffer-known-line-number #:*counted-line-function*
           #:buffer-goto-line #:buffer-column #:buffer-move-to-column #:line-column-position
           #:buffer-local #:with-read-only-lifted #:buffer-mapping #:buffer-saved-number #:buffer-content-number #:buffer-mark-saved
           #:unique-buffer-name
           #:buffer-line-ending #:buffer-crlf-p #:buffer-newline #:detect-line-ending
           #:*find-file-functions* #:*kill-buffer-functions* #:*before-edit-functions* #:run-before-edit))
