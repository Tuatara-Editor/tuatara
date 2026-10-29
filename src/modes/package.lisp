;;;; src/modes/package.lisp --- Package definition for tuatara.modes.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(defpackage #:tuatara.modes
  (:use #:cl #:alexandria)
  (:documentation "Major modes: a class tree rooted at FUNDAMENTAL-MODE, each mode with a keymap that
inherits its parent's, a modal class for modal editing, hooks, and the rules that choose a
mode for a file by name, extension or #! line. Behaviour that differs by mode is a
generic function specialised on the mode class.")
  (:export #:fundamental-mode #:define-major-mode #:register-major-mode #:forget-major-mode
           #:mode-info #:find-mode #:mode-keymap #:mode-lighter #:mode-names #:mode-file-rules
           #:set-major-mode #:find-or-make-buffer #:buffer-major-mode #:set-auto-mode #:mode-for-file #:mode-for-size #:mode-largest-file #:add-mode-hook
           #:mode-modal-class #:mode-enter #:mode-exit #:mode-syntax-highlights #:mode-marks-point-p #:mode-keeps-place-p #:mode-line-extra #:mode-comment-syntax #:mode-block-comment-syntax
           #:mode-indent-lines
           #:mode-info-p #:mode-info-name #:mode-info-parent #:mode-info-doc #:mode-info-lighter
           #:mode-info-keymap #:mode-info-modal-class #:mode-info-hooks #:mode-precedence #:shebang-interpreter
           #:names-mode-p #:mode-setting #:buffer-setting))
