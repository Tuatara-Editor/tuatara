;;;; lisp/terminal/package.lisp --- Package definition for tuatara.terminal-emulator.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

;; UIOP's define-package, as the names taken from the defaults may be defined
;; in files loaded after this one: each is made in TUATARA.DEFAULTS here, the
;; symbol that file then defines
(uiop:define-package #:tuatara.terminal-emulator
  (:use #:cl #:tuatara.command #:tuatara.modes #:tuatara.dates #:tuatara-user)
  (:shadowing-import-from #:tuatara.defaults #:point)
  (:import-from #:tuatara.buffer #:lf-newlines)
  (:import-from #:tuatara.runtime #:foreign-library-p #:foreign-call #:ignoring-errors-reported #:refuse)
  (:import-from #:tuatara.text #:whole-number)
  (:import-from #:tuatara.defaults
                #:address-scheme #:centred-text #:choose-display #:cursor-column #:decode-picture #:evidence-path-p
                #:evidence-visit-reason #:hl-line #:kill-ring-save #:line-numbers #:modal-sync #:mouse-set-point
                #:open-address #:picture-cell-pixels #:pieces-text #:regex-search-forward #:safe-directory-of #:say-or-insert
                #:set-made-text #:set-modal-state #:soft-wrap #:special-mode #:spellcheck #:take-in-clipboard
                #:theme-terminal-colour #:uri-path #:visit-path #:visual-line)
  (:documentation "The terminal in a buffer: a program run on a pseudo-terminal, its screen
kept by the terminal library and shown as the buffer's text, keys typed there
sent to it. The rest of the editor starts it through its command and keymap,
and reads text with a terminal's colours through ANSI-PIECES.")
  (:export #:install-terminal #:terminal-mode #:ansi-pieces))
