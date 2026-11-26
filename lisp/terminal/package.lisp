;;;; lisp/terminal/package.lisp --- Package definition for tuatara.terminal-emulator.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

;; UIOP's define-package, as the names taken from the defaults may be defined
;; in files loaded after this one: each is made in TUATARA.DEFAULTS here, the
;; symbol that file then defines
(uiop:define-package #:tuatara.terminal-emulator
  (:use #:cl #:tuatara.command #:tuatara.modes #:tuatara.dates #:tuatara-user)
  (:import-from #:tuatara.buffer #:lf-newlines)
  (:import-from #:tuatara.runtime #:define-record #:foreign-library-p #:define-c-function #:ignoring-errors-reported
                #:refuse #:define-setting)
  (:import-from #:tuatara.defaults #:point
                #:address-scheme #:centred-text #:choose-display #:current-column #:decode-picture #:display-fill-column
                #:evidence-path-p #:evidence-visit-reason #:current-line #:kill-ring-save #:line-numbers #:modal-sync
                #:mouse-set-point #:open-address #:picture-cell-pixels #:pieces-text #:regex-search-forward
                #:say-or-insert #:set-made-text #:set-modal-state #:soft-wrap #:special-mode #:spellcheck
                #:take-in-clipboard #:theme-terminal-colour #:untrusted-path-p #:uri-path #:visit-path)
  (:documentation "The terminal in a buffer: a program run on a pseudo-terminal, its screen
kept by the terminal library and shown as the buffer's text, keys typed there
sent to it. The rest of the editor starts it through its command and keymap,
and reads text with a terminal's colours through ANSI-PIECES.")
  (:export #:install-terminal #:terminal-mode #:ansi-pieces))
