;;;; lisp/compile/package.lisp --- Package definition for tuatara.compile.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

;; UIOP's define-package, as the names taken from the defaults may be defined
;; in files loaded after this one: each is made in TUATARA.DEFAULTS here, the
;; symbol that file then defines
(uiop:define-package #:tuatara.compile
  (:use #:cl #:tuatara.command #:tuatara.modes #:tuatara.dates #:tuatara-user)
  ;; the command of this name, apart from Common Lisp's function
  (:shadow #:compile)
  (:import-from #:tuatara.io #:octets-words)
  (:import-from #:tuatara.runtime #:ignoring-errors-reported)
  (:import-from #:tuatara.defaults
                #:buffer-in-mode #:commands-for-mode #:list-entry-noun #:list-visit #:places-buffer #:places-name
                #:project-root #:say-places-end #:set-made-text #:show-file-line #:special-mode #:step-place
                #:trusted-when-asked-p #:use-places #:visit-entry)
  (:documentation "Running a build or test command typed in the minibuffer, from the root of the
project, its output shown as it comes and the places it names stepped
through. The rest of the editor only installs it.")
  (:export #:install-compile))
