;;;; lisp/packages/package.lisp --- Package definition for tuatara.packages.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

;; UIOP's define-package, as the names taken from the defaults may be defined
;; in files loaded after this one: each is made in TUATARA.DEFAULTS here, the
;; symbol that file then defines
(uiop:define-package #:tuatara.packages
  (:use #:cl #:tuatara.command #:tuatara.modes #:tuatara.dates #:tuatara-user)
  ;; a parameter of this name, apart from Common Lisp's function
  (:shadow #:compile)
  (:import-from #:tuatara.io #:directory-name)
  (:import-from #:tuatara.runtime #:reporting-errors #:ignoring-errors-reported #:refusal #:refuse #:define-setting)
  (:import-from #:tuatara.git
                #:*git-safety* #:git-failure-words #:git-hash-p #:git-network-environment #:run-git-program)
  (:import-from #:tuatara.defaults
                #:*display-toggles* #:*global-keymap* #:*init-file-stop* #:*mode-commands*
                #:act-on-list #:buffer-in-mode #:commands-for-mode #:config-file #:define-state-list #:delete-entries
                #:display-toggle-name #:entry-list-keys #:entry-preview-again #:entry-preview-keys #:entry-preview-width
                #:escape-path #:field-line-key #:keep-shown-current #:list-entry-at #:list-entry-noun #:list-mark-face
                #:list-mark-sign #:list-preview-p #:list-question #:map-text-forms #:marked-list-mode #:mode-named
                #:outside-program-install-words #:preview-entry #:render-table-list #:rimu-show-directory
                #:rimu-tree-size #:show-help #:side-preview-note #:state-line-fields #:state-list-drop
                #:state-list-entries #:state-list-find #:state-list-put #:table-columns-width #:unescape-path
                #:unmark-list #:visit-entry)
  (:documentation "Packages: where they come from, fetched in the background, checked, asked
about, compiled apart from the editor, loaded and listed. The rest of the
editor installs it, reads the init file with the packages it loads timed,
makes a package's settings, and runs a compile of a package asked for in a
process of its own; a package repository's tests load its package through
LOAD-PACKAGE-FOR-TEST.")
  (:export #:install-packages #:packages-directory #:call-with-start-packages #:define-package-setting
           #:declare-package-needs #:*package-needs*
           #:claim-package-setting #:compile-package-child #:load-package-for-test))
