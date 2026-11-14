;;;; lisp/lsp/package.lisp --- Package definition for tuatara.lsp.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

;; UIOP's define-package, as the names taken from the defaults may be defined
;; in files loaded after this one: each is made in TUATARA.DEFAULTS here, the
;; symbol that file then defines
(uiop:define-package #:tuatara.lsp
  (:use #:cl #:tuatara.command #:tuatara.modes #:tuatara.dates #:tuatara-user)
  (:shadowing-import-from #:tuatara.defaults #:point)
  (:import-from #:tuatara.buffer #:buffer-read-only-p)
  (:import-from #:tuatara.runtime #:define-record #:reporting-errors #:ignoring-errors-reported #:refusal #:refuse
                #:run-hook-until-success #:define-setting)
  (:import-from #:tuatara.defaults
                #:*after-save-functions* #:*before-save-functions* #:*code-file-extensions* #:*trusted*
                #:*outside-work-functions* #:*places-in-force* #:*save-path* #:*save-then*
                #:*selection-ranges-functions*
                #:add-gutter-provider #:buffer-in-mode #:buffer-indent-tabs-p #:buffer-tab-width #:buffer-visiting
                #:commands-for-mode #:directories-followed-changed #:follow-directories-for #:glob-alternatives
                #:glob-match #:go-to-definition
                #:goto #:goto-shown #:indent-lines #:indent-region #:language-modes #:language-name #:language-p
                #:keep-shown-current #:list-entry-noun #:list-visit #:make-definition #:make-snippet #:marked-directory
                #:mode-completion-at-point #:mode-language #:mode-find-definitions #:mode-name-at-point #:mode-outline
                #:org-block-header-arguments #:org-blocks-key #:org-buffer-parse #:org-buffer-parts
                #:org-commented-p #:org-mode #:org-node-children #:org-node-type #:org-parse-current-p
                #:org-parse-tree #:org-part-at #:org-part-code-lines #:org-part-index #:org-part-start
                #:org-parts-count #:org-property #:org-src-language #:path-uri #:places-buffer #:places-name
                #:prog-mode #:project-root #:project-trusted-p #:push-xref-marker #:put-snippet #:put-text-by-lines
                #:real-directory #:render-pieces #:safe-directory-of #:save-buffer-with-care
                #:say-places-end #:show-definitions #:show-help #:special-mode #:src-block #:state-list-drop
                #:step-place #:trusted-when-asked-p #:uri-path #:use-places #:visit-entry #:visit-path
                #:walk-directory #:word-bounds-at #:xref-find-definitions #:xref-go-back)
  (:documentation "Language servers and formatters: a server run for each project and language,
files kept in step with it, and what it knows shown in the editor (what it
found wrong, completion, definitions, hints, symbols, folds, the code blocks
of Org notes); a formatter run on a buffer before it is saved. The rest of the
editor installs it and asks what tools a language has here.")
  (:export #:install-lsp #:install-diagnostics-place #:language-tools-words #:*brought-language-servers-directory*
           #:language-server-program-name-p #:language-server-programs-here))
