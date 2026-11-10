;;;; lisp/git/package.lisp --- Package definition for tuatara.git.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

;; UIOP's define-package, as the names taken from the defaults may be defined
;; in files loaded after this one: each is made in TUATARA.DEFAULTS here, the
;; symbol that file then defines
(uiop:define-package #:tuatara.git
  (:use #:cl #:tuatara.command #:tuatara.modes #:tuatara.dates #:tuatara-user)
  (:import-from #:tuatara.io #:directory-name)
  (:import-from #:tuatara.runtime #:reporting-errors #:ignoring-errors-reported #:refusal #:refuse)
  (:import-from #:tuatara.defaults
                #:*after-save-functions* #:*command-refusal-functions* #:*file-changing-commands* #:*global-keymap*
                #:*made-text-keymap* #:*rimu-keymap* #:*trust-changed-functions*
                #:ask-project-trust #:buffer-in-mode #:buffer-private-folder #:check-not-saving #:column-text
                #:commands-for-buffers #:commands-for-mode #:conflict-look-through #:delete-private-folder
                #:diff-files #:diff-mode #:drop-selection #:entry-lines #:entry-member #:show-file-metadata
                #:folder-holds-evidence-p
                #:ssh-askpass-window-p
                #:follow-directories-for
                #:forget-abandoned-folders
                #:fresh-made-buffer
                #:git-commit-mode #:git-rebase-mode #:goto-shown #:kill-text #:killed-job-buffer-p #:rimu-archive-list
                #:list-entry-at #:list-folded #:list-section-at #:list-step-entry #:list-toggle-all-folds
                #:list-toggle-fold #:make-owned-folder #:make-rimu-listing #:marked-directory #:new-private-folder
                #:open-externally #:paths-changed #:private-cache-directory #:private-folder
                #:project-look-only-reason #:project-trusted-p #:quit-window #:read-native-path #:read-new-native-path
                #:read-whole-number #:real-folder #:rename-path-following #:render-made-table-list #:render-pieces
                #:rimu-buffer-name #:rimu-column-allowed-p #:rimu-column-in-background #:rimu-diff #:rimu-directory
                #:rimu-directory-p #:rimu-duplicates #:rimu-enter #:rimu-entry-at #:rimu-find #:rimu-find-text
                #:rimu-git #:rimu-goto-file #:rimu-home #:rimu-last-directory #:rimu-list-p #:rimu-make-list
                #:rimu-path #:rimu-quick-edit #:rimu-reading-layout #:rimu-rewound #:rimu-show-directory
                #:rimu-show-layout #:rimu-source #:rimu-source-buffer-name #:rimu-source-call-with-entry
                #:rimu-source-listing #:rimu-source-open #:rimu-source-refusal-words #:rimu-source-shown
                #:rimu-source-taken #:rimu-source-top #:*rimu-disk-commands*
                #:rimu-toggle #:rimu-toggle-picture #:rimu-toggle-sizes #:rimu-update #:rimu-verify-files
                #:save-buffer-with-care #:save-buffers-kill-editor #:say-in-background #:sectioned-pieces
                #:set-made-text #:rimu-set-column #:show-list-text #:shown-file #:special-mode #:table-list-rows
                #:text-columns #:list-toggle-section #:trash-changed #:view-made-text #:visit-path)
  (:documentation "Git from the editor. Every git command is run, never
through a shell nor on the editor's thread, with what keeps a repository not
trusted from running anything of its own. It has the status, log, commits,
branches, history and the rest of the git command, as git users expect them,
and rimu's git column and its lists of a folder as a commit left it. Other
parts of the editor run git through what is exported here.")
  (:export #:install-git
           ;; running git, for the change gutter, rimu, trust and packages
           #:run-git #:run-git-program #:*git-safety* #:git-network-environment #:git-failure-words #:git-hash-p
           #:repository-top #:git-stamps #:*git-look-seconds* #:git-blob-octets #:git-object-name))
