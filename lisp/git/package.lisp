;;;; lisp/git/package.lisp --- Package definition for tuatara.git.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

;; UIOP's define-package, as the names taken from the defaults may be defined
;; in files loaded after this one: each is made in TUATARA.DEFAULTS here, the
;; symbol that file then defines
(uiop:define-package #:tuatara.git
  (:use #:cl #:tuatara.command #:tuatara.modes #:tuatara.dates #:tuatara-user)
  (:import-from #:tuatara.io #:directory-name)
  (:import-from #:tuatara.runtime #:define-record #:reporting-errors #:ignoring-errors-reported #:refusal #:refuse
                #:define-setting)
  (:import-from #:tuatara.defaults
                #:*after-save-functions* #:ask-project-trust #:buffer-in-mode #:buffer-private-directory
                #:call-answering-ssh #:call-with-details-shown #:check-not-saving #:column-text
                #:*command-refusal-functions*
                #:commands-for-mode #:conflict-look-through #:delete-private-directory #:diff-files #:diff-mode
                #:directory-holds-evidence-p #:drop-selection #:entry-lines #:entry-member #:*file-changing-commands*
                #:directories-followed-changed #:follow-directories-for #:forget-abandoned-directories
                #:fresh-made-buffer #:git-commit-mode
                #:git-rebase-mode #:*global-keymap* #:goto-shown #:kill-text #:killed-job-buffer-p #:list-entry-at
                #:list-folded #:list-section-at #:list-step-entry #:list-toggle-all-folds #:list-toggle-fold
                #:list-toggle-section #:*made-text-keymap* #:make-owned-directory #:make-rimu-listing
                #:marked-directory #:new-private-directory #:open-externally #:paths-changed #:private-cache-directory
                #:private-directory #:project-look-only-reason #:project-trusted-p #:quit-window #:read-native-path
                #:read-new-native-path #:read-whole-number #:real-directory #:rename-path-following
                #:render-made-table-list #:render-pieces #:rimu-archive-list #:rimu-buffer-name #:rimu-column-allowed-p
                #:rimu-column-in-background #:rimu-diff #:rimu-directory #:rimu-directory-p #:*rimu-disk-commands*
                #:rimu-duplicates #:rimu-enter #:rimu-find #:rimu-find-text #:rimu-git #:rimu-goto-file #:rimu-home
                #:*rimu-keymap* #:rimu-last-directory #:rimu-list-p #:rimu-make-list #:rimu-path #:rimu-quick-edit
                #:rimu-reading-layout #:rimu-rewound #:rimu-set-column #:rimu-show-directory #:rimu-show-layout
                #:rimu-show-nearest #:rimu-source #:rimu-source-buffer-name #:rimu-source-call-with-entry
                #:rimu-source-listing #:rimu-source-open #:rimu-source-refusal-words #:rimu-source-shown
                #:rimu-source-taken #:rimu-source-top #:rimu-toggle #:rimu-toggle-picture
                #:rimu-update #:rimu-verify-files #:save-buffer-with-care #:save-buffers-kill-editor
                #:plain-path-p #:say-in-background #:sectioned-pieces #:set-made-text #:show-file-metadata
                #:show-list-text #:shown-file #:special-mode #:ssh-askpass-window-p #:ssh-refusal-words
                #:table-list-rows #:text-columns #:trash-changed
                #:*trust-changed-functions* #:trusted-project-key #:trusted-project-key-current-p #:view-made-text
                #:visit-path)
  (:documentation "Git from the editor. Every git command is run, never
through a shell nor on the editor's thread, with what keeps a repository not
trusted from running anything of its own. It has the status, log, commits,
branches, history and the rest of the git command, as git users expect them,
and rimu's git column and its lists of a directory as a commit left it. Other
parts of the editor run git through what is exported here.")
  (:export #:install-git
           ;; running git, for the change gutter, rimu, trust and packages
           #:run-git #:run-git-program #:*git-safety* #:git-network-environment #:git-failure-words #:git-hash-p
           #:repository-top #:git-stamps #:*git-look-seconds* #:git-blob-octets #:git-object-name))
