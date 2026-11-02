;;;; lisp/git/package.lisp --- Package definition for tuatara.git.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

;; UIOP's define-package, as the names taken from the defaults may be defined
;; in files loaded after this one: each is made in TUATARA.DEFAULTS here, the
;; symbol that file then defines
(uiop:define-package #:tuatara.git
  (:use #:cl #:tuatara.command #:tuatara.modes #:tuatara.dates #:tuatara-user)
  (:import-from #:tuatara.runtime #:reporting-errors #:ignoring-errors-reported #:refusal)
  (:import-from #:tuatara.defaults
                #:*after-save-functions* #:*command-refusal-functions* #:*file-changing-commands* #:*global-keymap*
                #:*made-text-keymap* #:*trust-changed-functions*
                #:ask-project-trust #:buffer-in-mode #:buffer-private-folder #:check-not-saving #:column-text
                #:commands-for-buffers #:commands-for-mode #:conflict-look-through #:delete-private-folder #:diff-mode
                #:drop-selection #:entry-lines #:entry-member #:folder-holds-evidence-p #:follow-directories-for
                #:forget-abandoned-folders #:fresh-made-buffer #:git-commit-mode #:git-rebase-mode #:goto-shown
                #:kill-text #:killed-job-buffer-p #:list-entry-at #:list-folded #:list-section-at #:list-step-entry
                #:list-toggle-all-folds #:list-toggle-fold #:make-owned-folder #:marked-directory #:new-private-folder
                #:paths-changed #:private-cache-directory #:private-folder #:project-look-only-reason
                #:project-trusted-p #:quit-window #:read-native-path #:read-new-native-path #:read-whole-number
                #:real-folder #:rename-path-following #:render-made-table-list #:render-pieces #:rimu-directory
                #:rimu-list-p #:rimu-rewind #:rimu-rewind-leave #:rimu-rewind-show #:rimu-rewound-p #:rimu-show-nearest
                #:save-buffer-with-care #:save-buffers-kill-editor #:say-in-background #:sectioned-pieces
                #:set-made-text #:show-list-text #:shown-file #:special-mode #:table-list-rows #:toggle-list-section
                #:trash-changed #:view-made-text #:visit-path)
  (:documentation "Git from the editor: every git command run, never through a shell nor on the
editor's thread, with what keeps a repository not trusted from running anything
of its own; and the status, log, commits, branches, history and the rest of
the git command, as Magit has them. Other parts of the editor run git and read
what it says through what is exported here.")
  (:export #:install-git
           ;; running git
           #:run-git #:run-git-program #:*git-safety* #:git-network-environment #:git-failure-words #:git-hash-p
           #:repository-top #:git-stamps #:*git-look-seconds* #:git-blob-octets #:git-object-name
           ;; git in the background, and its commits read
           #:start-git-job #:git-job-output #:git-job-try #:parse-git-commits #:parse-git-commit-fields
           #:git-commit-hash #:git-commit-subject #:git-commit-ago #:*git-commit-format* #:*git-shown-limit*
           ;; a file's history, for rimu
           #:git-blame #:git-diff-file #:git-log-file #:git-file-menu #:git-rewind-file #:git-rewind-words
           #:*git-rewind-keymap*))
