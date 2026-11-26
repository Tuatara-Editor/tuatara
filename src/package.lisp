;;;; src/package.lisp --- The umbrella package.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

;; UIOP's define-package, as lisp/api.lisp adds the names it exports: loaded
;; again, this package is put back as written here, with no warning
(uiop:define-package #:tuatara
  (:use #:cl)
  (:documentation "The names packages are written against: the version here, and every name a
package may use once the defaults are loaded (lisp/api.lisp). A package's own
Lisp package uses this one; TUATARA-USER, which init files are read in, does too.")
  (:export #:*version* #:version #:*build* #:*built-at* #:version-line))

(defpackage #:tuatara-user
  (:use #:cl #:tuatara)
  (:documentation "The package user configuration files are read in: BIND-KEY and UNBIND-KEY are
available unqualified, and the public API of TUATARA is used.")
  (:export #:bind-key #:unbind-key #:define-recorded-keys #:set-face #:load-theme #:load-package #:define-theme
           #:define-named-pattern
           #:package-source #:load-package-file #:package-file #:*package-ask-before-loading*
           #:*package-check-signatures* #:*package-sources* #:*light-theme* #:*dark-theme* #:*startup-buffer*
           #:*auto-scratch-buffers* #:*scratch-text-kept*
           #:set-colouring #:set-indentation
           ;; the API's names of the editor's message and current buffer
           #:message #:current-buffer
           ;; options an init file sets by these names, defined where they are used
           #:*messages-kept* #:*mode-line-name-limit* #:*sort-ignores-case* #:*reload-changed-files*
           #:*line-numbers-default*
           #:*indent-new-lines* #:*tab-width* #:*indent-with-tabs* #:*tab-width-modes* #:*indent-with-tabs-modes*
           #:*show-matching-bracket* #:*backtrace-on-error* #:*tab-key-action* #:*say-matching-bracket*
           #:*debug-mode* #:*debug-log-limit* #:*idle-collection-seconds* #:*csv-separator* #:*list-header-lock*
           #:*csv-header*
           ;; saving work and the session
           #:*auto-save* #:*auto-save-directory* #:*auto-save-idle* #:*auto-save-interval* #:*auto-save-size-limit*
           #:*keep-session*
           #:*keep-session-interval* #:*ext4-save-in-place*
           ;; encryption, and wiping files
           #:*gpg-program* #:*gpg-passphrase-from* #:*metadata-look-up-key-owners* #:*wipe-passes* #:*cache-directory*
           #:*cache-keep* #:*state-directory* #:*packages-directory* #:*package-size-limit* #:*archive-seconds*
           #:*archive-free-reserve* #:*archive-listing-limit* #:*differences-limit* #:*gpg-loopback-warning*
           ;; language servers and formatters
           #:*language-servers* #:*language-formatters* #:*lsp-languages*
           #:*lsp-signature-help* #:*lsp-inlay-hints*
           #:*lsp-wait* #:*lsp-largest-message* #:*lsp-largest-document* #:*lsp-watch-files* #:*lsp-watch-skipped*
           #:*lsp-idle-delay* #:*lsp-hover-echo* #:*lsp-highlight-symbol* #:*lsp-code-lenses* #:*lsp-outline*
           #:*lsp-diagnostics-display* #:*lsp-diagnostics-style* #:*lsp-semantic-tokens* #:*lsp-semantic-faces*
           #:*format-on-save*
           #:*format-in-sandbox* #:*format-seconds*
           ;; the programs that run the grammars
           #:*syntax-helper-unconfined*
           #:*grammars-on*
           ;; file-local variables, snippets, bookmarks and version control
           #:*file-local-variables* #:*snippet-directories* #:*snippet-tab* #:*bookmarks-kept*
           #:*bookmark-set-asks-categories* #:*git-log-length*
           #:*git-recent-commits* #:*git-menus* #:*git-slim-actions* #:*git-diff-lines* #:*git-section-lines*
           #:*git-watch-seconds* #:*vc-gutter-enabled*
           ;; rimu, the lists of directories
           #:*rimu-details* #:*rimu-hidden-columns* #:*rimu-permissions* #:*rimu-dotfiles* #:*rimu-git-info*
           #:*rimu-directory-sizes* #:*rimu-sort*
           #:*rimu-directories-first* #:*rimu-ignored-extensions*
           #:*rimu-reuse-lists* #:*rimu-preview* #:*rimu-preview-delay* #:*rimu-preview-size* #:*rimu-follow-changes*
           ;; the directory of a buffer with no file, and what makes a project
           #:*no-file-directory* #:*project-root-markers* #:*compile-command* #:*compile-output-limit*
           #:*last-used-first* #:*path-delete-by-branches* #:*complete-paths* #:*complete-paths-modes*
           ;; completing a word from the words in the buffers
           #:*word-completion-keep-case*
           ;; Org: notes, the agenda, capture, code files and dynamic blocks
           #:*org-startup-indented* #:*org-bullets* #:*org-list-bullets* #:*org-checkbox-signs*
           #:*org-complete-checkboxes* #:*org-return-cycles-checkboxes* #:*org-return-cycles-todo*
           #:*org-startup-shows-pictures*
           #:*org-log-checkboxes*
           #:*org-standard-todo-keywords* #:*org-todo-keywords-not-cycled* #:*org-todo-keyword-faces*
           #:*org-todo-follows-checkboxes* #:*org-cycle-hide-drawers* #:*org-fold-metadata* #:*org-hide-leading-stars*
           #:*org-cycle-include-plain-lists* #:*org-return-follows-link* #:*org-src-languages*
           #:*code-files-make-directories* #:*org-link-descriptive* #:*org-clock-into-drawer* #:*org-log-done*
           #:*org-log-into-drawer* #:*org-log-repeat* #:*org-log-reschedule* #:*org-log-redeadline*
           #:*org-agenda-files* #:*capture-template-directories* #:*org-refile-targets*
           #:*org-deadline-warning-days* #:*org-agenda-log-mode* #:*org-agenda-span*
           #:*org-agenda-start-on-weekday* #:*org-agenda-time-grid* #:*org-agenda-category-width*
           #:*org-agenda-untimed-text* #:*org-agenda-event-kind-text* #:*org-agenda-task-kind-text*
           #:*org-agenda-reminder-kind-text* #:*org-reminder-tag*
           #:*org-agenda-date-in-title* #:*org-agenda-current-time-string*
           #:*org-agenda-custom-commands* #:*org-tags-column* #:*org-agenda-autosave-files*
           #:*org-agenda-refresh-minutes*
           #:*org-notify-tag* #:*org-notify-before* #:*org-notify-day-time* #:*notify-program*
           #:*org-notify-alarms*
           #:*org-toc-on-save* #:*org-read-date-popup-calendar* #:*org-read-date-in-calendar* #:*org-toc-max-depth*
           #:*org-toc-style* #:*org-depth-limit* #:*org-formula-depth-limit*
           #:define-dblock-writer #:define-clocktable-formatter
           ;; Markdown
           #:*markdown-toc-on-save*
           ;; the calendar and the journal
           #:*calendar-week-start-day* #:*calendar-months* #:*calendar-months-per-row* #:*calendar-mark-days*
           #:*journal-directory* #:*journal-keyword* #:*journal-title-format* #:*journal-interval*
           #:*journal-file-type* #:*journal-file-name* #:*journal-org-header*
           ;; the terminal
           #:*kitty-keyboard* #:*altgr-as-meta* #:*terminal-send-escape* #:*terminal-editor-keys*
           #:*terminal-paste-keys* #:*terminal-title-limit*
           #:*terminal-compress-after* #:*terminal-picture-memory* #:*terminal-place* #:*terminal-size*
           #:*terminal-width* #:*terminal-program* #:*terminal-scrollback*
           ;; marks, places kept, input methods, help windows and Escape
           #:*help-window-select* #:*note-template-file-limit* #:*escape-quits* #:*remember-places*
           #:*remembered-places-limit* #:*default-input-method* #:turn-on-input-method #:*mark-ring-length*
           #:*set-mark-command-repeat-pop*
           ;; the sandbox programs decoding files run in
           #:*sandbox* #:*sandbox-seconds* #:*sandbox-memory* #:*sandbox-input-limit* #:*sandbox-memory-copy-limit*
           #:*program-output-limit*
           #:*directory-tree-depth-limit* #:*office-note-size-limit* #:*office-value-length-limit*
           #:*sandbox-total-memory*
           #:*sandbox-seccomp* #:*picture-memory* #:*pdf-view-dark-pages* #:*office-view-pages* #:*user-full-name*
           #:*open-externally-program* #:*open-externally-programs* #:*hex-view-binary-files* #:*open-externally-kinds*
           #:*open-externally-ask* #:*rimu-delete-to-trash* #:*latex-engines*
           #:*latex-main-font* #:*latex-sans-font*
           #:*latex-mono-font* #:*latex-redaction-commands* #:*logos-directory*
           ;; the cursor, and its look in each of the modal states
           #:*cursor-shape* #:*cursor-colour*
           #:*tuatara-normal-cursor-shape* #:*tuatara-normal-cursor-colour* #:*tuatara-insert-cursor-shape*
           #:*tuatara-insert-cursor-colour* #:*tuatara-support-cursor-shape* #:*tuatara-support-cursor-colour*
           ;; where buffers are shown, and window sizes
           #:place-buffers #:*split-height-threshold* #:*split-width-threshold*
           #:*window-min-height* #:*window-min-width* #:*scroll-margin* #:*scroll-recenter* #:*wheel-scroll-lines*
           ;; files with formatted names, searching files
           #:set-formatted-file-header
           #:*formatted-file-directory* #:*formatted-file-extension* #:*formatted-file-keywords*
           #:*formatted-file-sort-keywords* #:*formatted-file-prompts* #:*formatted-file-type*
           #:*formatted-file-types* #:*search-files-min-input* #:*search-files-limit* #:*indexed-directories*
           #:*indexed-files-most*
           #:*daemon-indicator* #:*processing-indicator* #:*indexing-indicator* #:*searching-indicator*
           #:*indexes-kept-on-disk* #:*preview-files* #:*preview-formatted-files* #:*preview-formatted-search*
           #:*preview-file-search* #:*preview-buffers* #:*search-results-save-files*
           #:*search-files-lines-limit* #:*typing-pause* #:*line-record-modes*
           ;; the kill ring and the system clipboard
           #:*kill-ring-length* #:*kill-ring-octets* #:*undo-limit* #:*use-system-clipboard* #:*clipboard-size-limit*
           #:*show-completion-count* #:*prompt-above-candidates* #:*key-hint-rows* #:*fill-column* #:*comment-column* #:*leader-key*
           #:*sentence-end-double-space* #:*extra-sentence-abbreviations* #:*removed-sentence-abbreviations*
           #:*git-commit-fill-column* #:*fill-paragraph-limit* #:*fill-ask-column* #:*global-current-line-mode*
           #:*global-soft-wrap-mode* #:*soft-wrap-modes* #:*centred-text-width* #:*centred-text-modes*
           #:*soft-wrap-at-fill-column* #:*soft-wrap-indicator* #:*global-display-fill-column-mode*
           #:*display-fill-column-modes*
           #:*current-line-modes* #:*global-current-column-mode* #:*current-column-modes* #:*global-spellcheck-mode*
           #:*spellcheck-modes* #:*spellcheck-languages* #:*spellcheck-excluded-faces* #:*spellcheck-comment-faces*
           #:*spellcheck-string-faces* #:*global-centred-text-mode* #:*line-numbers-modes*
           #:*global-inline-pictures-mode* #:*inline-pictures-modes* #:*inline-picture-width* #:*formula-preamble*
           #:*global-invisible-characters-mode* #:*invisible-characters-modes* #:*invisible-characters-when-safe*
           #:*global-text-direction-mode* #:*text-direction-modes* #:*text-direction-when-safe*
           ;; files visited lately
           #:*remember-recent-files* #:*recent-files-limit* #:*recent-files-auto-cleanup* #:*relative-age-limit*
           ;; pairs of delimiters
           #:*pair-delimiters* #:*auto-close-mode* #:*auto-close-delimiters* #:*auto-close-safe-exclusions*
           ;; cursors
           #:*cursors-limit* #:*modal-editing* #:*insert-escape-keys* #:*mode-line-tuatara* #:*rectangle-cursors-limit*
           #:*split-limit-ask* #:*split-limit* #:*tuatara-normal-tab-modes*
           ;; the window frontend
           #:*gui-font* #:*gui-renderer* #:*gui-title* #:*gui-padding* #:*window-colours-from-desktop* #:*tui-padding*
           #:*tui-pictures* #:*tui-mouse* #:*tui-colours* #:*follow-desktop-style* #:*open-dropped-files-safely*
           #:*untrusted-directories* #:*open-binary-files-safely* #:*evidence-directories*
           ;; files on another machine
           #:*remote-seconds* #:*remote-preview-size-limit* #:*client-kept-without-buffers*))

;; the system calls that take a file's name are called, not copied into each caller, so that what watches them
;; (test/fences.lisp) sees every one: a call costs nothing beside the system's own work
(declaim (notinline sb-posix:open sb-posix:creat sb-posix:opendir sb-posix:access sb-posix:chdir sb-posix:unlink
                    sb-posix:rmdir sb-posix:mkdir sb-posix:mkfifo sb-posix:rename sb-posix:link sb-posix:symlink
                    sb-posix:chmod sb-posix:chown sb-posix:lchown sb-posix:truncate))
