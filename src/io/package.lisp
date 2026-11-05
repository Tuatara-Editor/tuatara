;;;; src/io/package.lisp --- Package definition for tuatara.io.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(defpackage #:tuatara.io
  (:use #:cl #:alexandria)
  (:import-from #:tuatara.runtime #:refuse)
  (:import-from #:tuatara.text #:drop-cached)
  (:import-from #:tuatara-user #:*default-directory* #:*cache-directory* #:*state-directory* #:*sandbox-program* #:*sandbox-seconds* #:*sandbox-memory* #:*sandbox-input-limit* #:*sandbox-total-memory* #:*sandbox-seccomp* #:*program-output-limit* #:*deepest-folder-tree*
                #:*save-in-place*)
  (:documentation "Files: visiting them so that no other program can change the text shown,
saving them so that no crash or power cut loses a file, noticing when they change on
disk, and reverting; on every Linux file system, with faster paths where a file system
offers them; copying, moving, deleting and wiping files and folders, the trash, and
running programs, shut in a sandbox when they read files from elsewhere.")
  (:export #:buffer-file-stamp #:buffer-plaintext-p)
  (:export #:octets-words
   ;; errors
   #:file-system-error #:file-system-error-operation #:file-system-error-path #:file-system-error-reason #:file-system-error-errno
   #:file-changed-on-disk #:file-changed-on-disk-path
   ;; what the system says about files
   #:stamp #:stamp-p #:path-stamp #:path-size #:fd-stamp #:stamp-kind #:make-stamp #:same-content-p
   #:stamp-dev #:stamp-ino #:stamp-size #:stamp-mtime #:stamp-nlink #:stamp-mode #:stamp-uid #:stamp-gid #:stamp-ctime #:file-holes-p
   #:fd-file-system #:file-system-class #:free-bytes #:free-inodes #:set-path-mode #:umask-bits #:now-nanoseconds #:read-only-mount-p #:can-give-owner-p #:writable-p #:writing-refused #:host-writing-refusal #:writing-refusal #:writing-refused-anywhere-p #:check-writing #:check-tree-writing #:fd-real-path #:host-say-writing #:tell-writing
   #:take-read-lease #:lease-breaking-p #:release-lease
   ;; copies and scratch files
   #:clone-fd #:copy-fd-range #:copy-fd-contents #:read-fd-octets #:read-fd-into #:octet-pieces #:make-octet-pieces #:add-octet-piece #:octet-pieces-size #:joined-octets #:wipe-octet-pieces #:read-fd-at #:read-fd-exactly #:advise-will-need #:write-octets #:writer #:make-writer #:writer-p #:writer-fd #:writer-flush #:writer-length #:writer-octets #:writer-sap #:writer-rope #:writer-spaces #:copy-xattrs #:host-save-transform #:wipe-file #:copy-on-write-p #:fd-xattr-names #:fd-xattr-value #:path-times
   ;; waiting
   #:terminal-window-size #:set-terminal-window-size #:wait-for-fds #:make-wake-pipe #:wake #:drain-wake-pipe #:close-wake-pipe
   #:poll-fd #:socket-pair #:shut-socket #:listen-at #:accept-on #:connect-to #:peer-credentials #:peer-process #:pidfd-process #:mount-namespace #:+most-socket-path+ #:send-with-descriptors #:receive-with-descriptors #:+most-descriptors+
   #:native #:remove-file #:touch-path #:make-symbolic-link #:make-hard-link #:copy-regular-file #:copy-open-file-to #:scratch-name #:percent-encoded #:url-path #:path-file-name #:percent-decoded #:percent-decoded-octets #:path-within-p #:real-path #:real-path-so-far #:real-path-or-parent #:really-within-p #:ensure-directories #:native-file-name #:native-file-stem #:native-file-type #:file-octets #:file-text #:home-directory-names #:home-directory #:expand-path #:default-directory #:directory-pathname #:directory-entries #:list-directory #:listed #:make-listed #:listed-p #:listed-name #:listed-octets #:listed-stamp #:listed-used #:listed-device #:listed-target #:listed-target-stamp #:listed-label #:read-entry-start #:call-with-entry #:call-with-file #:read-fd-start #:octets-kind #:pdf-header-at #:octets-integer #:octets-signed #:octets-put-integer #:octets-at-p #:octets-search #:octets-ascii #:base64-octets #:base64-string #:openpgp-packet-header #:openpgp-packet-p #:octets-picture-size #:tiff-octets-p #:tiff-entries #:box-contents-start #:octets-reader #:j2k-header #:jp2-header #:jxl-header #:jxl-file-header #:jpeg-segments #:box-children #:box-find #:octet-at #:*compound-file-signature* #:kind-name-disagrees-p #:sandbox-status #:sandbox-status-now #:sandbox-run #:sandbox-start #:end-sandbox #:end-sandboxes #:sandbox-memory-scope-p #:real-place #:sealed-copy #:memory-file #:sealed-file-size #:sandbox-run-on-file #:sandbox-find-program #:system-program #:sandbox-filtered-p #:*sandbox-path* #:*sandbox-file-limit* #:find-program #:program-search-path #:*default-program-path* #:program-missing #:program-output #:program-environment #:*own-variables* #:program-variable #:variable-listed-p #:listed-environment #:host-program-environment #:call-with-program #:done-with-program #:user-shell #:session-command #:session-leader #:session-processes #:signal-session #:process-unreaped-p #:filter-through-program #:*program-said-limit* #:command-words #:setpriv-command #:desktop-p #:askpass-program #:*askpass-programs* #:*restricted-variables* #:restricted-environment #:version-numbers #:version-at-least-p
   #:rename-path #:make-directory #:path-join #:parent-path #:copy-path-tree #:delete-tree #:move-path #:trash-path #:trash-for #:home-trash #:scratch-directory #:buffer-scratch-directory #:kind-words #:size-words
   #:open-scratch-file #:open-unnamed-file #:name-unnamed-file #:cache-directory #:chosen-directory #:directory-in-memory-p #:directories-in-memory-words #:data-home #:read-line-bounded #:read-stream-octets #:session-log #:move-session-log #:state-directory #:write-private-file #:write-new-file #:replace-file #:open-lock-file #:lock-file-held-p #:call-with-state-lock
   #:sync-directory #:run-program-again #:*saved-program* #:slash-ended #:directory-name #:setsid-command #:drive-trash-sound-p
   #:ensure-private-directory #:path-kind #:clean-path #:path-folder-and-name #:owner-name #:owner-group-name #:user-font-directories
   ;; new names, and what a program shut in came to
   #:random-name #:numbered-name #:create-free-file #:copy-to-free-name
   ;; files read whole, zip archives
   #:fd-octets #:crc32 #:zip-octets #:zip-entries #:zip-entry-octets #:zip-locked-p #:inflate #:zip-error
   #:*zip-largest-entry* #:*zip-largest-ratio*
   ;; memory outside Lisp
   #:with-octets-sap #:with-foreign-memory #:sap-octets #:null-pointer #:null-pointer-p
   #:sap-string #:with-c-string #:c-type #:define-c-function
   ;; visiting, checking, saving, reverting
   #:*memory-read-limit* #:*lease-large-files* #:*clone-large-files* #:*snapshot-large-files* #:*snapshot-free-margin*
   #:open-for-reading #:open-file-text #:visit-file #:buffer-for-file #:copy-descriptor-to-scratch #:scratch-file-text #:safe-visit-p #:end-safe-visit #:safe-visit-reason #:host-visit-safely-reason #:binary-head-p #:+binary-sample+ #:file-head #:call-with-file-heads #:open-examined
   #:*index-job-hook* #:index-job-of #:index-job-status #:index-progress #:start-indexing #:finish-indexing #:stop-indexing
   #:counted-line-number #:buffer-known-line-number #:counted-line-start #:counted-lines #:file-isolation #:file-status #:finish-snapshots #:snapshots
   #:check-file #:file-disk-state #:finish-leases #:leases-moving-p #:take-file-looks #:directory-class
   #:watch-files #:watch-directories #:take-directory-news #:*poll-interval* #:*settle-time* #:*burst-time* #:*settle-limit* #:saving-p #:append-from-disk
   #:resolve-links #:write-text-file #:save-buffer-file
   #:start-save #:finish-saves #:saves-running #:save-of #:changes-unsaved-p #:save-progress
   #:save-job #:save-job-p #:save-job-buffer #:save-job-status #:save-job-error #:save-job-path
   #:save-job-target #:save-job-total #:save-job-started #:save-job-thread
   #:revert-buffer-file))
