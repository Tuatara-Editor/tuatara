;;;; src/runtime/package.lisp --- Package definition for tuatara.runtime.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(defpackage #:tuatara.runtime
  (:use #:cl #:alexandria)
  (:documentation "What every part of the editor runs on: records, the host the core asks, where its
own files are, C functions, threads and the pool of workers that runs pieces of work in
parallel, the number of processors, work stopped at once, waiting with a time limit, locks,
jobs in the background and their one runner, the running of hooks, settings, refusals and
the reports of debug mode.")
  (:export #:define-record #:document-accessors #:one-line #:control-code-p #:host #:*host* #:host-wake #:jobs-of
           #:make-spares #:take-spare #:give-back-spare #:with-spare
           #:resource-directories #:own-variable #:local-time-stamp #:foreign-library-p #:*source-tree*
           #:cpu-count #:run-tasks
           ;; calling C
           #:define-c-function #:c-type #:null-pointer #:null-pointer-p #:with-c-string #:sap-string #:%sysconf
           #:*task-wrappers* #:*around-long-work* #:wait-on #:wait-on-until
           #:*debug-on* #:*debug-start* #:*debug-reports* #:debug-report #:with-thread-guard #:make-guarded-thread
           #:bisect #:first-index-at-or-after #:with-depth-limit #:carry-settings #:queue-debug-report #:*stop-wait*
           #:join-within
           #:thread-result #:left-running
           #:set-debug #:milliseconds-since #:seconds-since #:deadline-in #:deadline-passed-p #:seconds-left
           #:wait-until #:*debug-log-path* #:log-in-use-p #:debug-report-line #:backtrace-lines
           #:reporting-errors #:handling-errors-reported #:ignoring-errors-reported
           #:close-fd-quietly #:with-fd-closed-on-error
           #:+o-cloexec+ #:duplicate-fd #:fd-for-program
           ;; hooks
           #:quit #:quit-words #:run-hook #:run-hook-until-success #:run-hook-until-done #:call-hook-function
           #:hook-function-name
           #:host-say-problem #:*hook-failures-said*
           #:*secret-at-hand* #:refusal #:refuse
           ;; settings and the values they take
           #:define-setting #:setting-values #:setting-values-words #:setting-value-fault #:set-setting
           #:setting-standard-value #:setting-in-force #:settings-snapshot #:put-back-misfit-settings
           #:settings-watch #:make-settings-watch #:settings-moved-p
           #:host-secret-buffer-p #:secret-buffer-p #:failure-words #:note-editor-busy
           #:start-hang-watch #:*hang-report-seconds* #:lock-fd #:unlock-fd #:open-rotated-log
           ;; work stopped at once
           #:background-work #:start-background-work #:background-work-key #:background-work-value
           #:stop-background-work
           #:stoppable #:stoppable-work #:stop-work #:check-stopped #:*slow-call-seconds*
           ;; jobs in the background
           #:job #:job-p #:job-buffer #:job-status #:job-error #:job-cancel #:current-job-cancel
           #:job-thread #:job-started #:job-wake
           #:job-frame #:job-wake-editor #:job-process #:job-program-started
           #:*jobs* #:*job* #:host-job-started #:host-job-waits-p #:host-take-in-job #:host-wait-for-job
           #:host-end-program
           #:*job-buffer-name* #:job-buffer-name #:job-stopped #:job-running-p #:job-check-stop
           #:job-percent #:job-doing #:define-job-words #:job-state-words #:job-description #:job-verb #:job-left-words
           #:job-consequence #:job-stop #:job-holds-queue-p #:job-shows-processing-p #:job-at-exit #:job-take-in
           #:job-guarded-spans #:*job-taken-in* #:span-touched-p #:job-guards #:guard-touched #:job-guard-question
           #:job-guard-refusal
           #:job-ending-words #:job-stop-question #:part-percent #:running-percent #:running-job #:two-phase-percent
           #:two-phase-words
           #:report-job #:start-background-result #:start-job-thread #:condition-words #:forget-job #:take-in-jobs
           #:stop-and-settle))
