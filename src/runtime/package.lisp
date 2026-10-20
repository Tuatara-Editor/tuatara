;;;; src/runtime/package.lisp --- Package definition for tuatara.runtime.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(defpackage #:tuatara.runtime
  (:use #:cl #:alexandria)
  (:documentation "Threads: the pool of workers that runs pieces of work in parallel, the number of processors, waiting on a condition with a time limit, jobs in the background and their one runner, and the reports of debug mode.")
  (:export #:jobs-of #:make-spares #:take-spare #:give-back-spare #:with-spare #:resource-directories #:foreign-library-p #:foreign-call #:heap-free #:heap-budget #:*source-tree* #:cpu-count #:run-tasks #:ensure-pool #:*pool* #:pool #:pool-size #:wait-on #:wait-on-until
           #:*debug-on* #:*debug-start* #:*debug-reports* #:*debug-wake-function* #:debug-report #:with-thread-guard #:make-guarded-thread #:bisect #:carry-settings #:queue-debug-report #:*stop-wait* #:join-within #:left-running
           #:set-debug #:milliseconds-since #:seconds-since #:deadline-in #:deadline-passed-p #:seconds-left #:*debug-log-path* #:log-in-use-p #:debug-report-line #:backtrace-lines
           #:reporting-errors #:ignoring-errors-reported #:*secret-at-hand* #:note-editor-busy #:start-hang-watch #:*hang-report-seconds* #:lock-fd #:open-rotated-log
           ;; work stopped at once
           #:stoppable #:stoppable-work #:stop-work #:check-stopped #:*slow-call-seconds*
           ;; jobs in the background
           #:job #:job-p #:job-buffer #:job-status #:job-error #:job-cancel #:job-thread #:job-started #:job-wake #:job-process #:job-program-started
           #:*jobs* #:*job* #:*job-waker* #:*job-buffer-name* #:job-buffer-name #:job-stopped #:job-running-p #:job-check-stop
           #:job-percent #:job-doing #:define-job-words #:job-state-words #:job-description #:job-verb #:job-left-words
           #:job-consequence #:job-stop #:job-holds-queue-p #:job-shows-processing-p #:job-at-exit #:job-take-in
           #:job-guarded-spans #:*job-taken-in* #:span-touched-p #:job-guards #:guard-touched #:job-guard-question #:job-guard-refusal
           #:job-ending-words #:job-stop-question #:part-percent #:running-percent #:running-job #:two-phase-percent #:two-phase-words
           #:report-job #:start-background-result #:start-job-thread #:condition-words #:forget-job #:take-in-jobs #:stop-and-settle))
