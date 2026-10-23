;;;; lisp/org/clock.lisp --- Org's clock: time spent on a task, kept in its LOGBOOK, shown while it runs.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; C-c C-x C-i clocks in on the headline point is under: a line CLOCK:
;;;; [2026-10-01 Thu 17:30] goes into its :LOGBOOK: drawer, the newest first,
;;;; after its planning line and properties, as Org puts it. C-c C-x C-o
;;;; clocks out, the line finished with the time it ended and how long it
;;;; ran (=> 1:12); C-c C-x C-q cancels it, the line taken away; C-c C-x C-j
;;;; goes to the task. While it runs the mode line and the window's title
;;;; show [0:22] (Task); quitting asks whether to clock out first.

(in-package #:tuatara.defaults)

(defvar *org-clock-into-drawer* t
  "True for clock lines to go into a :LOGBOOK: drawer of their headline;
NIL for them to go right under it.")

(defstruct (org-clock (:conc-name org-clock-))
  "The clock running: the BUFFER of the task, an anchor on the start of its
CLOCK: LINE, when it STARTED (universal time, whole minutes) and the TASK's title."
  buffer line started task)

(defvar *org-clock* nil
  "The clock running, an ORG-CLOCK, or NIL.")

(defun org-duration-text (minutes)
  "MINUTES as a clock line writes its duration: \" 1:12\", \"39:35\"."
  (multiple-value-bind (hours minutes) (floor (max 0 minutes) 60)
    (format nil "~2d:~2,'0d" hours minutes)))

(defun org-minute (time)
  "The universal TIME without its seconds, as a timestamp keeps it."
  (* 60 (floor time 60)))

(defun org-clock-string (clock now)
  "What the mode line shows of CLOCK at the universal time NOW: [0:22] (Task)."
  (format nil "[~a] (~a)" (string-left-trim " " (org-duration-text (floor (- now (org-clock-started clock)) 60)))
          (org-clock-task clock)))

(defun org-clock-line-start (clock)
  "Where CLOCK's line begins now in its buffer (octets), or NIL when it is gone."
  (let ((buffer (org-clock-buffer clock)))
    (and (live-buffer-p buffer)
         (ignore-errors (tuatara.text:anchor-position (org-clock-line clock) (tuatara.buffer:buffer-version buffer))))))

(defun org-clock-open-line (clock)
  "CLOCK's line in its buffer as two values, its start and end (octets), when
it is still an open CLOCK: line; else NIL."
  (let* ((buffer (org-clock-buffer clock))
         (start (org-clock-line-start clock)))
    (when start
      (let* ((end (tuatara.buffer:buffer-line-end buffer start))
             (line (tuatara.buffer:buffer-string buffer :start start :end end)))
        (multiple-value-bind (from to duration) (org-read-clock-line line)
          (when (and from (not to) (not duration))
            (values start end)))))))

(defun org-clock-in-here (buffer &key (now (org-now)))
  "Clock in on the task at BUFFER's point, the clock running clocked out
first: the list item with a checkbox whose first line point is on, else the
headline it is under. CLOCK: and the time go at the top of its :LOGBOOK:
drawer (ORG-LOG-EDIT), made when it has none; a headline's with no drawer
when *ORG-CLOCK-INTO-DRAWER* is NIL."
  ;; the clock running stopped first: its line changes the text the new one goes into
  (when *org-clock* (org-clock-out-now :now now))
  (multiple-value-bind (tree text) (org-tree-now buffer)
    (let* ((*org-current-text* (or text ""))
           (position (org-point-char buffer))
           (item (and tree (org-checkbox-item-at tree text position)))
           (owner (or item (and tree (org-current-headline tree position)))))
      (if (null owner)
          (message "Not under a headline: clock in on a task")
          (let* ((started (org-minute now))
                 (line (format nil "CLOCK: ~a" (org-timestamp-text started)))
                 (title (if item (org-checkbox-title item text) (or (org-property owner :raw-value) "")))
                 (edit (org-log-edit owner text (list line) :drawer (or item *org-clock-into-drawer*)))
                 (at (search line (third edit)))
                 ;; where the clock's line begins in the text after the edit
                 (line-start (+ (first edit) (string-line-start (third edit) at))))
            (org-apply-edits buffer (list edit) :point position)
            (setf *org-clock* (make-org-clock :buffer buffer :started started :task title
                                              :line (tuatara.text:make-anchor (tuatara.buffer:buffer-version buffer)
                                                                              (org-char->byte buffer line-start) :bias :after)))
            (message "Clock starts at ~a: ~a" (format-time "%R" started) title))))))

(define-command org-clock-in ()
  "Clock in on the task point is under (\\`C-c C-x C-i'): CLOCK: and the time go into its :LOGBOOK: drawer, made
when it has none, after its planning line and properties; a clock running is
clocked out first. While it runs, the mode line and the window's title show
how long, and the task. On a list item's line with a checkbox, on that item:
its CLOCK: line goes into a :LOGBOOK: drawer under it."
  (org-clock-in-here (current-buffer)))

(defun org-clock-out-now (&key (now (org-now)))
  "Clock out of the clock running: its line finished with the time it ended
and how long it ran (=> 1:12)."
  (let ((clock *org-clock*))
    ;; a buffer that may not be written keeps the clock running, its line open
    (check-writable (org-clock-buffer clock))
    (setf *org-clock* nil)
    (multiple-value-bind (start end) (org-clock-open-line clock)
      (if (null start)
          (message "The clock's line is gone: nothing to finish")
          (let* ((buffer (org-clock-buffer clock))
                 (ended (org-minute now))
                 (minutes (floor (- ended (org-clock-started clock)) 60))
                 (line (tuatara.buffer:buffer-string buffer :start start :end end))
                 (indent (subseq line 0 (position #\C line)))
                 (new (format nil "~aCLOCK: ~a--~a => ~a" indent (org-timestamp-text (org-clock-started clock))
                              (org-timestamp-text ended) (org-duration-text minutes)))
                 (point (tuatara.buffer:buffer-point buffer)))
            (tuatara.buffer:buffer-replace buffer start end new)
            (setf (tuatara.buffer:buffer-point buffer)
                  (if (> point end) (+ point (- (tuatara.text:string-octet-length new) (- end start))) (min point (+ start (tuatara.text:string-octet-length new)))))
            (message "Clock stopped at ~a after ~a: ~a" (format-time "%R" ended)
                     (string-left-trim " " (org-duration-text minutes)) (org-clock-task clock)))))))

(define-command org-clock-out ()
  "Clock out (\\`C-c C-x C-o', from any buffer \\`C-x a o'):
the clock's line finished with the time it ended and how long it ran."
  (if *org-clock* (org-clock-out-now) (message "No clock is running")))

(define-command org-clock-cancel ()
  "Stop the clock running and take its line away
(\\`C-c C-x C-q'): its :LOGBOOK: drawer too, when nothing else is in it."
  (let ((clock *org-clock*))
    (if (null clock)
        (message "No clock is running")
        (progn
          (setf *org-clock* nil)
          (multiple-value-bind (start end) (org-clock-open-line clock)
            (if (null start)
                (message "The clock's line is gone: clock cancelled")
                (let* ((buffer (org-clock-buffer clock))
                       (rope (tuatara.buffer:buffer-text buffer))
                       (size (tuatara.buffer:buffer-size buffer))
                       (end (min size (1+ (tuatara.text:rope-line-end rope end))))
                       (before (tuatara.buffer:buffer-line-start buffer (max 0 (1- start))))
                       (above (and (plusp start) (tuatara.buffer:buffer-string buffer :start before :end start)))
                       (below (tuatara.buffer:buffer-string buffer :start end :end (min size (tuatara.buffer:buffer-line-end buffer end))))
                       (blank '(#\Space #\Tab #\Return #\Newline))
                       (empty-drawer (and above (string= (string-trim blank above) ":LOGBOOK:")
                                          (string= (string-trim blank below) ":END:"))))
                  (if empty-drawer
                      (tuatara.buffer:buffer-delete buffer before (min size (1+ (tuatara.text:rope-line-end rope end))))
                      (tuatara.buffer:buffer-delete buffer start end))
                  (message "Clock cancelled: ~a" (org-clock-task clock)))))))))

(define-command org-clock-goto ()
  "Go to the task the clock runs on (\\`C-c C-x C-j', from any buffer \\`C-x a j')."
  (let* ((clock *org-clock*)
         (start (and clock (org-clock-line-start clock))))
    (if (null start)
        (message "No clock is running")
        (let ((buffer (org-clock-buffer clock)))
          (show-buffer buffer)
          ;; the task's headline: the nearest line above beginning with *, the clock's line shown
          (goto-shown buffer (loop for line = (tuatara.buffer:buffer-line-start buffer start)
                                     then (tuatara.buffer:buffer-line-start buffer (1- line))
                                   when (or (zerop line) (eql (tuatara.buffer:buffer-char-at buffer line) #\*)) return line)
                      :open start)))))

(defun org-update-clock-line (buffer text position)
  "When the line of the character POSITION of BUFFER (TEXT its text) is a
finished CLOCK: line, write its duration again from its two times; true when it was."
  (let* ((start (string-line-start text position))
         (line (org-line-at text start)))
    (multiple-value-bind (from-text to-text) (org-read-clock-line line)
      (let ((from (and to-text (org-ts-universal-time from-text))) (to (and to-text (org-ts-universal-time to-text))))
        (when (and from to)
          (let* ((minutes (floor (- to from) 60))
                 (new (format nil "~a => ~a" (subseq line 0 (+ (search to-text line :from-end t) (length to-text)))
                              (org-duration-text minutes))))
            (org-replace-chars buffer start (+ start (length line)) new :point (min position (+ start (length new))))
            (message "Clock line: ~a" (string-left-trim " " (org-duration-text minutes)))
            t))))))

(defun org-clock-mode-line (editor buffer)
  "For the mode line: the clock running, how long and on what task."
  (declare (ignore editor buffer))
  (and *org-clock* (list (org-clock-string *org-clock* (org-now)) :org-mode-line-clock)))

(defun org-clock-title (editor)
  "For the window's title: the clock running, how long and on what task."
  (declare (ignore editor))
  (and *org-clock* (org-clock-string *org-clock* (org-now))))

(defun org-clock-tick (editor)
  "While a clock runs, ask to be called again when the minute changes, for the mode line to show it."
  (declare (ignore editor))
  (and *org-clock* (- 60 (mod (org-now) 60))))

(defun org-clock-quit-query ()
  "Before quitting: with a clock running, ask whether to clock out first."
  (when (and *org-clock* (yes-or-no-p* (format nil "The clock runs on ~a: clock out before quitting?" (org-clock-task *org-clock*))))
    (org-clock-out-now))
  t)

(defun install-org-clock ()
  "Bind Org's clock keys in Org buffers, under \\`C-c C-x' (clock, blocks and
more), show the clock running, and ask about it before quitting."
  (let ((keymap (mode-keymap 'org-mode)))
    (define-key keymap "C-c C-x C-i" 'org-clock-in)
    (define-key keymap "C-c C-x C-o" 'org-clock-out)
    (define-key keymap "C-c C-x C-q" 'org-clock-cancel)
    (define-key keymap "C-c C-x C-j" 'org-clock-goto)
    (label-keys keymap '(("C-c C-x" "clock, blocks and more"))))
  (pushnew 'org-clock-mode-line *mode-line-functions*)
  (pushnew 'org-clock-title *title-functions*)
  (pushnew 'org-clock-tick *tick-functions*)
  (pushnew 'org-clock-quit-query *quit-query-functions*)
  (pushnew 'org-clock-buffer-killed tuatara.buffer:*kill-buffer-functions*))

(defun org-clock-buffer-killed (buffer)
  "When BUFFER, being killed, holds the clock running, the clock stops: its
line is left as it is, open, and that is said."
  (let ((clock *org-clock*))
    (when (and clock (eq (org-clock-buffer clock) buffer))
      (setf *org-clock* nil)
      (message "The clock on ~a stopped with its buffer; its CLOCK: line stays open" (org-clock-task clock)))))
