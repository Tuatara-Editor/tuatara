;;;; src/command/host.lisp --- The command loop as the host the lower core asks.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; The runtime knows no editor; what it asks of one (TUATARA.RUNTIME:HOST)
;;;; the command loop answers as EDITOR-HOST, made the host as soon as it is
;;;; loaded. Its answers are methods beside what they come from: the wake in
;;;; editor.lisp, a hook's failure said in editor.lisp, a job tied to its
;;;; frame in frames.lisp.

(in-package #:tuatara.command)

(defclass editor-host (tuatara.runtime:host) ()
  (:documentation "The core with its command loop, answering what the runtime asks of an editor
\(TUATARA.RUNTIME:HOST)."))

(setf tuatara.runtime:*host* (make-instance 'editor-host))

(defmethod tuatara.runtime:host-secret-buffer-p ((host editor-host) buffer)
  "True when BUFFER's text, decrypted, is kept from the disk (TUATARA.IO:BUFFER-PLAINTEXT-P)."
  (tuatara.io:buffer-plaintext-p buffer))

(defun sandbox-outcome-words (outcome said &key (label "the program")
                                                   (seconds-setting 'tuatara-user:*sandbox-seconds*))
  "Why a program run shut in (SANDBOX-RUN) gave nothing, as a sentence
beginning with LABEL, from how it went, OUTCOME, and what it SAID, as
SANDBOX-RUN gives them. The reason is one of these. There is no sandbox here,
and why. It was stopped after the seconds SECONDS-SETTING (a setting's symbol)
holds, or by you. It printed more than it may. Or it failed, with the first
line of what it said. The result is NIL when OUTCOME is :DONE."
  (case outcome
    (:done nil)
    (:unavailable (format nil "~a runs only shut in the sandbox, and there is none here: ~a" label said))
    (:timeout (format nil "~a was stopped after ~d seconds (~(~a~))"
                      label (symbol-value seconds-setting) seconds-setting))
    (:cancelled (format nil "~a was stopped" label))
    (:too-much (format nil "~a printed more than it may" label))
    (t (let ((line (string-trim " " (or (first (uiop:split-string (or said "") :separator '(#\Newline))) ""))))
         (format nil "~a failed~@[: ~a~]" label
                 (and (plusp (length line))
                      (escape-controls (subseq line 0 (min 300 (length line))))))))))
