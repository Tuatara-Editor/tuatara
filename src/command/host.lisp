;;;; src/command/host.lisp --- The command loop as the host the lower core asks.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; The runtime knows no editor; what it asks of one (TUATARA.RUNTIME:HOST)
;;;; the command loop answers as EDITOR-HOST, made the host as soon as it is
;;;; loaded. Its answers are methods beside what they come from: the wake in
;;;; editor.lisp, a hook's failure said in redisplay.lisp, a job tied to its
;;;; frame in frames.lisp.

(in-package #:tuatara.command)

(defclass editor-host (tuatara.runtime:host) ()
  (:documentation "The core with its command loop, answering what the runtime asks of an editor
\(TUATARA.RUNTIME:HOST)."))

(setf tuatara.runtime:*host* (make-instance 'editor-host))

(defmethod tuatara.runtime:host-secret-buffer-p ((host editor-host) buffer)
  "True when BUFFER's text, decrypted, is kept from the disk (TUATARA.IO:BUFFER-PLAINTEXT-P)."
  (tuatara.io:buffer-plaintext-p buffer))
