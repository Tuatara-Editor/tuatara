;;;; lisp/host.lisp --- The defaults as the host the core asks.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; The core asks the features loaded on it what only they know through the
;;;; HOST-... generic functions (src/runtime/host.lisp). The defaults answer as
;;;; DEFAULTS-HOST, made from the command loop's EDITOR-HOST and made the host
;;;; as soon as they are loaded, so that what they guard (no writing in an
;;;; evidence folder) is guarded before any editor starts; each answer is a
;;;; method beside the feature it comes from.

(in-package #:tuatara.defaults)

(defclass defaults-host (editor-host) ()
  (:documentation "The editor as the defaults make it, answering the core's questions with its
features (TUATARA.RUNTIME:HOST), and the runtime's as the command loop does
\(EDITOR-HOST)."))

(setf tuatara.runtime:*host* (make-instance 'defaults-host))
