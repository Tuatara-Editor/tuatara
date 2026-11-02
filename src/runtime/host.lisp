;;;; src/runtime/host.lisp --- The host: what the core asks of the editor made on it.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; The core knows nothing of the features loaded on it, yet now and then it
;;;; needs an answer only they have: how wide a tab is in a buffer, what a
;;;; prefix key followed by C-h shows, whether a file may be written where it
;;;; is. Each such question is a generic function named HOST-..., in the
;;;; package of the part of the core that asks it, whose first argument is the
;;;; host, *HOST*. Its method on HOST is the core's own answer; the defaults
;;;; make a host of their own, a subclass of HOST, and answer for their
;;;; features with methods on it, each beside the feature it comes from.

(in-package #:tuatara.runtime)

(defclass host () ()
  (:documentation "The editor made on the core, as the core asks it what only its features know
\(the HOST-... generic functions): its methods are the core's own answers, those
of a subclass the answers of the features loaded."))

(defvar *host* (make-instance 'host)
  "The editor made on the core, which the core asks what only its features
know (HOST): the core's own while nothing else is loaded, the defaults' once
they are.")

(defgeneric host-wake (host)
  (:documentation "Have the editor running look again at what work on other threads has for it
\(a parse finished, a report made, words checked), called on such a thread;
nothing on the editor's own thread, which looks before it waits."))
