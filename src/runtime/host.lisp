;;;; src/runtime/host.lisp --- The host: what the core asks of what is loaded on it.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; Some answers the code asking cannot have, because they live in what is
;;;; loaded after it. Two kinds: the lower core asks the upper (the runtime,
;;;; which knows no editor, has an editor woken, a job tied to the frame it
;;;; was begun in, a failure said in the echo area); and the core asks the
;;;; features loaded on it (how wide a tab is in a buffer, what C-h after a
;;;; prefix key shows, whether a file may be written where it is). Each such
;;;; question is a generic function named HOST-..., in the package of the code
;;;; that asks it, whose first argument is the host, *HOST*; its method on
;;;; HOST, in its DEFGENERIC, is the answer of the code that asks, so that
;;;; every question has one from the moment it is defined. The command loop
;;;; answers more as EDITOR-HOST, made the host once it is loaded, and the
;;;; defaults as DEFAULTS-HOST, made from EDITOR-HOST, each answer a method
;;;; beside what it comes from.
;;;;
;;;; The names say what kind of question each is: HOST-SAY-... says something
;;;; to the person (a failure, a file written); HOST-SHOW-... shows a buffer
;;;; (*Help*, *Backtrace*); HOST-...-STARTED and HOST-...-MADE tell the host of
;;;; something that began; a verb does what it says (HOST-WAKE, HOST-VISIT-FILE,
;;;; HOST-TAKE-IN-JOB); and the others are answers, a noun or a predicate
;;;; (HOST-TAB-WIDTH, HOST-COMMAND-REFUSAL, HOST-TRUSTED-TEXT-P).

(in-package #:tuatara.runtime)

(defclass host () ()
  (:documentation "What is loaded on the core, as the code that asks the HOST-... generic
functions sees it: its methods are the answers of the code asking, those of its
subclasses (the command loop's EDITOR-HOST, the defaults' DEFAULTS-HOST) the
answers of what is loaded after it."))

(defvar *host* (make-instance 'host)
  "What is loaded on the core, which the core asks what only that knows
\(HOST): the command loop's once it is loaded, the defaults' once they are.")

(defgeneric host-wake (host)
  (:documentation "Have the editor running look again at what work on other threads has for it
\(a parse finished, a report made, words checked), called on such a thread;
nothing on the editor's own thread, which looks before it waits. Without a
command loop, there is no editor to wake.")
  (:method ((host host))
    nil))
