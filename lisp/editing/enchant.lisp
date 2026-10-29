;;;; lisp/editing/enchant.lisp --- Words checked by enchant, on a thread of its own, the answers kept.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; enchant (libenchant-2) chooses a speller for each language, nuspell,
;;;; hunspell, aspell or another, finds its dictionaries, and keeps the
;;;; personal word lists (~/.config/enchant/LANGUAGE.dic), the same lists
;;;; Emacs's jinx adds to. It is called directly, no speller program started.
;;;; Its calls are made on one thread only, which owns it: a word asked about
;;;; is answered there and the answer kept, so that the editor looks it up
;;;; again without waiting. The way of checking is jinx's (minad/jinx, by
;;;; Daniel Mendler), which binds enchant directly too.

(in-package #:tuatara.defaults)

(defvar *enchant-library* "libenchant-2.so.2"
  "The enchant library spelling is checked with; one that cannot be loaded
leaves the editor without spelling, which says so.")

(defvar *spelling-wake-function* nil
  "A function of no arguments called when words asked about are answered, so
that the editor redraws with their marks; NIL for none.")

(defvar *spelling-answers* (make-hash-table :test 'equal :synchronized t)
  "For each list of languages, as their names joined by spaces, a table of
the words asked about: :RIGHT, :WRONG, or :ASKED while the answer is on its way.")

(defvar *speller* nil
  "The thread enchant is called on, and its mailbox of work, a cons; NIL until spelling is first wanted.")

(defvar *speller-lock* (sb-thread:make-mutex :name "speller")
  "Held while the speller thread is started.")

(defvar *enchant-broker* nil
  "enchant's broker, made on the speller thread, which alone uses it; NIL before.")

(defvar *enchant-dictionaries* (make-hash-table :test 'equal)
  "On the speller thread: each language asked for, its enchant dictionary or :MISSING.")

(defparameter *longest-word* 64
  "Characters a word may have to be checked; a longer run of letters is not a word to spell.")

(defvar *spelling-draining* nil
  "True while the speller has been given SPELLING-DRAIN to run.")

(defvar *spelling-wanted-lock* (sb-thread:make-mutex :name "spelling wanted")
  "Held while *SPELLING-WANTED* and *SPELLING-DRAINING* are read or changed.")

(defun speller-mailbox ()
  "The speller thread's mailbox, the thread started the first time."
  (sb-thread:with-mutex (*speller-lock*)
    ;; a speller thread that ended (its memory exhausted, say) is started again
    (when (and *speller* (not (sb-thread:thread-alive-p (car *speller*))))
      (setf *speller* nil *enchant-broker* nil)
      (clrhash *enchant-dictionaries*)
      (sb-thread:with-mutex (*spelling-wanted-lock*) (setf *spelling-draining* nil)))
    (cdr (or *speller*
             (let ((mailbox (sb-concurrency:make-mailbox :name "speller")))
               (setf *speller*
                     (cons (tuatara.runtime:make-guarded-thread
                            (lambda ()
                              (loop (let ((work (sb-concurrency:receive-message mailbox)))
                                      (handler-case (funcall work)
                                        (error (e) (tuatara.runtime:debug-report :spelling "~a" e))))))
                            :name "tuatara speller")
                           mailbox)))))))

(defun speller-run (function)
  "Have the speller thread call FUNCTION, of no arguments, after the work
given it before; returns a cons whose car is set to FUNCTION's value and whose
cdr to T once it has run."
  (let ((answer (cons nil nil)))
    (sb-concurrency:send-message (speller-mailbox)
                                 (lambda () (unwind-protect (setf (car answer) (funcall function))
                                              (setf (cdr answer) t))))
    answer))

(defun languages-key (languages)
  "LANGUAGES, a list of names such as \"en_NZ\", as the key of *SPELLING-ANSWERS*."
  (format nil "~{~a~^ ~}" languages))

(defun spelling-table (languages)
  "The answers kept for LANGUAGES."
  (let ((key (languages-key languages)))
    (or (gethash key *spelling-answers*)
        (setf (gethash key *spelling-answers*) (make-hash-table :test 'equal :synchronized t)))))

(defun enchant-p ()
  "True when enchant is loaded, loading it the first time (FOREIGN-LIBRARY-P)."
  (and *enchant-library* (foreign-library-p *enchant-library*)))

(defun enchant-octets (word)
  "WORD as enchant takes it: UTF-8 with a zero after."
  (sb-ext:string-to-octets word :external-format :utf-8 :null-terminate t))

(defun enchant-dictionary (language)
  "On the speller thread: LANGUAGE's enchant dictionary, asked for the first
time it is wanted; NIL when enchant has none for it."
  (unless *enchant-broker*
    (setf *enchant-broker* (foreign-call "enchant_broker_init" sb-sys:system-area-pointer)))
  (let ((dictionary (or (gethash language *enchant-dictionaries*)
                        (setf (gethash language *enchant-dictionaries*)
                              (let* ((name (enchant-octets language))
                                     (pointer (sb-sys:with-pinned-objects (name)
                                                (foreign-call "enchant_broker_request_dict" sb-sys:system-area-pointer
                                                              (sb-sys:system-area-pointer *enchant-broker*)
                                                              (sb-sys:system-area-pointer (sb-sys:vector-sap name))))))
                                (if (zerop (sb-sys:sap-int pointer)) :missing pointer))))))
    (and (not (eq dictionary :missing)) dictionary)))

(defun enchant-right-p (dictionary word)
  "On the speller thread: true when DICTIONARY takes WORD as spelt right."
  (let ((octets (enchant-octets word)))
    (sb-sys:with-pinned-objects (octets)
      (zerop (foreign-call "enchant_dict_check" sb-alien:int
                           (sb-sys:system-area-pointer dictionary)
                           (sb-sys:system-area-pointer (sb-sys:vector-sap octets))
                           (sb-alien:long (1- (length octets))))))))

(defun enchant-suggestions (dictionary word)
  "On the speller thread: what DICTIONARY suggests for WORD, best first, as strings."
  (let ((octets (enchant-octets word)))
    (sb-alien:with-alien ((count sb-alien:unsigned-long 0))
      (let ((list (sb-sys:with-pinned-objects (octets)
                    (foreign-call "enchant_dict_suggest" sb-sys:system-area-pointer
                                  (sb-sys:system-area-pointer dictionary)
                                  (sb-sys:system-area-pointer (sb-sys:vector-sap octets))
                                  (sb-alien:long (1- (length octets)))
                                  ((* sb-alien:unsigned-long) (sb-alien:addr count))))))
        (unless (zerop (sb-sys:sap-int list))
          (prog1 (loop for i below count
                       collect (sb-alien:deref (sb-alien:sap-alien list (* (sb-alien:c-string :external-format :utf-8))) i))
            (foreign-call "enchant_dict_free_string_list" sb-alien:void
                          (sb-sys:system-area-pointer dictionary) (sb-sys:system-area-pointer list))))))))

(defun enchant-add (dictionary word &key session)
  "On the speller thread: have DICTIONARY take WORD as right from now on: in
its personal word list, kept on disk; with SESSION, only while the editor runs."
  (let ((octets (enchant-octets word)))
    (sb-sys:with-pinned-objects (octets)
      (if session
          (foreign-call "enchant_dict_add_to_session" sb-alien:void (sb-sys:system-area-pointer dictionary)
                        (sb-sys:system-area-pointer (sb-sys:vector-sap octets)) (sb-alien:long (1- (length octets))))
          (foreign-call "enchant_dict_add" sb-alien:void (sb-sys:system-area-pointer dictionary)
                        (sb-sys:system-area-pointer (sb-sys:vector-sap octets)) (sb-alien:long (1- (length octets))))))))

(defun spelling-of (languages word)
  "On the speller thread: :RIGHT when one of LANGUAGES' dictionaries takes
WORD as right, else :WRONG; :RIGHT when none of them has a dictionary."
  (let ((dictionaries (remove nil (mapcar #'enchant-dictionary languages))))
    (if (or (null dictionaries) (some (lambda (d) (enchant-right-p d word)) dictionaries)) :right :wrong)))

(defvar *spelling-generation* (list 0)
  "In its car, a count of the times answers about words came in or a word was
taken as right: what a window was found to show spelt wrong before a change
of it may differ.")

(defun spelling-answered ()
  "Have the editor redraw with the answers."
  (sb-ext:atomic-incf (car *spelling-generation*))
  (let ((wake *spelling-wake-function*))
    (when wake (ignore-errors (funcall wake)))))

(defun spelling-known (languages word)
  "What is known of WORD's spelling in LANGUAGES: :RIGHT, :WRONG, or NIL
while it has not been answered."
  (let ((answer (gethash word (spelling-table languages))))
    (and (not (eq answer :asked)) answer)))

(defparameter *spelling-answers-limit* 500000
  "Words whose answers are kept for one list of languages; past it they are
dropped, and the words shown asked about again, so that reading through a
file of many different words does not fill the memory.")

(defparameter *spelling-backlog* 4
  "Lots of words asked about that may wait for the speller; an older one is
dropped when a newer comes, its words asked again only if they are shown
again: the words on the screen are answered first, however fast it moves.")

(defvar *spelling-wanted* '()
  "The lots of words waiting for the speller, newest first, each (LANGUAGES
TABLE . WORDS); taken newest first by SPELLING-DRAIN.")

(defun spelling-drain ()
  "On the speller thread: answer the lots of words waiting, the newest first,
the editor woken after each, until none is left. A lot whose words cannot be
answered (the speller failing) has them taken as right, so that they are not
asked again and again, and the words after it are still answered."
  (let ((done nil))
    (unwind-protect
         (loop
           (let ((lot (sb-thread:with-mutex (*spelling-wanted-lock*)
                        (or (pop *spelling-wanted*) (progn (setf *spelling-draining* nil) nil)))))
             (unless lot (setf done t) (return))
             (destructuring-bind (languages table . words) lot
               (handler-case
                   (dolist (w words)
                     (when (eq (gethash w table) :asked) (setf (gethash w table) (spelling-of languages w))))
                 (error (e)
                   (tuatara.runtime:debug-report :spelling "~a" e)
                   (dolist (w words) (when (eq (gethash w table) :asked) (setf (gethash w table) :right))))))
             (spelling-answered)))
      ;; ended any other way: the next words asked about start it again
      (unless done
        (sb-thread:with-mutex (*spelling-wanted-lock*) (setf *spelling-draining* nil))))))

(defun spelling-ask (languages words)
  "Have WORDS checked in LANGUAGES on the speller thread, those not yet asked
about; when they are answered the editor redraws (*SPELLING-WAKE-FUNCTION*).
The newest asked are answered first (*SPELLING-BACKLOG*). Returns true when
some were asked about."
  (let ((table (spelling-table languages)))
    (when (> (hash-table-count table) *spelling-answers-limit*) (clrhash table))
    (let ((new '()))
      (dolist (w words)
        (unless (gethash w table)
          (setf (gethash w table) :asked)
          (push w new)))
      (when (and new (enchant-p))
        (let ((start nil))
          (sb-thread:with-mutex (*spelling-wanted-lock*)
            (push (list* languages table new) *spelling-wanted*)
            ;; the oldest lots over the backlog: dropped, their words to be asked again when shown
            (let ((old (nthcdr *spelling-backlog* *spelling-wanted*)))
              (when old
                (setf *spelling-wanted* (ldiff *spelling-wanted* old))
                (loop for (nil old-table . old-words) in old
                      do (dolist (w old-words) (when (eq (gethash w old-table) :asked) (remhash w old-table))))))
            (unless *spelling-draining* (setf *spelling-draining* t start t)))
          (when start (speller-run #'spelling-drain)))
        t))))

(defun spelling-suggest (languages word)
  "Suggestions for WORD from LANGUAGES' dictionaries, worked out on the
speller thread: returns what SPELLER-RUN does, its car a list of (SUGGESTION
. LANGUAGE), the best of each language first, none twice."
  (speller-run (lambda ()
                 (let ((seen '()))
                   (loop for language in languages
                         for dictionary = (enchant-dictionary language)
                         when dictionary
                           append (loop for s in (enchant-suggestions dictionary word)
                                        unless (member s seen :test #'string=)
                                          collect (progn (push s seen) (cons s language))))))))

(defun spelling-add (languages word language &key session)
  "Have WORD taken as right: added to LANGUAGE's personal word list, kept on
disk; with SESSION, to every one of LANGUAGES' dictionaries until the editor
quits. Done on the speller thread; the answers kept say so at once."
  ;; right at once wherever the dictionaries it went into are asked: LANGUAGE's, or for the session LANGUAGES'
  (let ((into (if session languages (list language))))
    (maphash (lambda (key table)
               (when (and (intersection into (uiop:split-string key :separator " ") :test #'string=)
                          (eq (gethash word table) :wrong))
                 (setf (gethash word table) :right)))
             *spelling-answers*))
  (setf (gethash word (spelling-table languages)) :right)
  (spelling-answered)
  (speller-run (lambda ()
                 (if session
                     (dolist (l languages)
                       (let ((d (enchant-dictionary l))) (when d (enchant-add d word :session t))))
                     (let ((d (enchant-dictionary language))) (when d (enchant-add d word)))))))
