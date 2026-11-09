;;;; lisp/remote/sftp.lisp --- Files on another machine: a client of SFTP, version 3, through the system's ssh.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; Files on a server are read, written and listed through the SSH File
;;;; Transfer Protocol (SFTP), version 3, as OpenSSH's sftp-server speaks it,
;;;; so that nothing need be installed there. The system's ssh is run once for
;;;; each server, killed with the editor, its standard input and output one end
;;;; of a local socket here: the options of *SSH-OPTIONS* (nothing forwarded, no
;;;; local or remote command), with what ssh asks put to you (asking.lisp), or
;;;; nothing asked and only known host keys when there is nobody to ask, the
;;;; host after --. Every answer is held to bounds (its length, its counts,
;;;; the names and handles in it), must answer a request asked and not yet
;;;; answered, and is waited for *REMOTE-SECONDS* at most; a server that breaks
;;;; any of this has its connection closed. Nothing a server says is run here,
;;;; and the names it gives are only data until cleaned to be shown.

(in-package #:tuatara.defaults)

(defvar *remote-seconds* 30
  "Seconds a server reached for its files (/ssh:HOST:/path) has to answer
each request, and ssh to connect: past them the connection is closed and the
work in hand fails, saying so. The work itself runs in the background, so
that the editor never waits on a server.")

(defparameter *sftp-types*
  '((:init 1) (:version 2) (:open 3) (:close 4) (:read 5) (:write 6) (:lstat 7) (:fstat 8) (:setstat 9)
    (:fsetstat 10) (:opendir 11) (:readdir 12) (:remove 13) (:mkdir 14) (:rmdir 15) (:realpath 16) (:stat 17)
    (:rename 18) (:readlink 19) (:symlink 20) (:status 101) (:handle 102) (:data 103) (:name 104) (:attrs 105)
    (:extended 200) (:extended-reply 201))
  "The kinds of SFTP packet, version 3, and their numbers.")

(defparameter *sftp-answers* '(:version :status :handle :data :name :attrs :extended-reply)
  "The kinds of SFTP packet a server sends; any other from it breaks the protocol.")

(defconstant +sftp-most-packet+ (* 260 1024)
  "The most octets one packet from a server may hold: OpenSSH's bound of 256 KB, and room for the fields.")

(defconstant +sftp-piece+ 32768 "The octets of a file asked for, or sent, in one request.")

(defconstant +sftp-pieces-ahead+ 16 "How many requests for pieces of a file are sent before their answers come.")

(defconstant +sftp-most-name+ 4096 "The most octets of a name or path a server gives.")

(defconstant +sftp-most-handle+ 256 "The most octets of a handle a server gives, as the protocol bounds it.")

(defconstant +sftp-most-extensions+ 64 "The most extensions a server names, or attributes of its own it gives a file.")

(defconstant +sftp-most-said+ 4096 "The most octets of a server's words for why a request failed.")

(defparameter *sftp-most-listed* 100000
  "The most entries of one folder on a server listed; past them the listing is cut, and says so.")

(defparameter *sftp-most-listed-characters* (* 64 1024 1024)
  "The most characters of names one listing of a folder on a server holds;
past them the listing is cut, as past *SFTP-MOST-LISTED* entries.")

(defparameter *sftp-status-words*
  #("done" "at the end" "there is no such file" "permission denied" "the server failed"
    "the server took the request for a bad one" "no connection" "the connection was lost"
    "the server does not do that")
  "What each status code of an SFTP answer says, from 0.")

;;; Fields of a packet

(defun sftp-put-integer (box value count)
  "Put the unsigned integer VALUE in COUNT octets of BOX, most significant first."
  (let ((at (tuatara.frontend:outbox-room box count)))
    (tuatara.io:octets-put-integer (tuatara.frontend:outbox-octets box) value count :at at :big-endian t)))

(defun sftp-put (box field)
  "Put FIELD in BOX as SFTP writes it: an integer as 4 octets, a string as
UTF-8 and octets as they are, each after its length; (:U64 N) as 8 octets;
(:ATTRIBUTES PLIST) as a file's attributes (SFTP-PUT-ATTRIBUTES)."
  (etypecase field
    (integer (sftp-put-integer box field 4))
    (string (sftp-put box (tuatara.text:string-octets field)))
    (vector (sftp-put-integer box (length field) 4)
     (tuatara.frontend:put-octets box (coerce field 'tuatara.text:octets)))
    (cons (ecase (first field)
            (:u64 (sftp-put-integer box (second field) 8))
            (:attributes (sftp-put-attributes box (second field)))))))

(defun sftp-put-attributes (box attributes)
  "Put ATTRIBUTES, a plist, in BOX as SFTP writes a file's: those of its
:SIZE, :UID and :GID (both or neither) and :PERMISSIONS given, the others
left as the server has them."
  (destructuring-bind (&key size uid gid permissions &allow-other-keys) attributes
    (let ((owner (and uid gid)))
      (sftp-put-integer box (logior (if size 1 0) (if owner 2 0) (if permissions 4 0)) 4)
      (when size (sftp-put-integer box size 8))
      (when owner (sftp-put-integer box uid 4) (sftp-put-integer box gid 4))
      (when permissions (sftp-put-integer box permissions 4)))))

(defun sftp-take-integer (inbox count)
  "The unsigned integer in the next COUNT octets of the packet being read from INBOX, most significant first."
  (tuatara.io:octets-integer (tuatara.frontend:inbox-octets inbox) (tuatara.frontend:take-place inbox count) count
                             :big-endian t))

(defun sftp-take-span (inbox most what)
  "Where the string next in the packet being read from INBOX is in its
octets, two values, its start and end; malformed past MOST octets, WHAT
naming it."
  (let ((length (sftp-take-integer inbox 4)))
    (unless (<= length most) (tuatara.frontend:malformed "~a of ~d octets, past its ~d" what length most))
    (let ((at (tuatara.frontend:take-place inbox length)))
      (values at (+ at length)))))

(defun sftp-take-text (inbox most what)
  "The string next in the packet being read from INBOX as text, at most MOST
octets (WHAT names it), what is not UTF-8 in it as raw bytes, so that it is
the same octets when sent back."
  (multiple-value-bind (start end) (sftp-take-span inbox most what)
    (tuatara.text:octets-string (tuatara.frontend:inbox-octets inbox) :start start :end end)))

(defun sftp-take-attributes (inbox)
  "The attributes of a file next in the packet being read from INBOX, as a
plist: :SIZE, :UID, :GID, :PERMISSIONS (the mode, its kind too), :ATIME and
:MTIME (seconds since 1970), each when given. Flags the protocol does not
have, or more than +SFTP-MOST-EXTENSIONS+ attributes of the server's own,
are malformed."
  (let ((flags (sftp-take-integer inbox 4)) (attributes '()))
    (unless (zerop (logandc2 flags #x8000000F))
      (tuatara.frontend:malformed "attributes with flags ~x the protocol does not have" flags))
    (when (logtest flags 1) (setf (getf attributes :size) (sftp-take-integer inbox 8)))
    (when (logtest flags 2)
      (setf (getf attributes :uid) (sftp-take-integer inbox 4) (getf attributes :gid) (sftp-take-integer inbox 4)))
    (when (logtest flags 4) (setf (getf attributes :permissions) (sftp-take-integer inbox 4)))
    (when (logtest flags 8)
      (setf (getf attributes :atime) (sftp-take-integer inbox 4) (getf attributes :mtime) (sftp-take-integer inbox 4)))
    (when (logtest flags #x80000000)
      (let ((count (sftp-take-integer inbox 4)))
        (unless (<= count +sftp-most-extensions+)
          (tuatara.frontend:malformed "~d attributes of the server's own, past ~d" count +sftp-most-extensions+))
        (dotimes (k (* 2 count))
          (sftp-take-span inbox +sftp-most-name+ "an attribute of the server's own"))))
    attributes))

(defun sftp-long-name-links (octets start end)
  "How many names (hard links) a file has, from its long name in OCTETS
between START and END, as OpenSSH's server writes it in a listing (as ls -l
does: its mode, then that count); NIL when it does not say one."
  (let* ((space (position 32 octets :start start :end end))
         (from (and space (position 32 octets :start space :end end :test-not #'eql)))
         (to (and from (position 32 octets :start from :end end))))
    (when (and to (< from to (+ from 10)) (loop for i from from below to always (<= 48 (aref octets i) 57)))
      (let ((count 0))
        (loop for i from from below to do (setf count (+ (* 10 count) (- (aref octets i) 48))))
        count))))

(defun sftp-take-packet (inbox)
  "The kind of the next packet of INBOX, read whole, its fields to be taken
in order; NIL when the other end has gone. A packet shorter than its kind, longer than
+SFTP-MOST-PACKET+, or of a kind no server sends, is malformed."
  (unless (tuatara.frontend:fill-inbox inbox 4) (return-from sftp-take-packet nil))
  (let ((length (tuatara.io:octets-integer (tuatara.frontend:inbox-octets inbox) (tuatara.frontend:inbox-start inbox)
                                           4 :big-endian t)))
    (unless (<= 1 length +sftp-most-packet+)
      (tuatara.frontend:malformed "a packet of ~d octets, past ~d" length +sftp-most-packet+))
    (unless (tuatara.frontend:fill-inbox inbox (+ 4 length)) (return-from sftp-take-packet nil))
    (let* ((start (tuatara.frontend:inbox-start inbox))
           (code (aref (tuatara.frontend:inbox-octets inbox) (+ start 4)))
           (kind (first (find code *sftp-types* :key #'second))))
      (setf (tuatara.frontend:inbox-at inbox) (+ start 5)
            (tuatara.frontend:inbox-stop inbox) (+ start 4 length)
            (tuatara.frontend:inbox-start inbox) (+ start 4 length))
      (unless (member kind *sftp-answers*)
        (tuatara.frontend:malformed "a packet of kind ~d, which no server sends" code))
      kind)))

(defun sftp-take-answer (inbox kind)
  "What the answer of KIND, its request's number taken already, says, its
fields read from INBOX to the last (malformed when any is left over): for
:STATUS, its code and the server's words, cleaned (a server of an early
version may give none); for :HANDLE, the handle's octets; for :DATA, where
the octets are in INBOX's octets, (START . END); for :NAME, a list of
\(NAME . ATTRIBUTES), NAME as SFTP-TAKE-TEXT reads it, ATTRIBUTES with
:LINKS when its long name gives it (SFTP-LONG-NAME-LINKS); for :ATTRS, the
attributes (SFTP-TAKE-ATTRIBUTES); for :EXTENDED-REPLY, where its octets are."
  (prog1
      (ecase kind
        (:status (let ((code (sftp-take-integer inbox 4)))
                   (if (= (tuatara.frontend:inbox-at inbox) (tuatara.frontend:inbox-stop inbox))
                       (list code "")
                       (prog1 (list code (tuatara.frontend:shown-words
                                          (tuatara.text:one-line (sftp-take-text inbox +sftp-most-said+ "words"))))
                         (sftp-take-span inbox 256 "a language tag")))))
        (:handle (multiple-value-bind (start end) (sftp-take-span inbox +sftp-most-handle+ "a handle")
                   (subseq (tuatara.frontend:inbox-octets inbox) start end)))
        (:data (multiple-value-bind (start end) (sftp-take-span inbox +sftp-most-packet+ "data")
                 (cons start end)))
        (:name (let ((count (sftp-take-integer inbox 4)))
                 ;; each name at least the lengths of its name and long name, and its flags
                 (unless (<= (* count 12) (- (tuatara.frontend:inbox-stop inbox) (tuatara.frontend:inbox-at inbox)))
                   (tuatara.frontend:malformed "~d names, more than the packet holds" count))
                 (loop repeat count
                       collect (let ((name (sftp-take-text inbox +sftp-most-name+ "a name")))
                                 (multiple-value-bind (start end)
                                     (sftp-take-span inbox (* 2 +sftp-most-name+) "a long name")
                                   (let ((attributes (sftp-take-attributes inbox))
                                         (links (sftp-long-name-links (tuatara.frontend:inbox-octets inbox) start end)))
                                     (cons name (if links (list* :links links attributes) attributes))))))))
        (:attrs (sftp-take-attributes inbox))
        (:extended-reply (let ((start (tuatara.frontend:inbox-at inbox)) (end (tuatara.frontend:inbox-stop inbox)))
                           (tuatara.frontend:take-place inbox (- end start))
                           (cons start end))))
    (tuatara.frontend:message-read inbox)))

(defun sftp-take-version (inbox)
  "The version a server's first packet, read from INBOX, gives and the names
of the extensions it offers, two values; malformed past +SFTP-MOST-EXTENSIONS+
of them."
  (let ((version (sftp-take-integer inbox 4)) (extensions '()))
    (loop while (< (tuatara.frontend:inbox-at inbox) (tuatara.frontend:inbox-stop inbox))
          do (when (>= (length extensions) +sftp-most-extensions+)
               (tuatara.frontend:malformed "more than ~d extensions" +sftp-most-extensions+))
             (push (sftp-take-text inbox 256 "an extension's name") extensions)
             (sftp-take-span inbox +sftp-most-name+ "an extension's data"))
    (values version (nreverse extensions))))

;;; The connection to a server

(defstruct (sftp-link (:constructor make-sftp-link (host)) (:copier nil))
  "The connection to the SFTP server of HOST, as ssh takes it, used by one
thread at a time, holding LOCK: PROCESS, the ssh reaching it; FD, this end
of the local socket ssh reads and writes; INBOX and OUTBOX, what the server
said and not yet read, and what is to be sent; FOLDER, a private folder
holding what ssh said on its error output; NEXT-ID, the number of the last
request; WAITING, how many requests sent have no answer yet; WANTING, how
many threads wait for the connection, for which whoever holds it gives it
up between its pieces (SFTP-LET-OTHERS-IN); GENERATION, counted up as the connection is closed, so
that a handle of one connection is never used on the next; EXTENSIONS,
the names of the extensions the server offers; REAL-PATHS, where paths lead
there as asked once for this connection (REMOTE-KEPT-REAL-PATH). FD is NIL
while no connection is open."
  (host "" :type string :read-only t)
  (lock (sb-thread:make-mutex :name "sftp connection") :read-only t)
  (process nil)
  (fd nil)
  (inbox nil)
  (outbox nil)
  (folder nil)
  (next-id 0 :type (unsigned-byte 32))
  (waiting 0 :type fixnum)
  (extensions '())
  (wanting 0 :type sb-ext:word)
  (generation 0 :type fixnum)
  (asking nil)
  (real-paths (make-hash-table :test 'equal) :read-only t))

(setf (documentation 'make-sftp-link 'function) "An SFTP-LINK to HOST, not yet connected."
      (documentation 'sftp-link-p 'function) "True when OBJECT is an SFTP-LINK."
      (documentation 'sftp-link-host 'function) "The machine LINK reaches, as ssh takes it."
      (documentation 'sftp-link-lock 'function) "The lock held while LINK is used."
      (documentation 'sftp-link-process 'function) "The ssh reaching LINK's server, a process, or NIL."
      (documentation 'sftp-link-fd 'function) "This end of the socket of LINK's ssh, or NIL when LINK is closed."
      (documentation 'sftp-link-inbox 'function) "What LINK's server said and is not yet read."
      (documentation 'sftp-link-outbox 'function) "What is to be sent to LINK's server."
      (documentation 'sftp-link-folder 'function) "The private folder of what LINK's ssh said, or NIL."
      (documentation 'sftp-link-next-id 'function) "The number of the last request sent over LINK."
      (documentation 'sftp-link-waiting 'function) "How many requests sent over LINK have no answer yet."
      (documentation 'sftp-link-wanting 'function) "How many threads wait for LINK's connection."
      (documentation 'sftp-link-generation 'function) "How many times LINK's connection has been closed."
      (documentation 'sftp-link-extensions 'function) "The names of the extensions LINK's server offers."
      (documentation 'sftp-link-asking 'function)
      "How LINK's ssh asks you what it needs, an SSH-ASKING, while it connects, or NIL."
      (documentation 'sftp-link-real-paths 'function)
      "Where paths lead on LINK's server, as asked over this connection.")

(sb-ext:defglobal **sftp-links** '()
  "The connections to servers, one for each host, as SFTP-LINK makes them.")

(sb-ext:defglobal **sftp-links-lock** (sb-thread:make-mutex :name "sftp connections")
  "The lock held while **SFTP-LINKS** is looked at or changed.")

(defun sftp-link (host)
  "The connection to HOST's SFTP server, made, not yet connected, when there is none."
  (sb-thread:with-mutex (**sftp-links-lock**)
    (or (find host **sftp-links** :key #'sftp-link-host :test #'string=)
        (first (push (make-sftp-link host) **sftp-links**)))))

(defun remote-name (host path)
  "The name of the file at PATH on HOST, as it is written to visit it: /ssh:HOST:PATH."
  (format nil "/ssh:~a:~a" host path))

(defun sftp-ssh-arguments (host asking)
  "The arguments of ssh reaching the SFTP server of HOST: what is asked as
ASKING has it (SSH-ASKING-OPTIONS), no connection of another shared
(ControlMaster), the options of *SSH-OPTIONS*, the host after --."
  (append (list "-T") (ssh-asking-options asking) (list "-o" "ControlMaster=no" "-o" "ControlPath=none")
          (ssh-option-arguments)
          (list "-s" "--" host "sftp")))

(defun sftp-ssh-said (link)
  "What LINK's ssh said on its error output, on one line, cleaned to be
shown, the first *PROGRAM-SAID-LIMIT* octets at most; \"\" when nothing."
  (let ((folder (sftp-link-folder link)))
    (or (and folder
             (ignoring-errors-reported ("reading what ssh said")
               (tuatara.frontend:shown-words
                (tuatara.text:one-line
                 (tuatara.text:lenient-string
                  (tuatara.io:file-octets (tuatara.io:path-join folder "e") :limit tuatara.io:*program-said-limit*))))))
        "")))

(defun sftp-close (link)
  "Close LINK's connection, its ssh ended and its folder deleted; it is
connected again when next used."
  (incf (sftp-link-generation link))
  (let ((fd (shiftf (sftp-link-fd link) nil)) (process (shiftf (sftp-link-process link) nil))
        (folder (shiftf (sftp-link-folder link) nil)))
    (when fd
      (ignoring-errors-reported ("closing the connection to ~a" (sftp-link-host link)) (sb-posix:close fd)))
    (when process (tuatara.io:done-with-program process))
    (close-link-asking link)
    (when folder (delete-private-folder folder))
    (clrhash (sftp-link-real-paths link))
    (setf (sftp-link-inbox link) nil (sftp-link-outbox link) nil (sftp-link-extensions link) '()
          (sftp-link-waiting link) 0)))

(defun close-link-asking (link)
  "Stop LINK's ssh asking anything more, once it is connected or has failed."
  (let ((asking (shiftf (sftp-link-asking link) nil)))
    (when asking (close-ssh-asking asking))))

(define-condition sftp-refusal (tuatara.io:file-system-error) ()
  (:documentation "Signalled when a server refuses a request, saying why: the connection stays good."))

(defun sftp-refuse (link operation path reason &rest arguments)
  "Signal an SFTP-REFUSAL: OPERATION on PATH, a path on LINK's server,
refused for the REASON FORMAT makes of ARGUMENTS, the connection still good."
  (error 'sftp-refusal :operation operation :path (remote-name (sftp-link-host link) path)
                       :reason (apply #'format nil reason arguments)))

(defun remote-missing-p (condition)
  "True when CONDITION, a refusal of a server, says there is no such file."
  (uiop:string-prefix-p (aref *sftp-status-words* 2) (tuatara.io:file-system-error-reason condition)))

(defun sftp-fail (link operation path reason &rest arguments)
  "Signal a TUATARA.IO:FILE-SYSTEM-ERROR: OPERATION on PATH, a path on LINK's
server, refused for the REASON FORMAT makes of ARGUMENTS."
  (error 'tuatara.io:file-system-error :operation operation :path (remote-name (sftp-link-host link) path)
                                       :reason (apply #'format nil reason arguments)))

(defun sftp-await-ssh (link on-slow)
  "Wait until LINK's ssh has something to say or is gone, answering what it
asks meanwhile (ANSWER-SSH-QUESTION); calling ON-SLOW, once, when that has
taken *SSH-CONNECTING-SECONDS*. Time you take to answer does not count. The
deadline for its first answer: *REMOTE-SECONDS* after the last question. A
FILE-SYSTEM-ERROR when a question goes unanswered."
  (let* ((fd (sftp-link-fd link))
         (asking (sftp-link-asking link))
         (fds (if asking (list fd (ssh-asking-question asking)) (list fd)))
         (deadline (tuatara.runtime:deadline-in *remote-seconds*))
         (slow-at (tuatara.runtime:deadline-in *ssh-connecting-seconds*))
         (slow nil))
    (loop
      (tuatara.runtime:job-check-stop)
      (let ((ready (tuatara.io:wait-for-fds fds (min 1/4 (max 0 (tuatara.runtime:seconds-left deadline))))))
        (cond ((and asking (consp ready) (member (ssh-asking-question asking) ready))
               (unless (answer-ssh-question asking)
                 (sftp-fail link "reach" "" "ssh's question was not answered: not connected"))
               (setf deadline (tuatara.runtime:deadline-in *remote-seconds*)))
              ((and (consp ready) (member fd ready)) (return deadline))
              ((tuatara.runtime:deadline-passed-p deadline) (return deadline))
              ((and (not slow) (tuatara.runtime:deadline-passed-p slow-at))
               (setf slow t)
               (funcall on-slow)))))))

(defun sftp-connect (link)
  "Connect LINK: ssh started, killed with the editor, what it asks put to
you when there is an editor to ask in (SFTP-AWAIT-SSH), and the version of the
protocol agreed. A FILE-SYSTEM-ERROR saying why when it cannot be. The echo
area says so while a connection takes more than a moment."
  (let ((host (sftp-link-host link)) (said nil))
    (unwind-protect
         (progn (sftp-start-ssh link)
                (sftp-agree-version link (lambda () (setf said (say-connecting host)))))
      (close-link-asking link)
      (when said (unsay-connecting said)))))

(defun sftp-start-ssh (link)
  "Start LINK's ssh, this end of its socket in LINK, ready to ask what it
needs when there is someone to ask (SSH-CAN-ASK-P); FILE-SYSTEM-ERROR when it
cannot be started."
  (let* ((host (sftp-link-host link))
         (program (tuatara.io:system-program *ssh-program*)))
    (when (host-refusal host) (sftp-fail link "reach" "" "~a" (host-refusal host)))
    (unless program (sftp-fail link "reach" "" "ssh is not installed in the system's folders of programs"))
    (setf (sftp-link-folder link) (new-private-folder (private-cache-directory "remote/")))
    (when (ssh-can-ask-p program)
      (setf (sftp-link-asking link) (open-ssh-asking program (sftp-link-folder link) host)))
    (multiple-value-bind (mine theirs) (tuatara.io:socket-pair)
      (setf (sftp-link-fd link) mine
            (sftp-link-inbox link) (tuatara.frontend:make-inbox mine :editor)
            (sftp-link-outbox link) (tuatara.frontend:make-outbox))
      ;; ssh's end of the socket is the start's to close, even one given up on and finished later
      (let ((stream (sb-sys:make-fd-stream theirs :input t :output t :buffering :none :auto-close nil))
            (asking (sftp-link-asking link)))
        (setf (sftp-link-process link)
              (handler-case
                  (tuatara.io:start-program (cons program (sftp-ssh-arguments host asking))
                                            :input stream :output stream
                                            :error (tuatara.io:path-join (sftp-link-folder link) "e")
                                            :if-error-exists :supersede
                                            :environment (apply #'ssh-environment (ssh-asking-environment asking))
                                            :with-editor t :close-fds (list theirs))
                (tuatara.runtime:refusal (condition) (sftp-fail link "reach" "" "~a" condition))
                (error (condition) (sftp-fail link "reach" "" "ssh could not be started: ~a" condition))))))))

(defun sftp-agree-version (link on-slow)
  "Agree the protocol's version over LINK, its ssh just started, waiting for it as SFTP-AWAIT-SSH does."
  (sftp-put-packet link :init (list 3))
  (sftp-flush link "" "reach")
  (let ((inbox (sftp-link-inbox link)))
    (setf (tuatara.frontend:inbox-deadline inbox) (sftp-await-ssh link on-slow))
    (case (sftp-take-packet inbox)
      ((nil) (sftp-unreached link))
      (:version (multiple-value-bind (version extensions) (sftp-take-version inbox)
                  (unless (= version 3)
                    (sftp-fail link "reach" "" "its SFTP server speaks version ~d, not 3" version))
                  (setf (sftp-link-extensions link) extensions)))
      (t (tuatara.frontend:malformed "a first packet that is not its version")))))

(defun sftp-unreached (link)
  "Signal that LINK's server could not be reached, in what ssh said: that its
host key changed since ssh last saw it, which is refused and is never asked
about, or, when nobody could be asked, that a key ssh does not know yet is
refused."
  (let ((said (sftp-ssh-said link)) (host (sftp-link-host link)))
    (when (or (search "IDENTIFICATION HAS CHANGED" said) (search "has changed and you have requested strict" said))
      (sftp-fail link "reach" "" "the host key of ~a differs from the one ssh knows, and ssh refuses it: check with ~
                                  the machine's owner before removing the old key with ssh-keygen -R" host))
    (when (search "ssh_askpass" said)
      (sftp-fail link "reach" "" "ssh could not run the small program that asks its questions, in the cache folder ~
                                  here (a folder programs may not be run from, mounted noexec?): ~a" said))
    (sftp-fail link "reach" "" "ssh could not reach it~:[~*~;: ~a~]~:[~;; a machine whose key ssh does not know yet ~
                                is refused: connect once with ssh ~a and check its key~]"
               (plusp (length said)) said
               (and (null (sftp-link-asking link)) (search "host key" said :test #'char-equal))
               host)))

(defun sftp-put-packet (link kind fields)
  "Put a packet of KIND with FIELDS (SFTP-PUT) in LINK's outbox, numbered
as the next request unless it is :INIT; its number."
  (let* ((box (sftp-link-outbox link))
         (at (tuatara.frontend:outbox-fill box))
         (id (and (not (eq kind :init))
                  (setf (sftp-link-next-id link) (ldb (byte 32 0) (1+ (sftp-link-next-id link)))))))
    (sftp-put-integer box 0 4)
    (sftp-put-integer box (second (assoc kind *sftp-types*)) 1)
    (when id
      (sftp-put-integer box id 4)
      (incf (sftp-link-waiting link)))
    (dolist (field fields) (sftp-put box field))
    (tuatara.io:octets-put-integer (tuatara.frontend:outbox-octets box) (- (tuatara.frontend:outbox-fill box) at 4) 4
                                   :at at :big-endian t)
    id))

(defun sftp-flush (link path operation)
  "Send what LINK's outbox holds, for OPERATION on PATH: a FILE-SYSTEM-ERROR
when the server takes none of it for *REMOTE-SECONDS*, or has gone."
  (case (tuatara.frontend:send-outbox (sftp-link-fd link) (sftp-link-outbox link) *remote-seconds*)
    (:stalled (sftp-fail link operation path "the server took nothing for ~d second~:p" *remote-seconds*))
    ((nil) (sftp-unreached link))))

(defun sftp-answer (link ids path operation)
  "Wait for the next answer of LINK's server, *REMOTE-SECONDS* at most: its
kind, what it says (SFTP-TAKE-ANSWER) and the number of the request it
answers, which must be one of IDS. An answer of :STATUS done or at the end
comes as its kind; another signals a FILE-SYSTEM-ERROR for OPERATION on
PATH saying why."
  (let ((inbox (sftp-link-inbox link)))
    (setf (tuatara.frontend:inbox-deadline inbox) (tuatara.runtime:deadline-in *remote-seconds*))
    (let ((kind (sftp-take-packet inbox)))
      (unless kind (sftp-unreached link))
      (when (eq kind :version) (tuatara.frontend:malformed "a second version"))
      (let ((id (sftp-take-integer inbox 4)))
        (unless (member id ids) (tuatara.frontend:malformed "an answer to request ~d, not asked" id))
        (let ((said (sftp-take-answer inbox kind)))
          (decf (sftp-link-waiting link))
          (if (eq kind :status)
              (destructuring-bind (code words) said
                (case code
                  (0 (values :done nil id))
                  (1 (values :end nil id))
                  (t (error 'sftp-refusal :operation operation :path (remote-name (sftp-link-host link) path)
                            :reason (format nil "~a~@[ (~a)~]"
                                (if (< code (length *sftp-status-words*))
                                    (aref *sftp-status-words* code)
                                    (format nil "the server gave status ~d" code))
                                ;; the server's words where ours say less than it may
                                (and (plusp (length words)) (not (member code '(2 3 8)))
                                     (tuatara.display:shorten-string words 200)))))))
              (values kind said id)))))))

(defun sftp-ask (link kind fields path operation)
  "Send LINK's server one request of KIND with FIELDS, and wait for its
answer (SFTP-ANSWER), for OPERATION on PATH: its kind and what it says."
  (let ((id (sftp-put-packet link kind fields)))
    (sftp-flush link path operation)
    (multiple-value-bind (answer said) (sftp-answer link (list id) path operation)
      (values answer said))))

(defvar *remote-cancel* nil
  "A cons whose car turns true when the work on this thread with a server is
no longer wanted; NIL for work that is only stopped as a job is.")

(defun remote-cancelled-p ()
  "True when the work on this thread with a server is no longer wanted (*REMOTE-CANCEL*)."
  (and *remote-cancel* (car *remote-cancel*)))

(defun call-with-sftp (host path operation function)
  "Call FUNCTION with the connection to HOST's SFTP server, connected first
when it is not, holding it alone; for OPERATION on PATH. A server that breaks
the protocol, or does not answer in time, has its connection closed and a
TUATARA.IO:FILE-SYSTEM-ERROR signalled saying so; so has one left with a
request half asked or answered, the work stopped meanwhile. Work no longer
wanted by the time the connection is free stops there, connecting to nothing."
  (let ((link (sftp-link host)) (done nil))
    (sb-ext:atomic-incf (sftp-link-wanting link))
    (sb-thread:with-mutex ((sftp-link-lock link))
      (sb-ext:atomic-decf (sftp-link-wanting link))
      (when (remote-cancelled-p) (error 'tuatara.runtime:job-stopped))
      (unwind-protect
           (handler-case
               (multiple-value-prog1
                   (progn (unless (sftp-link-fd link) (sftp-connect link))
                          (handler-bind ((tuatara.runtime:job-stopped
                                           ;; stopped between requests, the connection stays good
                                           (lambda (condition)
                                             (declare (ignore condition))
                                             (setf done (zerop (sftp-link-waiting link))))))
                            (funcall function link)))
                 (setf done t))
             (tuatara.frontend:malformed-message (condition)
               (let ((reason (tuatara.frontend:malformed-message-reason condition)))
                 (sftp-close link)
                 (sftp-fail link operation path "~:[the server broke the protocol: ~a~;~*it did not answer within ~
                                                 ~d second~:p~]"
                            (search "in time" reason) reason *remote-seconds*)))
             (sftp-refusal (condition)
               ;; a request the server refused leaves the connection good, when no other waits for its answer
               (setf done (zerop (sftp-link-waiting link)))
               (error condition)))
        (unless done (sftp-close link))))))

;;; Requests

(defun sftp-handle (link kind fields path operation)
  "A handle of LINK's server, from a request of KIND (:OPEN or :OPENDIR) with FIELDS."
  (multiple-value-bind (answer handle) (sftp-ask link kind fields path operation)
    (unless (eq answer :handle) (tuatara.frontend:malformed "~(~a~) where a handle was asked for" answer))
    handle))

(defun sftp-let-go (link handle path)
  "Close HANDLE on LINK's server."
  (sftp-ask link :close (list handle) path "close"))

(defun sftp-with-handle (link kind fields path operation function)
  "What FUNCTION returns, called with a handle of LINK's server from a
request of KIND (:OPEN or :OPENDIR) with FIELDS, for OPERATION on PATH, the
handle closed after; closed too as FUNCTION is left for the server having
refused a request, or the work being stopped, while no answer is still to
come over the connection, which then stays good. A connection broken is
closed whole, the handle with it (CALL-WITH-SFTP). The handle is closed once
at most."
  (let ((handle (sftp-handle link kind fields path operation)) (closed nil))
    (multiple-value-prog1
        (handler-bind (((or sftp-refusal tuatara.runtime:job-stopped)
                         (lambda (condition)
                           (declare (ignore condition))
                           (when (and (not closed) (sftp-link-fd link) (zerop (sftp-link-waiting link)))
                             (setf closed t)
                             (handler-case (sftp-let-go link handle path)
                               (sftp-refusal () nil))))))
          (funcall function handle))
      (unless closed
        (setf closed t)
        (sftp-let-go link handle path)))))

(defun sftp-done (link kind fields path operation)
  "Ask LINK's server a request of KIND with FIELDS, which it answers done."
  (unless (eq :done (sftp-ask link kind fields path operation))
    (tuatara.frontend:malformed "an answer of another kind where done was asked for")))

(defun sftp-attributes (link path &key (follow t) handle)
  "The attributes of the file at PATH on LINK's server (SFTP-TAKE-ATTRIBUTES),
or with HANDLE of the file it has open; with FOLLOW NIL, those of a link
itself. NIL when there is no such file."
  (handler-case
      (multiple-value-bind (answer attributes)
          (sftp-ask link (cond (handle :fstat) (follow :stat) (t :lstat)) (list (or handle path)) path "examine")
        (unless (eq answer :attrs) (tuatara.frontend:malformed "~(~a~) where attributes were asked for" answer))
        attributes)
    (sftp-refusal (condition)
      (if (remote-missing-p condition) nil (error condition)))))

(defun remote-clean-name-p (name)
  "True when NAME, from a server, shows as it is, with no control character
nor one that could hide or reorder what it says (TUATARA.FRONTEND:SHOWN-WORDS),
and holds no raw byte that is not UTF-8."
  (and (string= name (tuatara.frontend:shown-words name))
       (notany (lambda (c) (<= #xDC80 (char-code c) #xDCFF)) name)))

(defun sftp-real-path (link path)
  "Where PATH leads on LINK's server, every link followed, from the root, as
the server says it; refused when that would not show as it is
\(REMOTE-CLEAN-NAME-P)."
  (multiple-value-bind (answer names) (sftp-ask link :realpath (list path) path "find")
    (unless (and (eq answer :name) (= 1 (length names)) (uiop:string-prefix-p "/" (car (first names))))
      (tuatara.frontend:malformed "a path not from the root where one was asked for"))
    (let ((real (car (first names))))
      (unless (remote-clean-name-p real)
        (sftp-refuse link "find" path "it leads to a path that would not show as it is: ~a" (escape-controls real)))
      real)))

(defun sftp-extension-p (link name)
  "True when LINK's server offers the extension NAME."
  (and (member name (sftp-link-extensions link) :test #'string=) t))

;;; What the rest of the editor asks

(defun remote-attributes (host path &key (follow t))
  "The attributes of the file at PATH on HOST, as SFTP-TAKE-ATTRIBUTES gives
them, a link's own with FOLLOW NIL; NIL when there is no such file."
  (call-with-sftp host path "examine" (lambda (link) (sftp-attributes link path :follow follow))))

(defun remote-real-path (host path)
  "Where PATH leads on HOST, links followed, from the root; \"\" or . for the home folder there."
  (call-with-sftp host path "find" (lambda (link) (sftp-real-path link (if (string= path "") "." path)))))

(defun remote-kept-real-path (host path)
  "Where PATH leads on HOST, as REMOTE-REAL-PATH says, or NIL when it is not
there: once found, kept with the connection to HOST and not asked again over
it; an answer not had is asked again each time, and any other refusal is
signalled."
  (call-with-sftp host path "find"
                  (lambda (link)
                    (let ((kept (sftp-link-real-paths link)))
                      (or (gethash path kept)
                          (let ((real (handler-case (sftp-real-path link path)
                                        (sftp-refusal (condition)
                                          (if (remote-missing-p condition) nil (error condition))))))
                            (when real (setf (gethash path kept) real))
                            real))))))

(defun remote-kind (attributes)
  "The kind of file ATTRIBUTES give: :DIRECTORY, :FILE, :LINK or :OTHER; NIL when they do not say."
  (let ((permissions (getf attributes :permissions)))
    (and permissions
         (case (logand permissions #o170000)
           (#o040000 :directory) (#o100000 :file) (#o120000 :link) (t :other)))))

(defun remote-listing (host folder)
  "The entries of FOLDER on HOST, a path ending in /, each (NAME . ATTRIBUTES),
. and .. left out, in the server's order; with true as a second value when
it was cut at *SFTP-MOST-LISTED* entries, or *SFTP-MOST-LISTED-CHARACTERS*
characters of names."
  (call-with-sftp
   host folder "list"
   (lambda (link)
     (sftp-with-handle
      link :opendir (list folder) folder "list"
      (lambda (handle)
        (let ((entries '()) (count 0) (characters 0) (cut nil))
          (loop
            (multiple-value-bind (answer names) (sftp-ask link :readdir (list handle) folder "list")
              (when (eq answer :end) (return))
              (unless (eq answer :name) (tuatara.frontend:malformed "~(~a~) where names were asked for" answer))
              (dolist (entry names)
                (unless (member (car entry) '("." "..") :test #'string=)
                  (push entry entries)
                  (incf count)
                  (incf characters (length (car entry)))))
              (when (or (>= count *sftp-most-listed*) (>= characters *sftp-most-listed-characters*))
                (setf cut t)
                (return))
              (tuatara.runtime:job-check-stop)))
          (when cut
            ;; the first entries, as many as both bounds let be
            (setf entries (nreverse entries) characters 0 count 0)
            (setf entries (loop for entry in entries
                                while (and (< count *sftp-most-listed*)
                                           (< (incf characters (length (car entry))) *sftp-most-listed-characters*))
                                collect entry
                                do (incf count))))
          (values (if cut entries (nreverse entries)) cut)))))))

(defconstant +remote-first-part+ (* 1024 1024)
  "Octets of the start of a large file on another machine read before it opens.")

(defconstant +remote-tail-opening+ (* 64 1024)
  "Octets of the end of a large file on another machine read before it opens.")

(defstruct (arrival (:constructor make-arrival (size &key opening in-order)) (:copier nil))
  "How a file on another machine being read in pieces stands: SIZE, the
octets it holds there as it was opened; END, where it has been found to end,
SIZE unless the server gave no more past it; ARRIVED, the stretches (START .
END) here, in order, none touching another, a new list for each piece, never
changed in place, so that any thread may read it; WANTED, the stretch (START
. END) to read before the rest, or NIL; OPENING, true while only the file's
first part is to be read, the rest after it has opened; IN-ORDER, true to
read from the start to the end, nothing out of turn."
  (size 0 :type fixnum :read-only t)
  (end 0 :type fixnum)
  (arrived '() :type list)
  (wanted nil)
  (opening nil)
  (in-order nil :read-only t))

(setf (documentation 'make-arrival 'function) "An ARRIVAL of a file of SIZE octets, none of it here yet."
      (documentation 'arrival-p 'function) "True when OBJECT is an ARRIVAL."
      (documentation 'arrival-size 'function) "The octets ARRIVAL's file holds there, as it was opened."
      (documentation 'arrival-end 'function) "Where ARRIVAL's file has been found to end."
      (documentation 'arrival-arrived 'function) "The stretches (START . END) of ARRIVAL's file that are here."
      (documentation 'arrival-wanted 'function) "The stretch (START . END) of ARRIVAL's file to read first, or NIL."
      (documentation 'arrival-opening 'function) "True while only the first part of ARRIVAL's file is to be read."
      (documentation 'arrival-in-order 'function)
      "True when ARRIVAL's file is read from start to end, nothing out of turn.")

(defun arrival-add (arrival start end)
  "Note that the octets of ARRIVAL's file from START below END are here."
  (setf (arrival-arrived arrival) (tuatara.text:merge-ranges (cons (cons start end) (arrival-arrived arrival)))))

(defun arrival-complete-p (arrival)
  "True when all of ARRIVAL's file, as far as it was found to go, is here."
  (let ((arrived (arrival-arrived arrival)))
    (or (zerop (arrival-end arrival))
        (and arrived (zerop (car (first arrived))) (>= (cdr (first arrived)) (arrival-end arrival))))))

(defun arrival-holes (arrival)
  "The stretches (START . END) of ARRIVAL's file not here yet, in order."
  (let ((at 0) (holes '()))
    (loop for (start . end) in (arrival-arrived arrival)
          do (when (< at start) (push (cons at start) holes))
             (setf at (max at end)))
    (when (< at (arrival-size arrival)) (push (cons at (arrival-size arrival)) holes))
    (nreverse holes)))

(defun arrival-absent (arrival from to asked)
  "The offset of the first piece (+SFTP-PIECE+ octets) of ARRIVAL's file from
FROM below TO that is neither here nor among ASKED, a list of offsets, or NIL."
  (let ((at (* +sftp-piece+ (floor from +sftp-piece+)))
        (to (min to (arrival-end arrival)))
        (arrived (arrival-arrived arrival)))
    (loop while (< at to)
          do (let ((covering (find-if (lambda (range) (and (<= (car range) at) (< at (cdr range)))) arrived)))
               (cond (covering (setf at (* +sftp-piece+ (ceiling (cdr covering) +sftp-piece+))))
                     ((member at asked) (incf at +sftp-piece+))
                     (t (return at)))))))

(defun arrival-next (arrival asked)
  "The offset of the piece of ARRIVAL's file to ask for next, none of ASKED,
or NIL: first what a window wants (ARRIVAL-WANTED), then the start, then the
end (as much as the file reads back quickly, +KEPT-ENDS+, a little of it
while it opens), then the first piece not here going on from the start."
  (let ((wanted (arrival-wanted arrival)) (size (arrival-size arrival)))
    (when (arrival-in-order arrival)
      (return-from arrival-next (arrival-absent arrival 0 size asked)))
    (or (and wanted (arrival-absent arrival (car wanted) (cdr wanted) asked))
        (arrival-absent arrival 0 +remote-first-part+ asked)
        (let ((tail (min (if (arrival-opening arrival) +remote-tail-opening+ tuatara.text:+kept-ends+) size)))
          (arrival-absent arrival (- size tail) size asked))
        (and (not (arrival-opening arrival)) (arrival-absent arrival 0 size asked)))))

(defun sftp-let-others-in (link)
  "With no answer waited for: give up LINK's connection a moment when another
thread waits for it, so that a long transfer does not hold up other work on
that server, and take it back; a FILE-SYSTEM-ERROR when it was closed
meanwhile, even when connected again, the handles of the old connection being
of no use on the new."
  (when (plusp (sftp-link-wanting link))
    (let ((generation (sftp-link-generation link)))
      (sb-thread:release-mutex (sftp-link-lock link))
      (sb-thread:thread-yield)
      (sleep 1/100)
      (sb-thread:grab-mutex (sftp-link-lock link))
      (unless (and (sftp-link-fd link) (= generation (sftp-link-generation link)))
        (sftp-unreached link)))))

(defun remote-download (host path fd &key arrival on-piece most)
  "Read the file at PATH on HOST into the file open as FD, each piece where
it belongs, +SFTP-PIECE+ octets at a time, +SFTP-PIECES-AHEAD+ asked for
before their answers come, as many octets as the server said the file holds
as it was opened, its connection given up to other work between these when
some waits for it; stopped between pieces when the job doing it is asked to
stop. With ARRIVAL, an ARRIVAL of that file, the pieces are read as it says
\(ARRIVAL-NEXT) until it has all that is asked of it, ON-PIECE, a function of
none, called as each lot comes, and the file is refused when it is not as
ARRIVAL has it. With MOST, a file the server says holds more octets is
refused before any is read; the work stops, as a job asked to stop does, once
*REMOTE-CANCEL* says so. Its attributes then, as SFTP-TAKE-ATTRIBUTES gives
them."
  (call-with-sftp
   host path "read"
   (lambda (link)
     (sftp-with-handle
      link :open (list path 1 '(:attributes ())) path "read"
      (lambda (handle)
        (let* ((attributes (sftp-attributes link path :handle handle))
               (size (let ((size (or (getf attributes :size)
                                     (sftp-fail link "read" path "the server does not say its size"))))
                       (when (and most (> size most))
                         (sftp-refuse link "read" path "it holds ~a, more than ~a" (octets-words size) (octets-words most)))
                       size))
               (arrival (or arrival (make-arrival size :in-order t)))
               (asked (make-hash-table)))
          (unless (eq (remote-kind attributes) :file) (sftp-fail link "read" path "it is not a file"))
          (unless (= size (arrival-size arrival))
            (sftp-fail link "read" path "it changed on the server as it was being read"))
          (setf (arrival-end arrival) size)
          (flet ((ask (offset length)
                   (setf (gethash (sftp-put-packet link :read (list handle (list :u64 offset) length)) asked)
                         (cons offset length)))
                 (offsets () (loop for (offset) being the hash-values of asked collect offset)))
            (flet ((top-up ()
                     (loop while (< (hash-table-count asked) +sftp-pieces-ahead+)
                           do (let ((offset (arrival-next arrival (offsets))))
                                (unless offset (return))
                                (ask offset (min +sftp-piece+ (- (arrival-end arrival) offset))))))
                   (stopping ()
                     (or (remote-cancelled-p)
                         (and tuatara.runtime:*job* (car (tuatara.runtime:job-cancel tuatara.runtime:*job*)))))
                   (finished ()
                     (and (zerop (hash-table-count asked))
                          (if (arrival-opening arrival)
                              (null (arrival-next arrival '()))
                              (arrival-complete-p arrival)))))
              ;; a file read in order keeps as many asked as it may; one read for a window goes
              ;; a lot at a time, so that what is wanted next is chosen afresh, and others can get in
              (loop until (or (finished) (and (stopping) (zerop (hash-table-count asked))))
                    do (when (and (or (zerop (hash-table-count asked))
                                      (and (arrival-in-order arrival) (zerop (sftp-link-wanting link))))
                                  (not (stopping)))
                         (top-up))
                       (sftp-flush link path "read")
                       (multiple-value-bind (answer data id)
                           (sftp-answer link (loop for id being the hash-keys of asked collect id) path "read")
                         (destructuring-bind (offset . length) (gethash id asked)
                           (remhash id asked)
                           (case answer
                             ;; the file is shorter now than it was as opened
                             (:end (setf (arrival-end arrival) (min (arrival-end arrival) offset)))
                             (:data (let ((got (- (cdr data) (car data))))
                                      (unless (<= 1 got length)
                                        (tuatara.frontend:malformed "~d octets where at most ~d were asked for"
                                                                    got length))
                                      (tuatara.io:write-octets fd (tuatara.frontend:inbox-octets (sftp-link-inbox link))
                                                               :start (car data) :end (cdr data) :path path :at offset)
                                      (arrival-add arrival offset (+ offset got))
                                      ;; the rest of a piece the server gave short
                                      (when (and (< got length) (not (stopping)))
                                        (ask (+ offset got) (- length got)))))
                             (t (tuatara.frontend:malformed "~(~a~) where data was asked for" answer)))))
                       (when (zerop (hash-table-count asked))
                         (when on-piece (funcall on-piece))
                         (sftp-let-others-in link)))
              (when (remote-cancelled-p) (error 'tuatara.runtime:job-stopped))
              (tuatara.runtime:job-check-stop)))
          attributes))))))

(defvar *sftp-octets-sent* nil
  "A cons whose car SFTP-SEND-OCTETS sets to the octets sent so far, for the
work that shows its progress; NIL for none.")

(defun sftp-send-octets (link handle fd size path)
  "Write the SIZE octets of the file open as FD to the file open as HANDLE
on LINK's server, PATH, from its start, a piece of +SFTP-PIECE+ octets at a
time, +SFTP-PIECES-AHEAD+ sent before their answers come; stopped between
pieces when the job doing it is asked to stop."
  (let ((asked '()) (sent 0) (piece (make-array +sftp-piece+ :element-type '(unsigned-byte 8))))
    (loop while (or (< sent size) asked)
          do (loop while (and (< sent size) (< (length asked) +sftp-pieces-ahead+))
                   do (let ((count (tuatara.io:read-fd-at fd piece sent :end (min +sftp-piece+ (- size sent))
                                                                       :path path)))
                        (when (zerop count) (sftp-fail link "write" path "its copy here ended early"))
                        (push (sftp-put-packet link :write (list handle (list :u64 sent) (subseq piece 0 count)))
                              asked)
                        (incf sent count)))
             (when *sftp-octets-sent* (setf (car *sftp-octets-sent*) sent))
             (sftp-flush link path "write")
             (multiple-value-bind (answer said id) (sftp-answer link asked path "write")
               (declare (ignore said))
               (unless (eq answer :done) (tuatara.frontend:malformed "~(~a~) to a write" answer))
               (setf asked (remove id asked)))
             (tuatara.runtime:job-check-stop))))

(defun sftp-to-disk (link handle path)
  "Have LINK's server put what the file open as HANDLE holds on its disk, when it offers that (fsync@openssh.com)."
  (when (sftp-extension-p link "fsync@openssh.com")
    (sftp-done link :extended (list "fsync@openssh.com" handle) path "write")))

(defun sftp-entry-attributes (link path)
  "The attributes PATH's entry has in the listing of its folder on LINK's
server, with :LINKS, the count of its names, when the server's long name of
it says it (SFTP-LONG-NAME-LINKS); NIL when it is not listed. The folder is
read until the entry comes."
  (let* ((slash (position #\/ path :from-end t))
         (folder (if slash (subseq path 0 (1+ slash)) "."))
         (name (subseq path (if slash (1+ slash) 0))))
    (sftp-with-handle
     link :opendir (list folder) folder "examine"
     (lambda (handle)
       (loop
         (multiple-value-bind (answer names) (sftp-ask link :readdir (list handle) folder "examine")
           (case answer
             (:end (return nil))
             (:name (let ((entry (assoc name names :test #'string=)))
                      (when entry (return (cdr entry)))))
             (t (tuatara.frontend:malformed "~(~a~) where names were asked for" answer))))
         (tuatara.runtime:job-check-stop))))))

(defun sftp-owner-refusal (link handle path like)
  "Why the new file open as HANDLE at PATH on LINK's server cannot be given
the owner and group LIKE, attributes, say, in words; NIL when it has them,
given them by the server if need be."
  (let ((uid (getf like :uid)) (gid (getf like :gid)) (made (sftp-attributes link path :handle handle)))
    (unless (or (null uid) (and (eql uid (getf made :uid)) (eql gid (getf made :gid))))
      (handler-case
          (progn (sftp-done link :fsetstat (list handle (list :attributes (list :uid uid :gid gid))) path "write")
                 nil)
        (sftp-refusal () "a new file could not be given its owner and group")))))

(defun sftp-replace (link path fd size permissions like check &optional vet)
  "Write the SIZE octets of the file open as FD into a new file beside PATH
on LINK's server, made with PERMISSIONS (else the server's own), and rename
it over PATH once CHECK, a function of no arguments, signals nothing; NIL
once done. With LIKE, attributes, the new file is first given their owner
and group, and once written exactly PERMISSIONS; words saying why not when
the server will not, the new file taken away and PATH left as it was. VET,
when given, a function of the new file's name there, is called once it is
written, and what it signals leaves PATH as it was and takes the new file
away. A server that cannot rename over a file in one step
\(posix-rename@openssh.com) has only a new file written. A write that fails
or is stopped takes the new file away, over a new connection when answers
to it were still to come."
  (let* ((slash (position #\/ path :from-end t))
         (temporary (format nil "~a.~a.~a" (subseq path 0 (1+ slash)) (subseq path (1+ slash))
                            (tuatara.io:random-name)))
         (handle nil) (made nil))
    (unwind-protect
         (block replacing
           ;; written, made, and only when nothing has the name yet
           (setf handle (sftp-handle link :open (list temporary #x2A (list :attributes (list :permissions permissions)))
                                     temporary "write")
                 made t)
           (let ((why (and like (sftp-owner-refusal link handle temporary like))))
             (when why (return-from replacing why)))
           (sftp-send-octets link handle fd size path)
           (when (and like permissions)
             (handler-case
                 (sftp-done link :fsetstat (list handle (list :attributes (list :permissions permissions)))
                            temporary "write")
               (sftp-refusal () (return-from replacing "a new file could not be given its permissions"))))
           (sftp-to-disk link handle path)
           (sftp-let-go link (shiftf handle nil) path)
           (when vet (funcall vet temporary))
           (funcall check)
           (cond ((sftp-extension-p link "posix-rename@openssh.com")
                  (sftp-done link :extended (list "posix-rename@openssh.com" temporary path) path "write"))
                 ((null (sftp-attributes link path :follow nil))
                  (sftp-done link :rename (list temporary path) path "write"))
                 (t (sftp-fail link "write" path "the server cannot put a new file in the place of another in ~
                                                  one step (posix-rename@openssh.com)")))
           (setf made nil)
           nil)
      (if (and (sftp-link-fd link) (zerop (sftp-link-waiting link)))
          (when handle
            (ignoring-errors-reported ("closing ~a" temporary) (sftp-let-go link handle temporary)))
          ;; answers still to come, or the connection gone: a new one, the handle going with the old
          (when made
            (ignoring-errors-reported ("connecting again to take away ~a" temporary)
              (sftp-close link)
              (sftp-connect link))))
      (when (and made (sftp-link-fd link))
        (ignoring-errors-reported ("taking away ~a" temporary)
          (sftp-done link :remove (list temporary) temporary "remove"))))))

(defun sftp-write-in-place (link path fd size check partly)
  "Write the SIZE octets of the file open as FD over the file at PATH on
LINK's server itself, once CHECK, a function of no arguments that signals
when the file there may not be written over and else gives its attributes,
signals nothing: opened, found the very file CHECK found (its kind,
permissions, owner, group, size and time), written from its start and cut
to SIZE, so that it keeps its owner, group, permissions and other names.
PARTLY, a cons, has true put in its car when the work fails once writing
has begun: the file there is then left partly written. A server that will
not cut it to SIZE leaves the end of the old file after the new text, and
that is said."
  (let ((found (funcall check)))
    (sftp-with-handle
     link :open (list path #x02 '(:attributes ())) path "write"
     (lambda (handle)
       (let ((opened (sftp-attributes link path :handle handle)))
         (unless (every (lambda (key) (eql (getf found key) (getf opened key))) '(:permissions :uid :gid :size :mtime))
           (sftp-refuse link "write" path "the file opened is not the one found there a moment before")))
       (handler-bind ((serious-condition (lambda (condition)
                                           (declare (ignore condition))
                                           (setf (car partly) t))))
         (sftp-send-octets link handle fd size path)
         (handler-case (sftp-done link :fsetstat (list handle (list :attributes (list :size size))) path "write")
           (sftp-refusal ()
             (sftp-refuse link "write" path "the server would not cut it to its new length: the end of the old file ~
                                             is left after the new text")))
         (sftp-to-disk link handle path))))))

(defun remote-upload (host path fd size &key permissions expected like linked-refusal vet (partly (list nil)))
  "Write the SIZE octets of the file open as FD as the file at PATH on HOST,
only when the file there is as EXPECTED says, a function of its attributes
\(NIL when it is not there) that gives NIL when it may be written over, else
words saying why not; checked again just before the file is replaced. As a
rule into a new file beside it, with PERMISSIONS (else the server's own),
renamed over it (SFTP-REPLACE). LIKE, the attributes of the file there as
last read, makes the new file like it: its owner and group, and exactly
PERMISSIONS. When the server will not give it those, or the file there has
more than one name (hard links, as the server's long name of it says), the
file itself is written over in place (SFTP-WRITE-IN-PLACE), keeping its
owner, group, permissions, other names and the rest SFTP cannot copy; a
write that fails half-way then leaves it part written, and true is put in
the car of PARTLY, a cons. LINKED-REFUSAL, words or NIL: when given, a file
with more than one name is refused, saying them, not written in place. The
attributes of the file written, and as a second value why it was written in
place, in words, or NIL when it was replaced. With VET (SFTP-REPLACE), a
file that cannot be replaced is refused, never written in place."
  (call-with-sftp
   host path "write"
   (lambda (link)
     (flet ((check ()
              (let* ((attributes (sftp-attributes link path :follow nil))
                     (why (funcall expected attributes)))
                (when why (sftp-fail link "write" path "~a" why))
                attributes)))
       (check)
       (let ((why (and like
                       (let ((links (getf (sftp-entry-attributes link path) :links)))
                         (when (and links (> links 1))
                           (when linked-refusal
                             (sftp-refuse link "write" path "it has ~d names (hard links), and ~a" links linked-refusal))
                           (format nil "it has ~d names (hard links)" links))))))
         (unless why
           (setf why (sftp-replace link path fd size permissions like #'check vet)))
         (when (and why vet)
           (sftp-fail link "write" path "~a" why))
         (when why
           (sftp-write-in-place link path fd size #'check partly))
         (values (sftp-attributes link path) why))))))

(defun remote-change (host operation path &optional to)
  "Do OPERATION on PATH on HOST: :REMOVE a file, :RMDIR an empty folder,
:MKDIR a folder, with TO its permissions when given, or :RENAME it TO, never
over a file there already."
  (call-with-sftp host path (string-downcase operation)
                  (lambda (link)
                    (sftp-done link operation (ecase operation
                                                ((:remove :rmdir) (list path))
                                                (:mkdir (list path (list :attributes (and to (list :permissions to)))))
                                                (:rename (list path to)))
                               path (ecase operation
                                      (:remove "remove") (:rmdir "remove") (:mkdir "make") (:rename "rename"))))))

(defun close-remote-links (&optional editor)
  "Close every connection to a server (as the editor stops)."
  (declare (ignore editor))
  (dolist (link (sb-thread:with-mutex (**sftp-links-lock**) (shiftf **sftp-links** '())))
    (sb-thread:with-mutex ((sftp-link-lock link) :wait-p nil)
      (sftp-close link))))
