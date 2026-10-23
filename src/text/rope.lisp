;;;; src/text/rope.lisp --- Persistent rope of UTF-8 leaves with cached counts.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; The rope is a persistent B+tree.  Leaves are either in-memory octet vectors
;;;; (LEAF) or ranges of a memory-mapped file that never enters the Lisp heap
;;;; (SPAN); both hold at most +MAX-LEAF+ octets.  Branches hold up to
;;;; +MAX-FANOUT+ children of equal height.  Every node caches its byte,
;;;; character, UTF-16 unit and newline counts; for spans of a freshly opened
;;;; file the counts are unknown (-1) until INDEX-ROPE has scanned the file, in
;;;; parallel, which is how a file of gigabytes opens at once.  Nodes are never
;;;; mutated after construction, so a snapshot is the root pointer and an edit
;;;; copies only the path to the leaves it touches.  A batch of sorted edits is
;;;; applied in one descent (APPLY-EDITS).

(in-package #:tuatara.text)

;;; Types and tunables

(deftype octets ()
  "A simple vector of octets: the storage type of all text in Tuatara (UTF-8)."
  '(simple-array (unsigned-byte 8) (*)))

(deftype fixvec ()
  "A simple vector of fixnums, used for batches of positions."
  '(simple-array fixnum (*)))

(defconstant +max-leaf+ 32768
  "Maximum number of octets in a leaf or span. An indexed leaf never splits a
UTF-8 sequence. Every scan inside a leaf is bounded by this. Twice +SPAN-SIZE+,
so that an edited span becomes one in-memory leaf without splitting.")

(defconstant +target-leaf+ 4096
  "Size of the in-memory leaves built from fresh text, so that edits have
headroom before a leaf has to split.")

(defconstant +min-leaf+ 1024
  "Leaves smaller than this are merged with a neighbour when a batch rebuilds their parent.")

(defconstant +max-fanout+ 32
  "Maximum number of children of a branch.")

(defconstant +span-size+ 16384
  "Size of the file-backed spans a mapped file is cut into on open.")

(declaim (inline continuation-byte-p make-octets))

(defun continuation-byte-p (octet)
  "True when OCTET is a UTF-8 continuation byte (10xxxxxx), i.e. not the start of a character."
  (declare (type (unsigned-byte 8) octet))
  (= (logand octet #xC0) #x80))

(defun make-octets (length)
  "Return a fresh octet vector of LENGTH."
  (declare (type fixnum length))
  (make-array length :element-type '(unsigned-byte 8)))

(defconstant +block-slack+ 256
  "Octets a kept block has past its size, for a needle or a character read on past it.")

(sb-ext:defglobal **octet-blocks**
    (map 'simple-vector
         (lambda (size)
           (tuatara.runtime:make-spares (lambda () (make-octets (+ size +block-slack+)))))
         '(4096 65536 1048576))
  "Blocks of octets text is read into, kept to be read into again: of 4 KB,
64 KB and a megabyte, each with +BLOCK-SLACK+ octets more (WITH-OCTET-BLOCK).")

(defun octet-block-spares (size)
  "The kept blocks of the least size holding SIZE octets, or NIL when none is that large."
  (declare (type fixnum size))
  (cond ((<= size (+ 4096 +block-slack+)) (svref **octet-blocks** 0))
        ((<= size (+ 65536 +block-slack+)) (svref **octet-blocks** 1))
        ((<= size (+ 1048576 +block-slack+)) (svref **octet-blocks** 2))
        (t nil)))

(defmacro with-octet-block ((var size) &body body)
  "Run BODY with VAR bound to octets at least SIZE long: a block kept for
reading text into (**OCTET-BLOCKS**), given back after however BODY ends, or
for a size larger than a megabyte, new octets. BODY must not keep it."
  (let ((spares (gensym "SPARES")) (body-function (gensym "BODY")) (n (gensym "SIZE")))
    `(flet ((,body-function (,var)
              (declare (type octets ,var))
              ,@body))
       (declare (dynamic-extent #',body-function))
       (let* ((,n ,size) (,spares (octet-block-spares ,n)))
         (if ,spares
             (tuatara.runtime:with-spare (,var ,spares) (,body-function ,var))
             (,body-function (make-octets ,n)))))))

;;; UTF-8, strictly
;;;
;;; Every sequence of octets is text. A well-formed UTF-8 sequence (Unicode's
;;; table 3-7: no overlong forms, no surrogates, nothing past U+10FFFF) is one
;;; character; every other octet is a character of its own, a raw byte, which
;;; decodes to U+DC80..U+DCFF (the octet plus #xDC00, as Python's
;;; surrogateescape has it) and encodes back to that octet, so that a file of
;;; invalid UTF-8 is shown, edited and saved without losing an octet. Whether an
;;; octet starts a character depends on at most three octets before it and
;;; three after; every leaf and span starts where a character starts.

(defconstant +raw-byte-base+ #xDC00
  "A raw byte B decodes to the character with code B plus this.")

(declaim (inline raw-byte-code-p))
(defun raw-byte-code-p (code)
  "True when CODE is the code of a raw byte's character, U+DC80..U+DCFF."
  (<= #xDC80 code #xDCFF))

(declaim (inline control-code-p))
(defun control-code-p (code)
  "True when CODE is a control character's: 0 to 31, 127 to 159."
  (or (< code 32) (<= 127 code 159)))

(declaim (inline crlf-octets-p))
(defun crlf-octets-p (octets i avail)
  "True when OCTETS hold a carriage return at index I and a newline after it,
below AVAIL: a line end of two octets."
  (declare (type octets octets) (type fixnum i avail))
  (and (= (aref octets i) 13) (< (1+ i) avail) (= (aref octets (1+ i)) 10)))

(declaim (inline line-text-end))
(defun line-text-end (octets start newline crlf)
  "Where the text of a line ends in OCTETS, whose newline is at index NEWLINE
and which starts at index START or before: with CRLF, at the carriage return
just before the newline, when there is one after START; else at the newline."
  (declare (type octets octets) (type fixnum start newline))
  (if (and crlf (> newline start) (= (aref octets (1- newline)) 13)) (1- newline) newline))

(sb-ext:defglobal **utf8-classes**
    (let ((table (make-array 256 :element-type '(unsigned-byte 8))))
      (dotimes (b 256 table)
        (setf (aref table b)
              (cond ((< b #x80) 0) ((< b #x90) 1) ((< b #xA0) 2) ((< b #xC0) 3) ((< b #xC2) 4)
                    ((< b #xE0) 5) ((= b #xE0) 6) ((= b #xED) 8) ((< b #xF0) 7)
                    ((= b #xF0) 9) ((< b #xF4) 10) ((= b #xF4) 11) (t 4)))))
  "The class of each octet in decoding UTF-8: 0 ASCII; 1, 2 and 3 continuation
octets 80-8F, 90-9F and A0-BF; 4 never valid (C0, C1, F5-FF); 5 the leads of two
octets; 6 E0; 7 the other leads of three; 8 ED; 9 F0; 10 F1-F3; 11 F4.")
(declaim (type (simple-array (unsigned-byte 8) (256)) **utf8-classes**))

(sb-ext:defglobal **utf8-next**
    (let ((table (make-array 96 :element-type '(unsigned-byte 8) :initial-element 8)))
      (flet ((to (state classes next)
               (dolist (class classes) (setf (aref table (+ (* state 12) class)) next))))
        ;; at a character start: ASCII, a raw byte, or a lead
        (to 0 '(0) 0) (to 0 '(1 2 3 4) 9)
        (to 0 '(5) 1) (to 0 '(6) 4) (to 0 '(7) 2) (to 0 '(8) 5) (to 0 '(9) 6) (to 0 '(10) 3) (to 0 '(11) 7)
        ;; one, two or three more continuation octets of any value
        (to 1 '(1 2 3) 0) (to 2 '(1 2 3) 1) (to 3 '(1 2 3) 2)
        ;; after E0, ED, F0 and F4 the next octet has a narrower range
        (to 4 '(3) 1) (to 5 '(1 2) 1) (to 6 '(2 3) 2) (to 7 '(1) 2))
      table)
  "The UTF-8 decoder's transitions, at (+ (* STATE 12) CLASS): 0 a character is
complete; 1 to 7 more continuation octets are needed; 8 the sequence begun is
not well formed (its octets are raw bytes, and this one starts afresh); 9 this
octet, at a character start, is a raw byte.")
(declaim (type (simple-array (unsigned-byte 8) (96)) **utf8-next**))

(defmacro utf8-decode-form (byte-at i end)
  "A form giving the code of the character at I and its length in octets, the
octets read by (BYTE-AT index), none at or after END: a raw byte's character and
length 1 for an octet that starts no well-formed sequence."
  (let ((b0 (gensym "B0")) (state (gensym "STATE")) (code (gensym "CODE")) (k (gensym "K"))
        (p (gensym "P")) (b (gensym "B")) (next (gensym "NEXT")))
    `(let* ((,b0 (,byte-at ,i))
            (,state (aref **utf8-next** (aref **utf8-classes** ,b0))))
       (declare (type (unsigned-byte 8) ,b0 ,state))
       (cond ((zerop ,state) (values ,b0 1))
             ((= ,state 9) (values (+ +raw-byte-base+ ,b0) 1))
             (t (let ((,code (logand ,b0 (case ,state (1 #x1F) ((2 4 5) #x0F) (t #x07))))
                      (,k 1))
                  (declare (type fixnum ,code ,k))
                  (loop
                    (let ((,p (+ ,i ,k)))
                      (declare (type fixnum ,p))
                      (when (>= ,p ,end) (return (values (+ +raw-byte-base+ ,b0) 1)))
                      (let* ((,b (,byte-at ,p))
                             (,next (aref **utf8-next** (+ (* ,state 12) (aref **utf8-classes** ,b)))))
                        (declare (type (unsigned-byte 8) ,b ,next))
                        (when (= ,next 8) (return (values (+ +raw-byte-base+ ,b0) 1)))
                        (setf ,code (logior (ash ,code 6) (logand ,b #x3F)))
                        (incf ,k)
                        (when (zerop ,next) (return (values ,code ,k)))
                        (setf ,state ,next))))))))))

(defmacro utf8-char-start-form (byte-at i start end)
  "A form, true when the octet at I starts a character of the text running from
START below END, its octets read by (BYTE-AT index): any octet but a
continuation octet does; a continuation octet does unless a well-formed
sequence begun at most three octets before it takes it in."
  (let ((k (gensym "K")) (j (gensym "J")) (bj (gensym "BJ")))
    `(if (/= (logand (,byte-at ,i) #xC0) #x80)
         t
         (loop for ,k of-type fixnum from 1 to 3
               for ,j of-type fixnum = (- ,i ,k)
               do (when (< ,j ,start) (return t))
                  (let ((,bj (,byte-at ,j)))
                    (declare (type (unsigned-byte 8) ,bj))
                    (when (/= (logand ,bj #xC0) #x80)
                      (return (<= (the fixnum (nth-value 1 (utf8-decode-form ,byte-at ,j ,end))) ,k))))
               finally (return t)))))

(defmacro utf8-pending-form (byte-at start end)
  "A form giving how many octets at the end of the text from START below END
begin a well-formed sequence that is not finished there, which octets following
them could finish: 0, 1, 2 or 3."
  (let ((k (gensym "K")) (j (gensym "J")) (state (gensym "STATE")) (p (gensym "P")))
    `(loop for ,k of-type fixnum from 1 to 3
           for ,j of-type fixnum = (- ,end ,k)
           do (when (< ,j ,start) (return 0))
              (when (/= (logand (,byte-at ,j) #xC0) #x80)
                (let ((,state (aref **utf8-next** (aref **utf8-classes** (,byte-at ,j)))))
                  (declare (type (unsigned-byte 8) ,state))
                  (when (or (zerop ,state) (= ,state 9)) (return 0))
                  (loop for ,p of-type fixnum from (1+ ,j) below ,end
                        do (setf ,state (aref **utf8-next** (+ (* ,state 12) (aref **utf8-classes** (,byte-at ,p)))))
                           (when (or (zerop ,state) (= ,state 8)) (return-from nil)))
                  (return (if (or (zerop ,state) (= ,state 8)) 0 ,k))))
           finally (return 0))))

(defun octets-decode (octets i end)
  "The code of the character at index I of OCTETS, whose text ends at END, and
its length in octets (see this section's header: an octet that starts no
well-formed sequence is a raw byte of length 1)."
  (declare (type octets octets) (type fixnum i end) (optimize speed (safety 0)))
  (macrolet ((byte-at (p) `(aref octets ,p)))
    (utf8-decode-form byte-at i end)))

(defun octets-char-start-p (octets i start end)
  "True when index I of OCTETS, whose text runs from START below END, starts a character."
  (declare (type octets octets) (type fixnum i start end) (optimize speed (safety 0)))
  (macrolet ((byte-at (p) `(aref octets ,p)))
    (utf8-char-start-form byte-at i start end)))

(defun octets-pending (octets start end)
  "How many octets at the end of OCTETS' text from START below END begin a
sequence that following octets could finish (UTF8-PENDING-FORM)."
  (declare (type octets octets) (type fixnum start end) (optimize speed (safety 0)))
  (macrolet ((byte-at (p) `(aref octets ,p)))
    (utf8-pending-form byte-at start end)))

(defun sap-char-start-p (sap i start end)
  "True when offset I of the memory at SAP, whose text runs from START below END, starts a character."
  (declare (type sb-sys:system-area-pointer sap) (type fixnum i start end) (optimize speed (safety 0)))
  (macrolet ((byte-at (p) `(sb-sys:sap-ref-8 sap ,p)))
    (utf8-char-start-form byte-at i start end)))

(defconstant +ascii-mask+ #x8080808080808080 "The top bit of each octet of a word.")
(defconstant +low-seven+ #x7F7F7F7F7F7F7F7F "The other seven bits of each octet of a word.")
(defconstant +newlines+ #x0A0A0A0A0A0A0A0A "A word of eight newline octets.")

(declaim (inline word-newlines))
(defun word-newlines (word)
  "How many of the eight octets of WORD are newlines."
  (declare (type (unsigned-byte 64) word) (optimize speed (safety 0)))
  (let* ((x (logxor word +newlines+))
         (nonzero (logior (logand (+ (logand x +low-seven+) +low-seven+) #xFFFFFFFFFFFFFFFF) x)))
    (declare (type (unsigned-byte 64) x nonzero))
    (logcount (logandc1 nonzero +ascii-mask+))))

(defun summarise-sap (sap start end)
  "Scan the octets at SAP from offset START, which must start a character,
below END, and return four values: characters (a raw byte is one), UTF-16 code
units (a raw byte is one), newline octets, and raw bytes. Words of eight ASCII
octets go at once. Complexity: O(END - START)."
  (declare (type sb-sys:system-area-pointer sap) (type fixnum start end) (optimize speed (safety 0)))
  (let ((chars 0) (utf16 0) (lines 0) (invalid 0) (state 0) (pending 0) (i start)
        (classes **utf8-classes**) (next **utf8-next**))
    (declare (type fixnum chars utf16 lines invalid state pending i)
             (type (simple-array (unsigned-byte 8) (256)) classes)
             (type (simple-array (unsigned-byte 8) (96)) next))
    (loop
      (when (zerop state)
        (loop while (<= (+ i 8) end)
              do (let ((word (sb-sys:sap-ref-64 sap i)))
                   (declare (type (unsigned-byte 64) word))
                   (unless (zerop (logand word +ascii-mask+)) (return))
                   (incf lines (word-newlines word))
                   (incf chars 8) (incf utf16 8) (incf i 8))))
      (when (>= i end) (return))
      (let* ((b (sb-sys:sap-ref-8 sap i))
             (n (aref next (+ (* state 12) (aref classes b)))))
        (declare (type (unsigned-byte 8) b n))
        (case n
          (0 (incf chars) (incf utf16 (if (= pending 3) 2 1))
             (when (= b 10) (incf lines))
             (setf state 0 pending 0) (incf i))
          (9 (incf chars) (incf utf16) (incf invalid) (incf i))
          ;; the octets of the sequence begun are raw bytes; this octet starts afresh
          (8 (incf chars pending) (incf utf16 pending) (incf invalid pending)
             (setf state 0 pending 0))
          (t (setf state n) (incf pending) (incf i)))))
    (values (+ chars pending) (+ utf16 pending) lines (+ invalid pending))))

(defun newlines-sap (sap start end)
  "Count the newline octets at SAP from offset START below END, eight at a
time: a newline octet is never part of another character in UTF-8.
Complexity: O(END - START)."
  (declare (type sb-sys:system-area-pointer sap) (type fixnum start end) (optimize speed (safety 0)))
  (let ((lines 0) (i start))
    (declare (type fixnum lines i))
    (loop while (<= (+ i 8) end)
          do (incf lines (word-newlines (sb-sys:sap-ref-64 sap i)))
             (incf i 8))
    (loop while (< i end)
          do (when (= (sb-sys:sap-ref-8 sap i) 10) (incf lines))
             (incf i))
    lines))

(defun summarise (octets start end)
  "SUMMARISE-SAP over the octet vector OCTETS from START below END."
  (declare (type octets octets) (type fixnum start end))
  (sb-sys:with-pinned-objects (octets)
    (summarise-sap (sb-sys:vector-sap octets) start end)))

(defun unfinished-sequence-length (octets i end)
  "How many octets at index I of OCTETS, whose text ends at END, make one
piece of what is not UTF-8: a sequence well begun and cut short, its octets
up to where it goes wrong; else the one octet at I."
  (declare (type octets octets) (type fixnum i end) (optimize speed (safety 0)))
  (let ((state (aref **utf8-next** (aref **utf8-classes** (aref octets i)))) (k 1))
    (declare (type (unsigned-byte 8) state) (type fixnum k))
    (if (or (zerop state) (= state 9))
        1
        (loop (when (>= (+ i k) end) (return k))
              (let ((next (aref **utf8-next** (+ (* state 12) (aref **utf8-classes** (aref octets (+ i k)))))))
                (declare (type (unsigned-byte 8) next))
                (when (= next 8) (return k))
                (incf k)
                (setf state next))))))

(defun octets-string (octets &key (start 0) (end (length octets)) replacement)
  "OCTETS from START below END as a string: well-formed UTF-8 decoded, every
other octet as its raw byte's character (U+DC80..U+DCFF); with REPLACEMENT, a
character, each piece of what is not UTF-8 as that character instead: a
sequence cut short as one, any other octet as one each."
  (declare (type octets octets) (type fixnum start end) (type (or null character) replacement))
  (let ((out (make-string (- end start))) (n 0) (i start))
    (declare (type fixnum n i))
    (loop while (< i end)
          do (let ((b (aref octets i)))
               (if (< b #x80)
                   (progn (setf (schar out n) (code-char b)) (incf n) (incf i))
                   (multiple-value-bind (code length) (octets-decode octets i end)
                     (cond ((not (and replacement (raw-byte-code-p code)))
                            (setf (schar out n) (code-char code)) (incf n) (incf i length))
                           (t (setf (schar out n) replacement) (incf n)
                              (incf i (unfinished-sequence-length octets i end))))))))
    (if (= n (length out)) out (subseq out 0 n))))

(defun lenient-string (octets &key (start 0) (end (length octets)) (replacement #\?))
  "OCTETS from START below END read as UTF-8 text, what is not UTF-8 as
REPLACEMENT, a character or string: a program's output, a part of a file read
to be shown."
  (if (and (typep octets 'octets) (characterp replacement))
      (octets-string octets :start start :end end :replacement replacement)
      (sb-ext:octets-to-string octets :start start :end end :external-format (list :utf-8 :replacement replacement))))

(declaim (inline char-octet-length))
(defun char-octet-length (char)
  "How many octets CHAR takes in the text: as STRING-OCTETS writes it, a raw
byte's character as its one octet, any other lone surrogate as U+FFFD's three."
  (let ((code (char-code char)))
    (cond ((< code #x80) 1)
          ((raw-byte-code-p code) 1)
          ((< code #x800) 2)
          ((< code #x10000) 3)
          (t 4))))

(defun string-octet-length (string &key (start 0) (end (length string)))
  "How many octets STRING from START below END takes in the text, counted
without making them (CHAR-OCTET-LENGTH)."
  (loop for i from start below end sum (char-octet-length (char string i))))

(defun put-string-octets (string out at &key (start 0) (end (length string)))
  "Put STRING from START below END into OUT, octets, from AT, as
STRING-OCTETS writes it, a larger copy of OUT made first when it has no room:
that vector and where the next octet goes."
  (declare (type string string) (type octets out) (type fixnum at start end) (optimize speed))
  (let ((room (+ at (* 4 (- end start)))))
    (declare (type fixnum room))
    (when (> room (length out))
      (let ((larger (make-array (max room (* 2 (length out))) :element-type '(unsigned-byte 8))))
        (replace larger out :end2 at)
        (setf out larger))))
  (macrolet ((put (octet) `(progn (setf (aref out at) ,octet) (incf at)))
             (each-code ((code) &body body)
               ;; the loop written out for the usual kind of string, so that its characters are read directly
               `(flet ((body (,code) (declare (type (integer 0 #x10FFFF) ,code)) ,@body))
                  (declare (inline body))
                  (if (typep string '(simple-array character (*)))
                      (loop for i of-type fixnum from start below end do (body (char-code (schar string i))))
                      (loop for i of-type fixnum from start below end do (body (char-code (char string i))))))))
    (each-code (code)
       (cond ((< code #x80) (put code))
             ((raw-byte-code-p code) (put (- code +raw-byte-base+)))
             ((<= #xD800 code #xDFFF) (put #xEF) (put #xBF) (put #xBD))
             ((< code #x800)
              (put (logior #xC0 (ash code -6)))
              (put (logior #x80 (logand code #x3F))))
             ((< code #x10000)
              (put (logior #xE0 (ash code -12)))
              (put (logior #x80 (logand (ash code -6) #x3F)))
              (put (logior #x80 (logand code #x3F))))
             (t
              (put (logior #xF0 (ash code -18)))
              (put (logior #x80 (logand (ash code -12) #x3F)))
              (put (logior #x80 (logand (ash code -6) #x3F)))
              (put (logior #x80 (logand code #x3F)))))))
  (values out at))

(defun string-octets (string &key (start 0) (end (length string)))
  "STRING from START below END as UTF-8 octets, a raw byte's character
(U+DC80..U+DCFF) as its octet; any other lone surrogate, which no text decodes
to, as U+FFFD."
  (if (not (find-if (lambda (c) (<= #xD800 (char-code c) #xDFFF)) string :start start :end end))
      (sb-ext:string-to-octets string :external-format :utf-8 :start start :end end)
      (multiple-value-bind (out at)
          (put-string-octets string (make-array (* 4 (- end start)) :element-type '(unsigned-byte 8)) 0 :start start :end end)
        (subseq out 0 at))))

;;; Mapped files

(defstruct (mapping (:constructor %make-mapping (sap length path fd resources)) (:copier nil))
  "A memory-mapped file: SAP addresses LENGTH octets of PATH, read-only, outside
the Lisp heap, mapped from the open descriptor FD, which the mapping keeps (so
saving can copy straight from it). LIVE turns false when the mapping is closed.
RESOURCES is a cons whose car lists every (ADDRESS LENGTH FD) the mapping has
held; RETARGET-MAPPING adds one when the octets are moved to a snapshot."
  (sap (sb-sys:int-sap 0) :type sb-sys:system-area-pointer)
  (length 0 :type fixnum :read-only t)
  (path nil :read-only t)
  (fd -1 :type fixnum)
  (resources (list '()) :read-only t)
  (live t :type boolean))

(setf (documentation 'mapping-p 'function) "True when OBJECT is a MAPPING."
      (documentation 'mapping-sap 'function) "The foreign pointer to the mapped octets."
      (documentation 'mapping-length 'function) "Number of mapped octets."
      (documentation 'mapping-path 'function) "The mapped file's path."
      (documentation 'mapping-fd 'function) "The open descriptor the mapped octets come from."
      (documentation 'mapping-resources 'function) "A cons whose car lists the (ADDRESS LENGTH FD) the mapping holds."
      (documentation 'mapping-live 'function) "True until the mapping is closed with CLOSE-MAPPING.")

(defun release-resources (resources)
  "Unmap and close every (ADDRESS LENGTH FD) in the car of RESOURCES, once."
  (loop for (address length fd) in (shiftf (car resources) '())
        do (ignore-errors (sb-posix:munmap (sb-sys:int-sap address) length))
           (ignore-errors (sb-posix:close fd))))

(defun map-fd (fd size path)
  "Map SIZE octets of the open file FD read-only and return a MAPPING that owns
FD from now on, closing it with the mapping. PATH names the file in messages.
The mapping is released when it is garbage collected or closed explicitly."
  (declare (type fixnum fd size))
  (let* ((sap (sb-posix:mmap nil size sb-posix:prot-read sb-posix:map-private fd 0))
         (resources (list (list (list (sb-sys:sap-int sap) size fd))))
         (mapping (%make-mapping sap size path fd resources)))
    ;; Ask the kernel to start reading the file in; the index pass follows soon.
    ;; madvise(2) with MADV_WILLNEED (3 on Linux); sb-posix does not wrap it.
    (ignore-errors
     (sb-alien:alien-funcall
      (sb-alien:extern-alien "madvise" (function sb-alien:int sb-sys:system-area-pointer
                                                 sb-alien:unsigned-long sb-alien:int))
      sap size 3))
    (sb-ext:finalize mapping (lambda () (release-resources resources)) :dont-save t)
    mapping))

(defun map-file (path)
  "Map the regular file at PATH read-only and return a MAPPING, or NIL for an
empty file; an error for anything else, a named pipe or a device, which is
never waited on. The mapping keeps the file open and is released when it is
garbage collected or closed explicitly."
  (let ((fd (sb-posix:open (if (pathnamep path) (sb-ext:native-namestring path) path)
                           (logior sb-posix:o-rdonly sb-posix:o-nonblock)))
        (mapped nil))
    (unwind-protect
         (let ((stat (sb-posix:fstat fd)))
           (unless (= (logand (sb-posix:stat-mode stat) sb-posix:s-ifmt) sb-posix:s-ifreg)
             (error "~a is not a regular file." path))
           (unless (zerop (sb-posix:stat-size stat))
             (setf mapped (map-fd fd (sb-posix:stat-size stat) path))))
      (unless mapped (sb-posix:close fd)))))

(sb-ext:defglobal **mapping-lock** (sb-thread:make-mutex :name "tuatara mappings")
  "Held while a mapping is moved or closed, which threads other than the
editor's may do.")

(defun retarget-mapping (mapping fd)
  "Make MAPPING read its octets from FD, an open file holding the very same
octets (a snapshot of the original), so that every rope using MAPPING is safe
from changes made to the original file afterwards. The old region stays mapped
until the mapping is released, for any reader in the middle of it. Any thread
may do this. Returns MAPPING, or NIL (FD left open) when it was closed meanwhile."
  (sb-thread:with-mutex (**mapping-lock**)
    (when (mapping-live mapping)
      (let ((sap (sb-posix:mmap nil (mapping-length mapping) sb-posix:prot-read sb-posix:map-private fd 0)))
        (push (list (sb-sys:sap-int sap) (mapping-length mapping) fd) (car (mapping-resources mapping)))
        (setf (mapping-sap mapping) sap (mapping-fd mapping) fd)
        mapping))))

(defun close-mapping (mapping)
  "Unmap MAPPING now. Ropes still referring to it signal an error when read."
  (sb-thread:with-mutex (**mapping-lock**)
    (when (mapping-live mapping)
      (setf (mapping-live mapping) nil)
      (sb-ext:cancel-finalization mapping)
      (release-resources (mapping-resources mapping))))
  nil)

(declaim (inline check-mapping))
(defun check-mapping (mapping)
  "Signal an error unless MAPPING is still mapped."
  (unless (mapping-live mapping)
    (error "The mapping of ~a has been closed." (mapping-path mapping))))

;;; Nodes

(defstruct (node (:constructor nil) (:copier nil) (:predicate nil))
  "Summary slots shared by all nodes: the counts of the text below the node.
BYTES is always known; CHARS, UTF16, LINES and INVALID (raw bytes) are -1 while
unknown (unindexed spans)."
  (bytes 0 :type fixnum :read-only t)
  (chars -1 :type fixnum :read-only t)
  (utf16 -1 :type fixnum :read-only t)
  (lines -1 :type fixnum :read-only t)
  (invalid -1 :type fixnum :read-only t))

(declaim (inline node-indexed-p))
(defun node-indexed-p (node)
  "True when NODE's character, UTF-16 and line counts are known."
  (>= (node-chars node) 0))

(defstruct (leaf (:include node)
                 (:constructor %make-leaf (octets start bytes chars utf16 lines invalid))
                 (:copier nil))
  "An in-memory leaf: BYTES octets of the immutable vector OCTETS starting at START."
  (octets (make-octets 0) :type octets :read-only t)
  (start 0 :type fixnum :read-only t))

(defstruct (span (:include node)
                 (:constructor %make-span (mapping offset bytes chars utf16 lines invalid))
                 (:copier nil))
  "A file-backed leaf: BYTES octets of MAPPING starting at OFFSET."
  (mapping nil :type mapping :read-only t)
  (offset 0 :type fixnum :read-only t))

(defstruct (branch (:include node)
                   (:constructor %make-branch (children height bytes chars utf16 lines invalid))
                   (:copier nil))
  "A branch: an immutable vector of 1 to +MAX-FANOUT+ children, all of HEIGHT - 1."
  (children #() :type simple-vector :read-only t)
  (height 1 :type fixnum :read-only t))

(declaim (inline node-height))
(defun node-height (node)
  "Return the height of NODE: 0 for a leaf or span, 1 + child height for a branch."
  (if (branch-p node) (branch-height node) 0))

(defun make-leaf (octets &optional (start 0) (end (length octets)))
  "Return a leaf viewing OCTETS from START below END, without copying. Counts
are computed by one scan."
  (declare (type octets octets) (type fixnum start end))
  (multiple-value-bind (chars utf16 lines invalid) (summarise octets start end)
    (%make-leaf octets start (- end start) chars utf16 lines invalid)))

(defun make-branch (children)
  "Return a branch over CHILDREN, a simple-vector of nodes of equal height,
summing their counts; unknown counts in any child make the branch's unknown."
  (declare (type simple-vector children))
  (let ((bytes 0) (chars 0) (utf16 0) (lines 0) (invalid 0))
    (declare (type fixnum bytes chars utf16 lines invalid))
    (loop for c across children
          do (incf bytes (node-bytes c))
             (if (and (>= chars 0) (node-indexed-p c))
                 (progn (incf chars (node-chars c)) (incf utf16 (node-utf16 c)) (incf lines (node-lines c))
                        (incf invalid (node-invalid c)))
                 (setf chars -1 utf16 -1 lines -1 invalid -1)))
    (%make-branch children (1+ (node-height (aref children 0))) bytes chars utf16 lines invalid)))

;;; Piece access: the content of a leaf or span

(defun piece-copy (node start end out out-start)
  "Copy octets START below END of NODE's content (a leaf or span) into OUT at OUT-START."
  (declare (type fixnum start end out-start) (type octets out))
  (etypecase node
    (leaf (replace out (leaf-octets node) :start1 out-start
                                          :start2 (+ (leaf-start node) start)
                                          :end2 (+ (leaf-start node) end)))
    (span (let ((mapping (span-mapping node)))
            (check-mapping mapping)
            (sb-sys:with-pinned-objects (out)
              (sb-kernel:system-area-ub8-copy (mapping-sap mapping) (+ (span-offset node) start)
                                              (sb-sys:vector-sap out) out-start
                                              (- end start))))))
  out)

(defmacro with-piece-sap ((sap base) node &body body)
  "Run BODY with SAP pointing at memory holding NODE's content (a leaf's or a
span's) from offset BASE, nothing copied: a leaf's octets pinned meanwhile, a
span's mapping checked first."
  (let ((n (gensym "NODE")) (octets (gensym "OCTETS")) (mapping (gensym "MAPPING")))
    `(let ((,n ,node))
       (etypecase ,n
         (leaf (let ((,octets (leaf-octets ,n)))
                 (sb-sys:with-pinned-objects (,octets)
                   (let ((,sap (sb-sys:vector-sap ,octets)) (,base (leaf-start ,n)))
                     (declare (type sb-sys:system-area-pointer ,sap) (type fixnum ,base))
                     ,@body))))
         (span (let ((,mapping (span-mapping ,n)))
                 (check-mapping ,mapping)
                 (let ((,sap (mapping-sap ,mapping)) (,base (span-offset ,n)))
                   (declare (type sb-sys:system-area-pointer ,sap) (type fixnum ,base))
                   ,@body)))))))

(defun piece-units-octets (node count utf16)
  "How many octets of NODE's content its first COUNT characters take, or with
UTF16 its first COUNT UTF-16 units (a character of four octets two of them;
one cut in two counted out); all of it when it has fewer. Read in place."
  (declare (type fixnum count) (optimize speed (safety 1)))
  (let ((n (node-bytes node)))
    (declare (type fixnum n))
    (with-piece-sap (sap base) node
      (macrolet ((byte-at (p) `(sb-sys:sap-ref-8 sap ,p)))
        (let ((i base) (end (+ base n)) (remaining count))
          (declare (type fixnum i end remaining))
          (loop while (and (> remaining 0) (< i end))
                do (let ((length (nth-value 1 (utf8-decode-form byte-at i end))))
                     (declare (type fixnum length))
                     (decf remaining (if (and utf16 (= length 4)) 2 1))
                     (when (< remaining 0) (return))
                     (incf i length)))
          (- i base))))))

(defun piece-line-octets (node lines)
  "How many octets of NODE's content come before the start of its LINES-th
line from 1, just after its LINES-th newline; NIL when it has fewer. Read in place."
  (declare (type fixnum lines) (optimize speed (safety 1)))
  (let ((n (node-bytes node)) (remaining lines))
    (declare (type fixnum n remaining))
    (with-piece-sap (sap base) node
      (loop for i of-type fixnum from base below (+ base n)
            do (when (= (sb-sys:sap-ref-8 sap i) 10)
                 (decf remaining)
                 (when (zerop remaining) (return (- (1+ i) base))))))))

(defun piece-summarise (node start end)
  "SUMMARISE octets START below END of NODE's content: characters, UTF-16 units,
newlines and raw bytes."
  (declare (type fixnum start end))
  (etypecase node
    (leaf (summarise (leaf-octets node) (+ (leaf-start node) start) (+ (leaf-start node) end)))
    (span (let ((mapping (span-mapping node)))
            (check-mapping mapping)
            (summarise-sap (mapping-sap mapping) (+ (span-offset node) start) (+ (span-offset node) end))))))

(defun piece-newlines (node start end)
  "The newline octets from START below END of NODE's content, counted where
they lie, eight at a time (NEWLINES-SAP); a node counted already gives its
count when the range is all of it."
  (declare (type fixnum start end))
  (if (and (zerop start) (= end (node-bytes node)) (node-indexed-p node))
      (node-lines node)
      (etypecase node
        (leaf (let ((octets (leaf-octets node)))
                (sb-sys:with-pinned-objects (octets)
                  (newlines-sap (sb-sys:vector-sap octets) (+ (leaf-start node) start) (+ (leaf-start node) end)))))
        (span (let ((mapping (span-mapping node)))
                (check-mapping mapping)
                (newlines-sap (mapping-sap mapping) (+ (span-offset node) start) (+ (span-offset node) end)))))))

(defun piece-octet (node i)
  "Return octet I of NODE's content."
  (declare (type fixnum i))
  (etypecase node
    (leaf (aref (leaf-octets node) (+ (leaf-start node) i)))
    (span (let ((mapping (span-mapping node)))
            (check-mapping mapping)
            (sb-sys:sap-ref-8 (mapping-sap mapping) (+ (span-offset node) i))))))

;;; Building trees

(defun chunk-octets (octets start end)
  "Return a simple-vector of leaves viewing OCTETS from START below END (text
starting at a character start), each about +TARGET-LEAF+ octets and every one
starting where a character starts. Complexity: O(END - START)."
  (declare (type octets octets) (type fixnum start end))
  (let ((leaves (make-array 0 :adjustable t :fill-pointer 0)) (text-start start))
    (if (= start end)
        (vector-push-extend (make-leaf (make-octets 0)) leaves)
        (loop while (< start end)
              do (let ((cut (min end (+ start +target-leaf+))))
                   (declare (type fixnum cut))
                   (when (< cut end)
                     ;; at most three steps back: no character is longer than four octets
                     (loop while (and (> cut start) (not (octets-char-start-p octets cut text-start end)))
                           do (decf cut)))
                   (vector-push-extend (make-leaf octets start cut) leaves)
                   (setf start cut))))
    (coerce leaves 'simple-vector)))

(defun group-into-branches (nodes)
  "Group NODES, a simple-vector of equal-height nodes, into branches of at most
+MAX-FANOUT+ children with sizes as even as possible. Returns a simple-vector."
  (declare (type simple-vector nodes))
  (let* ((n (length nodes))
         (groups (ceiling n +max-fanout+))
         (base (floor n groups))
         (extra (mod n groups))
         (result (make-array groups))
         (pos 0))
    (dotimes (g groups result)
      (let ((size (if (< g extra) (1+ base) base)))
        (setf (aref result g) (make-branch (subseq nodes pos (+ pos size))))
        (incf pos size)))))

(defun build-tree (nodes)
  "Return a single node over NODES (a non-empty simple-vector of equal-height nodes)."
  (declare (type simple-vector nodes))
  (loop while (> (length nodes) 1)
        do (setf nodes (group-into-branches nodes)))
  (aref nodes 0))

;;; Ropes

(defstruct (rope (:constructor %make-rope (root)) (:copier nil))
  "An immutable text. ROOT is a leaf, a span or a branch. Every operation returns
a new rope sharing structure with the old one; the old one stays valid, which is
what makes snapshots free."
  (root (make-leaf (make-octets 0)) :type node :read-only t))

(setf (documentation 'rope-p 'function) "True when OBJECT is a ROPE.")

(declaim (inline rope-bytes rope-indexed-p))
(defun rope-bytes (rope) "Number of octets in ROPE. O(1)." (node-bytes (rope-root rope)))
(defun rope-indexed-p (rope)
  "True when ROPE's character, UTF-16 and line counts are known everywhere."
  (node-indexed-p (rope-root rope)))

(defun octets->rope (octets &key (start 0) (end (length octets)))
  "Return a rope viewing OCTETS from START below END, any octets at all,
which must not be modified afterwards. Complexity: O(END - START), one scan, no copy."
  (declare (type octets octets))
  (%make-rope (build-tree (chunk-octets octets start end))))

(defun string->rope (string)
  "Return a rope holding STRING, encoded as UTF-8 (STRING-OCTETS). Complexity: O(length of STRING)."
  (octets->rope (string-octets string)))

(defun make-rope (&optional (text ""))
  "Return a rope holding TEXT, a string or an octet vector of UTF-8; empty by default."
  (etypecase text
    (string (string->rope text))
    ((array (unsigned-byte 8) (*)) (octets->rope (coerce text 'octets)))))

(defun slice-piece (node start end)
  "A leaf or span over octets START below END of NODE's content, sharing its
storage; the counts of the part are computed for an indexed NODE. Complexity:
O(END - START)."
  (declare (type fixnum start end))
  (etypecase node
    (leaf (make-leaf (leaf-octets node) (+ (leaf-start node) start) (+ (leaf-start node) end)))
    (span (if (node-indexed-p node)
              (multiple-value-bind (chars utf16 lines invalid) (piece-summarise node start end)
                (%make-span (span-mapping node) (+ (span-offset node) start) (- end start) chars utf16 lines invalid))
              (%make-span (span-mapping node) (+ (span-offset node) start) (- end start) -1 -1 -1 -1)))))

(defun rope-pieces (rope start end)
  "The leaves and spans of ROPE holding octets START below END, in order, as a
list; the one or two the range cuts are replaced by slices (SLICE-PIECE).
START and END should be character boundaries. Complexity: O(log n + leaves)."
  (let ((out '()))
    (map-leaf-pieces (lambda (node s e)
                       (push (if (and (zerop s) (= e (node-bytes node))) node (slice-piece node s e)) out))
                     rope :start start :end end)
    (nreverse out)))

(defun rope-slice (rope start end)
  "A rope holding ROPE's text from byte START below END, sharing its leaves and
spans rather than copying them, so that a slice of many gigabytes is made in
the time it takes to list its spans. START and END are character boundaries.
Complexity: O(log n + leaves in the range)."
  (let ((nodes (clean-junctions (remove-if (lambda (n) (zerop (node-bytes n))) (rope-pieces rope start end)))))
    (%make-rope (if nodes (build-tree (coerce nodes 'simple-vector)) (make-leaf (make-octets 0))))))

(defun splice-rope (rope start end &rest middles)
  "A rope holding ROPE's text before START, then the text of each of MIDDLES
(ropes) in turn, then ROPE's text from END. Leaves and spans are shared, not
copied, so ropes over files of many gigabytes splice in the time it takes to
list their spans; only the two leaves the cuts fall in are sliced. START and
END are character boundaries of ROPE with START <= END."
  (let ((nodes (append (rope-pieces rope 0 start)
                       (loop for middle in middles
                             append (let ((acc '())) (map-leaves (lambda (n) (push n acc)) middle) (nreverse acc)))
                       (rope-pieces rope end (rope-bytes rope)))))
    (setf nodes (clean-junctions (remove-if (lambda (n) (zerop (node-bytes n))) nodes)))
    (%make-rope (if nodes (build-tree (coerce nodes 'simple-vector)) (make-leaf (make-octets 0))))))

(defun clean-junction-p (left right)
  "True unless the octets of LEFT, a leaf or span, end with the unfinished
start of a sequence that the first octet of RIGHT continues."
  (or (zerop (node-bytes left)) (zerop (node-bytes right))
      (not (continuation-byte-p (piece-octet right 0)))
      (let* ((k (min 3 (node-bytes left))) (tail (make-octets k)))
        (piece-copy left (- (node-bytes left) k) (node-bytes left) tail 0)
        (zerop (octets-pending tail 0 k)))))

(defun clean-junctions (nodes)
  "NODES, a list of leaves and spans in order, with every pair whose junction
would split a character (possible only around raw bytes) remade as leaves
cut where characters start."
  (let ((out '()))
    (dolist (node nodes (nreverse out))
      (let ((left (first out)))
        (if (and left (not (clean-junction-p left node)))
            (let* ((a (node-bytes left)) (b (node-bytes node)) (octets (make-octets (+ a b))))
              (piece-copy left 0 a octets 0)
              (piece-copy node 0 b octets a)
              (pop out)
              (loop for leaf across (chunk-octets octets 0 (+ a b)) do (push leaf out)))
            (push node out))))))

(defun unindexed-spans (mapping start end)
  "Return a simple-vector of spans of +SPAN-SIZE+ covering MAPPING from START
below END, with unknown counts. Cuts may fall inside UTF-8 sequences; INDEX-ROPE
re-cuts them. Nothing is read from the file. Complexity: O(number of spans)."
  (declare (type fixnum start end))
  (let ((spans (make-array (ceiling (- end start) +span-size+))))
    (loop for i from 0 below (length spans)
          for s = (+ start (* i +span-size+))
          do (setf (aref spans i) (%make-span mapping s (min +span-size+ (- end s)) -1 -1 -1 -1)))
    spans))

(defun mapping-rope (mapping &optional (start 0) (end (mapping-length mapping)))
  "A rope over MAPPING's octets from START below END, all of them unless
given, with unknown character and line counts until INDEX-ROPE. Nothing is
read. Complexity: O(octets / +SPAN-SIZE+)."
  (%make-rope (build-tree (unindexed-spans mapping start end))))

(defun open-file-rope (path)
  "Return a rope over the file at PATH, memory-mapped, with unknown character
and line counts, and the mapping as a second value (NIL for an empty file).
Byte-based operations work immediately; character and line operations need
INDEX-ROPE first. Complexity: O(file size / +SPAN-SIZE+), nothing is read."
  (let ((mapping (map-file path)))
    (if mapping
        (values (%make-rope (build-tree (unindexed-spans mapping 0 (mapping-length mapping)))) mapping)
        (values (make-rope) nil))))

(defun check-indexed (rope)
  "Signal an error unless ROPE is indexed."
  (unless (rope-indexed-p rope)
    (error "The text is not indexed yet; character and line counts are unknown until INDEX-ROPE completes.")))

(defun rope-chars (rope)
  "Number of characters in ROPE. O(1). Signals an error while ROPE is unindexed."
  (check-indexed rope) (node-chars (rope-root rope)))
(defun rope-utf16 (rope)
  "Number of UTF-16 code units ROPE would occupy. O(1). Signals an error while unindexed."
  (check-indexed rope) (node-utf16 (rope-root rope)))
(defun rope-invalid (rope)
  "Number of raw bytes in ROPE: octets that are not part of well-formed UTF-8.
O(1). Signals an error while ROPE is unindexed."
  (check-indexed rope) (node-invalid (rope-root rope)))
(defun rope-lines (rope)
  "Number of newline characters in ROPE; the number of lines is one more. O(1).
Signals an error while ROPE is unindexed."
  (check-indexed rope) (node-lines (rope-root rope)))

(defun map-leaves (function rope)
  "Call FUNCTION with each leaf or span of ROPE in order."
  (declare (type function function))
  (labels ((walk (node)
             (if (branch-p node)
                 (loop for c across (branch-children node) do (walk c))
                 (funcall function node))))
    (walk (rope-root rope))))

(defun rope-statistics (rope)
  "Return a plist describing ROPE's structure: :height, :leaves (in-memory),
:spans (file-backed), :unindexed-spans, :memory-bytes (octets held by in-memory
leaves), :file-bytes (octets served from mapped files). For diagnostics and tests."
  (let ((leaves 0) (spans 0) (unindexed 0) (memory 0) (file 0))
    (map-leaves (lambda (node)
                  (if (leaf-p node)
                      (progn (incf leaves) (incf memory (node-bytes node)))
                      (progn (incf spans) (incf file (node-bytes node))
                             (unless (node-indexed-p node) (incf unindexed)))))
                rope)
    (list :height (node-height (rope-root rope)) :leaves leaves :spans spans
          :unindexed-spans unindexed :memory-bytes memory :file-bytes file)))

;;; Indexing: computing the counts of a mapped file in parallel

(defun char-boundary-at (sap offset end)
  "Return the first offset at or after OFFSET (but not past END) that starts a
character of the mapped text at SAP, which ends at END: at most three octets on."
  (declare (type sb-sys:system-area-pointer sap) (type fixnum offset end))
  (loop while (and (< offset end) (not (sap-char-start-p sap offset 0 end)))
        do (incf offset))
  offset)

(defun index-range (mapping start end)
  "Cut MAPPING's octets from START below END into indexed spans of at most
+SPAN-SIZE+, each starting where a character starts, and return them as a list.
START and END must be character boundaries. Complexity: O(END - START)."
  (declare (type fixnum start end))
  (let ((sap (mapping-sap mapping)) (length (mapping-length mapping)) (spans '()))
    (loop while (< start end)
          do (let ((cut (min end (+ start +span-size+))))
               (declare (type fixnum cut))
               (when (< cut end)
                 (loop while (and (> cut start) (not (sap-char-start-p sap cut 0 length)))
                       do (decf cut)))
               (multiple-value-bind (chars utf16 lines invalid) (summarise-sap sap start cut)
                 (push (%make-span mapping start (- cut start) chars utf16 lines invalid) spans))
               (setf start cut)))
    (nreverse spans)))

(defun donor-spans (donor)
  "The indexed spans of DONOR by mapping: a hash table from each mapping to a
simple-vector of its spans in DONOR, in order of their offsets."
  (let ((table (make-hash-table :test 'eq)))
    (map-leaves (lambda (node)
                  (when (and (span-p node) (node-indexed-p node))
                    (push node (gethash (span-mapping node) table))))
                donor)
    (maphash (lambda (mapping spans)
               (setf (gethash mapping table)
                     (sort (coerce spans 'simple-vector) #'< :key #'span-offset)))
             table)
    table))

(defun run-pieces (start end spans)
  "The pieces a run of a mapping's octets from START below END, both character
boundaries, is made of when SPANS (a donor's indexed spans of the mapping, by
offset) are to be reused: a list of spans taken whole and (START . END) ranges
to read, in order."
  (let ((pieces '()) (pos start)
        (i (let ((lo 0) (hi (length spans)))
             ;; the first span that ends after START
             (setf lo (tuatara.runtime:bisect (mid lo hi) (<= (+ (span-offset (aref spans mid)) (node-bytes (aref spans mid))) start)) hi lo)
             lo)))
    (flet ((read-to (to) (when (< pos to) (push (cons pos to) pieces) (setf pos to))))
      (loop while (and (< i (length spans)) (< pos end))
            do (let* ((span (aref spans i))
                      (s (span-offset span))
                      (e (+ s (node-bytes span))))
                 (cond ((>= s end) (return))
                       ((and (= s pos) (<= e end)) (push span pieces) (setf pos e))
                       ;; a span cut by the run's start or end, or after a gap: read what the run has of it
                       (t (read-to (min e end))))
                 (incf i)))
      (read-to end))
    (nreverse pieces)))

;;; What is counted so far, while a text is being counted

(defstruct (tally (:constructor make-tally ()) (:copier nil))
  "What INDEX-ROPE has counted so far of a text, for the line numbers wanted
before all of it is counted: the text cut into segments, in order, each done
once its newlines are counted. STARTS and ENDS are where each segment begins
and ends in the text, LINES its newlines, -1 until counted; READY turns true
once INDEX-ROPE has filled them in. DONE, CUMULATIVE, COUNTED and REACHED are
kept by TALLY-ADVANCE on the one thread that reads the tally: how many
segments from the first are counted, the newlines before each of them, the
newlines in all of them, and where they end. LAST, on that thread too, is the
line TALLY-LINE-NUMBER found last, as (BYTE LINE), the next counted from it."
  (starts (make-array 0 :element-type 'fixnum) :type (simple-array fixnum (*)))
  (ends (make-array 0 :element-type 'fixnum) :type (simple-array fixnum (*)))
  (lines (make-array 0 :element-type 'fixnum) :type (simple-array fixnum (*)))
  (ready nil)
  (done 0 :type fixnum)
  (cumulative (make-array 0 :element-type 'fixnum) :type (simple-array fixnum (*)))
  (counted 0 :type fixnum)
  (reached 0 :type fixnum)
  (last nil))

(setf (documentation 'make-tally 'function) "Return a TALLY, for INDEX-ROPE to fill in."
      (documentation 'tally-p 'function) "True when OBJECT is a TALLY.")

(defun tally-fill (tally starts ends lines)
  "Give TALLY the segments INDEX-ROPE cut the text into (STARTS, ENDS and
LINES, fixnum vectors), before any of them is counted."
  (setf (tally-starts tally) (coerce starts '(simple-array fixnum (*)))
        (tally-ends tally) (coerce ends '(simple-array fixnum (*)))
        (tally-lines tally) (coerce lines '(simple-array fixnum (*)))
        (tally-cumulative tally) (make-array (length lines) :element-type 'fixnum :initial-element 0))
  (sb-thread:barrier (:write))
  (setf (tally-ready tally) t))

(defun tally-note (tally segment start end lines)
  "Tell TALLY that SEGMENT, from START below END of the text, holds LINES
newlines: its range first, its count after, so that a reader seeing the count
sees the range."
  (setf (aref (tally-starts tally) segment) start
        (aref (tally-ends tally) segment) end)
  (sb-thread:barrier (:write))
  (setf (aref (tally-lines tally) segment) lines))

(defun tally-advance (tally)
  "Take in the segments of TALLY counted since last time, from the first not
yet taken in, while they come in order: the text counted from its start grows."
  (when (tally-ready tally)
    (let ((lines (tally-lines tally)))
      (loop while (< (tally-done tally) (length lines))
            do (let ((n (aref lines (tally-done tally))))
                 (when (minusp n) (return))
                 (sb-thread:barrier (:read))
                 (setf (aref (tally-cumulative tally) (tally-done tally)) (tally-counted tally))
                 (incf (tally-counted tally) n)
                 (setf (tally-reached tally) (aref (tally-ends tally) (tally-done tally)))
                 (incf (tally-done tally)))))
    tally))

(defun tally-segment (tally key value)
  "The last of TALLY's counted segments whose KEY (TALLY-STARTS or
TALLY-CUMULATIVE) is at most VALUE, found by halving; NIL when none is."
  (let ((vector (funcall key tally)) (lo 0) (hi (tally-done tally)))
    (setf lo (tuatara.runtime:bisect (mid lo hi) (<= (aref vector mid) value)) hi lo)
    (and (plusp lo) (1- lo))))

(defun tally-line-number (tally rope byte)
  "The line, from 0, holding offset BYTE of ROPE, the text TALLY counts, when
the count has passed it from the start; NIL otherwise. Counts from the line
found last when that is nearer (the rows of a window, one after another, each
read only from the one before), else from the start of BYTE's segment: reads
at most one segment of the text."
  (tally-advance tally)
  (cond ((zerop byte) 0)
        ((> byte (tally-reached tally)) nil)
        (t (let ((i (tally-segment tally #'tally-starts byte)))
             (when i
               (let* ((start (aref (tally-starts tally) i))
                      (last (tally-last tally))
                      (line (if (and last (<= start (first last)) (< (abs (- byte (first last))) (- byte start)))
                                (if (<= (first last) byte)
                                    (+ (second last) (rope-count-newlines rope (first last) byte))
                                    (- (second last) (rope-count-newlines rope byte (first last))))
                                (+ (aref (tally-cumulative tally) i) (rope-count-newlines rope start byte)))))
                 (setf (tally-last tally) (list byte line))
                 line))))))

(defun tally-line-start (tally rope line)
  "The offset where line LINE, from 0, of ROPE, the text TALLY counts, starts,
when the count has passed it from the start; NIL otherwise. Reads at most one
segment of the text."
  (tally-advance tally)
  (cond ((zerop line) 0)
        ((> line (tally-counted tally)) nil)
        (t (let ((i (tally-segment tally #'tally-cumulative (1- line))))
             (and i (let ((newline (rope-nth-newline rope (aref (tally-starts tally) i) (aref (tally-ends tally) i)
                                                     (- line (aref (tally-cumulative tally) i)))))
                      (and newline (1+ newline))))))))

(defun tally-counted-lines (tally)
  "Two values: the newlines TALLY has counted from the text's start, and the
offset the count has reached."
  (tally-advance tally)
  (values (tally-counted tally) (tally-reached tally)))

(defun rope-count-newlines (rope start end)
  "The newlines of ROPE from byte START below END: those of pieces counted
already taken as they are, the rest counted where they lie (PIECE-NEWLINES),
nothing copied. Complexity: O(log n + the octets of pieces not counted yet)."
  (declare (type fixnum start end))
  (let ((count 0))
    (declare (type fixnum count))
    (map-leaf-pieces (lambda (node piece-start piece-end)
                       (incf count (the fixnum (piece-newlines node piece-start piece-end))))
                     rope :start start :end end)
    count))

(defun rope-nth-newline (rope start end n)
  "The offset of the Nth newline (from 1) of ROPE from byte START below END,
found by reading 64 KB at a time; NIL when there are fewer."
  (declare (type fixnum start end n))
  (let ((block (make-octets (min 65536 (max 0 (- end start))))) (pos start) (left n))
    (declare (type fixnum pos left))
    (loop while (< pos end)
          do (let* ((stop (min end (+ pos 65536))) (length (- stop pos)))
               (rope-copy-into rope pos stop block)
               (let ((here (count 10 block :end length)))
                 (if (< here left)
                     (decf left here)
                     (let ((at -1))
                       (loop repeat left do (setf at (position 10 block :start (1+ at) :end length)))
                       (return-from rope-nth-newline (+ pos at)))))
               (setf pos stop)))
    nil))

(defun index-rope (rope &key (threads (tuatara.runtime:cpu-count)) (chunk (* 2 1024 1024)) donor progress cancel tally)
  "Return ROPE with every unindexed span replaced by indexed spans cut at
character boundaries. Runs of unindexed spans are scanned in parallel on up to
THREADS threads in pieces of about CHUNK octets, taken in the order of the
text. In-memory leaves and already indexed spans are kept as they are. DONOR,
an indexed rope over the same mappings (as it was before edits made ROPE),
gives its spans where a run covers them whole, so that only the pieces at the
ends of runs are read. PROGRESS, a cons, has its cdr set to the octets to read
and its car to those read so far, as the work goes. TALLY, a TALLY, is told
each piece's newlines as it is counted, for line numbers wanted before all of
the text is counted (TALLY-LINE-NUMBER, TALLY-LINE-START). When CANCEL's car
turns true the reading stops and NIL is returned. Returns ROPE itself when
nothing is unindexed.
Complexity: O(unindexed octets / THREADS + number of leaves); with a donor,
O(leaves + the octets of the pieces read)."
  (if (rope-indexed-p rope)
      rope
      (let ((items '())
            (donated (and donor (donor-spans donor))))
        ;; Collect leaves in order, coalescing runs of contiguous unindexed spans
        ;; of the same mapping into (mapping start end) entries.
        (map-leaves (lambda (node)
                      (if (and (span-p node) (not (node-indexed-p node)))
                          (let ((run (first items)))
                            (if (and (consp run) (eq (first run) (span-mapping node))
                                     (= (third run) (span-offset node)))
                                (setf (third run) (+ (span-offset node) (node-bytes node)))
                                (push (list (span-mapping node) (span-offset node)
                                            (+ (span-offset node) (node-bytes node)))
                                      items)))
                          (push node items)))
                    rope)
        (setf items (nreverse items))
        ;; Each run becomes spans taken from the donor and ranges to read; the
        ;; ranges are cut into tasks of about CHUNK octets, interior cut points
        ;; moved to character boundaries so that every task and both of its
        ;; neighbours agree on where the boundary is.
        (let ((tasks (make-array 0 :adjustable t :fill-pointer 0))
              (placement (make-array 0 :adjustable t :fill-pointer 0))
              (sizes (make-array 0 :adjustable t :fill-pointer 0))
              (to-read 0)
              ;; the text cut into segments, in order, for TALLY: a node counted
              ;; already, or a piece to read
              (segment-starts (make-array 0 :element-type 'fixnum :adjustable t :fill-pointer 0))
              (segment-ends (make-array 0 :element-type 'fixnum :adjustable t :fill-pointer 0))
              (segment-lines (make-array 0 :element-type 'fixnum :adjustable t :fill-pointer 0))
              (at 0))
          (declare (type fixnum at))
          (setf items
                (loop for item in items
                      append (if (and (consp item) donated (gethash (first item) donated))
                                 (destructuring-bind (mapping start end) item
                                   (loop for piece in (run-pieces start end (gethash mapping donated))
                                         collect (if (consp piece) (list mapping (car piece) (cdr piece)) piece)))
                                 (list item))))
          (loop for item in items
                for index from 0
                do (if (consp item)
                       (destructuring-bind (mapping start end) item
                         (check-mapping mapping)
                         (let ((sap (mapping-sap mapping)) (length (mapping-length mapping)) (pos start)
                               (base (- at start)))
                           (loop while (< pos end)
                                 do (let* ((a pos) (b (min end (+ pos chunk)))
                                           (segment (fill-pointer segment-starts)))
                                      (vector-push-extend (+ a base) segment-starts)
                                      (vector-push-extend (+ b base) segment-ends)
                                      (vector-push-extend -1 segment-lines)
                                      ;; each task moves its own cuts to where characters start, reading
                                      ;; the octets there itself, so that nothing is read before the
                                      ;; tasks run in parallel; both neighbours of a cut agree on it
                                      (vector-push-extend (lambda ()
                                                            (if (and cancel (car cancel))
                                                                :cancelled
                                                                (let* ((from (if (= a start) a (min end (char-boundary-at sap a length))))
                                                                       (to (if (= b end) b (min end (char-boundary-at sap b length))))
                                                                       (spans (index-range mapping from to)))
                                                                  (when tally
                                                                    (tally-note tally segment (+ from base) (+ to base)
                                                                                (loop for span in spans sum (node-lines span))))
                                                                  (when progress (sb-ext:atomic-incf (car progress) (- b a)))
                                                                  spans)))
                                                          tasks)
                                      (vector-push-extend index placement)
                                      (vector-push-extend (- b a) sizes)
                                      (incf to-read (- b a))
                                      (setf pos b)))
                           (incf at (- end start))))
                       (progn
                         (vector-push-extend at segment-starts)
                         (vector-push-extend (+ at (node-bytes item)) segment-ends)
                         (vector-push-extend (node-lines item) segment-lines)
                         (incf at (node-bytes item)))))
          (when tally
            (tally-fill tally segment-starts segment-ends segment-lines))
          (when progress
            (setf (car progress) 0 (cdr progress) to-read))
          (let* ((results (tuatara.runtime:run-tasks (coerce tasks 'simple-vector) :parallelism threads))
                 (leaves (make-array 0 :adjustable t :fill-pointer 0))
                 (r 0))
            (when (or (and cancel (car cancel)) (find :cancelled results))
              (return-from index-rope nil))
            (loop for item in items
                  for index from 0
                  do (if (consp item)
                         (loop while (and (< r (length placement)) (= (aref placement r) index))
                               do (dolist (span (aref results r)) (vector-push-extend span leaves))
                                  (incf r))
                         (vector-push-extend item leaves)))
            (%make-rope (if (zerop (length leaves))
                            (make-leaf (make-octets 0))
                            (build-tree (coerce leaves 'simple-vector)))))))))

(defun rope-count-chars (rope start end)
  "The characters of ROPE from byte START below END, both character
boundaries, counted by reading them (a raw byte is one, as the index counts),
for a rope not indexed yet: 64 KB at a time. Complexity: O(END - START)."
  (declare (type fixnum start end))
  (let ((count 0) (block (make-octets (+ (min 65536 (max 0 (- end start))) 4))) (pos start) (total (rope-bytes rope)))
    (declare (type fixnum count pos total))
    (loop while (< pos end)
          do (let* ((stop (min end (+ pos 65536)))
                    (have (min total (+ stop 4))))
               (rope-copy-into rope pos have block)
               (let ((cut (- stop pos)) (avail (- have pos)))
                 (declare (type fixnum cut avail))
                 (when (< stop end)
                   ;; end the block where a character starts, so that none is cut in two
                   (loop while (and (> cut 0) (not (octets-char-start-p block cut 0 avail)))
                         do (decf cut))
                   (when (zerop cut) (setf cut (- stop pos))))
                 (incf count (the fixnum (nth-value 0 (summarise block 0 cut))))
                 (incf pos cut))))
    count))

;;; Locating leaves

(defmacro define-locator (name measure doc)
  "Define NAME (ROPE VALUE) returning six values: the leaf whose MEASURE range
contains VALUE (the last leaf when VALUE equals the total), the leaf's starting
byte, character, UTF-16 and line counts, and the leaf's starting MEASURE. MEASURE
is one of the symbols BYTES, CHARS, UTF16 or LINES; DOC is the docstring."
  (let ((accessor (symbolicate 'node- measure)))
    `(defun ,name (rope value)
       ,doc
       (declare (type fixnum value))
       (let ((node (rope-root rope)) (bytes 0) (chars 0) (utf16 0) (lines 0))
         (declare (type fixnum bytes chars utf16 lines))
         (loop while (branch-p node)
               do (let* ((children (branch-children node)) (last (1- (length children))))
                    (loop for i fixnum from 0 to last
                          for child = (aref children i)
                          do (when (or (= i last)
                                       (< value (+ ,measure (,accessor child))))
                               (setf node child)
                               (return))
                             (incf bytes (node-bytes child)) (incf chars (node-chars child))
                             (incf utf16 (node-utf16 child)) (incf lines (node-lines child)))))
         (values node bytes chars utf16 lines ,measure)))))

(define-locator locate-byte bytes "Locate the leaf containing byte offset VALUE of ROPE.")
(define-locator locate-char chars "Locate the leaf containing character offset VALUE of ROPE (ROPE must be indexed).")
(define-locator locate-utf16 utf16 "Locate the leaf containing UTF-16 offset VALUE of ROPE (ROPE must be indexed).")

(defun locate-newline (rope n)
  "Locate the leaf containing the Nth newline of ROPE (1-based). Returns the leaf
and its starting byte and line counts. N must be between 1 and (rope-lines ROPE)."
  (declare (type fixnum n))
  (let ((node (rope-root rope)) (bytes 0) (lines 0))
    (declare (type fixnum bytes lines))
    (loop while (branch-p node)
          do (let* ((children (branch-children node)) (last (1- (length children))))
               (loop for i fixnum from 0 to last
                     for child = (aref children i)
                     do (when (or (= i last) (<= n (+ lines (node-lines child))))
                          (setf node child)
                          (return))
                        (incf bytes (node-bytes child)) (incf lines (node-lines child)))))
    (values node bytes lines)))

;;; Index conversions

(defun check-byte-offset (rope byte)
  "Signal an error unless BYTE is a byte offset within ROPE (0 to its length inclusive)."
  (unless (<= 0 byte (rope-bytes rope))
    (error "Byte offset ~a is outside the text (0..~a)." byte (rope-bytes rope))))

(defun byte->char (rope byte)
  "Return the character offset of byte offset BYTE in ROPE. BYTE must be a
character boundary. Complexity: O(log n + leaf size)."
  (declare (type fixnum byte))
  (check-indexed rope) (check-byte-offset rope byte)
  (multiple-value-bind (leaf lbytes lchars) (locate-byte rope byte)
    (multiple-value-bind (chars) (piece-summarise leaf 0 (- byte lbytes))
      (+ lchars chars))))

(defun char->byte (rope char)
  "Return the byte offset of character offset CHAR in ROPE (0 to rope-chars inclusive).
Complexity: O(log n + leaf size)."
  (declare (type fixnum char))
  (check-indexed rope)
  (unless (<= 0 char (rope-chars rope))
    (error "Character offset ~a is outside the text (0..~a)." char (rope-chars rope)))
  (multiple-value-bind (leaf lbytes lchars) (locate-char rope char)
    (+ lbytes (piece-units-octets leaf (- char lchars) nil))))

(defun byte->utf16 (rope byte)
  "Return the UTF-16 offset of byte offset BYTE in ROPE. BYTE must be a character boundary."
  (declare (type fixnum byte))
  (check-indexed rope) (check-byte-offset rope byte)
  (multiple-value-bind (leaf lbytes lchars lutf16) (locate-byte rope byte)
    (declare (ignore lchars))
    (multiple-value-bind (chars utf16) (piece-summarise leaf 0 (- byte lbytes))
      (declare (ignore chars))
      (+ lutf16 utf16))))

(defun utf16->byte (rope unit)
  "Return the byte offset of UTF-16 offset UNIT in ROPE. An offset inside a
surrogate pair maps to the start of that character."
  (declare (type fixnum unit))
  (check-indexed rope)
  (unless (<= 0 unit (rope-utf16 rope))
    (error "UTF-16 offset ~a is outside the text (0..~a)." unit (rope-utf16 rope)))
  (multiple-value-bind (leaf lbytes lchars lutf16) (locate-utf16 rope unit)
    (declare (ignore lchars))
    (+ lbytes (piece-units-octets leaf (- unit lutf16) t))))

(defun byte->line (rope byte)
  "Return the 0-based line number containing byte offset BYTE in ROPE."
  (declare (type fixnum byte))
  (check-indexed rope) (check-byte-offset rope byte)
  (multiple-value-bind (leaf lbytes lchars lutf16 llines) (locate-byte rope byte)
    (declare (ignore lchars lutf16))
    (+ llines (piece-newlines leaf 0 (- byte lbytes)))))

(defun line->byte (rope line)
  "Return the byte offset where 0-based LINE starts in ROPE. LINE may be at most
the number of newlines, in which case the last line's start is returned."
  (declare (type fixnum line))
  (check-indexed rope)
  (unless (<= 0 line (rope-lines rope))
    (error "Line ~a is outside the text (0..~a)." line (rope-lines rope)))
  (if (zerop line)
      0
      (multiple-value-bind (leaf lbytes llines) (locate-newline rope line)
        (+ lbytes (or (piece-line-octets leaf (- line llines))
                      (error "Newline ~a not found in its leaf; the rope is corrupt." line))))))

(defmacro with-rope-window ((window base end) (rope from to size) &body body)
  "Run BODY with WINDOW holding ROPE's octets from FROM below TO, both clipped
to the text, from its index 0, BASE bound to the offset they start at and END
to how many there are: a few octets around a place, to decode there. SIZE, a
constant, is the most TO - FROM can be. WINDOW is on the stack, so that
nothing is made; BODY must not keep it."
  (let ((r (gensym "ROPE")) (to-var (gensym "TO")))
    `(let* ((,r ,rope)
            (,base (max 0 ,from))
            (,to-var (max ,base (min (rope-bytes ,r) ,to)))
            (,end (- ,to-var ,base))
            (,window (make-array ,size :element-type '(unsigned-byte 8))))
       (declare (type fixnum ,base ,to-var ,end) (dynamic-extent ,window) (ignorable ,base))
       (rope-copy-into ,r ,base ,to-var ,window)
       ,@body)))

(defun char-boundary-p (rope byte)
  "True when byte offset BYTE of ROPE starts a character or is the end of the text.
Works on unindexed ropes too."
  (declare (type fixnum byte))
  (check-byte-offset rope byte)
  (or (= byte (rope-bytes rope))
      (with-rope-window (window base end) (rope (- byte 3) (+ byte 4) 7)
        (octets-char-start-p window (- byte base) 0 end))))

;;; Reading

(defun map-leaf-pieces (function rope &key (start 0) (end (rope-bytes rope)))
  "Call FUNCTION with (NODE PIECE-START PIECE-END) for each leaf or span of ROPE
that overlaps byte range START below END, clipped to the range, in order. Read
the content with PIECE-COPY, PIECE-OCTETS or PIECE-OCTET.
Complexity: O(log n + number of leaves in the range)."
  (declare (type fixnum start end) (type function function))
  (check-byte-offset rope start) (check-byte-offset rope end)
  (when (< start end)
    (labels ((walk (node node-start)
               (declare (type fixnum node-start))
               (if (branch-p node)
                   (let ((pos node-start))
                     (declare (type fixnum pos))
                     (loop for child across (branch-children node)
                           do (let ((child-end (+ pos (node-bytes child))))
                                (when (and (< pos end) (> child-end start))
                                  (walk child pos))
                                (setf pos child-end))))
                   (let ((s (max 0 (- start node-start)))
                         (e (min (node-bytes node) (- end node-start))))
                     (when (< s e) (funcall function node s e))))))
      (walk (rope-root rope) 0))))

(defun map-rope-sources (function rope)
  "Call FUNCTION for each stretch of ROPE's octets in order: (:MEMORY OCTETS START
END) for octets held in memory, (:FILE MAPPING OFFSET LENGTH) for octets of a
mapped file, neighbouring spans of one mapping joined into one stretch. Saving
uses it to let the kernel copy what comes from files."
  (declare (type function function))
  (let ((pending nil))
    (flet ((flush () (when pending (apply function :file pending) (setf pending nil))))
      (map-leaf-pieces (lambda (node s e)
                         (declare (type fixnum s e))
                         (etypecase node
                           (leaf (flush)
                                 (funcall function :memory (leaf-octets node) (+ (leaf-start node) s) (+ (leaf-start node) e)))
                           (span (let ((mapping (span-mapping node)) (offset (+ (span-offset node) s)))
                                   (if (and pending (eq (first pending) mapping)
                                            (= (+ (second pending) (third pending)) offset))
                                       (incf (third pending) (- e s))
                                       (progn (flush) (setf pending (list mapping offset (- e s)))))))))
                       rope)
      (flush))))

(defun rope-copy-into (rope start end out &optional (out-start 0))
  "Copy ROPE's octets from byte START below END into OUT at OUT-START, which must
have room; return the number of octets copied. Allocates nothing."
  (declare (type fixnum start end out-start) (type octets out))
  (let ((pos out-start))
    (declare (type fixnum pos))
    ;; on the stack, as is POS
    (flet ((copy (node s e)
             (declare (type fixnum s e))
             (piece-copy node s e out pos)
             (incf pos (- e s))))
      (declare (dynamic-extent #'copy))
      (map-leaf-pieces #'copy rope :start start :end end))
    (- pos out-start)))

(defun rope-copy-to-sap (rope start end sap)
  "Copy ROPE's octets from byte START below END to foreign memory at SAP, which
must have room; return the number of octets copied. Allocates nothing."
  (declare (type fixnum start end) (type sb-sys:system-area-pointer sap))
  (let ((pos 0))
    (declare (type fixnum pos))
    ;; on the stack, as is POS
    (flet ((copy (node s e)
             (declare (type fixnum s e))
             (with-piece-sap (from base) node
               (sb-kernel:system-area-ub8-copy from (+ base s) sap pos (- e s)))
             (incf pos (- e s))))
      (declare (dynamic-extent #'copy))
      (map-leaf-pieces #'copy rope :start start :end end))
    pos))

(defun rope-octets (rope &key (start 0) (end (rope-bytes rope)))
  "Return a fresh octet vector holding ROPE's text from byte START below END."
  (declare (type fixnum start end))
  (let ((out (make-octets (max 0 (- end start)))))
    (rope-copy-into rope start end out)
    out))

(defun rope-string (rope &key (start 0) (end (rope-bytes rope)))
  "Return ROPE's text from byte START below END as a string, raw bytes as their
characters (U+DC80..U+DCFF). Both offsets must be character boundaries."
  (unless (and (char-boundary-p rope start) (char-boundary-p rope end))
    (error "Byte range ~a..~a does not fall on character boundaries." start end))
  (octets-string (rope-octets rope :start start :end end)))

(defun rope-octet (rope byte)
  "Return the octet at byte offset BYTE of ROPE. Complexity: O(log n)."
  (declare (type fixnum byte))
  (unless (< -1 byte (rope-bytes rope))
    (error "Byte offset ~a is outside the text (0..~a)." byte (1- (rope-bytes rope))))
  (multiple-value-bind (leaf lbytes) (locate-byte rope byte)
    (piece-octet leaf (- byte lbytes))))

;;; Characters and scanning

;;; Grapheme clusters: what a reader takes for one character, a letter with
;;; its accents, an emoji with its skin tone or a family of emoji joined by
;;; U+200D, a Hangul syllable written as jamo, a flag of two regional
;;; indicators. Unicode's rules (UAX #29), but for the conjuncts of Indic
;;; scripts, and with a pictographic symbol standing for an extended
;;; pictographic one.

(defconstant +cluster-reach+ 256
  "The most octets a grapheme cluster is taken to have: a longer run of marks
is cut into clusters of that many, so that stepping over one costs no more.")

(declaim (inline regional-indicator-code-p))
(defun regional-indicator-code-p (code)
  "True when CODE is a regional indicator's, one of the letters two of which make a flag."
  (<= #x1F1E6 code #x1F1FF))

(defun cluster-joins-p (before after regional)
  "True when the characters of codes BEFORE and AFTER, the one following the
other, belong to one grapheme cluster: AFTER a mark, a joiner or a skin tone,
BEFORE a prefixed sign, jamo making one Hangul syllable, an emoji after a
joiner (U+200D), or the second regional indicator of a flag, REGIONAL being
how many run up to BEFORE, BEFORE among them. A control character, a line's
end and a raw byte join nothing. Complexity: O(1)."
  (declare (type fixnum before after regional))
  (if (and (< before #x300) (< after #x300))
      nil
      (let ((b (sb-unicode:grapheme-break-class (code-char before)))
            (a (sb-unicode:grapheme-break-class (code-char after))))
        (cond ((or (member b '(:control :cr :lf)) (member a '(:control :cr :lf))) nil)
              ((member a '(:extend :zwj :spacing-mark)) t)
              ((eq b :prepend) t)
              ((eq b :l) (and (member a '(:l :v :lv :lvt)) t))
              ((member b '(:lv :v)) (and (member a '(:v :t)) t))
              ((member b '(:lvt :t)) (eq a :t))
              ((eq b :zwj) (eq (sb-unicode:general-category (code-char after)) :so))
              ((eq b :regional-indicator) (and (eq a :regional-indicator) (oddp regional)))
              (t nil)))))

(defun octets-cluster-end (octets i end)
  "The index after the grapheme cluster starting at index I of OCTETS, whose
text ends at END (CLUSTER-JOINS-P): no more than +CLUSTER-REACH+ octets
on, but for the last character's own."
  (declare (type octets octets) (type fixnum i end))
  (multiple-value-bind (before length) (octets-decode octets i end)
    (let ((p (+ i length))
          (limit (min end (+ i +cluster-reach+)))
          (regional (if (regional-indicator-code-p before) 1 0)))
      (declare (type fixnum p limit regional before))
      (loop while (< p limit)
            do (multiple-value-bind (after length) (octets-decode octets p end)
                 (unless (cluster-joins-p before after regional) (return))
                 (setf regional (if (regional-indicator-code-p after) (1+ regional) 0)
                       before after)
                 (incf p length)))
      p)))

(defun string-cluster-end (string start &optional (end (length string)))
  "The index after the grapheme cluster of STRING starting at index START, no
further than END (CLUSTER-JOINS-P): past a letter's accents, an emoji's skin
tone, a flag's second half; no more than +CLUSTER-REACH+ characters on."
  (declare (type string string) (type fixnum start end))
  (if (>= start end)
      end
      (let* ((before (char-code (char string start)))
             (regional (if (regional-indicator-code-p before) 1 0))
             (limit (min end (+ start +cluster-reach+)))
             (p (1+ start)))
        (declare (type fixnum before regional limit p))
        (loop while (< p limit)
              do (let ((after (char-code (char string p))))
                   (unless (cluster-joins-p before after regional) (return))
                   (setf regional (if (regional-indicator-code-p after) (1+ regional) 0)
                         before after)
                   (incf p)))
        p)))

(defun string-cluster-start (string end)
  "The index where the grapheme cluster of STRING before index END begins, or
the one holding the character before END (STRING-CLUSTER-END); 0 at most."
  (declare (type string string) (type fixnum end))
  (if (<= end 0)
      0
      (let ((q (1- end)))
        ;; back while characters join, a flag's halves however many run; then on by
        ;; whole clusters from there, which pairs regional indicators as they come
        (loop while (and (plusp q) (> q (- end +cluster-reach+))
                         (cluster-joins-p (char-code (char string (1- q))) (char-code (char string q)) 1))
              do (decf q))
        (loop for p = q then next
              for next = (string-cluster-end string p (length string))
              when (>= next end) return p))))

(defun rope-next-boundary (rope byte &optional clusters)
  "Return the byte offset of the character after the one starting at BYTE, or
the text length when BYTE is at or past the last character. With CLUSTERS,
of the grapheme cluster after the one starting at BYTE: past a letter's
accents, an emoji's skin tone, a flag's second half (CLUSTER-JOINS-P).
Complexity: O(log n)."
  (declare (type fixnum byte))
  (let ((total (rope-bytes rope)))
    (cond ((>= byte total) total)
          (clusters
           (with-rope-window (window base end) (rope byte (+ byte +cluster-reach+ 4) (+ +cluster-reach+ 4))
             (+ byte (octets-cluster-end window 0 end))))
          (t
           (with-rope-window (window base end) (rope byte (+ byte 4) 4)
             (+ byte (nth-value 1 (octets-decode window 0 end))))))))

(defun rope-previous-boundary (rope byte &optional clusters)
  "Return the byte offset of the character before byte offset BYTE, or 0 when
BYTE is 0. With CLUSTERS, the start of the grapheme cluster before BYTE, or
holding the character before it (CLUSTER-JOINS-P). Complexity: O(log n)."
  (declare (type fixnum byte))
  (cond ((<= byte 0) 0)
        (clusters
         (with-rope-window (window base end) (rope (- byte +cluster-reach+ 3) (+ byte 4) (+ +cluster-reach+ 7))
           (let* ((at (- byte base))
                  ;; the first octets of a window cut from the text may be taken for characters' starts
                  (low (if (zerop base) 0 3)))
             (flet ((start-before (i)
                      (loop for p from (1- i) downto low
                            when (octets-char-start-p window p 0 end) return p
                            finally (return low))))
               ;; back while characters join, a flag's halves however many run; then on by
               ;; whole clusters from there, which pairs regional indicators as they come
               (let* ((q (start-before at))
                      (after (octets-decode window q end)))
                 (loop while (> q low)
                       do (let* ((r (start-before q))
                                 (code (octets-decode window r end)))
                            (unless (cluster-joins-p code after 1) (return))
                            (setf q r after code)))
                 (loop for p = q then next
                       for next = (octets-cluster-end window p end)
                       when (>= next at) return (+ base p)))))))
        (t
         (with-rope-window (window base end) (rope (- byte 7) (+ byte 4) 11)
           (loop for p from (1- byte) downto (max 0 (- byte 4))
                 when (octets-char-start-p window (- p base) 0 end) return p
                 finally (return (max 0 (- byte 4))))))))


(defun rope-char-start (rope byte)
  "The start of the character of ROPE holding byte offset BYTE: BYTE itself
when a character starts there."
  (if (char-boundary-p rope byte) byte (rope-previous-boundary rope byte)))
(defun rope-char-at (rope byte)
  "Return the character starting at byte offset BYTE of ROPE, or NIL at the end
of the text: a raw byte's character (U+DC80..U+DCFF) for an octet that starts no
well-formed sequence. Decodes at most four octets. Complexity: O(log n)."
  (declare (type fixnum byte))
  (when (< byte (rope-bytes rope))
    (with-rope-window (window base end) (rope byte (+ byte 4) 4)
      (code-char (octets-decode window 0 end)))))

(defun rope-crlf-p (rope byte)
  "True when ROPE holds a carriage return at BYTE and a newline after it (CRLF-OCTETS-P)."
  (declare (type fixnum byte))
  (and (>= byte 0) (< (1+ byte) (rope-bytes rope))
       (let ((pair (make-array 2 :element-type '(unsigned-byte 8))))
         (declare (dynamic-extent pair))
         (rope-copy-into rope byte (+ byte 2) pair)
         (crlf-octets-p pair 0 2))))

(defun rope-find-octet (rope value &key (start 0) (end (rope-bytes rope)))
  "Return the byte offset of the first octet equal to VALUE in ROPE from START
below END, or NIL. Complexity: O(log n + distance scanned)."
  (declare (type (unsigned-byte 8) value) (type fixnum start end))
  (let ((pos start))
    (declare (type fixnum pos))
    (block scan
      ;; on the stack, as is POS: a search for the next newline makes nothing
      (flet ((look (node s e)
               (declare (type fixnum s e))
               (let ((hit (etypecase node
                            (leaf (let ((octets (leaf-octets node)) (base (leaf-start node)))
                                    (declare (type octets octets) (type fixnum base))
                                    (loop for i of-type fixnum from (+ base s) below (+ base e)
                                          when (= (aref octets i) value) return (- i base))))
                            (span (let ((mapping (span-mapping node)))
                                    (check-mapping mapping)
                                    (let ((sap (mapping-sap mapping)) (base (span-offset node)))
                                      (declare (type sb-sys:system-area-pointer sap) (type fixnum base))
                                      (loop for i of-type fixnum from (+ base s) below (+ base e)
                                            when (= (sb-sys:sap-ref-8 sap i) value) return (- i base))))))))
                 (when hit
                   (return-from scan (+ pos (- hit s))))
                 (incf pos (- e s)))))
        (declare (dynamic-extent #'look))
        (map-leaf-pieces #'look rope :start start :end end))
      nil)))

(defmacro do-rope-windows ((block low high) (rope start end &key from-end) &body body)
  "Run BODY on each window of ROPE's octets from START below END, in order, or
with FROM-END (read when the macro is expanded) from the end back: LOW and
HIGH bound the window, BLOCK holds its octets from index 0. Windows start at
256 octets and double up to 64 KB, so that a search that ends soon reads
little and a long one reads in large blocks; BLOCK is on the stack, so that
none of it is garbage. BODY must not keep BLOCK, and leaves with RETURN-FROM."
  (let ((r (gensym "ROPE")) (s (gensym "START")) (e (gensym "END")) (size (gensym "SIZE")))
    `(let ((,r ,rope) (,s ,start) (,e ,end) (,size 256))
       (declare (type fixnum ,s ,e ,size))
       (let ((,block (make-array 65536 :element-type '(unsigned-byte 8))))
         (declare (type octets ,block) (dynamic-extent ,block))
         ,(if from-end
              `(let ((,high ,e))
                 (declare (type fixnum ,high))
                 (loop while (> ,high ,s)
                       do (let ((,low (max ,s (- ,high ,size))))
                            (declare (type fixnum ,low))
                            (rope-copy-into ,r ,low ,high ,block)
                            ,@body
                            (setf ,high ,low)
                            (when (< ,size 65536)
                              (setf ,size (* 2 ,size))))))
              `(let ((,low ,s))
                 (declare (type fixnum ,low))
                 (loop while (< ,low ,e)
                       do (let ((,high (min ,e (+ ,low ,size))))
                            (declare (type fixnum ,high))
                            (rope-copy-into ,r ,low ,high ,block)
                            ,@body
                            (setf ,low ,high)
                            (when (< ,size 65536)
                              (setf ,size (* 2 ,size)))))))))))

(defun rope-escaped-p (rope position)
  "True when the octet at POSITION of ROPE follows an odd number of
backslashes: escaped, since a backslash escapes the character after it.
Complexity: O(log n) for each backslash before it."
  (let ((count 0) (q position))
    (declare (type fixnum count q))
    (loop while (and (plusp q) (= 92 (rope-octet rope (1- q)))) do (incf count) (decf q))
    (oddp count)))

(defparameter *openers* "([{"
  "The brackets that open a pair, each in the place of the one that closes it in *CLOSERS*.")

(defparameter *closers* ")]}"
  "The brackets that close a pair, each in the place of the one that opens it in *OPENERS*.")

(defparameter *opening-octets* (map 'list #'char-code *openers*)
  "The octets of the brackets that open a pair (*OPENERS*).")

(defparameter *closing-octets* (map 'list #'char-code *closers*)
  "The octets of the brackets that close a pair (*CLOSERS*), each in the place of the one that opens it.")

(defparameter *bracket-octets* (append *opening-octets* *closing-octets*)
  "The octets of the brackets (*OPENERS* and *CLOSERS*).")

(defun bracket-weights (openers closers)
  "A table of 256 by octet: 1 for each bracket in OPENERS, -1 for each in CLOSERS, 0 for the rest."
  (let ((weight (make-array 256 :element-type '(signed-byte 8) :initial-element 0)))
    (loop for c across openers do (setf (aref weight (char-code c)) 1))
    (loop for c across closers do (setf (aref weight (char-code c)) -1))
    weight))

(sb-ext:define-load-time-global **bracket-weights**
    (list *openers* *closers* (bracket-weights *openers* *closers*))
  "*OPENERS*, *CLOSERS* and their BRACKET-WEIGHTS, made once.")

(defun rope-bracket-match (rope position &key backward inside (openers *openers*) (closers *closers*)
                                              (limit most-positive-fixnum) (start 0) (end (rope-bytes rope))
                                              (spans #()) escapes)
  "The partner of the opening bracket at byte POSITION of ROPE, or with BACKWARD
of the closing one just before it, found by the octets alone (brackets are
ASCII, and no other character's octets can be taken for one): each opening
bracket in OPENERS goes a level deeper and each closing one in CLOSERS a level
back, whatever their kind. Forward, the offset just after the bracket that
comes back to the first level; backward, the offset of the one that does. A
second value is true when the two are of one kind, paired by their places in
OPENERS and CLOSERS (( with ), [ with ], { with }).
With INSIDE, POSITION is taken to be within a pair already, no bracket there:
the end or the start of that pair is found, and the second value is true.
SPANS, a simple-vector of ranges (FROM . TO) in order, none within another,
are text whose brackets count for nothing (strings, comments); with ESCAPES,
nor do brackets after an odd number of backslashes (ROPE-ESCAPED-P). Only the
octets from START below END are looked at, and at most LIMIT of them. NIL when
there is no such bracket at POSITION, or no partner there.
Complexity: O(log n + distance scanned + spans before POSITION)."
  (declare (type fixnum position start end) (type simple-vector spans))
  (check-byte-offset rope position)
  (let ((weight (destructuring-bind (kept-openers kept-closers table) **bracket-weights**
                  (if (and (string= openers kept-openers) (string= closers kept-closers))
                      table
                      (bracket-weights openers closers))))
        (depth (if inside 1 0))
        (total (rope-bytes rope))
        (count (length spans))
        (own nil))
    (declare (type fixnum depth total count) (type (simple-array (signed-byte 8) (256)) weight))
    (labels ((kind (octet)
               (let ((c (code-char octet))) (or (position c openers) (position c closers))))
             (same-kind-p (octet) (or inside (eql (kind octet) own))))
      (if backward
          (let ((k (1- count)))
            (declare (type fixnum k))
            (flet ((counts-p (q)
                     ;; the last span starting at or before Q holds it, or none does
                     (loop while (and (>= k 0) (> (the fixnum (car (svref spans k))) q)) do (decf k))
                     (not (or (and (>= k 0) (< q (the fixnum (cdr (svref spans k)))))
                              (and escapes (rope-escaped-p rope q))))))
              (when (or inside (and (plusp position) (= -1 (aref weight (rope-octet rope (1- position))))))
                (unless inside (setf own (kind (rope-octet rope (1- position)))))
                (do-rope-windows (block low high) (rope (max start 0 (- position limit)) position :from-end t)
                  (loop for i of-type fixnum from (- high low 1) downto 0
                        do (let ((w (aref weight (aref block i))) (q (+ low i)))
                             (declare (type fixnum q))
                             (when (and (/= w 0) (or (and (not inside) (= q (1- position))) (counts-p q)))
                               (decf depth w)
                               (when (zerop depth)
                                 (return-from rope-bracket-match (values q (same-kind-p (aref block i)))))))))
                nil)))
          (let ((k 0))
            (declare (type fixnum k))
            (flet ((counts-p (q)
                     ;; the first span ending after Q holds it, or none does
                     (loop while (and (< k count) (<= (the fixnum (cdr (svref spans k))) q)) do (incf k))
                     (not (or (and (< k count) (<= (the fixnum (car (svref spans k))) q))
                              (and escapes (rope-escaped-p rope q))))))
              (when (or inside (and (< position total) (= 1 (aref weight (rope-octet rope position)))))
                (unless inside (setf own (kind (rope-octet rope position))))
                (do-rope-windows (block low high) (rope position (min end total (+ position limit)))
                  (loop for i of-type fixnum from 0 below (- high low)
                        do (let ((w (aref weight (aref block i))) (q (+ low i)))
                             (declare (type fixnum q))
                             (when (and (/= w 0) (or (and (not inside) (= q position)) (counts-p q)))
                               (incf depth w)
                               (when (zerop depth)
                                 (return-from rope-bracket-match (values (1+ q) (same-kind-p (aref block i)))))))))
                nil)))))))

(defun rope-find-octet-backward (rope value &key (end (rope-bytes rope)) (start 0))
  "Return the byte offset of the last octet equal to VALUE in ROPE from START
below END, or NIL. Complexity: O(log n + distance scanned)."
  (declare (type (unsigned-byte 8) value) (type fixnum start end))
  (check-byte-offset rope start) (check-byte-offset rope end)
  (do-rope-windows (block low high) (rope start end :from-end t)
    (let ((hit (position value block :end (- high low) :from-end t)))
      (when hit (return-from rope-find-octet-backward (+ low hit)))))
  nil)

(defun rope-find-any-octet (rope octets &key (start 0) (end (rope-bytes rope)) from-end)
  "The byte offset of the first octet of ROPE from START below END that is one
of OCTETS (a list), or with FROM-END the last; NIL when there is none.
Complexity: O(log n + distance scanned)."
  (declare (type fixnum start end))
  (check-byte-offset rope start) (check-byte-offset rope end)
  (let ((wanted (make-array 256 :element-type 'bit :initial-element 0)))
    (dolist (octet octets) (setf (sbit wanted octet) 1))
    (if from-end
        (do-rope-windows (block low high) (rope start end :from-end t)
          (loop for i of-type fixnum from (- high low 1) downto 0
                when (= 1 (sbit wanted (aref block i))) do (return-from rope-find-any-octet (+ low i))))
        (do-rope-windows (block low high) (rope start end)
          (loop for i of-type fixnum from 0 below (- high low)
                when (= 1 (sbit wanted (aref block i))) do (return-from rope-find-any-octet (+ low i)))))
    nil))

(defparameter *blank-octets* '(32 9 10 13)
  "The octets of the blanks between words and lines: a space, a tab, a newline
and a carriage return.")

(defparameter *text-octets* (loop for octet below 256 unless (member octet *blank-octets*) collect octet)
  "The octets that make a line more than blank: all but the blanks (*BLANK-OCTETS*).")

(defun rope-skip-octets (rope position octets &key backward (bound (if backward 0 (rope-bytes rope))))
  "POSITION of ROPE moved over the octets in OCTETS, a list of them (such as
*BLANK-OCTETS*): forward to the first octet at or after it that is not one of
them, or BOUND (the end of ROPE) when there is none before it; with BACKWARD,
back to just after the last octet before it that is not one of them, or BOUND
\(0). Complexity: O(log n + distance moved)."
  (declare (type fixnum position bound))
  (check-byte-offset rope position)
  (let ((skipped (make-array 256 :element-type 'bit :initial-element 0)))
    (dolist (octet octets) (setf (sbit skipped octet) 1))
    (if backward
        (do-rope-windows (block low high) (rope (min bound position) position :from-end t)
          (loop for i of-type fixnum from (- high low 1) downto 0
                when (zerop (sbit skipped (aref block i))) do (return-from rope-skip-octets (+ low i 1))))
        (do-rope-windows (block low high) (rope position (max bound position))
          (loop for i of-type fixnum from 0 below (- high low)
                when (zerop (sbit skipped (aref block i))) do (return-from rope-skip-octets (+ low i)))))
    bound))

(sb-ext:define-load-time-global **blank-line-octets**
    (let ((table (make-array 256 :element-type 'bit :initial-element 0)))
      (dolist (octet *blank-octets* table)
        (unless (= octet 10) (setf (sbit table octet) 1))))
  "One bit for each octet, 1 for those a blank line may hold: the blanks
\(*BLANK-OCTETS*) but the newline.")

(declaim (inline blank-line-octet-p))
(defun blank-line-octet-p (octet)
  "True for an octet a blank line may hold: a blank (*BLANK-OCTETS*) but the newline."
  (= 1 (sbit (the simple-bit-vector **blank-line-octets**) octet)))

(defun map-line-slices (function rope start end left right &key crlf octets width block)
  "Call FUNCTION for each line of ROPE that starts at a byte offset in [START,
END), START being a line start: with the line's start, the byte offsets of its
LEFT-th and RIGHT-th characters (its end, where it is shorter), the columns
those stand at (the line's length in characters, where it is shorter), and the
line's end: its newline, with CRLF the carriage return before one, or the
text's end. With OCTETS, three more: the block of octets read, where in the
text it starts and where it ends; a line shorter than a block is in it whole,
its newline too. Columns count characters, as TUATARA.BUFFER:BUFFER-COLUMN
does: a raw byte is one. With WIDTH, they are columns on the screen
instead: a printable ASCII character takes one, any other what WIDTH says, a
function of its code, the column it stands at and what it gave as its second
value for the character before (that character's code for a printable ASCII
one, 0 at the line's start) returning the columns and that second value, so
that a tab can reach its stop and a grapheme cluster take the columns of its
first character; a tab across LEFT or
RIGHT is in the slice (its start or end column then outside them), another
character across one out of it, and a character of no width (a combining
mark) goes with the one before it. The text is read once, a block at a time:
as large as the lines asked for, from 4 KB to a megabyte, never more than
the text left, into BLOCK when given octets as large, else into one kept
for reading into (WITH-OCTET-BLOCK), so that nothing is made for it: FUNCTION
must not keep the block. An ASCII octet is a character by itself, so most lines cost a look at
each octet. Complexity: O(END - START + the length of the last line)."
  (declare (type function function) (type fixnum start end left right)
           (optimize speed))
  (let* ((total (rope-bytes rope))
         (size (max (+ 16 (* 4 right))
                    (min (* 1024 1024) (- total start) (max 4096 (* 2 (- (min end total) start))))))
         (block-start 0) (block-end 0)
         (pos start))
    (declare (type fixnum total size block-start block-end pos))
    (unless (and block (>= (length (the octets block)) size))
      (return-from map-line-slices
        (with-octet-block (kept size)
          (map-line-slices function rope start end left right
                           :crlf crlf :octets octets :width width :block kept))))
    (let ((block block))
      (declare (type octets block))
      (flet ((fill-from (at)
               (declare (type fixnum at))
               (setf block-start at
                     block-end (min total (+ at size)))
               (rope-copy-into rope block-start block-end block)))
        (loop while (and (< pos end) (<= pos total))
              do ;; a slice takes at most four octets a character, and a line end two
                 (when (or (< pos block-start) (> (min total (+ pos (* 4 right) 5)) block-end))
                   (fill-from pos))
                 (let ((i (- pos block-start))
                       (avail (- block-end block-start))
                       (column 0) (slice-start -1) (start-column 0) (before 0))
                   (declare (type fixnum i avail column slice-start start-column before))
                   (if (null width)
                       (loop
                         (when (and (= column left) (< slice-start 0))
                           (setf slice-start (+ block-start i) start-column column))
                         (when (or (= column right) (>= i avail)) (return))
                         (let ((b (aref block i)))
                           (when (or (= b 10)
                                     (and crlf (crlf-octets-p block i avail)))
                             (return))
                           (if (< b 128)
                               (incf i)
                               (incf i (the fixnum (nth-value 1 (octets-decode block i avail))))))
                         (incf column))
                       ;; columns on the screen: a tab to its stop, others as WIDTH says; a tab
                       ;; across an edge taken in (its columns made spaces by the caller), another
                       ;; character across one left out, one of no width kept with the one before
                       (loop
                         ;; characters of no width can fill the block before RIGHT: read on, the
                         ;; line from its start where the block began before it
                         (when (and (> (+ i 4) avail) (< block-end total))
                           (let ((at (+ block-start i)))
                             (declare (type fixnum at))
                             (fill-from (if (> pos block-start) pos at))
                             (setf i (- at block-start) avail (- block-end block-start))))
                         (when (>= i avail) (return))
                         (let* ((b (aref block i))
                                (tab (= b 9)))
                           (when (or (= b 10)
                                     (and crlf (crlf-octets-p block i avail)))
                             (return))
                           (multiple-value-bind (w len next)
                               (if (< 31 b 127)
                                   (values 1 1 b)
                                   (multiple-value-bind (code length) (if (< b 128) (values b 1) (octets-decode block i avail))
                                     (multiple-value-bind (w next) (funcall (the function width) code column before)
                                       (values w length next))))
                             (declare (type fixnum w len next))
                             (setf before next)
                             (when (and (< slice-start 0) (plusp w)
                                        (or (>= column left)
                                            (and tab (< column left (+ column w)))))
                               (setf slice-start (+ block-start i) start-column column))
                             (when (and (>= slice-start 0) (plusp w)
                                        (or (>= column right)
                                            (and (not tab) (< column right (+ column w)))))
                               (return))
                             (incf i len)
                             (incf column w)))))
                   (let ((slice-end (+ block-start i)))
                     (declare (type fixnum slice-end))
                     (when (< slice-start 0) (setf slice-start slice-end start-column column))
                     (let* ((newline (let ((from slice-end))
                                       (declare (type fixnum from))
                                       (loop
                                         (let ((hit (position 10 block :start (- from block-start)
                                                                       :end (- block-end block-start))))
                                           (when hit (return (+ block-start (the fixnum hit))))
                                           (when (>= block-end total) (return total))
                                           ;; read on from the line's start, so that a line shorter than a
                                           ;; block is in the block whole; a longer one, from where the search is
                                           (if (> pos block-start)
                                               (fill-from pos)
                                               (progn (setf from block-end) (fill-from from)))))))
                            (line-end (cond ((or (not crlf) (>= newline total) (<= newline slice-end)) newline)
                                            ((> newline block-start)
                                             (+ block-start (line-text-end block 0 (- newline block-start) crlf)))
                                            ((rope-crlf-p rope (1- newline)) (1- newline))
                                            (t newline))))
                       (declare (type fixnum newline line-end))
                       (if octets
                           (funcall function pos slice-start slice-end start-column column line-end
                                    block block-start block-end)
                           (funcall function pos slice-start slice-end start-column column line-end))
                       (setf pos (1+ newline))))))))
    nil))

(defun rope-line-downward (rope start &key blank (limit most-positive-fixnum))
  "The start of the first line of ROPE, from the one starting at byte START
down, that is blank (nothing but spaces, tabs and carriage returns) with
BLANK, or has text without; the end of the text when none is. NIL once LIMIT
octets have been read without an answer. Complexity: O(log n + distance
scanned)."
  (declare (type fixnum start))
  (check-byte-offset rope start)
  (let* ((total (rope-bytes rope))
         (top (min total (+ start limit)))
         (line start)
         (empty t))
    (declare (type fixnum total top line))
    (do-rope-windows (block low high) (rope start top)
      (loop for i of-type fixnum from 0 below (- high low)
            for octet of-type (unsigned-byte 8) = (aref block i)
            do (cond ((= octet 10)
                      (when (and blank empty) (return-from rope-line-downward line))
                      (setf line (+ low i 1) empty t))
                     ((blank-line-octet-p octet))
                     (t (unless blank (return-from rope-line-downward line))
                        (setf empty nil)))))
    (cond ((< top total) nil)
          ((and blank empty (< line total)) line)
          (t total))))

(defun rope-line-upward (rope start &key blank (limit most-positive-fixnum))
  "The start of the first line of ROPE, from the one starting at byte START
up, that is blank (nothing but spaces, tabs and carriage returns) with BLANK,
or has text without. NIL when none is before the start of the text; NIL and
:LIMIT once LIMIT octets have been read without an answer. Complexity:
O(log n + distance scanned)."
  (declare (type fixnum start))
  (check-byte-offset rope start)
  (let* ((total (rope-bytes rope))
         (own-end (or (rope-find-octet rope 10 :start start :end (min total (+ start limit)))
                      (if (< (+ start limit) total) (return-from rope-line-upward (values nil :limit)) total)))
         (wanted-empty (and blank t)))
    (declare (type fixnum total own-end))
    ;; the line at START first
    (when (eq (null (rope-find-any-octet rope *text-octets* :start start :end own-end)) wanted-empty)
      (return-from rope-line-upward start))
    ;; then each line above, read backward from the newline that ends it
    (when (zerop start) (return-from rope-line-upward nil))
    (let ((bottom (max 0 (- start limit))) (empty t))
      (do-rope-windows (block low high) (rope bottom (1- start) :from-end t)
        (loop for i of-type fixnum from (- high low 1) downto 0
              for octet of-type (unsigned-byte 8) = (aref block i)
              do (cond ((= octet 10)
                        (when (eq empty wanted-empty) (return-from rope-line-upward (+ low i 1)))
                        (setf empty t))
                       ((blank-line-octet-p octet))
                       (t (setf empty nil)))))
      (cond ((plusp bottom) (values nil :limit))
            ((eq empty wanted-empty) 0)
            (t nil)))))

(defun rope-line-start (rope byte)
  "Return the byte offset of the start of the line containing byte offset BYTE:
the position after the previous newline, or 0. Complexity: O(log n + the part
of the line before BYTE), and once ROPE's line counts are made no more than
64 KB of it, however long the line: a longer one is found by its number."
  (declare (type fixnum byte))
  (check-byte-offset rope byte)
  (let* ((near (max 0 (- byte 65536)))
         (newline (or (rope-find-octet-backward rope 10 :start near :end byte)
                      (cond ((zerop near) nil)
                            ((rope-indexed-p rope) (1- (line->byte rope (byte->line rope byte))))
                            (t (rope-find-octet-backward rope 10 :end near))))))
    (if newline (1+ newline) 0)))

(defun rope-line-end (rope byte)
  "Return the byte offset of the newline ending the line containing BYTE, or the
text length when the last line has none. Complexity: O(log n + the part of the
line after BYTE), and once ROPE's line counts are made no more than 64 KB of
it, however long the line: a longer one is found by its number."
  (declare (type fixnum byte))
  (check-byte-offset rope byte)
  (let* ((total (rope-bytes rope))
         (near (min total (+ byte 65536))))
    (declare (type fixnum total near))
    (or (rope-find-octet rope 10 :start byte :end near)
        (cond ((= near total) total)
              ((rope-indexed-p rope)
               (let ((line (byte->line rope byte)))
                 (if (< line (rope-lines rope)) (1- (line->byte rope (1+ line))) total)))
              (t (or (rope-find-octet rope 10 :start near) total))))))

;;; Pieces of a text that end at line starts

(defun rope-line-pieces (rope start end size)
  "Pieces of ROPE from START below END, each about SIZE octets and ending at a
line start, or at END: a list of (START END)."
  (declare (type fixnum start end size))
  (let ((pieces '()) (p start))
    (declare (type fixnum p))
    (loop while (< p end)
          do (let* ((nominal (+ p size))
                    (cut (if (>= nominal end)
                             end
                             (let ((newline (rope-find-octet rope 10 :start nominal :end end)))
                               (if newline (1+ newline) end)))))
               (push (list p cut) pieces)
               (setf p cut)))
    (nreverse pieces)))

;;; Edits

(defstruct (edit (:constructor make-edit (start end &optional insert)) (:copier nil))
  "One edit against a text: delete the octets from START below END (byte offsets
in the text the edit applies to) and put INSERT, an octet vector or NIL, in their
place. A pure insertion has START = END; a pure deletion has no INSERT. After
APPLY-EDITS, DELETED holds the octets that were removed, for undo."
  (start 0 :type fixnum)
  (end 0 :type fixnum)
  (insert nil :type (or null octets))
  (deleted nil))

(setf (documentation 'make-edit 'function)
      "Return an EDIT deleting byte range START below END and inserting INSERT (an octet vector or NIL)."
      (documentation 'edit-p 'function) "True when OBJECT is an EDIT."
      (documentation 'edit-start 'function) "Byte offset where EDIT's deleted range starts."
      (documentation 'edit-end 'function) "Byte offset where EDIT's deleted range ends (exclusive)."
      (documentation 'edit-insert 'function) "The octets EDIT inserts, or NIL."
      (documentation 'edit-deleted 'function)
      "The octets EDIT removed, filled in by APPLY-EDITS; NIL before that or when nothing was deleted.")

(declaim (inline edit-insert-length edit-delete-length))
(defun edit-insert-length (edit)
  "Number of octets EDIT inserts."
  (let ((insert (edit-insert edit))) (if insert (length (the octets insert)) 0)))
(defun edit-delete-length (edit)
  "Number of octets EDIT deletes."
  (- (edit-end edit) (edit-start edit)))

(defun check-edits (rope edits)
  "Signal an error unless EDITS (a simple-vector of EDIT) are within ROPE, sorted
by START and non-overlapping. Also clears each edit's DELETED slot, so the same
edit objects can be applied again. Returns true when two edits that both insert
meet (one ends where the next starts, or only deletions right after the one
stand between). Complexity: O(number of edits)."
  (declare (type simple-vector edits))
  (let ((total (rope-bytes rope)) (previous-end -1) (previous-inserts nil) (meeting nil))
    (declare (type fixnum total previous-end))
    (loop for e of-type edit across edits
          do (when (edit-deleted e) (setf (edit-deleted e) nil))
             (unless (and (<= 0 (edit-start e)) (<= (edit-start e) (edit-end e)) (<= (edit-end e) total))
               (error "Edit ~a..~a is outside the text (0..~a) or reversed." (edit-start e) (edit-end e) total))
             (unless (<= previous-end (edit-start e))
               (error "Edits overlap or are not sorted: an edit ends at ~a, the next starts at ~a."
                      previous-end (edit-start e)))
             (when (and previous-inserts (edit-insert e) (= previous-end (edit-start e)))
               (setf meeting t))
             ;; an insert's reach goes on over deletions right after it
             (setf previous-inserts (or (and (edit-insert e) t)
                                        (and previous-inserts (= previous-end (edit-start e))))
                   previous-end (edit-end e)))
    meeting))

(defun rebuild-uncounted-span (node leaf-start edits i j scratch)
  "Apply EDITS[I..J) to NODE, a span whose counts are not known yet (its edges
may fall inside a character), whose text starts at byte LEAF-START, and return
the simple-vector of nodes that replace it: the octets before the first edit
and after the last stay spans of the file, still uncounted, and only the
stretch between, whose edges are the edits' own and so fall between
characters, becomes in-memory leaves with their counts (REBUILD-LEAF).
Complexity: O(the edited stretch + edit sizes)."
  (declare (type span node) (type fixnum leaf-start i j) (type simple-vector edits))
  (let* ((size (node-bytes node))
         (lo (loop for k from i below j minimize (max 0 (- (edit-start (aref edits k)) leaf-start))))
         (hi (loop for k from i below j maximize (min size (- (edit-end (aref edits k)) leaf-start))))
         (hi (max lo hi))
         (middle (let ((octets (make-octets (- hi lo))))
                   (piece-copy node lo hi octets 0)
                   (%make-leaf octets 0 (- hi lo) -1 -1 -1 -1)))
         (mapping (span-mapping node))
         (offset (span-offset node)))
    (concatenate 'simple-vector
                 (if (plusp lo) (vector (%make-span mapping offset lo -1 -1 -1 -1)) #())
                 (rebuild-leaf middle (+ leaf-start lo) edits i j scratch)
                 (if (< hi size) (vector (%make-span mapping (+ offset hi) (- size hi) -1 -1 -1 -1)) #()))))

(defun rebuild-leaf (node leaf-start edits i j scratch)
  "Apply EDITS[I..J) to NODE (a leaf or span), whose text starts at byte
LEAF-START of the whole rope, and return a simple-vector of the in-memory leaves
that replace it (empty when all of its text is deleted). Edits that begin before
LEAF-START are deletions continuing from an earlier leaf; their insertions
belong to the leaf where they began. SCRATCH is a cons whose car is an octet
vector of +MAX-LEAF+ octets that a span's content is copied into, made when
the first span is edited and reused across the batch.
Complexity: O(leaf size + edit sizes)."
  (declare (type fixnum leaf-start i j) (type simple-vector edits) (type cons scratch)
           (optimize speed (safety 1)))
  (when (and (span-p node) (not (node-indexed-p node)))
    (return-from rebuild-leaf (rebuild-uncounted-span node leaf-start edits i j scratch)))
  (let* ((old (if (leaf-p node)
                  (leaf-octets node)
                  (piece-copy node 0 (node-bytes node) (or (car scratch) (setf (car scratch) (make-octets +max-leaf+))) 0)))
         (base (if (leaf-p node) (leaf-start node) 0))
         (old-len (node-bytes node))
         (new-len old-len)
         (indexed (node-indexed-p node))
         (dchars 0) (dutf16 0) (dlines 0) (dinvalid 0)
         ;; Multi-cursor commands share one insertion vector across all edits:
         ;; summarise it once, not once per edit.
         (last-ins nil) (ins-chars 0) (ins-utf16 0) (ins-lines 0) (ins-invalid 0))
    (declare (type octets old)
             (type fixnum base old-len new-len dchars dutf16 dlines dinvalid ins-chars ins-utf16 ins-lines ins-invalid)
             (type (or null octets) last-ins))
    ;; Pass 1: the new length, the count deltas, and the deleted octets for undo.
    (loop for k of-type fixnum from i below j
          for e of-type edit = (aref edits k)
          do (let ((s (max 0 (- (edit-start e) leaf-start)))
                   (en (min old-len (- (edit-end e) leaf-start))))
               (declare (type fixnum s en))
               (when (> en s)
                 (decf new-len (- en s))
                 (when indexed
                   (multiple-value-bind (c u l v) (summarise old (+ base s) (+ base en))
                     (declare (type fixnum c u l v))
                     (decf dchars c) (decf dutf16 u) (decf dlines l) (decf dinvalid v)))
                 (push (subseq old (+ base s) (+ base en)) (edit-deleted e)))
               (when (and (edit-insert e) (>= (edit-start e) leaf-start))
                 (let ((ins (edit-insert e)))
                   (declare (type octets ins))
                   (when indexed
                     (unless (eq ins last-ins)
                       (multiple-value-setq (ins-chars ins-utf16 ins-lines ins-invalid) (summarise ins 0 (length ins)))
                       (setf last-ins ins))
                     (incf dchars ins-chars) (incf dutf16 ins-utf16) (incf dlines ins-lines) (incf dinvalid ins-invalid))
                   (incf new-len (length ins))))))
    (if (zerop new-len)
        #()
        ;; Pass 2: build the new octets in one pass.
        (let ((new (make-octets new-len)) (src 0) (dst 0))
          (declare (type octets new) (type fixnum src dst))
          (loop for k of-type fixnum from i below j
                for e of-type edit = (aref edits k)
                do (let ((s (max 0 (- (edit-start e) leaf-start)))
                         (en (min old-len (- (edit-end e) leaf-start))))
                     (declare (type fixnum s en))
                     (replace new old :start1 dst :start2 (+ base src) :end2 (+ base s))
                     (incf dst (- s src))
                     (setf src en)
                     (when (and (edit-insert e) (>= (edit-start e) leaf-start))
                       (let ((ins (edit-insert e)))
                         (declare (type octets ins))
                         (replace new ins :start1 dst)
                         (incf dst (length ins))))))
          (replace new old :start1 dst :start2 (+ base src) :end2 (+ base old-len))
          (cond ((> new-len +max-leaf+) (chunk-octets new 0 new-len))
                (indexed (vector (%make-leaf new 0 new-len
                                             (+ (node-chars node) dchars)
                                             (+ (node-utf16 node) dutf16)
                                             (+ (node-lines node) dlines)
                                             (+ (node-invalid node) dinvalid))))
                (t (vector (make-leaf new))))))))

(defun merge-small-leaves (leaves)
  "Return LEAVES (a simple-vector of leaves and spans) with adjacent ones merged
into in-memory leaves wherever one of them is smaller than +MIN-LEAF+ and the
pair fits in +MAX-LEAF+. Unindexed spans are left alone."
  (declare (type simple-vector leaves))
  (if (< (length leaves) 2)
      leaves
      (let ((out (make-array 0 :adjustable t :fill-pointer 0))
            (current (aref leaves 0)))
        (loop for k from 1 below (length leaves)
              for next = (aref leaves k)
              do (if (and (node-indexed-p current) (node-indexed-p next)
                          (or (< (node-bytes current) +min-leaf+) (< (node-bytes next) +min-leaf+))
                          (<= (+ (node-bytes current) (node-bytes next)) +max-leaf+))
                     (let* ((a (node-bytes current)) (b (node-bytes next))
                            (merged (make-octets (+ a b))))
                       (piece-copy current 0 a merged 0)
                       (piece-copy next 0 b merged a)
                       (setf current (%make-leaf merged 0 (+ a b)
                                                 (+ (node-chars current) (node-chars next))
                                                 (+ (node-utf16 current) (node-utf16 next))
                                                 (+ (node-lines current) (node-lines next))
                                                 (+ (node-invalid current) (node-invalid next)))))
                     (progn (vector-push-extend current out) (setf current next))))
        (vector-push-extend current out)
        (coerce out 'simple-vector))))

(defun rebuild-branch (branch branch-start edits i j scratch)
  "Apply EDITS[I..J) to BRANCH, whose text starts at byte BRANCH-START, and return
a simple-vector of branches of the same height that replace it (empty when all
text below it is deleted). Untouched children are shared with the old tree.
SCRATCH is passed down to REBUILD-LEAF."
  (declare (type branch branch) (type fixnum branch-start i j) (type simple-vector edits)
           (optimize speed (safety 1)))
  (let* ((children (branch-children branch))
         (last (1- (length children)))
         (new (make-array 0 :adjustable t :fill-pointer 0))
         (child-start branch-start)
         (k i))
    (declare (type fixnum child-start k last))
    (loop for ci from 0 to last
          for child = (aref children ci)
          do (let* ((child-end (+ child-start (node-bytes child)))
                    ;; m: the first edit at or beyond K that starts at or after CHILD-END
                    ;; (binary search, the edits are sorted by start), except that the
                    ;; last child takes everything that is left.
                    (m (if (= ci last)
                           j
                           (tuatara.runtime:bisect (mid k j) (< (edit-start (the edit (aref edits mid))) child-end)))))
               (declare (type fixnum child-end m))
               (if (= k m)
                   (vector-push-extend child new)
                   (loop for node across (if (branch-p child)
                                             (rebuild-branch child child-start edits k m scratch)
                                             (rebuild-leaf child child-start edits k m scratch))
                         do (vector-push-extend node new)))
               ;; Only the last edit of a child can delete past CHILD-END (the edits
               ;; are non-overlapping); if it does, it continues into the next child.
               (setf k (if (and (> m k) (> (edit-end (the edit (aref edits (1- m)))) child-end))
                           (1- m)
                           m))
               (setf child-start child-end)))
    (let ((nodes (coerce new 'simple-vector)))
      (cond ((zerop (length nodes)) #())
            (t (unless (branch-p (aref nodes 0))
                 (setf nodes (merge-small-leaves nodes)))
               (group-into-branches nodes))))))

(defun insert-shape (insert)
  "Two values for INSERT, an octet vector or NIL: whether it begins with a
continuation octet, and how many octets at its end begin a sequence it does
not finish."
  (if (or (null insert) (zerop (length insert)))
      (values nil 0)
      (values (continuation-byte-p (aref insert 0)) (octets-pending insert 0 (length insert)))))

(defun continuation-at-p (rope byte)
  "True when the octet at BYTE of ROPE is a continuation octet; NIL at the end."
  (and (< byte (rope-bytes rope)) (continuation-byte-p (rope-octet rope byte))))

(defun rope-pending-before (rope byte)
  "How many octets just before BYTE of ROPE begin a sequence not finished there (0 to 3)."
  (with-rope-window (window base end) (rope (- byte 3) byte 3)
    (octets-pending window 0 end)))

(defun past-continuations (rope byte)
  "BYTE of ROPE moved past the continuation octets there, at most three, and
on to where a character starts."
  (let ((total (rope-bytes rope)) (end byte))
    (loop while (and (< end total) (< (- end byte) 3) (continuation-at-p rope end)) do (incf end))
    (loop while (and (< end total) (not (char-boundary-p rope end))) do (incf end))
    end))

(defun group-replacement (rope edits members start end)
  "The octets that replace START below END of ROPE for the edits of EDITS whose
indices, in order, are MEMBERS: the old octets around and between their ranges
and their inserts."
  (let ((parts '()) (at start))
    (dolist (k members)
      (let ((e (aref edits k)))
        (when (< at (edit-start e)) (push (rope-octets rope :start at :end (edit-start e)) parts))
        (when (edit-insert e) (push (edit-insert e) parts))
        (setf at (edit-end e))))
    (when (< at end) (push (rope-octets rope :start at :end end) parts))
    (apply #'concatenate 'octets (nreverse parts))))

(defun widen-group (rope edits group)
  "Widen GROUP, a list (START END MEMBERS), in place until neither of its edges
joins the unfinished start of a sequence to octets that continue it: over the
unfinished octets before it, and over the continuation octets after it. True
when it widened."
  (let ((widened nil))
    (loop
      (destructuring-bind (start end members) group
        (let* ((octets (group-replacement rope edits members start end))
               (next-continues (if (plusp (length octets))
                                   (continuation-byte-p (aref octets 0))
                                   (continuation-at-p rope end)))
               (before (if next-continues (rope-pending-before rope start) 0)))
          (when (plusp before)
            (setf (first group) (- start before) widened t
                  octets (group-replacement rope edits members (first group) end)))
          (let ((after (and (plusp (length octets))
                            (plusp (octets-pending octets 0 (length octets)))
                            (continuation-at-p rope end))))
            (when after
              (setf (second group) (past-continuations rope end) widened t))
            (unless (or (plusp before) after)
              (return widened))))))))

(defun joining-inserts-p (edits)
  "True when two edits of EDITS that insert meet, the first's insert ending
with the unfinished start of a sequence and the second's beginning with a
continuation octet: one right after the other, or with only deletions between
them that take away all that stood between."
  (let ((last nil) (reach -1))
    (loop for e across edits
          thereis (and last (edit-insert e) (= reach (edit-start e))
                       (plusp (nth-value 1 (insert-shape (edit-insert last))))
                       (insert-shape (edit-insert e)))
          do (cond ((edit-insert e) (setf last e))
                   ((/= reach (edit-start e)) (setf last nil)))
             (setf reach (edit-end e)))))

(defun widen-edits (rope edits &optional (meeting t))
  "EDITS (a sorted simple-vector) as they are; or, where octets they bring
together could form one character across their edges (possible only where ROPE
holds raw bytes, or where inserts of raw bytes meet), edits grouped and
widened so that each group's octets are counted as the one run they become:
edits within three octets of each other go together, and a group grows over
the unfinished octets before it and the continuation octets after it. Returns
the edits to apply and, when they differ, a vector giving for each of EDITS the
index of the one that holds it and where its own range starts within that one's.
MEETING false says no two edits that insert meet (CHECK-EDITS finds out)."
  (let ((n (length edits)))
    (if (or (zerop n)
            (and (zerop (node-invalid (rope-root rope))) (not (and meeting (joining-inserts-p edits)))))
        (values edits nil)
        (let ((groups '()) (done '()) (changed nil))
          (dotimes (k n)
            (let ((e (aref edits k)) (last (first groups)))
              (if (and last (<= (edit-start e) (+ (second last) 3)))
                  (setf (second last) (max (second last) (edit-end e))
                        (third last) (cons k (third last))
                        changed t)
                  (push (list (edit-start e) (edit-end e) (list k)) groups))))
          ;; each group's edits gathered newest first: in their order again
          (dolist (group groups) (setf (third group) (nreverse (third group))))
          (dolist (group (nreverse groups))
            (when (widen-group rope edits group) (setf changed t))
            (loop while (and done (<= (first group) (+ (second (first done)) 3)))
                  do (let ((previous (pop done)))
                       (setf group (list (first previous) (max (second previous) (second group))
                                         (append (third previous) (third group)))
                             changed t)
                       (widen-group rope edits group)))
            (push group done))
          (if (not changed)
              (values edits nil)
              (let* ((groups (nreverse done))
                     (widened (make-array (length groups)))
                     (owners (make-array n)))
                (loop for (start end members) in groups
                      for g from 0
                      do (let ((octets (group-replacement rope edits members start end)))
                           (dolist (k members)
                             (setf (aref owners k) (cons g (- (edit-start (aref edits k)) start))))
                           (setf (aref widened g) (make-edit start end (and (plusp (length octets)) octets)))))
                (values widened owners)))))))

(defun apply-edits (rope edits)
  "Apply EDITS to ROPE and return two values: the new rope, and a fixnum vector
with, for each edit, the byte offset in the new text just after its insertion
(or at its deletion point). EDITS is a sequence of EDIT objects sorted by START
and non-overlapping; positions are byte offsets in ROPE at character boundaries.
Each edit's DELETED slot is filled with the removed octets for undo. ROPE is
unchanged. Edited file-backed spans become in-memory leaves; the rest of the
file stays mapped. ROPE may be still uncounted, a file freshly opened: a span
whose counts are unknown keeps the octets on either side of its edits as
uncounted spans (REBUILD-UNCOUNTED-SPAN), and the new rope is uncounted too,
for INDEX-ROPE to count, given ROPE counted as donor. Complexity: O(number of
edits + touched leaves + log n)."
  (let* ((edits (coerce edits 'simple-vector))
         (n (length edits)))
    (multiple-value-bind (applied owners) (widen-edits rope edits (check-edits rope edits))
      (let* ((m (length applied))
             (root (rope-root rope))
             (scratch (list nil))
             (nodes (cond ((zerop m) (vector root))
                          ((branch-p root) (rebuild-branch root 0 applied 0 m scratch))
                          (t (rebuild-leaf root 0 applied 0 m scratch))))
             (new-root (if (zerop (length nodes))
                           (make-leaf (make-octets 0))
                           (build-tree nodes)))
             (positions (make-array n :element-type 'fixnum))
             (delta 0))
        (declare (type fixnum m delta))
        ;; Collapse a root with a single child; finish the deleted-octets records.
        (loop while (and (branch-p new-root) (= 1 (length (branch-children new-root))))
              do (setf new-root (aref (branch-children new-root) 0)))
        (loop for e across applied
              do (let ((pieces (edit-deleted e)))
                   (when pieces
                     (setf (edit-deleted e)
                           (if (null (cdr pieces))
                               (car pieces)
                               (apply #'concatenate 'octets (nreverse pieces)))))))
        ;; widened edits hand each edit its own deleted octets
        (when owners
          (loop for k from 0 below n
                for e = (aref edits k)
                do (destructuring-bind (g . offset) (aref owners k)
                     (setf (edit-deleted e)
                           (and (> (edit-end e) (edit-start e))
                                (subseq (edit-deleted (aref applied g)) offset (+ offset (edit-delete-length e))))))))
        (loop for k of-type fixnum from 0 below n
              for e of-type edit = (aref edits k)
              do (setf (aref positions k) (+ (edit-start e) delta (edit-insert-length e)))
                 (incf delta (- (edit-insert-length e) (edit-delete-length e))))
        (values (%make-rope new-root) positions)))))

(defun inverse-edits (edits positions)
  "Return a simple-vector of edits that undo EDITS after they were applied,
given the POSITIONS vector APPLY-EDITS returned. The inverse edits are sorted and
apply to the new text. Complexity: O(number of edits)."
  (let* ((edits (coerce edits 'simple-vector))
         (out (make-array (length edits))))
    (loop for k from 0 below (length edits)
          for e = (aref edits k)
          for after = (aref positions k)
          do (setf (aref out k)
                   (make-edit (- after (edit-insert-length e)) after (edit-deleted e))))
    out))

(defun rope-insert (rope byte text)
  "Return a new rope with TEXT (a string or octet vector) inserted at byte offset
BYTE of ROPE, which must be a character boundary."
  (multiple-value-bind (new positions)
      (apply-edits rope (vector (make-edit byte byte (etypecase text
                                                       (string (string-octets text))
                                                       (octets text)))))
    (declare (ignore positions))
    new))

(defun rope-delete (rope start end)
  "Return a new rope with the octets from START below END of ROPE removed."
  (multiple-value-bind (new positions) (apply-edits rope (vector (make-edit start end)))
    (declare (ignore positions))
    new))

;;; Invariants, for tests

(defun check-rope (rope)
  "Verify the structural invariants of ROPE and return T, or signal an error
naming the first violation: uniform height, fanout bounds, leaf sizes, counts
matching a rescan, and no indexed leaf starting inside a character."
  (labels ((check (node expected-height)
             (unless (= (node-height node) expected-height)
               (error "Node height ~a, expected ~a." (node-height node) expected-height))
             (if (branch-p node)
                 (let ((children (branch-children node)))
                   (unless (<= 1 (length children) +max-fanout+)
                     (error "Branch with ~a children." (length children)))
                   (loop for c across children do (check c (1- expected-height)))
                   (let ((indexed (every #'node-indexed-p children)))
                     (unless (= (node-bytes node) (reduce #'+ children :key #'node-bytes))
                       (error "Branch byte count disagrees with its children."))
                     (if indexed
                         (unless (and (= (node-chars node) (reduce #'+ children :key #'node-chars))
                                      (= (node-utf16 node) (reduce #'+ children :key #'node-utf16))
                                      (= (node-lines node) (reduce #'+ children :key #'node-lines)))
                           (error "Branch counts disagree with its children."))
                         (when (node-indexed-p node)
                           (error "Branch claims counts but has an unindexed child.")))))
                 (progn
                   (when (> (node-bytes node) +max-leaf+)
                     (error "Leaf of ~a octets exceeds ~a." (node-bytes node) +max-leaf+))
                   (when (node-indexed-p node)
                     (multiple-value-bind (c u l v) (piece-summarise node 0 (node-bytes node))
                       (unless (and (= (node-chars node) c) (= (node-utf16 node) u) (= (node-lines node) l)
                                    (= (node-invalid node) v))
                         (error "Leaf counts ~a/~a/~a/~a disagree with rescan ~a/~a/~a/~a."
                                (node-chars node) (node-utf16 node) (node-lines node) (node-invalid node) c u l v))))))))
    (check (rope-root rope) (node-height (rope-root rope)))
    ;; every indexed leaf starts where a character of the whole text starts
    (let ((at 0))
      (map-leaves (lambda (node)
                    (when (and (node-indexed-p node) (plusp at) (not (char-boundary-p rope at)))
                      (error "An indexed leaf starts at ~a, inside a character." at))
                    (incf at (node-bytes node)))
                  rope))
    t))
