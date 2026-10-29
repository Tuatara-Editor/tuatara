;;;; src/dates/zones.lisp --- Time zones by name: the system's zoneinfo files, their table of changes and the rule after it.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; A time zone is a table of changes, universal times at which the offset
;;;; from universal time changes, and a yearly rule for the times after the
;;;; table (the TZ string at the end of a zoneinfo file, RFC 8536). Times here
;;;; are universal times, seconds from 1900-01-01 00:00 UTC; local times are
;;;; counted the same way on the zone's clock, so a local time's day is its
;;;; FLOOR by 86400, as DAY-NUMBER counts days.

(in-package #:tuatara.dates)

(defstruct (zone-rule (:constructor make-zone-rule (standard-name standard-offset &optional daylight-name daylight-offset to-daylight to-standard)))
  "A yearly rule of a time zone: its standard time's name and offset (seconds
east of UTC), and when it has summer time, that time's name and offset and
the days and times it begins and ends. A day is (:MONTH M WEEK WEEKDAY) for
the WEEKth WEEKDAY (0 Sunday) of month M, week 5 the last; (:YEAR-DAY N), 1
to 365, 29 February never counted; or (:DAY N), 0 to 365, counting it. Each
day is followed by the local time of the change, seconds into the day."
  (standard-name "" :type string)
  (standard-offset 0 :type integer)
  (daylight-name nil)
  (daylight-offset nil)
  (to-daylight nil)
  (to-standard nil))

(defstruct (time-zone (:constructor %make-time-zone (name changes offsets first-offset rule)))
  "A time zone: its NAME as the zoneinfo names it (Pacific/Auckland), or NIL;
the universal times its offset changed at, in order, the offset in force
after each, the offset before the first, and the yearly rule after the
last (a ZONE-RULE, or NIL)."
  (name nil)
  (changes #() :type simple-vector)
  (offsets #() :type simple-vector)
  (first-offset 0 :type integer)
  (rule nil))

(defparameter *zoneinfo-largest* (* 1024 1024)
  "The most octets of a zoneinfo file read: a larger file is no time zone.")

(defun fixed-time-zone (offset &optional name)
  "A time zone always OFFSET seconds east of UTC, named NAME."
  (%make-time-zone name #() #() offset (make-zone-rule (or name "") offset)))

(defun ruled-time-zone (name changes offsets first-offset rule)
  "A time zone named NAME of CHANGES, universal times in order, OFFSETS, the
offset after each, FIRST-OFFSET before them, and RULE (a ZONE-RULE or NIL)
after the last."
  (%make-time-zone name (coerce changes 'simple-vector) (coerce offsets 'simple-vector) first-offset rule))

;;; The yearly rule

(defun rule-day (day year)
  "The day number of DAY, as a ZONE-RULE writes one, in YEAR."
  (ecase (first day)
    (:month (destructuring-bind (month week wanted) (rest day)
              (let* ((first (day-number year month 1))
                     (at (+ first (mod (- wanted (weekday first)) 7) (* 7 (1- week)))))
                (loop while (> at (+ first (month-length year month) -1)) do (decf at 7))
                at)))
    (:year-day (let ((n (second day)))
                 (+ (day-number year 1 1) (1- n) (if (and (leap-year-p year) (>= n 60)) 1 0))))
    (:day (+ (day-number year 1 1) (second day)))))

(defun rule-changes (rule year)
  "The universal times RULE's summer time begins and ends in YEAR: two values."
  (destructuring-bind (start-day start-time) (zone-rule-to-daylight rule)
    (destructuring-bind (end-day end-time) (zone-rule-to-standard rule)
      (values (- (+ (* 86400 (rule-day start-day year)) start-time) (zone-rule-standard-offset rule))
              (- (+ (* 86400 (rule-day end-day year)) end-time) (zone-rule-daylight-offset rule))))))

(defun rule-offset (rule time)
  "The offset RULE gives at the universal TIME. O(1)."
  (if (null (zone-rule-to-daylight rule))
      (zone-rule-standard-offset rule)
      (let ((year (day-date (floor (+ time (zone-rule-standard-offset rule)) 86400))))
        (multiple-value-bind (start end) (rule-changes rule year)
          (if (if (< start end)
                  (and (<= start time) (< time end))
                  (not (and (<= end time) (< time start))))
              (zone-rule-daylight-offset rule)
              (zone-rule-standard-offset rule))))))

;;; Offsets

(defun zone-offset (zone time)
  "How many seconds ZONE's clock is ahead of universal time (behind it, below
0) at the universal TIME. O(log changes)."
  (let* ((changes (time-zone-changes zone))
         (count (length changes))
         (rule (time-zone-rule zone)))
    (cond ((or (zerop count) (< time (svref changes 0)))
           (if (and (zerop count) rule) (rule-offset rule time) (time-zone-first-offset zone)))
          ((and rule (>= time (svref changes (1- count))))
           (rule-offset rule time))
          (t
           ;; the last change at or before TIME
           (let ((low 0) (high (1- count)))
             (loop while (< low high)
                   do (let ((middle (ceiling (+ low high) 2)))
                        (if (<= (svref changes middle) time) (setf low middle) (setf high (1- middle)))))
             (svref (time-zone-offsets zone) low))))))

(defun zone-local-time (zone time)
  "The universal TIME as ZONE's clock shows it, counted as a universal time is."
  (+ time (zone-offset zone time)))

(defun zone-universal-time (zone local)
  "The universal time at which ZONE's clock shows LOCAL. A time the clock
shows twice, as summer time ends, is the first; a time it skips, as summer
time begins, is counted in the offset before the change, so it falls that
much after it."
  (let* ((before (zone-offset zone (- local 172800)))
         (offsets (remove-duplicates (list before (zone-offset zone local) (zone-offset zone (+ local 172800)))))
         (times (loop for offset in offsets
                      when (= offset (zone-offset zone (- local offset))) collect (- local offset))))
    (if times (reduce #'min times) (- local before))))

;;; The zoneinfo

(defun zoneinfo-directory ()
  "The directory of the system's zoneinfo files: TZDIR when set, else /usr/share/zoneinfo/."
  (let ((dir (sb-posix:getenv "TZDIR")))
    (if (and dir (plusp (length dir)))
        (if (char= (char dir (1- (length dir))) #\/) dir (concatenate 'string dir "/"))
        "/usr/share/zoneinfo/")))

(defun zone-name-p (name)
  "True when NAME can be a zone's name in the zoneinfo: parts apart by /,
each of letters, digits, _ + and -, none beginning with a dot or a -."
  (and (stringp name) (< 0 (length name) 256)
       (loop for part in (uiop:split-string name :separator "/")
             always (and (plusp (length part))
                         (not (char= (char part 0) #\-))
                         (every (lambda (c) (or (char<= #\a c #\z) (char<= #\A c #\Z) (char<= #\0 c #\9) (find c "_+-")))
                                part)))))

(defun read-zone-rule (text)
  "The ZONE-RULE a TZ string TEXT says (NZST-12NZDT,M9.5.0,M4.1.0/3), or NIL
when it says none this reads."
  (let ((at 0) (end (length text)))
    (labels ((name ()
               (cond ((and (< at end) (char= (char text at) #\<))
                      (let ((close (position #\> text :start at)))
                        (when close (prog1 (subseq text (1+ at) close) (setf at (1+ close))))))
                     (t (let ((stop (or (position-if-not #'alpha-char-p text :start at) end)))
                          (when (>= (- stop at) 3) (prog1 (subseq text at stop) (setf at stop)))))))
             (number ()
               (let ((stop (or (position-if-not #'digit-char-p text :start at) end)))
                 (when (> stop at) (prog1 (parse-integer text :start at :end stop) (setf at stop)))))
             (clock ()
               ;; [+-]hh[:mm[:ss]] as seconds
               (let ((sign (cond ((and (< at end) (char= (char text at) #\-)) (incf at) -1)
                                 ((and (< at end) (char= (char text at) #\+)) (incf at) 1)
                                 (t 1)))
                     (hours (number)))
                 (when hours
                   (let ((seconds (* 3600 hours)))
                     (loop for scale in '(60 1)
                           while (and (< at end) (char= (char text at) #\:))
                           do (incf at)
                              (let ((n (number))) (unless n (return-from clock nil)) (incf seconds (* scale n))))
                     (* sign seconds)))))
             (expect (c) (when (and (< at end) (char= (char text at) c)) (incf at) t))
             (day ()
               (cond ((expect #\M)
                      (let* ((month (number)) (week (and (expect #\.) (number))) (wd (and (expect #\.) (number))))
                        (when (and month week wd (<= 1 month 12) (<= 1 week 5) (<= 0 wd 6))
                          (list :month month week wd))))
                     ((expect #\J) (let ((n (number))) (when (and n (<= 1 n 365)) (list :year-day n))))
                     (t (let ((n (number))) (when (and n (<= 0 n 365)) (list :day n))))))
             (change ()
               (let ((day (day)) (time 7200))
                 (when day
                   (when (expect #\/)
                     (setf time (clock))
                     (unless time (return-from change nil)))
                   (list day time)))))
      (let* ((standard (name)) (standard-clock (and standard (clock))))
        (when standard-clock
          (let ((standard-offset (- standard-clock)))
            (if (= at end)
                (make-zone-rule standard standard-offset)
                (let* ((daylight (name))
                       (daylight-offset (and daylight
                                             (if (or (= at end) (char= (char text at) #\,))
                                                 (+ standard-offset 3600)
                                                 (let ((c (clock))) (and c (- c)))))))
                  (when daylight-offset
                    (let* ((start (and (expect #\,) (change)))
                           (finish (and start (expect #\,) (change))))
                      (when (and finish (= at end))
                        (make-zone-rule standard standard-offset daylight daylight-offset start finish))))))))))))

(defun read-zoneinfo (octets name)
  "The time zone a zoneinfo file's OCTETS hold, named NAME; NIL when they are none (RFC 8536)."
  (let ((length (length octets)))
    (labels ((header-at (at)
               ;; the six counts of a header at AT, or NIL
               (when (and (<= (+ at 44) length)
                          (= (tuatara.io:octets-be octets at 4) #x545A6966)) ; TZif
                 (loop for i below 6 collect (tuatara.io:octets-be octets (+ at 20 (* 4 i)) 4))))
             (signed-at (at count)
               (tuatara.io:octets-signed (tuatara.io:octets-be octets at count) (* 8 count)))
             (block-length (counts size)
               (destructuring-bind (utc-count std-count leap-count time-count type-count char-count) counts
                 (+ (* time-count size) time-count (* type-count 6) char-count (* leap-count (+ size 4)) std-count utc-count))))
      (let ((first (header-at 0)))
        (when first
          (let* ((version (aref octets 4))
                 (two (>= version (char-code #\2)))
                 (data-at (if two (+ 44 (block-length first 4) 44) 44))
                 (counts (if two (header-at (- data-at 44)) first))
                 (size (if two 8 4)))
            (when (and counts (<= (+ data-at (block-length counts size)) length))
              (destructuring-bind (utc-count std-count leap-count time-count type-count char-count) counts
                (declare (ignore utc-count std-count leap-count char-count))
                (when (plusp type-count)
                  (let* ((indices-at (+ data-at (* time-count size)))
                         (types-at (+ indices-at time-count))
                         (type-offsets (coerce (loop for i below type-count collect (signed-at (+ types-at (* 6 i)) 4)) 'simple-vector))
                         (changes (make-array time-count))
                         (offsets (make-array time-count)))
                    (dotimes (i time-count)
                      (let ((index (aref octets (+ indices-at i))))
                        (unless (< index type-count) (return-from read-zoneinfo nil))
                        (setf (svref changes i) (+ +unix-epoch+ (signed-at (+ data-at (* size i)) size))
                              (svref offsets i) (svref type-offsets index))))
                    (let* ((footer-at (+ data-at (block-length counts size)))
                           (rule (when (and two (< footer-at length) (= (aref octets footer-at) 10))
                                   (let ((close (position 10 octets :start (1+ footer-at))))
                                     (when (and close (every (lambda (o) (< 31 o 127)) (subseq octets (1+ footer-at) close)))
                                       (read-zone-rule (map 'string #'code-char (subseq octets (1+ footer-at) close))))))))
                      (%make-time-zone name changes offsets (svref type-offsets 0) rule))))))))))))

(defun zoneinfo-file-octets (path)
  "The octets of the zoneinfo file at PATH, NIL when it cannot be read or is larger than *ZONEINFO-LARGEST*."
  (ignore-errors (tuatara.io:file-octets path :limit *zoneinfo-largest*)))

(defvar *zones* (make-hash-table :test 'equal :synchronized t)
  "The time zones read from the zoneinfo, by name; NIL for a name that is none.")

(defun zone-named (name)
  "The time zone the system's zoneinfo names NAME (Europe/Stockholm, UTC), or
NIL when there is none by that name. Read once, then kept."
  (when (zone-name-p name)
    (multiple-value-bind (zone known) (gethash name *zones*)
      (if known
          zone
          (setf (gethash name *zones*)
                (let ((octets (zoneinfo-file-octets (concatenate 'string (zoneinfo-directory) name))))
                  (and octets (read-zoneinfo octets name))))))))

(defun system-zone-name ()
  "The name of the time zone the system keeps its clock in: TZ when it names
a zone of the zoneinfo, else the zone /etc/localtime leads to; NIL when
neither names one."
  (let ((tz (sb-posix:getenv "TZ")))
    (or (and tz (let ((name (string-left-trim ":" tz))) (and (zone-named name) name)))
        ;; the link followed to its end: a toolbox's leads through /run/host
        (let* ((path (ignore-errors (namestring (truename "/etc/localtime"))))
               (at (and path (search "zoneinfo/" path)))
               (name (and at (subseq path (+ at 9))))
               (name (if (and name (eql 0 (search "posix/" name))) (subseq name 6) name)))
          (and name (zone-name-p name) (zone-named name) name)))))

(defun system-time-zone ()
  "The time zone the system keeps its clock in: by name (SYSTEM-ZONE-NAME),
else as /etc/localtime holds it, else TZ read as a rule, else as far as the
clock's offset now tells it."
  (or (let ((name (system-zone-name))) (and name (zone-named name)))
      (let ((octets (zoneinfo-file-octets "/etc/localtime"))) (and octets (read-zoneinfo octets nil)))
      (let* ((tz (sb-posix:getenv "TZ")) (rule (and tz (read-zone-rule tz))))
        (and rule (%make-time-zone nil #() #() (zone-rule-standard-offset rule) rule)))
      (fixed-time-zone (local-offset (get-universal-time)))))

(setf (documentation 'make-zone-rule 'function) "A ZONE-RULE of STANDARD-NAME and STANDARD-OFFSET, and when it has summer time, DAYLIGHT-NAME, DAYLIGHT-OFFSET and the days and times TO-DAYLIGHT and TO-STANDARD it begins and ends."
      (documentation 'zone-rule-standard-name 'function) "The name of a ZONE-RULE's standard time (CET, NZST)."
      (documentation 'zone-rule-standard-offset 'function) "How many seconds a ZONE-RULE's standard time is ahead of UTC."
      (documentation 'zone-rule-daylight-name 'function) "The name of a ZONE-RULE's summer time (CEST, NZDT), or NIL when it has none."
      (documentation 'zone-rule-daylight-offset 'function) "How many seconds a ZONE-RULE's summer time is ahead of UTC, or NIL when it has none."
      (documentation 'zone-rule-to-daylight 'function) "When a ZONE-RULE's summer time begins: (DAY TIME), DAY as ZONE-RULE says and TIME seconds into it on standard time; NIL when it has none."
      (documentation 'zone-rule-to-standard 'function) "When a ZONE-RULE's summer time ends: (DAY TIME), DAY as ZONE-RULE says and TIME seconds into it on summer time; NIL when it has none."
      (documentation 'time-zone-p 'function) "True when the object given is a TIME-ZONE."
      (documentation 'time-zone-name 'function) "A time zone's name as the zoneinfo names it (Pacific/Auckland), or NIL."
      (documentation 'time-zone-rule 'function) "A time zone's yearly rule after its table of changes, a ZONE-RULE, or NIL.")
